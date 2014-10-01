/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file Closure.cpp
 *
 * @brief Implementation of class CXXR::Closure and associated C
 * interface.
 */

#include "CXXR/Closure.h"

#include <cstdlib>
#include "CXXR/ArgList.hpp"
#include "CXXR/ArgMatcher.hpp"
#include "CXXR/BailoutContext.hpp"
#include "CXXR/ClosureContext.hpp"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/ListFrame.hpp"
#include "CXXR/PlainContext.hpp"
#include "CXXR/ReturnBailout.hpp"
#include "CXXR/ReturnException.hpp"
#include "CXXR/errors.h"
#include "CXXR/jit/CompiledExpression.hpp"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*BODYp)(SEXP x) = BODY;
	SEXP (*CLOENVp)(SEXP x) = CLOENV;
	SEXP (*FORMALSp)(SEXP x) = FORMALS;
	Rboolean (*RDEBUGp)(SEXP x) = RDEBUG;
	int (*RSTEPp)(SEXP x) = RSTEP;
	void (*SET_CLOENVp)(SEXP x, SEXP v) = SET_CLOENV;
	void (*SET_RDEBUGp)(SEXP x, Rboolean v) = SET_RDEBUG;
	void (*SET_RSTEPp)(SEXP x, int v) = SET_RSTEP;
    }
}

Closure::Closure(const PairList* formal_args, RObject* body, Environment* env)
    : FunctionBase(CLOSXP), m_debug(false),
      m_num_invokes(0), m_compiled_body(0),
      m_matcher(expose(new ArgMatcher(formal_args))),
      m_body(body), m_environment(env)
{
}

Closure::~Closure() {
#ifdef ENABLE_LLVM_JIT
    if (m_compiled_body) {
	delete m_compiled_body;
    }
#endif
}

RObject* Closure::apply(ArgList* arglist, Environment* env,
			const Expression* call) const
{
    arglist->wrapInPromises(env);
    return invoke(env, arglist, call);
}

Closure* Closure::clone() const
{
    return expose(new Closure(*this));
}

// Implementation of class Closure::DebugScope is in eval.cpp (for the
// time being).

void Closure::detachReferents()
{
    m_matcher.detach();
    m_body.detach();
    m_environment.detach();
    RObject::detachReferents();
}

RObject* Closure::execute(Environment* env) const
{
    RObject* ans;
    Evaluator::maybeCheckForUserInterrupts();
    Environment::ReturnScope returnscope(env);
    Closure::DebugScope debugscope(this);
    try {
	++m_num_invokes;
#ifdef ENABLE_LLVM_JIT
	if (m_compiled_body
	    && m_compiled_body->hasMatchingFrameLayout(env)) {
	    PlainContext boctxt;
	    ans = m_compiled_body->evalInEnvironment(env);
	} else {
	    if (!m_compiled_body && m_num_invokes == 100) {
		// Compile the body, but stay in the interpreter because the
		// frame hasn't been setup for a compiled function.
		// TODO(kmillar): recompile functions as needed.
		compile();
	    }
#endif
	    BailoutContext boctxt;
	    ans = Evaluator::evaluate(m_body, env);
#ifdef ENABLE_LLVM_JIT
	}
#endif
	if (ans && ans->sexptype() == BAILSXP) {
	    ReturnBailout* rbo = dynamic_cast<ReturnBailout*>(ans);
	    if (!rbo || rbo->environment() != env)
		abort();
	    R_Visible = Rboolean(rbo->printResult());
	    ans = rbo->value();
	}
    }
    catch (ReturnException& rx) {
	if (rx.environment() != env)
	    throw;
	ans = rx.value();
    }
    Environment::monitorLeaks(ans);
    env->maybeDetachFrame();
    return ans;
}

RObject* Closure::invoke(Environment* env, const ArgList* arglist,
			 const Expression* call,
			 const Frame* method_bindings) const
{
#ifndef NDEBUG
    if (arglist->status() != ArgList::PROMISED)
	Rf_error("Internal error: unwrapped arguments to Closure::invoke");
#endif
    GCStackRoot<Frame> newframe(
#ifdef ENABLE_LLVM_JIT
	m_compiled_body ? m_compiled_body->createFrame() :
#endif
	CXXR_NEW(ListFrame));
    GCStackRoot<Environment>
	newenv(CXXR_NEW(Environment(environment(), newframe)));
    // Perform argument matching:
    {
        ClosureContext cntxt(const_cast<Expression*>(call), env, this,
			     environment(), arglist->list());
	m_matcher->match(newenv, arglist);
    }

    // Set up context and perform evaluation.  Note that ans needs to
    // be protected in case the destructor of ClosureContext executes
    // an on.exit function.
    GCStackRoot<> ans;
    {
	Environment* syspar = env;
	// If this is a method call, change syspar and merge in
	// supplementary bindings:
	if (method_bindings) {
	    Frame::BindingRange mbrange = method_bindings->bindingRange();
	    for (Frame::BindingRange::const_iterator it = mbrange.begin();
		 it != mbrange.end(); ++it) {
		const Frame::Binding& mbbdg = *it;
		const Symbol* sym = mbbdg.symbol();
		if (!newframe->binding(sym)) {
		    newframe->importBinding(&mbbdg);
		}
	    }
	    FunctionContext* fctxt = FunctionContext::innermost();
	    while (fctxt && fctxt->function()->sexptype() == SPECIALSXP)
		fctxt = FunctionContext::innermost(fctxt->nextOut());
	    syspar = (fctxt ? fctxt->callEnvironment() : Environment::global());
	}
	ClosureContext cntxt(const_cast<Expression*>(call),
			     syspar, this, newenv, arglist->list());
	ans = execute(newenv);
    }
    return ans;
}

void Closure::compile() const {
#ifdef ENABLE_LLVM_JIT
    try {
	m_compiled_body = JIT::CompiledExpression::compileFunctionBody(this);
    } catch (...) {
	// Compilation failed.  Continue on with the interpreter.
    }
#endif
}

const char* Closure::typeName() const
{
    return staticTypeName();
}

void Closure::visitReferents(const_visitor* v) const
{
    const ArgMatcher* matcher = m_matcher;
    const GCNode* body = m_body;
    const GCNode* environment = m_environment;
    RObject::visitReferents(v);
    if (matcher)
	(*v)(matcher);
    if (body)
	(*v)(body);
    if (environment)
	(*v)(environment);
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::Closure)
