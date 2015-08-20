/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
#include "CXXR/GCStackFrameBoundary.hpp"
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

bool Closure::s_debugging_enabled = true;

Closure::Closure(const PairList* formal_args, RObject* body, Environment* env)
    : FunctionBase(CLOSXP), m_debug(false),
      m_num_invokes(0)
{
    m_matcher = new ArgMatcher(formal_args);
    m_body = body;
    m_environment = env;
}

Closure::~Closure() {
}

RObject* Closure::apply(ArgList* arglist, Environment* env,
			const Expression* call) const
{
    arglist->wrapInPromises(env);
    return invoke(env, arglist, call);
}

Closure* Closure::clone() const
{
    return new Closure(*this);
}

// Implementation of class Closure::DebugScope is in eval.cpp (for the
// time being).

void Closure::detachReferents()
{
    m_matcher.detach();
    m_body.detach();
    m_environment.detach();
    m_compiled_body.detach();
    FunctionBase::detachReferents();
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
    return ans;
}

RObject* Closure::invoke(Environment* env, const ArgList* arglist,
			 const Expression* call,
			 const Frame* method_bindings) const
{
    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() { return invokeImpl(env, arglist, call, method_bindings); });
}

RObject* Closure::invokeImpl(Environment* env, const ArgList* arglist,
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
	new ListFrame);
    GCStackRoot<Environment>
	newenv(new Environment(environment(), newframe));
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
	    method_bindings->visitBindings([&](const Frame::Binding* binding) {
		    const Symbol* sym = binding->symbol();
		    if (!newframe->binding(sym)) {
			newframe->importBinding(binding);
		    }
		});
	    FunctionContext* fctxt = FunctionContext::innermost();
	    while (fctxt && fctxt->function()->sexptype() == SPECIALSXP)
		fctxt = FunctionContext::innermost(fctxt->nextOut());
	    syspar = (fctxt ? fctxt->callEnvironment() : Environment::global());
	}
	ClosureContext cntxt(const_cast<Expression*>(call),
			     syspar, this, newenv, arglist->list());
	ans = execute(newenv);
    }
    Environment::monitorLeaks(ans);
    newenv->maybeDetachFrame();

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
    const GCNode* compiled_body = m_compiled_body;

    FunctionBase::visitReferents(v);
    if (matcher)
	(*v)(matcher);
    if (body)
	(*v)(body);
    if (environment)
	(*v)(environment);
    if (compiled_body)
	(*v)(compiled_body);
}
