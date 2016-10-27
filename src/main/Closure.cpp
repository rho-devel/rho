/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
 */

/** @file Closure.cpp
 *
 * @brief Implementation of class Closure and associated C
 * interface.
 */

#include "rho/Closure.hpp"

#include <cstdlib>
#include "rho/ArgList.hpp"
#include "rho/ArgMatcher.hpp"
#include "rho/BailoutContext.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/Expression.hpp"
#include "rho/GCStackFrameBoundary.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/Frame.hpp"
#include "rho/PlainContext.hpp"
#include "rho/ReturnBailout.hpp"
#include "rho/ReturnException.hpp"
#include "rho/errors.hpp"
#include "rho/jit/CompiledExpression.hpp"

using namespace std;
using namespace rho;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace rho {
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

void Closure::compile() const {
#ifdef ENABLE_LLVM_JIT
    try {
	m_compiled_body = JIT::CompiledExpression::compileFunctionBody(this);
    } catch (...) {
	// Compilation failed.  Continue on with the interpreter.
    }
#endif
}

Environment* Closure::createExecutionEnv() const {
    Frame* frame =
#ifdef ENABLE_LLVM_JIT
        m_compiled_body ? m_compiled_body->createFrame():
#endif
        new Frame;
    return new Environment(environment(), frame);
}

const char* Closure::typeName() const
{
    return staticTypeName();
}

void Closure::setEnvironment(Environment* new_env) {
    m_environment = new_env;
    invalidateCompiledCode();
}

void Closure::setFormals(PairList* formals) {
    m_matcher = new ArgMatcher(formals);
    invalidateCompiledCode();
}

void Closure::setBody(RObject* body) {
    m_body = body;
    invalidateCompiledCode();
}

void Closure::invalidateCompiledCode() {
    m_num_invokes = 0;
    m_compiled_body = nullptr;
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

void SET_FORMALS(SEXP closure, SEXP formals) {
    SEXP_downcast<Closure*>(closure)->setFormals(
	SEXP_downcast<PairList*>(formals));
}

void SET_BODY(SEXP closure, SEXP body) {
    SEXP_downcast<Closure*>(closure)->setBody(body);
}
