/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
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

#include "CXXR/Context.hpp"
#include "CXXR/ArgMatcher.hpp"
#include "CXXR/Evaluator.h"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/JMPException.hpp"

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
      m_matcher(expose(new ArgMatcher(formal_args))),
      m_body(body), m_environment(env)
{
}

RObject* Closure::apply(const Expression* call, const PairList* args,
			Environment* env)
{
    GCStackRoot<PairList> prepared_args(ArgMatcher::prepareArgs(args, env));
    // +5 to allow some capacity for local variables:
    GCStackRoot<Environment>
	newenv(expose(new Environment(environment(),
				      m_matcher->numFormals() + 5)));
    // Set up environment:
    {
	Context cntxt;
	Rf_begincontext(&cntxt, Context::RETURN, const_cast<Expression*>(call),
			environment(), env, const_cast<PairList*>(args), this);
	m_matcher->match(newenv, prepared_args);
    }
    // Perform evaluation:
    GCStackRoot<> ans;
    {
	RObject* syspar = env;
	// Change syspar if in Context::GENERIC:
	{
	    Context* innerctxt = Context::innermost();
	    if (innerctxt && innerctxt->callflag == Context::GENERIC)
		syspar = innerctxt->sysparent;
	}
	Context cntxt;
	Rf_begincontext(&cntxt, Context::RETURN, const_cast<Expression*>(call),
			newenv, syspar, prepared_args, this);
	newenv->setSingleStepping(m_debug);
	if (m_debug)
	    debug(newenv, call, prepared_args, env);
	bool redo;
	do {
	    redo = false;
	    try {
		ans = Evaluator::evaluate(m_body, newenv);
	    }
	    catch (JMPException& e) {
		// cout << __LINE__ << " Seeking " << e.context << "; in " << &cntxt << endl;
		if (e.context != &cntxt)
		    throw;
		if (R_ReturnedValue == R_RestartToken) {
		    cntxt.callflag = Context::RETURN;  /* turn restart off */
		    R_ReturnedValue = 0;  /* remove restart token */
		    redo = true;
		}
		else ans = R_ReturnedValue;
	    }
	} while (redo);
    }
    Environment::monitorLeaks(ans);
    newenv->maybeDetachFrame();
    return ans;
}

Closure* Closure::clone() const
{
    return expose(new Closure(*this));
}

// Implementation of Closure::debug() is in eval.cpp (for the time
// being).

void Closure::detachReferents()
{
    m_matcher.detach();
    m_body.detach();
    m_environment.detach();
    RObject::detachReferents();
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
