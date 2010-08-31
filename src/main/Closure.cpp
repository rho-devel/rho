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

#include <cstdlib>
#include "CXXR/ArgMatcher.hpp"
#include "CXXR/BailoutContext.hpp"
#include "CXXR/ClosureContext.hpp"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/ReturnBailout.hpp"
#include "CXXR/ReturnException.hpp"

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
        ClosureContext cntxt(const_cast<Expression*>(call), env, this,
			     environment(), const_cast<PairList*>(args));
	m_matcher->match(newenv, prepared_args);
    }
    // Perform evaluation:
    GCStackRoot<> ans;
    {
	Environment* syspar = env;
	// Change syspar if generic:
	{
	    ClosureContext* innercctxt = ClosureContext::innermost();
	    if (innercctxt && innercctxt->isGeneric())
		syspar = innercctxt->callEnvironment();
	}
	ClosureContext cntxt(const_cast<Expression*>(call),
			     syspar, this, newenv, prepared_args);
	Environment::ReturnScope returnscope(newenv);
	newenv->setSingleStepping(m_debug);
	if (m_debug)
	    debug(newenv, call, prepared_args, env);
	try {
	    {
		BailoutContext boctxt;
		ans = Evaluator::evaluate(m_body, newenv);
	    }
	    if (ans && ans->sexptype() == BAILSXP) {
		ReturnBailout* rbo = dynamic_cast<ReturnBailout*>(ans.get());
		if (!rbo || rbo->environment() != newenv)
		    abort();
		R_Visible = Rboolean(rbo->printResult());
		ans = rbo->value();
	    }
	}
	catch (ReturnException& rx) {
	    if (rx.environment() != newenv)
		throw;
	    ans = rx.value();
	}
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
