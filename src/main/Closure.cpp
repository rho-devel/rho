/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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

#include "RCNTXT.h"
#include "CXXR/ArgMatcher.hpp"
#include "CXXR/Evaluator.h"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.h"
#include "CXXR/JMPException.hpp"

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*BODYp)(SEXP x) = BODY;
	Rboolean (*DEBUGp)(SEXP x) = DEBUG;
	SEXP (*CLOENVp)(SEXP x) = CLOENV;
	SEXP (*FORMALSp)(SEXP x) = FORMALS;
	void (*SET_CLOENVp)(SEXP x, SEXP v) = SET_CLOENV;
	void (*SET_DEBUGp)(SEXP x, Rboolean v) = SET_DEBUG;
    }
}

Closure::Closure(PairList* formal_args, const RObject* body,
		 Environment* env)
    : FunctionBase(CLOSXP), m_debug(false),
      m_matcher(expose(new ArgMatcher(formal_args))),
      m_body(body), m_environment(env)
{
}

RObject* Closure::apply(Expression* call, PairList* args, Environment* env)
{
    GCStackRoot<PairList> prepared_args(ArgMatcher::prepareArgs(args, env));
    // +5 to allow some capacity for local variables:
    GCStackRoot<Environment>
	newenv(expose(new Environment(environment(),
				      m_matcher->numFormals() + 5)));
    // Set up environment:
    {
	RCNTXT cntxt;
	Rf_begincontext(&cntxt, CTXT_RETURN, call,
			environment(), env, args, this);
	m_matcher->match(newenv, prepared_args);
	Rf_endcontext(&cntxt);
    }
    // Perform evaluation:
    GCStackRoot<> ans;
    {
	RCNTXT cntxt;
	if (R_GlobalContext->callflag == CTXT_GENERIC)
	    Rf_begincontext(&cntxt, CTXT_RETURN, call, newenv,
			    R_GlobalContext->sysparent, prepared_args, this);
	else
	    Rf_begincontext(&cntxt, CTXT_RETURN, call,
			    newenv, env, prepared_args, this);
	// ***** FIXME: add debugging logic here *****
	bool redo;
	do {
	    redo = false;
	    try {
		ans = Evaluator::evaluate(const_cast<RObject*>(m_body.get()),
					  newenv);
	    }
	    catch (JMPException& e) {
		// cout << __LINE__ << " Seeking " << e.context << "; in " << &cntxt << endl;
		if (e.context != &cntxt)
		    throw;
		if (R_ReturnedValue == R_RestartToken) {
		    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		    R_ReturnedValue = 0;  /* remove restart token */
		    redo = true;
		}
		else ans = R_ReturnedValue;
	    }
	} while (redo);
	Rf_endcontext(&cntxt);
    }
    return ans;
}

Closure* Closure::clone() const
{
    return expose(new Closure(*this));
}

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
	matcher->conductVisitor(v);
    if (body)
	body->conductVisitor(v);
    if (environment)
	environment->conductVisitor(v);
}
