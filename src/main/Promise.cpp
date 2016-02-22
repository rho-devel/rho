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
 *  https://www.R-project.org/Licenses/
 */

/** @file Promise.cpp
 *
 * @brief Implementation of class CXXR::Promise and associated C
 * interface.
 */

#include "CXXR/Promise.h"

#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/Bailout.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/PlainContext.hpp"
#include "CXXR/StackChecker.hpp"

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*PRCODEp)(SEXP x) = PRCODE;
	SEXP (*PRENVp)(SEXP x) = PRENV;
	SEXP (*PRVALUEp)(SEXP x) = PRVALUE;
    }
}

void Promise::detachReferents()
{
    m_value.detach();
    m_valgen.detach();
    m_environment.detach();
    RObject::detachReferents();
}

RObject* Promise::evaluate(Environment* /*env*/)
{
    if (m_value == Symbol::unboundValue()) {
	// Force promise:
	if (m_interrupted) {
	    Rf_warning(_("restarting interrupted promise evaluation"));
	    m_interrupted = false;
	}
	else if (m_under_evaluation)
	    Rf_error(_("promise already under evaluation: "
		       "recursive default argument reference "
		       "or earlier problems?"));
	m_under_evaluation = true;
	try {
	    IncrementStackDepthScope scope;
	    PlainContext cntxt;
	    RObject* val = Evaluator::evaluate(
		const_cast<RObject*>(m_valgen.get()),
		environment());
	    setValue(val);
	}
	catch (...) {
	    m_interrupted = true;
	    throw;
	}
	m_under_evaluation = false;
    }
    return value();
}

bool Promise::isMissingSymbol() const
{
    bool ans = false;
    /* This is wrong but I'm not clear why - arr
    if (m_value == Symbol::missingArgument())
     	return true;
    */
    if (m_value == Symbol::unboundValue() && m_valgen) {
	RObject* prexpr = PREXPR(const_cast<Promise*>(this));
	if (prexpr->sexptype() == SYMSXP) {
	    // According to Luke Tierney's comment to R_isMissing() in CR,
	    // if a cycle is found then a missing argument has been
	    // encountered, so the return value is true.
	    if (m_under_evaluation)
		return true;
	    try {
		const Symbol* promsym
		    = static_cast<const Symbol*>(prexpr);
		m_under_evaluation = true;
		ans = isMissingArgument(promsym, environment()->frame());
	    }
	    catch (...) {
		m_under_evaluation = false;
		throw;
	    }
	    m_under_evaluation = false;
	}
    }
    return ans;
}

void Promise::setValue(RObject* val)
{
    m_value = val;
    SET_NAMED(val, 2);
    if (val != Symbol::unboundValue())
	m_environment = nullptr;
}

const char* Promise::typeName() const
{
    return staticTypeName();
}

void Promise::visitReferents(const_visitor* v) const
{
    const GCNode* value = m_value;
    const GCNode* valgen = m_valgen;
    const GCNode* env = m_environment;
    RObject::visitReferents(v);
    if (value)
	(*v)(value);
    if (valgen)
	(*v)(valgen);
    if (env)
	(*v)(env);
}

// ***** C interface *****

SEXP Rf_mkPROMISE(SEXP expr, SEXP rho)
{
    GCStackRoot<> exprt(expr);
    GCStackRoot<Environment> rhort(SEXP_downcast<Environment*>(rho));
    return new Promise(exprt, rhort);
}

SEXP R_mkEVPROMISE(SEXP expr, SEXP value)
{
    return Promise::createEvaluatedPromise(
	SEXP_downcast<Expression*>(expr), value);
}

void SET_PRVALUE(SEXP x, SEXP v)
{
    Promise* prom = SEXP_downcast<Promise*>(x);
    prom->setValue(v);
}
