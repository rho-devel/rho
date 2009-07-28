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

/** @file Promise.cpp
 *
 * @brief Implementation of class CXXR::Promise and associated C
 * interface.
 */

#include "CXXR/Promise.h"

#include "CXXR/GCStackRoot.h"

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

void Promise::setValue(RObject* val)
{
    m_value = val;
    if (val != Symbol::unboundValue())
	m_environment = 0;
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
    if (value) value->conductVisitor(v);
    if (valgen) valgen->conductVisitor(v);
    if (env) env->conductVisitor(v);
}

// ***** C interface *****

SEXP Rf_mkPROMISE(SEXP expr, SEXP rho)
{
    GCStackRoot<> exprt(expr);
    GCStackRoot<Environment> rhort(SEXP_downcast<Environment*>(rho));
    return GCNode::expose(new Promise(exprt, *rhort));
}

void SET_PRVALUE(SEXP x, SEXP v)
{
    Promise* prom = SEXP_downcast<Promise*>(x);
    prom->setValue(v);
}
