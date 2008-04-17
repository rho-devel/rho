/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*PRCODEp)(SEXP x) = PRCODE;
	SEXP (*PRENVp)(SEXP x) = PRENV;
	int (*PRSEENp)(SEXP x) = PRSEEN;
	SEXP (*PRVALUEp)(SEXP x) = PRVALUE;
	void (*SETPRSEENp)(SEXP x, int v) = SET_PRSEEN;
	void (*SET_PRVALUEp)(SEXP x, SEXP v) = SET_PRVALUE;
    }
}

void Promise::setValue(RObject* val)
{
    m_value = val;
    devolveAge(m_value);
    if (val != SpecialSymbol::unboundValue())
	m_environment = 0;
}

const char* Promise::typeName() const
{
    return staticTypeName();
}

void Promise::visitChildren(const_visitor* v) const
{
    RObject::visitChildren(v);
    if (m_value) m_value->conductVisitor(v);
    if (m_valgen) m_valgen->conductVisitor(v);
    if (m_environment) m_environment->conductVisitor(v);
}

// ***** C interface *****

SEXP Rf_mkPROMISE(SEXP expr, SEXP rho)
{
    GCRoot<> exprt(expr);
    GCRoot<Environment> rhort(SEXP_downcast<Environment*>(rho));
    return new Promise(exprt, *rhort);
}
