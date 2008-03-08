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

/** @file RObject.cpp
 *
 * Class RObject and associated C interface functions.
 */

// Needed while visitChildren(visitor* v) is unimplemented.
#include <iostream>

#include "CXXR/RObject.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*ATTRIBptr)(SEXP e) = ATTRIB;
	Rboolean (*isNullptr)(SEXP s) = Rf_isNull;
	Rboolean (*isObjectptr)(SEXP s) = Rf_isObject;
	int (*LEVELSptr)(SEXP x) = LEVELS;
	int (*NAMEDptr)(SEXP x) = NAMED;
	Rboolean (*OBJECTptr)(SEXP e) = OBJECT;
	int (*SETLEVELSptr)(SEXP x, int v) = SETLEVELS;
	void (*SET_NAMEDptr)(SEXP x, int v) = SET_NAMED;
	void (*SET_OBJECTptr)(SEXP x, int v) = SET_OBJECT;
	void (*SET_TRACEptr)(SEXP x, int v) = SET_TRACE;
	void (*SET_TYPEOFptr)(SEXP x, SEXPTYPE v) = SET_TYPEOF;
	int (*TRACEptr)(SEXP x) = TRACE;
	SEXPTYPE (*TYPEOFptr)(SEXP e) = TYPEOF;
	void (*SET_S4_OBJECTptr)(SEXP x) = SET_S4_OBJECT;
	void (*UNSET_S4_OBJECTptr)(SEXP x) = UNSET_S4_OBJECT;
	Rboolean (*BINDING_IS_LOCKEDptr)(SEXP b) = BINDING_IS_LOCKED;
	Rboolean (*IS_ACTIVE_BINDINGptr)(SEXP b) = IS_ACTIVE_BINDING;
	void (*LOCK_BINDINGptr)(SEXP b) = LOCK_BINDING;
	void (*SET_ACTIVE_BINDING_BITptr)(SEXP b) = SET_ACTIVE_BINDING_BIT;
	void (*UNLOCK_BINDINGptr)(SEXP b) = UNLOCK_BINDING;
    }
}

const char*  RObject::typeName() const
{
    return Rf_type2char(sexptype());
}

void RObject::visitChildren(const_visitor* v) const
{
    if (m_attrib) m_attrib->conductVisitor(v);
    switch (sexptype()) {
    case ENVSXP:
	if (frame()) frame()->conductVisitor(v);
	if (enclosingEnvironment())
	    enclosingEnvironment()->conductVisitor(v);
	if (hashTable()) hashTable()->conductVisitor(v);
	break;
    case CLOSXP:
    case PROMSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case SYMSXP:
    case BCODESXP:
	if (tag()) tag()->conductVisitor(v);
	if (car()) car()->conductVisitor(v);
	if (cdr()) cdr()->conductVisitor(v);
	break;
    default:
	break;
    }
}

void RObject::visitChildren(visitor* v)
{
    cerr << "RObject::visitChildren(visitor* v) not implemented yet.\n";
    abort();
}

