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

/** @file ListVector.cpp
 *
 * Implementation of class ListVector and related functions.
 *
 * @todo Tidy up handling of names attribute, in particular to get rid
 * of <tt>const_cast</tt>.
 */

#include "CXXR/ListVector.h"

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*VECTOR_ELTp)(const SEXP x, R_xlen_t i) = VECTOR_ELT;
    }
}

// ***** C interface *****

SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    ListVector* lv = SEXP_downcast<ListVector*>(x, false);
    (*lv)[i] = v;
    return v;
}
