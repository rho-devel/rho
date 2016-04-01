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

/** @file ListVector.cpp
 *
 * Implementation of class ListVector and related functions.
 *
 * @todo Tidy up handling of names attribute, in particular to get rid
 * of <tt>const_cast</tt>.
 */

#include "rho/ListVector.hpp"
#include "rho/ExpressionVector.hpp"

using namespace rho;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace rho {
    namespace ForceNonInline {
	SEXP (*VECTOR_ELTp)(const SEXP x, R_xlen_t i) = VECTOR_ELT;
    }

    template <>
    const char* ListVector::staticTypeName() {
	return "list";
    }
}

// ***** C interface *****
SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %lu/%lu in SET_VECTOR_ELT"),
	      i, XLENGTH(x));

    /*  we need to allow vector-like types here */
    if(TYPEOF(x) == VECSXP) {
	ListVector* lv = SEXP_downcast<ListVector*>(x, false);
	(*lv)[i] = v;
	return v;
    } else if (TYPEOF(x) == EXPRSXP) {
	ExpressionVector* ev = SEXP_downcast<ExpressionVector*>(x, false);
	(*ev)[i] = v;
	return v;
    } else {
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_VECTOR_ELT", "list", Rf_type2char(TYPEOF(x)));
    }
}
