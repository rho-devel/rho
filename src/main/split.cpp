/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2006-2015 The R Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <Internal.h>
#include <R_ext/Itermacros.h>

SEXP attribute_hidden do_split(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* f_)
{
    SEXP x, f, counts, vec, nm, nmj;
    Rboolean have_names;

    x = x_;
    f = f_;
    if (!isVector(x))
	error(_("first argument must be a vector"));
    if (!isFactor(f))
	error(_("second argument must be a factor"));
    int nlevs = nlevels(f);
    R_xlen_t nfac = XLENGTH(f_);
    R_xlen_t nobs = XLENGTH(x_);
    if (nfac <= 0 && nobs > 0)
	error(_("group length is 0 but data length > 0"));
    if (nfac > 0 && (nobs % nfac) != 0)
	warning(_("data length is not a multiple of split variable"));
    nm = getAttrib(x, R_NamesSymbol);
    have_names = CXXRCONSTRUCT(Rboolean, nm != nullptr);
    PROTECT(counts = allocVector(INTSXP, nlevs));
    for (int i = 0; i < nlevs; i++) INTEGER(counts)[i] = 0;
    R_xlen_t i, i1;
    MOD_ITERATE1(nobs, nfac, i, i1, {
	int j = INTEGER(f)[i1];
	if (j != NA_INTEGER) {
	    /* protect against malformed factors */
	    if (j > nlevs || j < 1) error(_("factor has bad level"));
	    INTEGER(counts)[j - 1]++;
	}
    });
    /* Allocate a generic vector to hold the results. */
    /* The i-th element will hold the split-out data */
    /* for the ith group. */
    PROTECT(vec = allocVector(VECSXP, nlevs));
    for (R_xlen_t i = 0;  i < nlevs; i++) {
	SET_VECTOR_ELT(vec, i, allocVector(TYPEOF(x), INTEGER(counts)[i]));
	setAttrib(VECTOR_ELT(vec, i), R_LevelsSymbol,
		  getAttrib(x, R_LevelsSymbol));
	if(have_names)
	    setAttrib(VECTOR_ELT(vec, i), R_NamesSymbol,
		      allocVector(STRSXP, INTEGER(counts)[i]));
    }
    for (int i = 0; i < nlevs; i++) INTEGER(counts)[i] = 0;
    MOD_ITERATE1(nobs, nfac, i, i1, {
	int j = INTEGER(f)[i1];
	if (j != NA_INTEGER) {
	    int k = INTEGER(counts)[j - 1];
	    switch (TYPEOF(x)) {
	    case LGLSXP:
	    case INTSXP:
		INTEGER(VECTOR_ELT(vec, j - 1))[k] = INTEGER(x)[i];
		break;
	    case REALSXP:
		REAL(VECTOR_ELT(vec, j - 1))[k] = REAL(x)[i];
		break;
	    case CPLXSXP:
		COMPLEX(VECTOR_ELT(vec, j - 1))[k] = COMPLEX(x)[i];
		break;
	    case STRSXP:
		SET_STRING_ELT(VECTOR_ELT(vec, j - 1), k, STRING_ELT(x, i));
		break;
	    case VECSXP:
		SET_VECTOR_ELT(VECTOR_ELT(vec, j - 1), k, VECTOR_ELT(x, i));
		break;
	    case RAWSXP:
		RAW(VECTOR_ELT(vec, j - 1))[k] = RAW(x)[i];
		break;
	    default:
		UNIMPLEMENTED_TYPE("split", x);
	    }
	    if(have_names) {
		nmj = getAttrib(VECTOR_ELT(vec, j - 1), R_NamesSymbol);
		SET_STRING_ELT(nmj, k, STRING_ELT(nm, i));
	    }
	    INTEGER(counts)[j - 1] += 1;
	}
    });
    setAttrib(vec, R_NamesSymbol, getAttrib(f, R_LevelsSymbol));
    UNPROTECT(2);
    return vec;
}
