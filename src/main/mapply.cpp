/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2015  The R Core Team
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
 *
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

#include "CXXR/RAllocStack.h"

SEXP attribute_hidden
do_mapply(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP f = args[0], varyingArgs = args[1], constantArgs = args[2];
    int m, zero = 0;
    R_xlen_t *lengths, *counters, longest = 0;

    m = length(varyingArgs);
    SEXP vnames = PROTECT(getAttrib(varyingArgs, R_NamesSymbol));
    Rboolean named = CXXRCONSTRUCT(Rboolean, vnames != nullptr);

    lengths = static_cast<R_xlen_t *>(  CXXR_alloc(m, sizeof(R_xlen_t)));
    for (int i = 0; i < m; i++) {
	SEXP tmp1 = VECTOR_ELT(varyingArgs, i);
	lengths[i] = get_object_length(tmp1, rho);
	if (lengths[i] == 0) zero++;
	if (lengths[i] > longest) longest = lengths[i];
    }
    if (zero && longest)
	error(_("zero-length inputs cannot be mixed with those of non-zero length"));

    counters = static_cast<R_xlen_t *>( CXXR_alloc(m, sizeof(R_xlen_t)));
    if (m) memset(counters, 0, m * sizeof(R_xlen_t));

    SEXP mindex = PROTECT(allocVector(VECSXP, m));
    SEXP nindex = PROTECT(allocVector(VECSXP, m));

    /* build a call like
       f(dots[[1]][[4]], dots[[2]][[4]], dots[[3]][[4]], d=7)
    */

    SEXP fcall = R_NilValue; // -Wall
    if (constantArgs == R_NilValue)
	;
    else if (isVectorList(constantArgs))
	fcall = VectorToPairList(constantArgs);
    else
	error(_("argument 'MoreArgs' of 'mapply' is not a list"));
    PROTECT_INDEX fi;
    PROTECT_WITH_INDEX(fcall, &fi);

    Rboolean realIndx = CXXRCONSTRUCT(Rboolean, longest > INT_MAX);
    SEXP Dots = install("dots");
    for (int j = m - 1; j >= 0; j--) {
	SET_VECTOR_ELT(mindex, j, ScalarInteger(j + 1));
	SET_VECTOR_ELT(nindex, j, allocVector(realIndx ? REALSXP : INTSXP, 1));
	SEXP tmp1 = PROTECT(lang3(R_Bracket2Symbol, Dots, VECTOR_ELT(mindex, j)));
	SEXP tmp2 = PROTECT(lang3(R_Bracket2Symbol, tmp1, VECTOR_ELT(nindex, j)));
	REPROTECT(fcall = CONS(tmp2, fcall), fi);
	UNPROTECT(2);
	if (named && CHAR(STRING_ELT(vnames, j))[0] != '\0')
	    SET_TAG(fcall, installTrChar(STRING_ELT(vnames, j)));
    }

    REPROTECT(fcall = LCONS(f, fcall), fi);

    SEXP ans = PROTECT(allocVector(VECSXP, longest));

    for (int i = 0; i < longest; i++) {
	for (int j = 0; j < m; j++) {
	    counters[j] = (++counters[j] > lengths[j]) ? 1 : counters[j];
	    if (realIndx)
		REAL(VECTOR_ELT(nindex, j))[0] = double( counters[j]);
	    else
		INTEGER(VECTOR_ELT(nindex, j))[0] = int( counters[j]);
	}
	SEXP tmp = R_forceAndCall(fcall, m, rho);
	if (MAYBE_REFERENCED(tmp))
	    tmp = duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
    }

    for (int j = 0; j < m; j++)
	if (counters[j] != lengths[j])
	    warning(_("longer argument not a multiple of length of shorter"));

    UNPROTECT(5);
    return ans;
}
