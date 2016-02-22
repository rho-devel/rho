/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001-2014  The R Core Team
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "CXXR/GCStackRoot.hpp"

using namespace CXXR;

R_len_t Rf_length(SEXP s)
{
    if (Rf_isVector(s))
	return LENGTH(s);
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	{
	    int i = 0;
	    while (s != nullptr && s != R_NilValue) {
		i++;
		s = CDR(s);
	    }
	    return i;
	}
    case ENVSXP:
	return Rf_envxlength(s);
    default:
	return 1;
    }
}

R_xlen_t Rf_xlength(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	return XLENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    {
	// it is implausible this would be >= 2^31 elements, but allow it
	R_xlen_t i = 0;
	while (s != nullptr && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    }
    case ENVSXP:
	return Rf_envxlength(s);
    default:
	return 1;
    }
}
/* This is called by function() {}, where an invalid
   body should be impossible. When called from
   other places (eg do_asfunction) they
   should do this checking in advance */

/*  mkCLOSXP - return a closure with formals f,  */
/*             body b, and environment rho       */

SEXP Rf_mkCLOSXP(SEXP formals, SEXP body, SEXP rho)
{
    GCStackRoot<PairList> formrt(SEXP_downcast<PairList*>(formals));
    GCStackRoot<> bodyrt(body);
    GCStackRoot<Environment> envrt(rho ? SEXP_downcast<Environment*>(rho)
				   : Environment::global());
    switch (TYPEOF(body)) {
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case DOTSXP:
    case ANYSXP:
	Rf_error(_("invalid body argument for 'function'"));
	break;
    default:
	break;
    }
    return new Closure(formrt, bodyrt, envrt);
}
