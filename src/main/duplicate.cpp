/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *            (C) 2004  The R Foundation
 *  Copyright (C) 1998-2012 The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

#include "CXXR/DottedArgs.hpp"
#include "CXXR/GCStackRoot.hpp"

using namespace std;
using namespace CXXR;

/*  duplicate  -  object duplication  */

#ifdef R_PROFILING
static unsigned long duplicate_counter = static_cast<unsigned long>(-1);

unsigned long  attribute_hidden
get_duplicate_counter(void)
{
    return duplicate_counter;
}

void attribute_hidden reset_duplicate_counter(void)
{
    duplicate_counter = 0;
    return;
}
#endif

SEXP duplicate(SEXP s){
    if (!s) return nullptr;
    GCStackRoot<> srt(s);
#ifdef R_PROFILING
    duplicate_counter++;
#endif
    SEXP t = RObject::clone(s);
    if (!t) return s;
    return t;
}

SEXP shallow_duplicate(SEXP s) {
    // TODO(kmillar): implement shallow duplicates.
    return duplicate(s);
}

SEXP lazy_duplicate(SEXP s) {
    switch (TYPEOF(s)) {
    case NILSXP:
    case SYMSXP:
    case ENVSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case EXTPTRSXP:
    case BCODESXP:
    case WEAKREFSXP:
    case CHARSXP:
    case PROMSXP:
	break;
    case CLOSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case EXPRSXP:
    case VECSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
    case STRSXP:
    case S4SXP:
        SET_NAMED(s, 2);
	break;
    default:
	UNIMPLEMENTED_TYPE("lazy_duplicate", s);
    }
    return s;
}

/*****************/

/* Detect cycles that would be created by assigning 'child' as a
   component of 's' in a complex assignment without duplicating
   'child'.  This is called quite often but almost always returns
   FALSE. Could be made more efficient, at least with partial
   inlining, but probably not worth while until it starts showing up
   significantly in profiling. Based on code from Michael Lawrence. */
Rboolean R_cycle_detected(SEXP s, SEXP child) {
    if (s == child) {
	switch (TYPEOF(child)) {
	case NILSXP:
	case SYMSXP:
	case ENVSXP:
	case SPECIALSXP:
	case BUILTINSXP:
	case EXTPTRSXP:
	case BCODESXP:
	case WEAKREFSXP:
	    /* it's a cycle but one that is OK */
	    return FALSE; 
	default:
        return TRUE;
	}
    }
    if (ATTRIB(child) != R_NilValue) {
        if (R_cycle_detected(s, ATTRIB(child)))
            return TRUE;
    }
    if (isPairList(child)) {
        SEXP el = child;
        while(el != R_NilValue) {
	    if (s == el || R_cycle_detected(s, CAR(el)))
                return TRUE;
	    if (ATTRIB(el) != R_NilValue && R_cycle_detected(s, ATTRIB(el)))
		return TRUE;		
	    el = CDR(el);
	}
    } else if (isVectorList(child)) {
        for(int i = 0 ; i < Rf_length(child); i++)
	    if (R_cycle_detected(s, VECTOR_ELT(child, i)))
                return TRUE;
    }
    return FALSE;
}

/*****************/

void copyVector(SEXP s, SEXP t)
{
    R_xlen_t i, ns, nt;

    nt = XLENGTH(t);
    ns = XLENGTH(s);
    switch (TYPEOF(s)) {
    case STRSXP:
	for (i = 0; i < ns; i++)
	    SET_STRING_ELT(s, i, STRING_ELT(t, i % nt));
	break;
    case EXPRSXP:
	for (i = 0; i < ns; i++)
	    SET_VECTOR_ELT(s, i, VECTOR_ELT(t, i % nt));
	break;
    case LGLSXP:
	for (i = 0; i < ns; i++)
	    LOGICAL(s)[i] = LOGICAL(t)[i % nt];
	break;
    case INTSXP:
	for (i = 0; i < ns; i++)
	    INTEGER(s)[i] = INTEGER(t)[i % nt];
	break;
    case REALSXP:
	for (i = 0; i < ns; i++)
	    REAL(s)[i] = REAL(t)[i % nt];
	break;
    case CPLXSXP:
	for (i = 0; i < ns; i++)
	    COMPLEX(s)[i] = COMPLEX(t)[i % nt];
	break;
    case VECSXP:
	for (i = 0; i < ns; i++)
	    SET_VECTOR_ELT(s, i, VECTOR_ELT(t, i % nt));
	break;
    case RAWSXP:
	for (i = 0; i < ns; i++)
	    RAW(s)[i] = RAW(t)[i % nt];
	break;
    default:
	UNIMPLEMENTED_TYPE("copyVector", s);
    }
}

void copyListMatrix(SEXP s, SEXP t, Rboolean byrow)
{
    SEXP pt, tmp;
    int i, j, nr, nc;
    R_xlen_t ns;

    nr = nrows(s);
    nc = ncols(s);
    ns = (R_xlen_t( nr)) * nc;
    pt = t;
    if(byrow) {
	R_xlen_t NR = nr;
	PROTECT(tmp = allocVector(STRSXP, ns));
	for (i = 0; i < nr; i++)
	    for (j = 0; j < nc; j++) {
		SET_STRING_ELT(tmp, i + j * NR, duplicate(CAR(pt)));
		pt = CDR(pt);
		if(pt == R_NilValue) pt = t;
	    }
	for (i = 0; i < ns; i++) {
	    SETCAR(s, STRING_ELT(tmp, i++));
	    s = CDR(s);
	}
	UNPROTECT(1);
    }
    else {
	for (i = 0; i < ns; i++) {
	    SETCAR(s, duplicate(CAR(pt)));
	    s = CDR(s);
	    pt = CDR(pt);
	    if(pt == R_NilValue) pt = t;
	}
    }
}

void copyMatrix(SEXP s, SEXP t, Rboolean byrow)
{
    int i, j, nr, nc;
    R_xlen_t k, nt;

    nr = nrows(s);
    nc = ncols(s);
    nt = XLENGTH(t);
    k = 0;

    if (byrow) {
	R_xlen_t NR = nr;
	switch (TYPEOF(s)) {
	case STRSXP:
	    for (i = 0; i < nr; i++)
		for (j = 0; j < nc; j++)
		    SET_STRING_ELT(s, i + j * NR, STRING_ELT(t, k++ % nt));
	    break;
	case LGLSXP:
	    for (i = 0; i < nr; i++)
		for (j = 0; j < nc; j++)
		    LOGICAL(s)[i + j * NR] = LOGICAL(t)[k++ % nt];
	    break;
	case INTSXP:
	    for (i = 0; i < nr; i++)
		for (j = 0; j < nc; j++)
		    INTEGER(s)[i + j * NR] = INTEGER(t)[k++ % nt];
	    break;
	case REALSXP:
	    for (i = 0; i < nr; i++)
		for (j = 0; j < nc; j++)
		    REAL(s)[i + j * NR] = REAL(t)[k++ % nt];
	    break;
	case CPLXSXP:
	    for (i = 0; i < nr; i++)
		for (j = 0; j < nc; j++)
		    COMPLEX(s)[i + j * NR] = COMPLEX(t)[k++ % nt];
	    break;
	case VECSXP:
	    for (i = 0; i < nr; i++)
		for (j = 0; j < nc; j++)
		    SET_VECTOR_ELT(s, i + j * NR, VECTOR_ELT(t, k++ % nt));
	    break;
	case RAWSXP:
	    for (i = 0; i < nr; i++)
		for (j = 0; j < nc; j++)
		    RAW(s)[i + j * NR] = RAW(t)[k++ % nt];
	    break;
	default:
	    UNIMPLEMENTED_TYPE("copyMatrix", s);
	}
    }
    else
	copyVector(s, t);
}
