/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *            (C) 2004  The R Foundation
 *  Copyright (C) 1998-2014 The R Core Team.
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
#include "duplicate.h"

#include "CXXR/DottedArgs.hpp"
#include "CXXR/GCStackRoot.hpp"

using namespace std;
using namespace CXXR;

/*  duplicate  -  object duplication  */

/*  Because we try to maintain the illusion of call by
 *  value, we often need to duplicate entire data
 *  objects.  There are a couple of points to note.
 *  First, duplication of list-like objects is done
 *  iteratively to prevent growth of the pointer
 *  protection stack, and second, the duplication of
 *  promises requires that the promises be forced and
 *  the value duplicated.  */

/* This macro pulls out the common code in copying an atomic vector.
   The special handling of the scalar case (__n__ == 1) seems to make
   a small but measurable difference, at least for some cases
   and when (as in R 2.15.x) a for() loop was used.
*/
#ifdef __APPLE__
/* it seems some OS X builds do not copy >= 2^32 bytes fully */
#define DUPLICATE_ATOMIC_VECTOR(type, fun, to, from, deep) do {	\
  R_xlen_t __n__ = XLENGTH(from); \
  PROTECT(from); \
  PROTECT(to = allocVector(TYPEOF(from), __n__)); \
  if (__n__ == 1) fun(to)[0] = fun(from)[0]; \
  else { \
      R_xlen_t __this; \
      type *__to = fun(to), *__from = fun(from); \
      do { \
	 __this = (__n__ < 1000000) ? __n__ : 1000000; \
	 memcpy(__to, __from, __this * sizeof(type));  \
	 __n__ -= __this;  __to += __this; __from += __this; \
      } while(__n__ > 0); \
  } \
  DUPLICATE_ATTRIB(to, from, deep);		 \
  SET_TRUELENGTH(to, XTRUELENGTH(from)); \
  UNPROTECT(2); \
} while (0)
#else
#define DUPLICATE_ATOMIC_VECTOR(type, fun, to, from, deep) do {	\
  R_xlen_t __n__ = XLENGTH(from); \
  PROTECT(from); \
  PROTECT(to = allocVector(TYPEOF(from), __n__)); \
  if (__n__ == 1) fun(to)[0] = fun(from)[0]; \
  else memcpy(fun(to), fun(from), __n__ * sizeof(type)); \
  DUPLICATE_ATTRIB(to, from, deep); \
  SET_TRUELENGTH(to, XTRUELENGTH(from)); \
  UNPROTECT(2); \
} while (0)
#endif

/* The following macros avoid the cost of going through calls to the
   assignment functions (and duplicate in the case of ATTRIB) when the
   ATTRIB or TAG value to be stored is R_NilValue, the value the field
   will have been set to by the allocation function */
#define SHALLOW_DUPLICATE_ATTRIB(to, from, deep) do { \
  SEXP __a__ = ATTRIB(from); \
  if (__a__ != R_NilValue) { \
      SET_ATTRIB(to, shallow_duplicate1(__a__, deep)); \
    SET_OBJECT(to, OBJECT(from)); \
    IS_S4_OBJECT(from) ? SET_S4_OBJECT(to) : UNSET_S4_OBJECT(to);  \
  } \
} while (0)

#define DUPLICATE_ATTRIB(to, from, deep) do { \
  SEXP __a__ = ATTRIB(from); \
  if (__a__ != R_NilValue) { \
      SET_ATTRIB(to, duplicate1(__a__, deep)); \
    SET_OBJECT(to, OBJECT(from)); \
    IS_S4_OBJECT(from) ? SET_S4_OBJECT(to) : UNSET_S4_OBJECT(to);  \
  } \
} while (0)

#define COPY_TAG(to, from) do { \
  SEXP __tag__ = TAG(from); \
  if (__tag__ != R_NilValue) SET_TAG(to, __tag__); \
} while (0)


/* For memory profiling.  */
/* We want a count of calls to duplicate from outside
   which requires a wrapper function.

   The original duplicate() function is now duplicate1().

   I don't see how to make the wrapper go away when R_PROFILING
   is not defined, because we still need to be able to
   optionally rename duplicate() as Rf_duplicate().
*/

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
    SEXPTYPE sT = TYPEOF(s), tT = TYPEOF(t);
    if (sT != tT)
	error("vector types do not match in copyVector");
    R_xlen_t ns = XLENGTH(s), nt = XLENGTH(t);
    switch (sT) {
    case STRSXP:
	xcopyStringWithRecycle(s, t, 0, ns, nt);
	break;
    case LGLSXP:
	xcopyLogicalWithRecycle(LOGICAL(s), LOGICAL(t), 0, ns, nt);
	break;
    case INTSXP:
	xcopyIntegerWithRecycle(INTEGER(s), INTEGER(t), 0, ns, nt);
	break;
    case REALSXP:
	xcopyRealWithRecycle(REAL(s), REAL(t), 0, ns, nt);
	break;
    case CPLXSXP:
	xcopyComplexWithRecycle(COMPLEX(s), COMPLEX(t), 0, ns, nt);
	break;
    case EXPRSXP:
    case VECSXP:
	xcopyVectorWithRecycle(s, t, 0, ns, nt);
	break;
    case RAWSXP:
	xcopyRawWithRecycle(RAW(s), RAW(t), 0, ns, nt);
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
    int nr = nrows(s), nc = ncols(s);
    R_xlen_t nt = XLENGTH(t);

    if (byrow) {
	switch (TYPEOF(s)) {
	case STRSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		SET_STRING_ELT(s, didx, STRING_ELT(t, sidx));
	    break;
	case LGLSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		LOGICAL(s)[didx] = LOGICAL(t)[sidx];
	    break;
	case INTSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		INTEGER(s)[didx] = INTEGER(t)[sidx];
	    break;
	case REALSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		REAL(s)[didx] = REAL(t)[sidx];
	    break;
	case CPLXSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		COMPLEX(s)[didx] = COMPLEX(t)[sidx];
	    break;
	case EXPRSXP:
	case VECSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		SET_VECTOR_ELT(s, didx, VECTOR_ELT(t, sidx));
	    break;
	case RAWSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		RAW(s)[didx] = RAW(t)[sidx];
	    break;
	default:
	    UNIMPLEMENTED_TYPE("copyMatrix", s);
	}
    }
    else
	copyVector(s, t);
}

#define COPY_WITH_RECYCLE(VALTYPE, TNAME) \
void attribute_hidden \
xcopy##TNAME##WithRecycle(VALTYPE *dst, VALTYPE *src, R_xlen_t dstart, R_xlen_t n, R_xlen_t nsrc) { \
							\
    if (nsrc >= n) { /* no recycle needed */		\
	for(R_xlen_t i = 0; i < n; i++)		\
	    dst[dstart + i] = src[i];			\
	return;					\
    }							\
    if (nsrc == 1) {					\
	VALTYPE val = src[0];				\
	for(R_xlen_t i = 0; i < n; i++)			\
	    dst[dstart + i] = val;			\
	return;						\
    }							\
							\
    /* recycle needed */					\
    R_xlen_t sidx = 0;					\
    for(R_xlen_t i = 0; i < n; i++, sidx++) {		\
	if (sidx == nsrc) sidx = 0;			\
	dst[dstart + i] = src[sidx];			\
    }							\
}

COPY_WITH_RECYCLE(Rcomplex, Complex)	/* xcopyComplexWithRecycle */
COPY_WITH_RECYCLE(int, Integer)		/* xcopyIntegerWithRecycle */
COPY_WITH_RECYCLE(int, Logical)		/* xcopyLogicalWithRecycle */
COPY_WITH_RECYCLE(Rbyte, Raw)		/* xcopyRawWithRecycle */
COPY_WITH_RECYCLE(double, Real)		/* xcopyRealWithRecycle */

#define COPY_ELT_WITH_RECYCLE(TNAME, GETELT, SETELT) \
void attribute_hidden \
xcopy##TNAME##WithRecycle(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t n, R_xlen_t nsrc) { \
							\
    if (nsrc >= n) { /* no recycle needed */		\
	for(R_xlen_t i = 0; i < n; i++)		\
	    SETELT(dst, dstart + i, GETELT(src, i));	\
	return;					\
    }							\
    if (nsrc == 1) {					\
	SEXP val = GETELT(src, 0);			\
	for(R_xlen_t i = 0; i < n; i++)			\
	    SETELT(dst, dstart + i, val);		\
	return;						\
    }							\
							\
    /* recycle needed */					\
    R_xlen_t sidx = 0;					\
    for(R_xlen_t i = 0; i < n; i++, sidx++) {		\
	if (sidx == nsrc) sidx = 0;			\
	SETELT(dst, dstart + i, GETELT(src, sidx));	\
    }							\
}

COPY_ELT_WITH_RECYCLE(String, STRING_ELT, SET_STRING_ELT) /* xcopyStringWithRecycle */
COPY_ELT_WITH_RECYCLE(Vector, VECTOR_ELT, SET_VECTOR_ELT) /* xcopyVectorWithRecycle */

#define FILL_WITH_RECYCLE(VALTYPE, TNAME) \
void attribute_hidden xfill##TNAME##MatrixWithRecycle(VALTYPE *dst, VALTYPE *src,	\
    R_xlen_t dstart, R_xlen_t drows, R_xlen_t srows,		\
    R_xlen_t cols, R_xlen_t nsrc) {				\
								\
    FILL_MATRIX_ITERATE(dstart, drows, srows, cols, nsrc)	\
	dst[didx] = src[sidx];					\
}

FILL_WITH_RECYCLE(Rcomplex, Complex)	/* xfillComplexMatrixWithRecycle */
FILL_WITH_RECYCLE(int, Integer)		/* xfillIntegerMatrixWithRecycle */
FILL_WITH_RECYCLE(int, Logical)		/* xfillLogicalMatrixWithRecycle */
FILL_WITH_RECYCLE(Rbyte, Raw)		/* xfillRawMatrixWithRecycle */
FILL_WITH_RECYCLE(double, Real)		/* xfillRealMatrixWithRecycle */

#define FILL_ELT_WITH_RECYCLE(TNAME, GETELT, SETELT) \
void attribute_hidden xfill##TNAME##MatrixWithRecycle(SEXP dst, SEXP src,	\
    R_xlen_t dstart, R_xlen_t drows, R_xlen_t srows,		\
    R_xlen_t cols, R_xlen_t nsrc) {				\
								\
    FILL_MATRIX_ITERATE(dstart, drows, srows, cols, nsrc)	\
	SETELT(dst, didx, GETELT(src, sidx));			\
}

FILL_ELT_WITH_RECYCLE(String, STRING_ELT, SET_STRING_ELT) /* xfillStringMatrixWithRecycle */
FILL_ELT_WITH_RECYCLE(Vector, VECTOR_ELT, SET_VECTOR_ELT) /* xfillVectorMatrixWithRecycle */
