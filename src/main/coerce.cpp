/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995,1996  Robert Gentleman, Ross Ihaka
 *  Copyright (C) 1997-2015  The R Core Team
 *  Copyright (C) 2003-2015  The R Foundation
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

/** @file coerce.cpp
 *
 * Coercions from one R type to another, including various 'is' and
 * 'as' functions.  Also 'quote'.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* interval at which to check interrupts */
#define NINTERRUPT 10000000

#include <Defn.h> /*-- Maybe modularize into own Coerce.h ..*/
#include <Internal.h>
#include <float.h> /* for DBL_DIG */
#define R_MSG_mode	_("invalid 'mode' argument")
#define R_MSG_list_vec	_("applies only to lists and vectors")
#include <Rmath.h>
#include <Print.h>
#include "CXXR/GCStackRoot.hpp"

using namespace std;
using namespace CXXR;

/* This section of code handles type conversion for elements */
/* of data vectors.  Type coercion throughout R should use these */
/* routines to ensure consistency. */

/* Coercion warnings will be OR'ed : */
#define WARN_NA	   1
#define WARN_INT_NA 2
#define WARN_IMAG  4
#define WARN_RAW  8

/* The following two functions copy or clear the attributes.  They
   avoid calling the assignment functions when possible, since the
   write barrier (and possibly cache behavior on some architectures)
   makes assigning more costly than dereferencing. */
namespace {
    inline void cDUPLICATE_ATTRIB(SEXP to, SEXP from)
    {
	if (ATTRIB(from)) SHALLOW_DUPLICATE_ATTRIB(to, from);
    }

    inline void CLEAR_ATTRIB(SEXP x)
    {
	if (ATTRIB(x)) {
	    x->clearAttributes();
	    if (IS_S4_OBJECT(x)) UNSET_S4_OBJECT(x);
	}
    }
}

void attribute_hidden Rf_CoercionWarning(int warn)
{
/* FIXME: Use
   =====
   WarningMessage(R_NilValue, WARNING_....);
*/
    if (warn & WARN_NA)
	Rf_warning(_("NAs introduced by coercion"));
    if (warn & WARN_INT_NA)
	Rf_warning(_("NAs introduced by coercion to integer range"));
    if (warn & WARN_IMAG)
	Rf_warning(_("imaginary parts discarded in coercion"));
    if (warn & WARN_RAW)
	Rf_warning(_("out-of-range values treated as 0 in coercion to raw"));
}

int attribute_hidden
LogicalFromInteger(int x, int *warn)
{
    return (x == NA_INTEGER) ?
	NA_LOGICAL : (x != 0);
}

int attribute_hidden
LogicalFromReal(double x, int *warn)
{
    return ISNAN(x) ?
	NA_LOGICAL : (x != 0);
}

int attribute_hidden
LogicalFromComplex(Rcomplex x, int *warn)
{
    return (ISNAN(x.r) || ISNAN(x.i)) ?
	NA_LOGICAL : (x.r != 0 || x.i != 0);
}

int attribute_hidden
LogicalFromString(SEXP x, int *warn)
{
    if (x != R_NaString) {
	if (StringTrue(CHAR(x))) return 1;
	if (StringFalse(CHAR(x))) return 0;
    }
    return NA_LOGICAL;
}

int attribute_hidden
Rf_IntegerFromLogical(int x, int *warn)
{
    return (x == NA_LOGICAL) ?
	NA_INTEGER : x;
}

int attribute_hidden
Rf_IntegerFromReal(double x, int *warn)
{
    if (ISNAN(x))
	return NA_INTEGER;
    else if (x >= INT_MAX+1. || x <= INT_MIN ) {
	*warn |= WARN_INT_NA;
	return NA_INTEGER;
    }
    return int( x);
}

int attribute_hidden
Rf_IntegerFromComplex(Rcomplex x, int *warn)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	return NA_INTEGER;
    else if (x.r > INT_MAX+1. || x.r <= INT_MIN ) {
	*warn |= WARN_INT_NA;
	return NA_INTEGER;;
    }
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return int( x.r);
}


int attribute_hidden
Rf_IntegerFromString(SEXP x, int *warn)
{
    double xdouble;
    char *endp;
    if (x != R_NaString && !Rf_isBlankString(CHAR(x))) { /* ASCII */
	xdouble = R_strtod(CHAR(x), &endp); /* ASCII */
	if (isBlankString(endp)) {
#ifdef _R_pre_Version_3_3_0
	    if (xdouble > INT_MAX) {
		*warn |= WARN_INT_NA;
		return INT_MAX;
	    }
	    else if(xdouble < INT_MIN+1) {
		*warn |= WARN_INT_NA;
		return INT_MIN;// <- "wrong" as INT_MIN == NA_INTEGER currently; should have used INT_MIN+1
	    }
#else
	    // behave the same as IntegerFromReal() etc:
	    if (xdouble >= INT_MAX+1. || xdouble <= INT_MIN ) {
		*warn |= WARN_INT_NA;
		return NA_INTEGER;
	    }
#endif
	    else
		return int( xdouble);
	}
	else *warn |= WARN_NA;
    }
    return NA_INTEGER;
}

double attribute_hidden
RealFromLogical(int x, int *warn)
{
    return (x == NA_LOGICAL) ?
	NA_REAL : x;
}

double attribute_hidden
RealFromInteger(int x, int *warn)
{
    if (x == NA_INTEGER)
	return NA_REAL;
    else
	return x;
}

double attribute_hidden
RealFromComplex(Rcomplex x, int *warn)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	return NA_REAL;
    if (ISNAN(x.r)) return x.r;
    if (ISNAN(x.i)) return NA_REAL;
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return x.r;
}

double attribute_hidden
RealFromString(SEXP x, int *warn)
{
    double xdouble;
    char *endp;
    if (x != R_NaString && !isBlankString(CHAR(x))) { /* ASCII */
	xdouble = R_strtod(CHAR(x), &endp); /* ASCII */
	if (isBlankString(endp))
	    return xdouble;
	else
	    *warn |= WARN_NA;
    }
    return NA_REAL;
}

Rcomplex attribute_hidden
ComplexFromLogical(int x, int *warn)
{
    Rcomplex z;
    if (x == NA_LOGICAL) {
	z.r = NA_REAL;
	z.i = NA_REAL;
    }
    else {
	z.r = x;
	z.i = 0;
    }
    return z;
}

Rcomplex attribute_hidden
ComplexFromInteger(int x, int *warn)
{
    Rcomplex z;
    if (x == NA_INTEGER) {
	z.r = NA_REAL;
	z.i = NA_REAL;
    }
    else {
	z.r = x;
	z.i = 0;
    }
    return z;
}

Rcomplex attribute_hidden
ComplexFromReal(double x, int *warn)
{
    Rcomplex z;
#ifdef PRE_R_3_3_0
    if (ISNAN(x)) {
	z.r = NA_REAL;
	z.i = NA_REAL;
    }
    else {
#endif
	z.r = x;
	z.i = 0;
#ifdef PRE_R_3_3_0
    }
#endif
    return z;
}

Rcomplex attribute_hidden
ComplexFromString(SEXP x, int *warn)
{
    double xr, xi;
    Rcomplex z;
    const char *xx = CHAR(x); /* ASCII */
    char *endp;

    z.r = z.i = NA_REAL;
    if (x != R_NaString && !isBlankString(xx)) {
	xr = R_strtod(xx, &endp);
	if (isBlankString(endp)) {
	    z.r = xr;
	    z.i = 0.0;
	}
	else if (*endp == '+' || *endp == '-') {
	    xi = R_strtod(endp, &endp);
	    if (*endp++ == 'i' && isBlankString(endp)) {
		z.r = xr;
		z.i = xi;
	    }
	    else *warn |= WARN_NA;
	}
	else *warn |= WARN_NA;
    }
    return z;
}

SEXP attribute_hidden Rf_StringFromLogical(int x, int *warn)
{
    int w;
    formatLogical(&x, 1, &w);
    if (x == NA_LOGICAL) return NA_STRING;
    else return Rf_mkChar(EncodeLogical(x, w));
}

SEXP attribute_hidden Rf_StringFromInteger(int x, int *warn)
{
    int w;
    formatInteger(&x, 1, &w);
    if (x == NA_INTEGER) return NA_STRING;
    else return Rf_mkChar(EncodeInteger(x, w));
}

// dropTrailing0 and StringFromReal moved to printutils.cpp

SEXP attribute_hidden Rf_StringFromComplex(Rcomplex x, int *warn)
{
    int wr, dr, er, wi, di, ei;
    formatComplex(&x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
    if (ISNA(x.r) || ISNA(x.i)) // "NA" if Re or Im is (but not if they're just NaN)
	return NA_STRING;
    else /* EncodeComplex has its own anti-trailing-0 care :*/
	return Rf_mkChar(EncodeComplex(x, wr, dr, er, wi, di, ei, OutDec));
}

static SEXP StringFromRaw(Rbyte x, int *warn)
{
    char buf[3];
    sprintf(buf, "%02x", x);
    return Rf_mkChar(buf);
}

/* Conversion between the two list types (LISTSXP and VECSXP). */

SEXP Rf_PairToVectorList(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len = 0, named = 0;
    for (xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr)) {
	named = named | (TAG(xptr) != R_NilValue);
	len++;
    }
    PROTECT(x);
    PROTECT(xnew = Rf_allocVector(VECSXP, len));
    for (i = 0, xptr = x; i < len; i++, xptr = CDR(xptr))
	SET_VECTOR_ELT(xnew, i, CAR(xptr));
    if (named) {
	PROTECT(xnames = Rf_allocVector(STRSXP, len));
	xptr = x;
	for (i = 0, xptr = x; i < len; i++, xptr = CDR(xptr)) {
	    if(TAG(xptr) == R_NilValue)
		SET_STRING_ELT(xnames, i, R_BlankString);
	    else
		SET_STRING_ELT(xnames, i, PRINTNAME(TAG(xptr)));
	}
	Rf_setAttrib(xnew, R_NamesSymbol, xnames);
	UNPROTECT(1);
    }
    Rf_copyMostAttrib(x, xnew);
    UNPROTECT(2);
    return xnew;
}

SEXP Rf_VectorToPairList(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len, named;

    len = length(x);
    PROTECT(x);
    PROTECT(xnew = Rf_allocList(len)); /* limited to int */
    PROTECT(xnames = Rf_getAttrib(x, R_NamesSymbol));
    named = (xnames != R_NilValue);
    xptr = xnew;
    for (i = 0; i < len; i++) {
	if (NAMED(x) > NAMED(VECTOR_ELT(x, i)))
	    SET_NAMED(VECTOR_ELT(x, i), NAMED(x));
	SETCAR(xptr, VECTOR_ELT(x, i));
	if (named && CHAR(STRING_ELT(xnames, i))[0] != '\0') /* ASCII */
	    SET_TAG(xptr, Rf_installTrChar(STRING_ELT(xnames, i)));
	xptr = CDR(xptr);
    }
    if (len > 0)       /* can't set attributes on NULL */
	Rf_copyMostAttrib(x, xnew);
    UNPROTECT(3);
    return xnew;
}

static SEXP coerceToSymbol(SEXP v)
{
    SEXP ans = R_NilValue;
    int warn = 0;
    if (length(v) <= 0)
	Rf_error(_("invalid data of mode '%s' (too short)"),
	      Rf_type2char(TYPEOF(v)));
    PROTECT(v);
    switch(TYPEOF(v)) {
    case LGLSXP:
	ans = Rf_StringFromLogical(LOGICAL(v)[0], &warn);
	break;
    case INTSXP:
	ans = Rf_StringFromInteger(INTEGER(v)[0], &warn);
	break;
    case REALSXP:
	ans = Rf_StringFromReal(REAL(v)[0], &warn);
	break;
    case CPLXSXP:
	ans = Rf_StringFromComplex(COMPLEX(v)[0], &warn);
	break;
    case STRSXP:
	ans = STRING_ELT(v, 0);
	break;
    case RAWSXP:
	ans = StringFromRaw(RAW(v)[0], &warn);
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToSymbol", v);
    }
    if (warn) Rf_CoercionWarning(warn);/*2000/10/23*/
    ans = Rf_installChar(ans);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToLogical(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(LGLSXP, n = XLENGTH(v)));
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(ans)[i] = LogicalFromInteger(INTEGER(v)[i], &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(ans)[i] = LogicalFromReal(REAL(v)[i], &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(ans)[i] = LogicalFromComplex(COMPLEX(v)[i], &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(ans)[i] = LogicalFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(ans)[i] = LogicalFromInteger(int(RAW(v)[i]), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToLogical", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToInteger(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(INTSXP, n = XLENGTH(v)));
    ans->maybeTraceMemory(v);
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    INTEGER(ans)[i] = Rf_IntegerFromLogical(LOGICAL(v)[i], &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    INTEGER(ans)[i] = Rf_IntegerFromReal(REAL(v)[i], &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    INTEGER(ans)[i] = Rf_IntegerFromComplex(COMPLEX(v)[i], &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    INTEGER(ans)[i] = Rf_IntegerFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    INTEGER(ans)[i] = int(RAW(v)[i]);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToInteger", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToReal(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(REALSXP, n = XLENGTH(v)));
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    REAL(ans)[i] = RealFromLogical(LOGICAL(v)[i], &warn);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    REAL(ans)[i] = RealFromInteger(INTEGER(v)[i], &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    REAL(ans)[i] = RealFromComplex(COMPLEX(v)[i], &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    REAL(ans)[i] = RealFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    REAL(ans)[i] = RealFromInteger(int(RAW(v)[i]), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToReal", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToComplex(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = Rf_allocVector(CPLXSXP, n = XLENGTH(v)));
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    COMPLEX(ans)[i] = ComplexFromLogical(LOGICAL(v)[i], &warn);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    COMPLEX(ans)[i] = ComplexFromInteger(INTEGER(v)[i], &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    COMPLEX(ans)[i] = ComplexFromReal(REAL(v)[i], &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    COMPLEX(ans)[i] = ComplexFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    COMPLEX(ans)[i] = ComplexFromInteger(int(RAW(v)[i]), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToComplex", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToRaw(SEXP v)
{
    SEXP ans;
    int warn = 0, tmp;
    R_xlen_t i, n;

    PROTECT(ans = Rf_allocVector(RAWSXP, n = XLENGTH(v)));
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromLogical(LOGICAL(v)[i], &warn);
	    if(tmp == NA_INTEGER) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    RAW(ans)[i] = Rbyte( tmp);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = INTEGER(v)[i];
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    RAW(ans)[i] = Rbyte( tmp);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromReal(REAL(v)[i], &warn);
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    RAW(ans)[i] = Rbyte( tmp);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromComplex(COMPLEX(v)[i], &warn);
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    RAW(ans)[i] = Rbyte( tmp);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = Rf_IntegerFromString(STRING_ELT(v, i), &warn);
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    RAW(ans)[i] = Rbyte( tmp);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToRaw", v);
    }
    if (warn) Rf_CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToString(SEXP v)
{
    SEXP ans;
    int savedigits, warn = 0;
    R_xlen_t i, n;

    PROTECT(ans = Rf_allocVector(STRSXP, n = XLENGTH(v)));
    ans->maybeTraceMemory(v);
    cDUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromLogical(LOGICAL(v)[i], &warn));
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromInteger(INTEGER(v)[i], &warn));
	}
	break;
    case REALSXP:
	Rf_PrintDefaults();
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromReal(REAL(v)[i], &warn));
	}
	R_print.digits = savedigits;
	break;
    case CPLXSXP:
	Rf_PrintDefaults();
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, Rf_StringFromComplex(COMPLEX(v)[i], &warn));
	}
	R_print.digits = savedigits;
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, StringFromRaw(RAW(v)[i], &warn));
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToString", v);
    }
    if (warn) Rf_CoercionWarning(warn);/*2000/10/23*/
    UNPROTECT(1);
    return (ans);
}

static SEXP coerceToExpression(SEXP v)
{
    SEXP ans;
    R_xlen_t i, n;
    if (Rf_isVectorAtomic(v)) {
	n = XLENGTH(v);
	PROTECT(ans = Rf_allocVector(EXPRSXP, n));
	ans->maybeTraceMemory(v);
	switch (TYPEOF(v)) {
	case LGLSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarLogical(LOGICAL(v)[i]));
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarInteger(INTEGER(v)[i]));
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarReal(REAL(v)[i]));
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarComplex(COMPLEX(v)[i]));
	    break;
	case STRSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarString(STRING_ELT(v, i)));
	    break;
	case RAWSXP:
	    for (i = 0; i < n; i++)
		SET_XVECTOR_ELT(ans, i, Rf_ScalarRaw(RAW(v)[i]));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceToExpression", v);
	}
    }
    else {/* not used either */
	PROTECT(ans = Rf_allocVector(EXPRSXP, 1));
	SET_XVECTOR_ELT(ans, 0, Rf_duplicate(v));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToVectorList(SEXP v)
{
    SEXP ans, tmp;
    R_xlen_t i, n;
    n = Rf_xlength(v);
    PROTECT(ans = Rf_allocVector(VECSXP, n));
    ans->maybeTraceMemory(v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarLogical(LOGICAL(v)[i]));
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarInteger(INTEGER(v)[i]));
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarReal(REAL(v)[i]));
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarComplex(COMPLEX(v)[i]));
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarString(STRING_ELT(v, i)));
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, Rf_ScalarRaw(RAW(v)[i]));
	}
	break;
    case LISTSXP:
    case LANGSXP:
	tmp = v;
	for (i = 0; i < n; i++) {
	    SET_VECTOR_ELT(ans, i, CAR(tmp));
	    tmp = CDR(tmp);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToVectorList", v);
    }
    tmp = Rf_getAttrib(v, R_NamesSymbol);
    if (tmp != R_NilValue)
	Rf_setAttrib(ans, R_NamesSymbol, tmp);
    UNPROTECT(1);
    return (ans);
}

static SEXP coerceToPairList(SEXP v)
{
    SEXP ans, ansp;
    int i, n;
    n = LENGTH(v); /* limited to len */
    PROTECT(ansp = ans = Rf_allocList(n));
    for (i = 0; i < n; i++) {
	switch (TYPEOF(v)) {
	case LGLSXP:
	    SETCAR(ansp, Rf_allocVector(LGLSXP, 1));
	    INTEGER(CAR(ansp))[0] = INTEGER(v)[i];
	    break;
	case INTSXP:
	    SETCAR(ansp, Rf_allocVector(INTSXP, 1));
	    INTEGER(CAR(ansp))[0] = INTEGER(v)[i];
	    break;
	case REALSXP:
	    SETCAR(ansp, Rf_allocVector(REALSXP, 1));
	    REAL(CAR(ansp))[0] = REAL(v)[i];
	    break;
	case CPLXSXP:
	    SETCAR(ansp, Rf_allocVector(CPLXSXP, 1));
	    COMPLEX(CAR(ansp))[0] = COMPLEX(v)[i];
	    break;
	case STRSXP:
	    SETCAR(ansp, Rf_ScalarString(STRING_ELT(v, i)));
	    break;
	case RAWSXP:
	    SETCAR(ansp, Rf_allocVector(RAWSXP, 1));
	    RAW(CAR(ansp))[0] = RAW(v)[i];
	    break;
	case VECSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	case EXPRSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceToPairList", v);
	}
	ansp = CDR(ansp);
    }
    ansp = Rf_getAttrib(v, R_NamesSymbol);
    if (ansp != R_NilValue)
	Rf_setAttrib(ans, R_NamesSymbol, ansp);
    UNPROTECT(1);
    return (ans);
}

/* Coerce a pairlist to the given type */
static SEXP coercePairList(SEXP v, SEXPTYPE type)
{
    int i, n=0;
    SEXP rval= R_NilValue, vp, names;

    /* Hmm, this is also called to LANGSXP, and Rf_coerceVector already
       did the check of TYPEOF(v) == type */
    if(type == LISTSXP) return v;/* IS pairlist */

    names = v;
    if (type == EXPRSXP) {
	PROTECT(rval = Rf_allocVector(type, 1));
	SET_XVECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
	return rval;
    }
    else if (type == STRSXP) {
	n = length(v);
	PROTECT(rval = Rf_allocVector(type, n));
	for (vp = v, i = 0; vp != R_NilValue; vp = CDR(vp), i++) {
	    if (Rf_isString(CAR(vp)) && length(CAR(vp)) == 1)
		SET_STRING_ELT(rval, i, STRING_ELT(CAR(vp), 0));
	    else
		SET_STRING_ELT(rval, i, STRING_ELT(Rf_deparse1line(CAR(vp), CXXRFALSE), 0));
	}
    }
    else if (type == VECSXP) {
	rval = Rf_PairToVectorList(v);
	return rval;
    }
    else if (Rf_isVectorizable(v)) {
	n = length(v);
	PROTECT(rval = Rf_allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		LOGICAL(rval)[i] = Rf_asLogical(CAR(vp));
	    break;
	case INTSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		INTEGER(rval)[i] = Rf_asInteger(CAR(vp));
	    break;
	case REALSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		REAL(rval)[i] = Rf_asReal(CAR(vp));
	    break;
	case CPLXSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		COMPLEX(rval)[i] = Rf_asComplex(CAR(vp));
	    break;
	case RAWSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		RAW(rval)[i] = Rbyte( Rf_asInteger(CAR(vp)));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coercePairList", v);
	}
    }
    else
	Rf_error(_("'pairlist' object cannot be coerced to type '%s'"),
	      Rf_type2char(type));

    /* If any tags are non-null then we */
    /* need to add a names attribute. */
    for (vp = v, i = 0; vp != R_NilValue; vp = CDR(vp))
	if (TAG(vp) != R_NilValue)
	    i = 1;

    if (i) {
	i = 0;
	names = Rf_allocVector(STRSXP, n);
	for (vp = v; vp != R_NilValue; vp = CDR(vp), i++)
	    if (TAG(vp) != R_NilValue)
		SET_STRING_ELT(names, i, PRINTNAME(TAG(vp)));
	Rf_setAttrib(rval, R_NamesSymbol, names);
    }
    UNPROTECT(1);
    return rval;
}

/* Coerce a vector list to the given type */
static SEXP Rf_coerceVectorList(SEXP v, SEXPTYPE type)
{
    int warn = 0, tmp;
    R_xlen_t i, n;
    SEXP rval, names;

    names = v;
    rval = R_NilValue;	/* -Wall */

    /* expression -> list, new in R 2.4.0 */
    if (type == VECSXP)
	if (v->sexptype() == EXPRSXP) {
	    ExpressionVector* ev = static_cast<ExpressionVector*>(v);
	    GCStackRoot<ListVector>
		lv(ListVector::create(ev->begin(), ev->end()));
	    lv->copyAttribute(NamesSymbol, ev);
	    return lv;
	}
    if (type == EXPRSXP && TYPEOF(v) == VECSXP) {
	ListVector* lv = static_cast<ListVector*>(v);
	GCStackRoot<ExpressionVector>
	    ev(ExpressionVector::create(lv->begin(), lv->end()));
	ev->copyAttribute(NamesSymbol, lv);
	return ev;
    }

    if (type == STRSXP) {
	n = Rf_xlength(v);
	PROTECT(rval = Rf_allocVector(type, n));
	rval->maybeTraceMemory(v);
	for (i = 0; i < n;  i++) {
	    SEXP elt;
	    if (v->sexptype() == EXPRSXP) {
		ExpressionVector* ev = static_cast<ExpressionVector*>(v);
		elt = (*ev)[i];
	    }
	    else elt = VECTOR_ELT(v, i);
	    if (Rf_isString(elt) && length(elt) == 1)
		SET_STRING_ELT(rval, i, STRING_ELT(elt, 0));
#if 0
	    /* this will make as.character(list(s)) not backquote
	     * non-syntactic name s. It is not entirely clear that
	     * that is really desirable though....
	     */
	    else if (Rf_isSymbol(VECTOR_ELT(v, i)))
		SET_STRING_ELT(rval, i, PRINTNAME(VECTOR_ELT(v, i)));
#endif
	    else
		SET_STRING_ELT(rval, i,
			       STRING_ELT(Rf_deparse1line(elt, CXXRFALSE), 0));
	}
    }
    else if (type == LISTSXP) {
	rval = Rf_VectorToPairList(v);
	return rval;
    }
    else if (Rf_isVectorizable(v)) {
	n = Rf_xlength(v);
	PROTECT(rval = Rf_allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		LOGICAL(rval)[i] = Rf_asLogical(VECTOR_ELT(v, i));
	    }
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(rval)[i] = Rf_asInteger(VECTOR_ELT(v, i));
	    }
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(rval)[i] = Rf_asReal(VECTOR_ELT(v, i));
	    }
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		COMPLEX(rval)[i] = Rf_asComplex(VECTOR_ELT(v, i));
	    }
	    break;
	case RAWSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		tmp = Rf_asInteger(VECTOR_ELT(v, i));
		if (tmp < 0 || tmp > 255) { /* includes NA_INTEGER */
		    tmp = 0;
		    warn |= WARN_RAW;
		}
		RAW(rval)[i] = Rbyte( tmp);
	    }
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceVectorList", v);
	}
    }
    else
	Rf_error(_("(list) object cannot be coerced to type '%s'"),
	      Rf_type2char(type));

    if (warn) Rf_CoercionWarning(warn);
    names = Rf_getAttrib(v, R_NamesSymbol);
    if (names != R_NilValue)
	Rf_setAttrib(rval, R_NamesSymbol, names);
    UNPROTECT(1);
    return rval;
}

static SEXP coerceSymbol(SEXP v, SEXPTYPE type)
{
    SEXP rval = R_NilValue;
    if (type == EXPRSXP) {
	PROTECT(rval = Rf_allocVector(type, 1));
	SET_XVECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
    } else if (type == CHARSXP)
	rval = PRINTNAME(v);
    else if (type == STRSXP)
	rval = Rf_ScalarString(PRINTNAME(v));
    else
	Rf_warning(_("(symbol) object cannot be coerced to type '%s'"),
		Rf_type2char(type));
    return rval;
}

SEXP Rf_coerceVector(SEXP v, SEXPTYPE type)
{
    SEXP op, vp, ans = R_NilValue;	/* -Wall */
    int i,n;

    if (TYPEOF(v) == type)
	return v;
    /* code to allow classes to extend ENVSXP, SYMSXP, etc */
    if(IS_S4_OBJECT(v) && TYPEOF(v) == S4SXP) {
	SEXP vv = R_getS4DataSlot(v, ANYSXP);
	if(vv == R_NilValue)
	  Rf_error(_("no method for coercing this S4 class to a vector"));
	else if(TYPEOF(vv) == type)
	  return vv;
	v = vv;
    }

    switch (TYPEOF(v)) {
#ifdef NOTYET
    case NILSXP:
	ans = coerceNull(v, type);
	break;
#endif
    case SYMSXP:
	ans = coerceSymbol(v, type);
	break;
    case NILSXP:
    case LISTSXP:
	ans = coercePairList(v, type);
	break;
    case LANGSXP:
	if (type != STRSXP) {
	    ans = coercePairList(v, type);
	    break;
	}

	/* This is mostly copied from coercePairList, but we need to
	 * special-case the first element so as not to get operators
	 * put in backticks. */
	n = length(v);
	PROTECT(ans = Rf_allocVector(type, n));
	if (n == 0) {
	    /* Can this actually happen? */
	    UNPROTECT(1);
	    break;
	}
	i = 0;
	op = CAR(v);
	/* The case of practical relevance is "lhs ~ rhs", which
	 * people tend to split using as.character(), modify, and
	 * paste() back together. However, we might as well
	 * special-case all symbolic operators here. */
	if (TYPEOF(op) == SYMSXP) {
	    SET_STRING_ELT(ans, i, PRINTNAME(op));
	    i++;
	    v = CDR(v);
	}

	/* The distinction between strings and other elements was
	 * here "always", but is really dubious since it makes x <- a
	 * and x <- "a" come out identical. Won't fix just now. */
	for (vp = v;  vp != R_NilValue; vp = CDR(vp), i++) {
	    if (Rf_isString(CAR(vp)) && length(CAR(vp)) == 1)
		SET_STRING_ELT(ans, i, STRING_ELT(CAR(vp), 0));
	    else
		SET_STRING_ELT(ans, i, STRING_ELT(Rf_deparse1line(CAR(vp), CXXRFALSE), 0));
	}
	UNPROTECT(1);
	break;
    case VECSXP:
    case EXPRSXP:
	ans = Rf_coerceVectorList(v, type);
	break;
    case ENVSXP:
	Rf_error(_("environments cannot be coerced to other types"));
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

#define COERCE_ERROR_STRING "cannot coerce type '%s' to vector of type '%s'"

#define COERCE_ERROR							\
	Rf_error(_(COERCE_ERROR_STRING), Rf_type2char(TYPEOF(v)), Rf_type2char(type))

	switch (type) {
	case SYMSXP:
	    ans = coerceToSymbol(v);	    break;
	case LGLSXP:
	    ans = coerceToLogical(v);	    break;
	case INTSXP:
	    ans = coerceToInteger(v);	    break;
	case REALSXP:
	    ans = coerceToReal(v);	    break;
	case CPLXSXP:
	    ans = coerceToComplex(v);	    break;
	case RAWSXP:
	    ans = coerceToRaw(v);	    break;
	case STRSXP:
	    ans = coerceToString(v);	    break;
	case EXPRSXP:
	    ans = coerceToExpression(v);    break;
	case VECSXP:
	    ans = coerceToVectorList(v);    break;
	case LISTSXP:
	    ans = coerceToPairList(v);	    break;
	default:
	    COERCE_ERROR;
	}
	break;
    default:
	COERCE_ERROR;
    }
    return ans;
}
#undef COERCE_ERROR


SEXP Rf_CreateTag(SEXP x)
{
    if (Rf_isNull(x) || Rf_isSymbol(x))
	return x;
    if (Rf_isString(x)
	&& length(x) >= 1
	&& length(STRING_ELT(x, 0)) >= 1) {
	x = Rf_installTrChar(STRING_ELT(x, 0));
    } else
	x = Rf_installChar(STRING_ELT(Rf_deparse1(x, TRUE, SIMPLEDEPARSE), 0));
    return x;
}

static SEXP asFunction(SEXP x)
{
    SEXP f;
    int n;
    if (Rf_isFunction(x)) return x;
    if (MAYBE_REFERENCED(x)) PROTECT(x = Rf_duplicate(x));
    else PROTECT(x);

    if (Rf_isNull(x) || !Rf_isList(x)) {
	f = Rf_mkCLOSXP(nullptr, x, R_GlobalEnv);
    }
    else {
	n = length(x);
	SEXP formals = Rf_allocList(n - 1);
	SEXP pf = formals;
	while(--n) {
	    if (TAG(x) == R_NilValue) {
		SET_TAG(pf, Rf_CreateTag(CAR(x)));
		SETCAR(pf, R_MissingArg);
	    }
	    else {
		SETCAR(pf, CAR(x));
		SET_TAG(pf, TAG(x));
	    }
	    pf = CDR(pf);
	    x = CDR(x);
	}
	f = Rf_mkCLOSXP(formals, CAR(x), R_GlobalEnv);
    }
    UNPROTECT(1);
    return f;
}

static SEXP ascommon(SEXP call, SEXP u, SEXPTYPE type)
{
    /* -> as.vector(..) or as.XXX(.) : coerce 'u' to 'type' : */
    /* code assumes u is protected */

    SEXP v;
    if (type == CLOSXP) {
	return asFunction(u);
    }
    else if (Rf_isVector(u) || Rf_isList(u) || Rf_isLanguage(u)
	     || (Rf_isSymbol(u) && type == EXPRSXP)) {
	if (type != ANYSXP && TYPEOF(u) != type) v = Rf_coerceVector(u, type);
	else v = u;

	/* drop attributes() and class() in some cases for as.pairlist:
	   But why?  (And who actually coerces to pairlists?)
	 */
	if ((type == LISTSXP) &&
	    !(TYPEOF(u) == LANGSXP || TYPEOF(u) == LISTSXP ||
	      TYPEOF(u) == EXPRSXP || TYPEOF(u) == VECSXP)) {
      if (MAYBE_REFERENCED(v)) v = Rf_shallow_duplicate(v);
	    CLEAR_ATTRIB(v);
	}
	return v;
    }
    else if (Rf_isSymbol(u) && type == STRSXP)
	return Rf_ScalarString(PRINTNAME(u));
    else if (Rf_isSymbol(u) && type == SYMSXP)
	return u;
    else if (Rf_isSymbol(u) && type == VECSXP) {
	v = Rf_allocVector(VECSXP, 1);
	SET_VECTOR_ELT(v, 0, u);
	return v;
    }
    else Rf_errorcall(call, _(COERCE_ERROR_STRING),
		   Rf_type2char(TYPEOF(u)), Rf_type2char(type));
    return u;/* -Wall */
}

SEXP attribute_hidden do_asCharacterFactor(SEXP call, SEXP op, SEXP args,
                                           SEXP rho)
{
    SEXP x;
    x = CAR(args);
    return Rf_asCharacterFactor(x);
}

/* used in attrib.c, eval.c and unique.c */
SEXP Rf_asCharacterFactor(SEXP x)
{
    SEXP ans;

    if( !Rf_inherits(x, "factor") )
	Rf_error(_("attempting to coerce non-factor"));

    R_xlen_t i, n = XLENGTH(x);
    SEXP labels = Rf_getAttrib(x, R_LevelsSymbol);
    if (TYPEOF(labels) != STRSXP)
	Rf_error(_("malformed factor"));
    int nl = LENGTH(labels);
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
      int ii = INTEGER(x)[i];
      if (ii == NA_INTEGER)
	  SET_STRING_ELT(ans, i, NA_STRING);
      else if (ii >= 1 && ii <= nl)
	  SET_STRING_ELT(ans, i, STRING_ELT(labels, ii - 1));
      else
	  Rf_error(_("malformed factor"));
    }
    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden do_asatomic(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP ans, x;

    SEXPTYPE type = STRSXP;
    switch(op->variant()) {
    case 0:
	type = STRSXP; break;
    case 1:
	type = INTSXP; break;
    case 2:
	type = REALSXP; break;
    case 3:
	type = CPLXSXP; break;
    case 4:
	type = LGLSXP; break;
    case 5:
	type = RAWSXP; break;
    }

    if (type == RAWSXP) {
	op->checkNumArgs(num_args, 1, call);
    }

    x = num_args ? args[0] : nullptr;
    if(TYPEOF(x) == type) {
	if(ATTRIB(x) == R_NilValue) return x;
	ans = MAYBE_REFERENCED(x) ? Rf_duplicate(x) : x;
	CLEAR_ATTRIB(ans);
	return ans;
    }
    ans = ascommon(call, x, type);
    CLEAR_ATTRIB(ans);
    return ans;
}

/* NB: as.vector is used for several other as.xxxx, including
   as.expression, as.list, as.pairlist, as.symbol, (as.single) */
SEXP attribute_hidden do_asvector(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP x, ans;
    SEXPTYPE type;

    op->checkNumArgs(num_args, 2, call);
    x = args[0];

    if (!Rf_isString(args[1]) || LENGTH(args[1]) != 1)
	errorcall_return(call, R_MSG_mode);
    if (!strcmp("function", (CHAR(STRING_ELT(args[1], 0))))) /* ASCII */
	type = CLOSXP;
    else
	type = Rf_str2type(CHAR(STRING_ELT(args[1], 0))); /* ASCII */

    /* "any" case added in 2.13.0 */
    if(type == ANYSXP || TYPEOF(x) == type) {
	switch(TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    if(ATTRIB(x) == R_NilValue) return x;
	    ans  = MAYBE_REFERENCED(x) ? Rf_duplicate(x) : x;
	    CLEAR_ATTRIB(ans);
	    return ans;
	case EXPRSXP:
	case VECSXP:
	    return x;
	default:
	    ;
	}
    }

    if(IS_S4_OBJECT(x) && TYPEOF(x) == S4SXP) {
	SEXP v = R_getS4DataSlot(x, ANYSXP);
	if(v == R_NilValue)
	    Rf_error(_("no method for coercing this S4 class to a vector"));
	x = v;
    }

    switch(type) {/* only those are valid : */
    case SYMSXP: /* for as.symbol */
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP: /* for as.expression */
    case VECSXP: /* list */
    case LISTSXP:/* for as.pairlist */
    case CLOSXP: /* non-primitive function */
    case RAWSXP:
    case ANYSXP: /* any */
	break;
    default:
	errorcall_return(call, R_MSG_mode);
    }
    ans = ascommon(call, x, type);
    switch(TYPEOF(ans)) { /* keep attributes for these: */
    case NILSXP: /* doesn't have any */
    case LISTSXP: /* but ascommon fiddled */
    case LANGSXP:
    case VECSXP:
    case EXPRSXP:
	break;
    default:
	CLEAR_ATTRIB(ans);
	break;
    }
    return ans;
}


SEXP attribute_hidden do_asfunction(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* envir_)
{
    SEXP arglist, envir, names, args, pargs, body;
    int i, n;

    /* Check the arguments; we need a list and environment. */

    arglist = x_;
    if (!Rf_isNewList(arglist))
	Rf_errorcall(call, _("list argument expected"));

    envir = envir_;
    if (Rf_isNull(envir)) {
	Rf_error(_("use of NULL environment is defunct"));
	envir = R_BaseEnv;
    } else
    if (!Rf_isEnvironment(envir))
	Rf_errorcall(call, _("invalid environment"));

    n = length(arglist);
    if (n < 1)
	Rf_errorcall(call, _("argument must have length at least 1"));
    names = Rf_getAttrib(arglist, R_NamesSymbol);
    PROTECT(pargs = args = Rf_allocList(n - 1));
    for (i = 0; i < n - 1; i++) {
	SETCAR(pargs, VECTOR_ELT(arglist, i));
	if (names != R_NilValue && *CHAR(STRING_ELT(names, i)) != '\0') /* ASCII */
	    SET_TAG(pargs, Rf_installTrChar(STRING_ELT(names, i)));
	else
	    SET_TAG(pargs, R_NilValue);
	pargs = CDR(pargs);
    }
    PROTECT(body = VECTOR_ELT(arglist, n-1));
    /* the main (only?) thing to rule out is body being
       a function already. If we test here then
       Rf_mkCLOSXP can continue to overreact when its
       test fails (PR#1880, 7535, 7702) */
    if(Rf_isList(body) || Rf_isLanguage(body) || Rf_isSymbol(body)
       || Rf_isExpression(body) || Rf_isVector(body) || isByteCode(body)
       )
	    args =  Rf_mkCLOSXP(args, body, envir);
    else
	    Rf_errorcall(call, _("invalid body for function"));
    UNPROTECT(2);
    return args;
}


/* primitive */
SEXP attribute_hidden do_ascall(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* args)
{
    SEXP ap, ans, names;
    int i, n;

    switch (TYPEOF(args)) {
    case LANGSXP:
	ans = args;
	break;
    case VECSXP:
	{
	    if(0 == (n = length(args)))
		Rf_errorcall(call, _("invalid length 0 argument"));
	    names = Rf_getAttrib(args, R_NamesSymbol);
	    GCStackRoot<PairList> tl(PairList::make(n - 1));
	    PROTECT(ap = ans = new Expression(nullptr, tl));
	    for (i = 0; i < n; i++) {
		SETCAR(ap, VECTOR_ELT(args, i));
		if (names != R_NilValue && !Rf_StringBlank(STRING_ELT(names, i)))
		    SET_TAG(ap, Rf_install(Rf_translateChar(STRING_ELT(names, i))));
		ap = CDR(ap);
	    }
	    UNPROTECT(1);
	    break;
	}
    case EXPRSXP:
	{
	    if(0 == (n = length(args)))
		Rf_errorcall(call, _("invalid length 0 argument"));
	    names = Rf_getAttrib(args, R_NamesSymbol);
	    GCStackRoot<PairList> tl(PairList::make(n - 1));
	    PROTECT(ap = ans = new Expression(nullptr, tl));
	    for (i = 0; i < n; i++) {
		SETCAR(ap, XVECTOR_ELT(args, i));
		if (names != R_NilValue && !Rf_StringBlank(STRING_ELT(names, i)))
		    SET_TAG(ap, Rf_installTrChar(STRING_ELT(names, i)));
		ap = CDR(ap);
	    }
	    UNPROTECT(1);
	    break;
	}
    case LISTSXP:
	{
	    ConsCell* cc = SEXP_downcast<ConsCell*>(args);
	    GCStackRoot<Expression> ansr(ConsCell::convert<Expression>(cc));
	    ans = ansr;
	    break;
	}
    default:
	Rf_errorcall(call, _("invalid argument list"));
	ans = R_NilValue;
    }
    SET_TAG(ans, R_NilValue);
    return ans;
}


/* int, not Rboolean, for NA_LOGICAL : */
int Rf_asLogical(SEXP x)
{
    int warn = 0;

    if (Rf_isVectorAtomic(x)) {
	if (XLENGTH(x) < 1)
	    return NA_LOGICAL;
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return LOGICAL(x)[0];
	case INTSXP:
	    return LogicalFromInteger(INTEGER(x)[0], &warn);
	case REALSXP:
	    return LogicalFromReal(REAL(x)[0], &warn);
	case CPLXSXP:
	    return LogicalFromComplex(COMPLEX(x)[0], &warn);
	case STRSXP:
	    return LogicalFromString(STRING_ELT(x, 0), &warn);
	case RAWSXP:
	    return LogicalFromInteger(int(RAW(x)[0]), &warn);
	default:
	    UNIMPLEMENTED_TYPE("asLogical", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	    return LogicalFromString(x, &warn);
    }
    return NA_LOGICAL;
}

int Rf_asInteger(SEXP x)
{
    int warn = 0, res;

    if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return Rf_IntegerFromLogical(LOGICAL(x)[0], &warn);
	case INTSXP:
	    return INTEGER(x)[0];
	case REALSXP:
	    res = Rf_IntegerFromReal(REAL(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case CPLXSXP:
	    res = Rf_IntegerFromComplex(COMPLEX(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case STRSXP:
	    res = Rf_IntegerFromString(STRING_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	default:
	    UNIMPLEMENTED_TYPE("asInteger", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	res = Rf_IntegerFromString(x, &warn);
	Rf_CoercionWarning(warn);
	return res;
    }
    return NA_INTEGER;
}

double Rf_asReal(SEXP x)
{
    int warn = 0;
    double res;

    if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    res = RealFromLogical(LOGICAL(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case INTSXP:
	    res = RealFromInteger(INTEGER(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case REALSXP:
	    return REAL(x)[0];
	case CPLXSXP:
	    res = RealFromComplex(COMPLEX(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	case STRSXP:
	    res = RealFromString(STRING_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return res;
	default:
	    UNIMPLEMENTED_TYPE("asReal", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	res = RealFromString(x, &warn);
	Rf_CoercionWarning(warn);
	return res;
    }
    return NA_REAL;
}

Rcomplex Rf_asComplex(SEXP x)
{
    int warn = 0;
    Rcomplex z;

    if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    z = ComplexFromLogical(LOGICAL(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	case INTSXP:
	    z = ComplexFromInteger(INTEGER(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	case REALSXP:
	    z = ComplexFromReal(REAL(x)[0], &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	case CPLXSXP:
	    return COMPLEX(x)[0];
	case STRSXP:
	    z = ComplexFromString(STRING_ELT(x, 0), &warn);
	    Rf_CoercionWarning(warn);
	    return z;
	default:
	    UNIMPLEMENTED_TYPE("asComplex", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	z = ComplexFromString(x, &warn);
	Rf_CoercionWarning(warn);
	return z;
    }
    z.r = NA_REAL;
    z.i = NA_REAL;
    return z;
}


/* return the type (= "detailed mode") of the SEXP */
SEXP attribute_hidden do_typeof(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_)
{
    return Rf_type2rstr(TYPEOF(x_));
}

/* Define many of the <primitive> "is.xxx" functions :
   Note that  Rf_isNull, Rf_isNumeric, etc are defined in util.c or ../include/Rinlinedfuns.h
*/
SEXP attribute_hidden do_is(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP ans;

    PROTECT(ans = Rf_allocVector(LGLSXP, 1));

    switch (op->variant()) {
    case NILSXP:	/* is.null */
	LOGICAL(ans)[0] = Rf_isNull(args[0]);
	break;
    case LGLSXP:	/* is.logical */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == LGLSXP);
	break;
    case INTSXP:	/* is.integer */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == INTSXP)
	    && !Rf_inherits(args[0], "factor");
	break;
    case REALSXP:	/* is.double */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == REALSXP);
	break;
    case CPLXSXP:	/* is.complex */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == CPLXSXP);
	break;
    case STRSXP:	/* is.character */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == STRSXP);
	break;
    case SYMSXP:	/* is.symbol === is.name */
	if(IS_S4_OBJECT(args[0]) && (TYPEOF(args[0]) == S4SXP)) {
	    SEXP dot_xData = R_getS4DataSlot(args[0], SYMSXP);
	    LOGICAL(ans)[0] = (TYPEOF(dot_xData) == SYMSXP);
	}
	else
	    LOGICAL(ans)[0] = (TYPEOF(args[0]) == SYMSXP);
	break;
    case ENVSXP:	/* is.environment */
	if(IS_S4_OBJECT(args[0]) && (TYPEOF(args[0]) == S4SXP)) {
	    SEXP dot_xData = R_getS4DataSlot(args[0], ENVSXP);
	    LOGICAL(ans)[0] = (TYPEOF(dot_xData) == ENVSXP);
	}
	else
	    LOGICAL(ans)[0] = (TYPEOF(args[0]) == ENVSXP);
	break;
    case VECSXP:	/* is.list */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == VECSXP ||
			   TYPEOF(args[0]) == LISTSXP);
	break;
    case LISTSXP:	/* is.pairlist */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == LISTSXP ||
			   TYPEOF(args[0]) == NILSXP);/* pairlist() -> NULL */
	break;
    case EXPRSXP:	/* is.expression */
	LOGICAL(ans)[0] = TYPEOF(args[0]) == EXPRSXP;
	break;
    case RAWSXP:	/* is.raw */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == RAWSXP);
	break;

    case 50:		/* is.object */
	LOGICAL(ans)[0] = OBJECT(args[0]);
	break;
    case 51:		/* isS4 */
	LOGICAL(ans)[0] = IS_S4_OBJECT(args[0]) != 0;
	break;
/* no longer used: is.data.frame is R code
    case 80:
	LOGICAL(ans)[0] = isFrame(CAR(args));
	break;
*/

    case 100:		/* is.numeric */
	LOGICAL(ans)[0] = Rf_isNumeric(args[0]) &&
	    !Rf_isLogical(args[0]);  /* Rf_isNumeric excludes factors */
	break;
    case 101:		/* is.matrix */
	LOGICAL(ans)[0] = Rf_isMatrix(args[0]);
	break;
    case 102:		/* is.array */
	LOGICAL(ans)[0] = Rf_isArray(args[0]);
	break;

    case 200:		/* is.atomic */
	switch(TYPEOF(args[0])) {
	case NILSXP:
	    /* NULL is atomic (S compatibly), but not in Rf_isVectorAtomic(.) */
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    LOGICAL(ans)[0] = 1;
	    break;
	default:
	    LOGICAL(ans)[0] = 0;
	    break;
	}
	break;
    case 201:		/* is.recursive */
	switch(TYPEOF(args[0])) {
	case VECSXP:
	case LISTSXP:
	case CLOSXP:
	case ENVSXP:
	case PROMSXP:
	case LANGSXP:
	case SPECIALSXP:
	case BUILTINSXP:
	case DOTSXP:
	case ANYSXP:
	case EXPRSXP:
	// Not recursive, as long as not subsettable (on the R level)
	// case EXTPTRSXP:
	// case BCODESXP:
	// case WEAKREFSXP:
	    LOGICAL(ans)[0] = 1;
	    break;
	default:
	    LOGICAL(ans)[0] = 0;
	    break;
	}
	break;

    case 300:		/* is.call */
	LOGICAL(ans)[0] = TYPEOF(args[0]) == LANGSXP;
	break;
    case 301:		/* is.language */
	LOGICAL(ans)[0] = (TYPEOF(args[0]) == SYMSXP ||
			   TYPEOF(args[0]) == LANGSXP ||
			   TYPEOF(args[0]) == EXPRSXP);
	break;
    case 302:		/* is.function */
	LOGICAL(ans)[0] = Rf_isFunction(args[0]);
	break;

    case 999:		/* is.single */
	Rf_errorcall(call, _("type \"single\" unimplemented in R"));
    default:
	Rf_errorcall(call, _("unimplemented predicate"));
    }
    UNPROTECT(1);
    return (ans);
}

/* What should is.vector do ?
 * In S, if an object has no attributes it is a vector, otherwise it isn't.
 * It seems to make more sense to check for a dim attribute.
 */

// is.vector(x, mode) :
SEXP attribute_hidden do_isvector(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* mode_)
{
    SEXP ans, a, x;
    const char *stype;

    x = x_;
    if (!Rf_isString(mode_) || LENGTH(mode_) != 1)
	errorcall_return(call, R_MSG_mode);

    stype = CHAR(STRING_ELT(mode_, 0)); /* ASCII */

    /* "name" and "symbol" are synonymous */
    if (streql(stype, "name"))
      stype = "symbol";

    PROTECT(ans = Rf_allocVector(LGLSXP, 1));
    if (streql(stype, "any")) {
	/* isVector is inlined, means atomic or VECSXP or EXPRSXP */
	LOGICAL(ans)[0] = Rf_isVector(x);
    }
    else if (streql(stype, "numeric")) {
	LOGICAL(ans)[0] = (Rf_isNumeric(x) && !Rf_isLogical(x));
    }
    /* So this allows any type, including undocumented ones such as
       "closure", but not aliases such as "name" and "function". */
    else if (streql(stype, Rf_type2char(TYPEOF(x)))) {
	LOGICAL(ans)[0] = 1;
    }
    else
	LOGICAL(ans)[0] = 0;

    /* We allow a "names" attribute on any vector. */
    if (LOGICAL(ans)[0] && ATTRIB(x_) != R_NilValue) {
	a = ATTRIB(x_);
	while(a != R_NilValue) {
	    if (TAG(a) != R_NamesSymbol) {
		LOGICAL(ans)[0] = 0;
		break;
	    }
	    a = CDR(a);
	}
    }
    UNPROTECT(1);
    return (ans);
}

namespace {
    inline int LIST_VEC_NA(SEXP s)
    {
	if (!Rf_isVector(s) || length(s) != 1)
	    return 0;
	else {
	    switch (TYPEOF(s)) {
	    case LGLSXP:
	    case INTSXP:
		return (INTEGER(s)[0] == NA_INTEGER);
		break;
	    case REALSXP:
		return ISNAN(REAL(s)[0]);
		break;
	    case STRSXP:
		return (STRING_ELT(s, 0) == NA_STRING);
		break;
	    case CPLXSXP:
		return (ISNAN(COMPLEX(s)[0].r)
			|| ISNAN(COMPLEX(s)[0].i));
		break;
	    default:
		return 0;
	    }
	}
    }
}
    
SEXP attribute_hidden do_isna(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP ans, dims, names, x;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(args[0]) && !Rf_isVector(args[0])))
	errorcall_return(call, "is.na " R_MSG_list_vec);

#endif
    x = args[0];
    n = Rf_xlength(x);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));
    if (Rf_isVector(x)) {
	PROTECT(dims = Rf_getAttrib(x, R_DimSymbol));
	if (Rf_isArray(x))
	    PROTECT(names = Rf_getAttrib(x, R_DimNamesSymbol));
	else
	    PROTECT(names = Rf_getAttrib(x, R_NamesSymbol));
    }
    else dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case LGLSXP:
       for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (LOGICAL(x)[i] == NA_LOGICAL);
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (INTEGER(x)[i] == NA_INTEGER);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = ISNAN(REAL(x)[i]);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (ISNAN(COMPLEX(x)[i].r) ||
			       ISNAN(COMPLEX(x)[i].i));
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (STRING_ELT(x, i) == NA_STRING);
	break;
    case LISTSXP:
	for (i = 0; i < n; i++) {
	    LOGICAL(ans)[i] = LIST_VEC_NA(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	for (i = 0; i < n; i++) {
	    SEXP s = VECTOR_ELT(x, i);
	    LOGICAL(ans)[i] = LIST_VEC_NA(s);
	}
	break;
    case RAWSXP:
	/* no such thing as a raw NA */
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
	break;
    default:
	Rf_warningcall(call, _("%s() applied to non-(list or vector) of type '%s'"),
		    "is.na", Rf_type2char(TYPEOF(x)));
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
    }
    if (dims != R_NilValue)
	Rf_setAttrib(ans, R_DimSymbol, dims);
    if (names != R_NilValue) {
	if (Rf_isArray(x))
	    Rf_setAttrib(ans, R_DimNamesSymbol, names);
	else
	    Rf_setAttrib(ans, R_NamesSymbol, names);
    }
    if (Rf_isVector(x))
	UNPROTECT(2);
    UNPROTECT(1);
    UNPROTECT(1); /*ans*/
    return ans;
}

// Check if x has missing values; the anyNA.default() method
static Rboolean anyNA(SEXP call, SEXP op, SEXP args, SEXP env)
/* Original code:
   Copyright 2012 Google Inc. All Rights Reserved.
   Author: Tim Hesterberg <rocket@google.com>
   Distributed under GPL 2 or later
*/
{
    SEXP x = CAR(args);
    SEXPTYPE xT = TYPEOF(x);
    Rboolean isList = Rboolean(xT == VECSXP || xT == LISTSXP),
	recursive = FALSE;

    if (isList && length(args) > 1)
	recursive = Rboolean(Rf_asLogical(CADR(args)));
    if (OBJECT(x) || (isList && !recursive)) {
	SEXP e0 = PROTECT(Rf_lang2(Rf_install("is.na"), x));
	SEXP e = PROTECT(Rf_lang2(Rf_install("any"), e0));
	SEXP res = PROTECT(Rf_eval(e, env));
	int ans = Rf_asLogical(res);
	UNPROTECT(3);
	return Rboolean(ans == 1); // so NA answer is false.
    }

    R_xlen_t i, n = Rf_xlength(x);
    switch (xT) {
    case REALSXP:
    {
	double *xD = REAL(x);
	for (i = 0; i < n; i++)
	    if (ISNAN(xD[i])) return TRUE;
	break;
    }
    case INTSXP:
    {
	int *xI = INTEGER(x);
	for (i = 0; i < n; i++)
	    if (xI[i] == NA_INTEGER) return TRUE;
	break;
    }
    case LGLSXP:
    {
	int *xI = LOGICAL(x);
	for (i = 0; i < n; i++)
	    if (xI[i] == NA_LOGICAL) return TRUE;
	break;
    }
    case CPLXSXP:
    {
	Rcomplex *xC = COMPLEX(x);
	for (i = 0; i < n; i++)
	    if (ISNAN(xC[i].r) || ISNAN(xC[i].i)) return TRUE;
	break;
    }
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (STRING_ELT(x, i) == NA_STRING) return TRUE;
	break;
    case RAWSXP: /* no such thing as a raw NA:  is.na(.) gives FALSE always */
	return FALSE;
    case NILSXP: // is.na() gives a warning..., but we do not.
	return FALSE;
    // The next two cases are only used if recursive = TRUE
    case LISTSXP:
    {
	SEXP call2, args2, ans;
	args2 = PROTECT(Rf_shallow_duplicate(args));
	call2 = PROTECT(Rf_shallow_duplicate(call));
	for (i = 0; i < n; i++, x = CDR(x)) {
	    SETCAR(args2, CAR(x)); SETCADR(call2, CAR(x));
	    if ((Rf_DispatchOrEval(call2, op, args2, env, &ans,
				   MissingArgHandling::Keep, 1)
		 && Rf_asLogical(ans)) || anyNA(call2, op, args2, env)) {
		UNPROTECT(2);
		return TRUE;
	    }
	}
	UNPROTECT(2);
	break;
    }
    case VECSXP:
    {
	SEXP call2, args2, ans;
	args2 = PROTECT(Rf_shallow_duplicate(args));
	call2 = PROTECT(Rf_shallow_duplicate(call));
	for (i = 0; i < n; i++) {
	    SETCAR(args2, VECTOR_ELT(x, i)); SETCADR(call2, VECTOR_ELT(x, i));
	    if ((Rf_DispatchOrEval(call2, op, args2, env, &ans,
				   MissingArgHandling::Keep, 1)
		 && Rf_asLogical(ans)) || anyNA(call2, op, args2, env)) {
		UNPROTECT(2);
		return TRUE;
	    }
	}
	UNPROTECT(2);
	break;
    }

    default:
	Rf_error("anyNA() applied to non-(list or vector) of type '%s'",
		 Rf_type2char(TYPEOF(x)));
    }
    return FALSE;
} // anyNA()

SEXP attribute_hidden do_anyNA(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    static SEXP do_anyNA_formals = NULL;

    if (length(args) < 1 || length(args) > 2)
	Rf_errorcall(call, "anyNA takes 1 or 2 arguments");

    if (Rf_DispatchOrEval(call, op, args, rho, &ans,
			  MissingArgHandling::Keep, 1))
	return ans;

    if(length(args) == 1) {
	SEXP_downcast<Expression*>(call)->check1arg("x");
 	ans = Rf_ScalarLogical(anyNA(call, op, args, rho));
   } else {
	/* This is a primitive, so we manage argument matching ourselves.
	   But this takes a little time.
	 */
	if (do_anyNA_formals == NULL)
	    do_anyNA_formals = Rf_allocFormalsList2(Rf_install("x"),
						    R_RecursiveSymbol);
	PROTECT(args = Rf_matchArgs(do_anyNA_formals, args, call));
	if(CADR(args) ==  R_MissingArg) SETCADR(args, Rf_ScalarLogical(FALSE));
	ans = Rf_ScalarLogical(anyNA(call, op, args, rho));
	UNPROTECT(1);
    }
    return ans;
}


SEXP attribute_hidden do_isnan(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP ans, dims, names, x;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(args[0]) && !Rf_isVector(args[0]))
	errorcall_return(call, "is.nan " R_MSG_list_vec);
#endif
    x = args[0];
    n = Rf_xlength(x);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));
    if (Rf_isVector(x)) {
	PROTECT(dims = Rf_getAttrib(x, R_DimSymbol));
	if (Rf_isArray(x))
	    PROTECT(names = Rf_getAttrib(x, R_DimNamesSymbol));
	else
	    PROTECT(names = Rf_getAttrib(x, R_NamesSymbol));
    }
    else dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = R_IsNaN(REAL(x)[i]);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (R_IsNaN(COMPLEX(x)[i].r) ||
			       R_IsNaN(COMPLEX(x)[i].i));
	break;
    default:
	Rf_errorcall(call, _("default method not implemented for type '%s'"),
		     Rf_type2char(TYPEOF(x)));
    }
    if (dims != R_NilValue)
	Rf_setAttrib(ans, R_DimSymbol, dims);
    if (names != R_NilValue) {
	if (Rf_isArray(x))
	    Rf_setAttrib(ans, R_DimNamesSymbol, names);
	else
	    Rf_setAttrib(ans, R_NamesSymbol, names);
    }
    if (Rf_isVector(x))
	UNPROTECT(2);
    UNPROTECT(1);
    UNPROTECT(1); /*ans*/
    return ans;
}

SEXP attribute_hidden do_isfinite(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP ans, x, names, dims;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(CAR(args)) && !Rf_isVector(CAR(args)))
	errorcall_return(call, "is.finite " R_MSG_list_vec);
#endif
    x = args[0];
    n = Rf_xlength(x);
    ans = Rf_allocVector(LGLSXP, n);
    if (Rf_isVector(x)) {
	dims = Rf_getAttrib(x, R_DimSymbol);
	if (Rf_isArray(x))
	    names = Rf_getAttrib(x, R_DimNamesSymbol);
	else
	    names = Rf_getAttrib(x, R_NamesSymbol);
    }
    else dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
	break;
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (INTEGER(x)[i] != NA_INTEGER);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = R_FINITE(REAL(x)[i]);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (R_FINITE(COMPLEX(x)[i].r) && R_FINITE(COMPLEX(x)[i].i));
	break;
    default:
	Rf_errorcall(call, _("default method not implemented for type '%s'"),
		     Rf_type2char(TYPEOF(x)));
    }
    if (dims != R_NilValue)
	Rf_setAttrib(ans, R_DimSymbol, dims);
    if (names != R_NilValue) {
	if (Rf_isArray(x))
	    Rf_setAttrib(ans, R_DimNamesSymbol, names);
	else
	    Rf_setAttrib(ans, R_NamesSymbol, names);
    }
    return ans;
}

SEXP attribute_hidden do_isinfinite(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP ans, x, names, dims;
    double xr, xi;
    R_xlen_t i, n;

#ifdef stringent_is
    if (!Rf_isList(CAR(args)) && !Rf_isVector(CAR(args)))
	errorcall_return(call, "is.infinite " R_MSG_list_vec);
#endif
    x = args[0];
    n = Rf_xlength(x);
    ans = Rf_allocVector(LGLSXP, n);
    if (Rf_isVector(x)) {
	dims = Rf_getAttrib(x, R_DimSymbol);
	if (Rf_isArray(x))
	    names = Rf_getAttrib(x, R_DimNamesSymbol);
	else
	    names = Rf_getAttrib(x, R_NamesSymbol);
    }
    else	dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
	    xr = REAL(x)[i];
	    if (ISNAN(xr) || R_FINITE(xr))
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] = 1;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    xr = COMPLEX(x)[i].r;
	    xi = COMPLEX(x)[i].i;
	    if ((ISNAN(xr) || R_FINITE(xr)) && (ISNAN(xi) || R_FINITE(xi)))
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] = 1;
	}
	break;
    default:
	Rf_errorcall(call, _("default method not implemented for type '%s'"),
		     Rf_type2char(TYPEOF(x)));
    }
    if (!Rf_isNull(dims))
	Rf_setAttrib(ans, R_DimSymbol, dims);
    if (!Rf_isNull(names)) {
	if (Rf_isArray(x))
	    Rf_setAttrib(ans, R_DimNamesSymbol, names);
	else
	    Rf_setAttrib(ans, R_NamesSymbol, names);
    }
    return ans;
}

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_call(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rest, evargs, rfun, tmp;

    PROTECT(rfun = Rf_eval(CAR(args), rho));
    /* zero-length string check used to be here but Rf_install gives
       better error message.
     */
    if (!Rf_isString(rfun) || length(rfun) != 1)
	errorcall_return(call, _("first argument must be a character string"));
    const char *str = Rf_translateChar(STRING_ELT(rfun, 0));
    if (streql(str, ".Internal")) Rf_error("illegal usage");
    PROTECT(rfun = Rf_install(str));
    PROTECT(evargs = Rf_shallow_duplicate(CDR(args)));
    for (rest = evargs; rest != R_NilValue; rest = CDR(rest)) {
	PROTECT(tmp = Rf_eval(CAR(rest), rho));
	if (NAMED(tmp)) MARK_NOT_MUTABLE(tmp);
	SETCAR(rest, tmp);
    }
    rfun = LCONS(rfun, evargs);
    UNPROTECT(3);
    return (rfun);
}

SEXP attribute_hidden do_docall(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* what_, CXXR::RObject* args_, CXXR::RObject* envir_)
{
    SEXP c, fun, names, envir;
    int i, n;

    fun = what_;
    envir = envir_;
    SEXP args = args_;

    /* must be a string or a function:
       zero-length string check used to be here but Rf_install gives
       better error message.
     */
    if(!(Rf_isFunction(fun) || (Rf_isString(fun) && length(fun) == 1)))
	Rf_error(_("'what' must be a function or character string"));

#ifdef __maybe_in_the_future__
    if (!isNull(args) && !isVectorList(args))
	error(_("'args' must be a list or expression"));
#else
    if (!Rf_isNull(args) && !Rf_isNewList(args))
        Rf_error(_("'%s' must be a list"), "args");
#endif

    if (!Rf_isEnvironment(envir))
	Rf_error(_("'envir' must be an environment"));

    n = length(args);
    names = Rf_getAttrib(args, R_NamesSymbol);

    GCStackRoot<PairList> tl(PairList::make(n));
    PROTECT(c = call = new Expression(nullptr, tl));
    if( Rf_isString(fun) ) {
	const char *str = Rf_translateChar(STRING_ELT(fun, 0));
	if (streql(str, ".Internal")) Rf_error("illegal usage");
	SETCAR(c, Rf_install(str));
    } else {
	if(TYPEOF(fun) == SPECIALSXP && streql(PRIMNAME(fun), ".Internal"))
	    Rf_error("illegal usage");
	SETCAR(c, fun);
    }
    c = CDR(c);
    for (i = 0; i < n; i++) {
#ifndef NEW
	SETCAR(c, VECTOR_ELT(args, i));
#else
	SETCAR(c, Promise::createEvaluatedPromise(VECTOR_ELT(args, i),
						  VECTOR_ELT(args, i)));
#endif
	if (Rf_ItemName(names, int(i)) != R_NilValue)
	    SET_TAG(c, Rf_installTrChar(Rf_ItemName(names, i)));
	c = CDR(c);
    }
    UNPROTECT(1);
    return Rf_eval(call, envir);
}


/*
   do_substitute has two arguments, an expression and an environment
   (optional).	Symbols found in the expression are substituted with their
   values as found in the environment.	There is no inheritance so only
   the supplied environment is searched. If no environment is specified
   the environment in which substitute was called is used.  If the
   specified environment is R_GlobalEnv it is converted to R_NilValue, for
   historical reasons. In Rf_substitute(), R_NilValue signals that no
   substitution should be done, only extraction of promise expressions.
   Arguments to do_substitute should not be evaluated.
*/

SEXP Rf_substitute(SEXP lang, SEXP rho)
{
    SEXP t;
    switch (TYPEOF(lang)) {
    case PROMSXP:
	return Rf_substitute(PREXPR(lang), rho);
    case SYMSXP:
	if (rho != R_NilValue) {
	    t = Rf_findVarInFrame3( rho, lang, TRUE);
	    if (t != R_UnboundValue) {
		if (TYPEOF(t) == PROMSXP) {
		    do {
			t = PREXPR(t);
		    } while(TYPEOF(t) == PROMSXP);
		    /* make sure code will not be modified: */
		    if (NAMED(t) < 2) SET_NAMED(t, 2);
		    return t;
		}
		else if (TYPEOF(t) == DOTSXP)
		    Rf_error(_("'...' used in an incorrect context"));
		if (rho != R_GlobalEnv)
		    return t;
	    }
	}
	return (lang);
    case LANGSXP:
	return Rf_substituteList(lang, rho);
    default:
	return (lang);
    }
}


/* Work through a list doing Rf_substitute on the
   elements taking particular care to handle '...' */

SEXP attribute_hidden Rf_substituteList(SEXP el, SEXP rho)
{
    SEXP h, p = R_NilValue, res = R_NilValue;

    if (Rf_isNull(el)) return el;

    while (el != R_NilValue) {
	/* walk along the pairlist, substituting elements.
	   res is the result
	   p is the current last element
	   h is the element currently being processed
	 */
	if (CAR(el) == R_DotsSymbol) {
	    if (rho == R_NilValue)
		h = R_UnboundValue;	/* so there is no substitution below */
	    else
		h = Rf_findVarInFrame3(rho, CAR(el), TRUE);
	    if (h == R_UnboundValue)
		h = CONS(R_DotsSymbol, R_NilValue);
	    else if (h == R_NilValue  || h == R_MissingArg)
		h = R_NilValue;
	    else if (TYPEOF(h) == DOTSXP)
		h = Rf_substituteList(h, R_NilValue);
	    else
		Rf_error(_("'...' used in an incorrect context"));
	} else {
	    h = Rf_substitute(CAR(el), rho);
	    if (Rf_isLanguage(el))
		h = LCONS(h, R_NilValue);
	    else
		h = CONS(h, R_NilValue);
	    SET_TAG(h, TAG(el));
	}
	if (h != R_NilValue) {
	    if (res == R_NilValue)
		PROTECT(res = h);
	    else
		SETCDR(p, h);
	    /* now set 'p': dots might have expanded to a list of length > 1 */
	    while (CDR(h) != R_NilValue) h = CDR(h);
	    p = h;
	}
	el = CDR(el);
    }
    if(res != R_NilValue) UNPROTECT(1);
    return res;
}


/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_substitute(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP argList, env, s, t;
    static SEXP do_substitute_formals = NULL;

    if (do_substitute_formals == NULL)
        do_substitute_formals = Rf_allocFormalsList2(Rf_install("expr"),
						     Rf_install("env"));

    /* argument matching */
    PROTECT(argList = Rf_matchArgs(do_substitute_formals, args, call));

    /* set up the environment for substitution */
    if (CADR(argList) == R_MissingArg)
	env = rho;
    else
	env = Rf_eval(CADR(argList), rho);
    if (env == R_GlobalEnv)	/* For historical reasons, don't substitute in R_GlobalEnv */
	env = R_NilValue;
    else if (TYPEOF(env) == VECSXP)
	env = Rf_NewEnvironment(R_NilValue, Rf_VectorToPairList(env), R_BaseEnv);
    else if (TYPEOF(env) == LISTSXP)
	env = Rf_NewEnvironment(R_NilValue, Rf_duplicate(env), R_BaseEnv);
    if (env != R_NilValue && TYPEOF(env) != ENVSXP)
	Rf_errorcall(call, _("invalid environment specified"));

    PROTECT(env);
    PROTECT(t = CONS(Rf_duplicate(CAR(argList)), R_NilValue));
    s = Rf_substituteList(t, env);
    UNPROTECT(3);
    return CAR(s);
}

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_quote(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP val = CAR(args);
    /* Make sure expression has NAMED == 2 before being returning
       in order to avoid modification of source code */
    if (NAMED(val) != 2) SET_NAMED(val, 2);
    return(val);
}

typedef struct {
    CXXRCONST char *s;
    SEXPTYPE sexp;
    Rboolean canChange;
} classType;

static classType classTable[] = {
    { "logical",	LGLSXP,	   TRUE },
    { "integer",	INTSXP,	   TRUE },
    { "double",		REALSXP,   TRUE },
    { "raw",		RAWSXP,    TRUE },
    { "complex",	CPLXSXP,   TRUE },
    { "character",	STRSXP,	   TRUE },
    { "expression",	EXPRSXP,   TRUE },
    { "list",		VECSXP,	   TRUE },
    { "environment",    ENVSXP,    FALSE },
    { "char",		CHARSXP,   TRUE },
    { "externalptr",	EXTPTRSXP,  FALSE },
    { "weakref",	WEAKREFSXP, FALSE },
    { "name",		SYMSXP,	   FALSE },

    { nullptr,	        SEXPTYPE(-1), FALSE}
};

static int class2type(const char *s)
{
    /* return the type if the class string is one of the basic types, else -1.
       Note that this is NOT Rf_str2type:  only certain types are defined to be basic
       classes; e.g., "language" is a type but many classes correspond to objects of
       this type.
    */
    int i; CXXRCONST char *si;
    for(i = 0; ; i++) {
	si = classTable[i].s;
	if(!si)
	    return -1;
	if(!strcmp(s, si))
	    return i;
    }
    /* cannot get here return -1; */
}

static SEXP do_unsetS4(SEXP obj, SEXP newClass)
{
  if(Rf_isNull(newClass))  { /* NULL class is only valid for S3 objects */
    Rf_warning(_("Setting class(x) to NULL;   result will no longer be an S4 object"));
  }
  else if(length(newClass) > 1)
    Rf_warning(_("Setting class(x) to multiple strings (\"%s\", \"%s\", ...); result will no longer be an S4 object"),
	    Rf_translateChar(STRING_ELT(newClass, 0)),
	    Rf_translateChar(STRING_ELT(newClass, 1)));
  else
    Rf_warning(_("Setting class(x) to \"%s\" sets attribute to NULL; result will no longer be an S4 object"),
	    CHAR(Rf_asChar(newClass)));
  UNSET_S4_OBJECT(obj);
  return obj;
}

/* set the class to value, and return the modified object.  This is
   NOT a primitive assignment operator , because there is no code in R
   that changes type in place. */
static SEXP R_set_class(SEXP obj, SEXP value, SEXP call)
{
    int nProtect = 0;
    // use of zero-length vector used to be documented.
    if(!length(value)) { // usually NULL
	Rf_setAttrib(obj, R_ClassSymbol, value);
	if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	  do_unsetS4(obj, value);
	return obj;
    }
    if(TYPEOF(value) != STRSXP) {
	SEXP dup;
	/* assumes value is protected, which it is in R_do_set_class */
	PROTECT(dup = Rf_duplicate(value));
	PROTECT(value = Rf_coerceVector(dup, STRSXP));
	nProtect += 2;
    }
    if(length(value) > 1) {
	Rf_setAttrib(obj, R_ClassSymbol, value);
	if(IS_S4_OBJECT(obj)) /*  multiple strings only valid for S3 objects */
	  do_unsetS4(obj, value);
    }
    else if(length(value) == 0) {
	UNPROTECT(nProtect); nProtect = 0;
	Rf_error(_("invalid replacement object to be a class string"));
    }
    else {
	const char *valueString;
	int whichType;

	SEXP cur_class; SEXPTYPE valueType;
	valueString = CHAR(Rf_asChar(value)); /* ASCII */
	whichType = class2type(valueString);
	valueType = (whichType == -1) ? SEXPTYPE( -1) : classTable[whichType].sexp;
	PROTECT(cur_class = R_data_class(obj, FALSE)); nProtect++;
	/*  assigning type as a class deletes an explicit class attribute. */
	if(valueType != CXXRCONSTRUCT(SEXPTYPE, -1)) {
	    Rf_setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      do_unsetS4(obj, value);
	    if(classTable[whichType].canChange) {
		PROTECT(obj = ascommon(call, obj, valueType));
		nProtect++;
	    }
	    else if(valueType != TYPEOF(obj))
		Rf_error(_("\"%s\" can only be set as the class if the object has this type; found \"%s\""),
		      valueString, Rf_type2char(TYPEOF(obj)));
	    /* else, leave alone */
	}
	else if(!strcmp("numeric", valueString)) {
	    Rf_setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      do_unsetS4(obj, value);
	    switch(TYPEOF(obj)) {
	    case INTSXP: case REALSXP: break;
	    default: PROTECT(obj = Rf_coerceVector(obj, REALSXP));
		nProtect++;
	    }
	}
	/* the next 2 special cases mirror the special code in
	 * R_data_class */
	else if(!strcmp("matrix", valueString)) {
	    if(length(Rf_getAttrib(obj, R_DimSymbol)) != 2)
		Rf_error(_("invalid to set the class to matrix unless the dimension attribute is of length 2 (was %d)"),
		 length(Rf_getAttrib(obj, R_DimSymbol)));
	    Rf_setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj))
	      do_unsetS4(obj, value);
	}
	else if(!strcmp("array", valueString)) {
	    if(length(Rf_getAttrib(obj, R_DimSymbol)) <= 0)
		Rf_error(_("cannot set class to \"array\" unless the dimension attribute has length > 0"));
	    Rf_setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      UNSET_S4_OBJECT(obj);
	}
	else { /* set the class but don't do the coercion; that's
		  supposed to be done by an as() method */
	    Rf_setAttrib(obj, R_ClassSymbol, value);
	}
    }
    UNPROTECT(nProtect);
    return obj;
}

SEXP attribute_hidden R_do_set_class(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object, CXXR::RObject* klass)
{
    return R_set_class(object, klass, call);
}

/* primitive */
SEXP attribute_hidden do_storage_mode(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* obj, CXXR::RObject* value)
{
/* storage.mode(obj) <- value */
    SEXP ans;
    SEXPTYPE type;

    if (!Rf_isValidString(value) || STRING_ELT(value, 0) == NA_STRING)
	Rf_error(_("'value' must be non-null character string"));
    type = Rf_str2type(CHAR(STRING_ELT(value, 0)));
    if(type == SEXPTYPE( -1)) {
	/* For backwards compatibility we allow "real" and "single" */
	if(streql(CHAR(STRING_ELT(value, 0)), "real")) {
	    Rf_error("use of 'real' is defunct: use 'double' instead");
	} else if(streql(CHAR(STRING_ELT(value, 0)), "single")) {
	    Rf_error("use of 'single' is defunct: use mode<- instead");
	} else
	    Rf_error(_("invalid value"));
    }
    if(TYPEOF(obj) == type) return obj;
    if(Rf_isFactor(obj))
	Rf_error(_("invalid to change the storage mode of a factor"));
    PROTECT(ans = Rf_coerceVector(obj, type));
    SHALLOW_DUPLICATE_ATTRIB(ans, obj);
    UNPROTECT(1);
    return ans;
}

#include "CXXR/ArgList.hpp"

const Symbol* ArgList::coerceTag(const RObject* tag)
{
    const char* symname = nullptr;
    if (tag->sexptype() == STRSXP) {
	const StringVector* strv = static_cast<const StringVector*>(tag);
	if (strv->size() >= 1) {
	    const String* strv0 = (*strv)[0];
	    if (strv0 && strv0->size() >= 1)
		symname = Rf_translateChar(const_cast<String*>(strv0));
	}
    }
    if (!symname) {
	StringVector* strv
	    = static_cast<StringVector*>(Rf_deparse1(const_cast<RObject*>(tag),
						     TRUE, SIMPLEDEPARSE));
	symname = (*strv)[0]->c_str();
    }
    return Symbol::obtain(symname);
}
