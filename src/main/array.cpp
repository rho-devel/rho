/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2015   The R Core Team
 *  Copyright (C) 2002-2015   The R Foundation
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

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <R_ext/RS.h>     /* for Calloc/Free */
#include <R_ext/Applic.h> /* for dgemm */
#include <R_ext/Itermacros.h>

#include "duplicate.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/RAllocStack.h"
#include "CXXR/Subscripting.hpp"

using namespace CXXR;

/* "GetRowNames" and "GetColNames" are utility routines which
 * locate and return the row names and column names from the
 * dimnames attribute of a matrix.  They are useful because
 * old versions of R used pair-based lists for dimnames
 * whereas recent versions use vector based lists.

 * These are now very old, plus
 * ``When the "dimnames" attribute is
 *   grabbed off an array it is always adjusted to be a vector.''

 They are used in bind.c and subset.c, and advertised in Rinternals.h
*/
SEXP GetRowNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 0);
    else
	return R_NilValue;
}

SEXP GetColNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 1);
    else
	return R_NilValue;
}

SEXP attribute_hidden do_matrix(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP vals, ans, snr, snc, dimnames;
    int nr = 1, nc = 1, byrow, miss_nr, miss_nc;
    R_xlen_t lendat;

    op->checkNumArgs(num_args, call);
    vals = args[0]; args = (args + 1);
    switch(TYPEOF(vals)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case EXPRSXP:
	case VECSXP:
	    break;
	default:
	    error(_("'data' must be of a vector type, was '%s'"),
		type2char(TYPEOF(vals)));
    }
    lendat = XLENGTH(vals);
    snr = args[0]; args = (args + 1);
    snc = args[0]; args = (args + 1);
    byrow = asLogical(args[0]); args = (args + 1);
    if (byrow == NA_INTEGER)
	error(_("invalid '%s' argument"), "byrow");
    dimnames = args[0];
    args = (args + 1);
    miss_nr = asLogical(args[0]); args = (args + 1);
    miss_nc = asLogical(args[0]);

    if (!miss_nr) {
	if (!isNumeric(snr)) error(_("non-numeric matrix extent"));
	nr = asInteger(snr);
	if (nr == NA_INTEGER)
	    error(_("invalid 'nrow' value (too large or NA)"));
	if (nr < 0)
	    error(_("invalid 'nrow' value (< 0)"));
    }
    if (!miss_nc) {
	if (!isNumeric(snc)) error(_("non-numeric matrix extent"));
	nc = asInteger(snc);
	if (nc == NA_INTEGER)
	    error(_("invalid 'ncol' value (too large or NA)"));
	if (nc < 0)
	    error(_("invalid 'ncol' value (< 0)"));
    }
    if (miss_nr && miss_nc) {
	if (lendat > INT_MAX) error("data is too long");
	nr = int( lendat);
    } else if (miss_nr) {
	if (lendat > (double) nc * INT_MAX) error("data is too long");
	// avoid division by zero
	if (nc == 0) {
	    if (lendat) error(_("nc = 0 for non-null data"));
	    else nr = 0;
	} else
	    nr = (int) ceil((double) lendat / (double) nc);
    } else if (miss_nc) {
	if (lendat > (double) nr * INT_MAX) error("data is too long");
	// avoid division by zero
	if (nr == 0) {
	    if (lendat) error(_("nr = 0 for non-null data"));
	    else nc = 0;
	} else
	    nc = (int) ceil((double) lendat / (double) nr);
    }

    if(lendat > 0) {
	R_xlen_t nrc = R_xlen_t( nr) * nc;
	if (lendat > 1 && nrc % lendat != 0) {
	    if (((lendat > nr) && (lendat / nr) * nr != lendat) ||
		((lendat < nr) && (nr / lendat) * lendat != nr))
		warning(_("data length [%d] is not a sub-multiple or multiple of the number of rows [%d]"), lendat, nr);
	    else if (((lendat > nc) && (lendat / nc) * nc != lendat) ||
		     ((lendat < nc) && (nc / lendat) * lendat != nc))
		warning(_("data length [%d] is not a sub-multiple or multiple of the number of columns [%d]"), lendat, nc);
	}
	else if ((lendat > 1) && (nrc == 0)){
	    warning(_("data length exceeds size of matrix"));
	}
    }

#ifndef LONG_VECTOR_SUPPORT
    if (double(nr) * double(nc) > INT_MAX)
	error(_("too many elements specified"));
#endif

    PROTECT(ans = allocMatrix(TYPEOF(vals), nr, nc));
    if(lendat) {
	if (isVector(vals))
	    copyMatrix(ans, vals, CXXRCONSTRUCT(Rboolean, byrow));
	else
	    copyListMatrix(ans, vals, CXXRCONSTRUCT(Rboolean, byrow));
    } else if (isVector(vals)) { /* fill with NAs */
	R_xlen_t N = R_xlen_t( nr) * nc, i;
	switch(TYPEOF(vals)) {
	case STRSXP:
	    for (i = 0; i < N; i++)
		SET_STRING_ELT(ans, i, NA_STRING);
	    break;
	case LGLSXP:
	    for (i = 0; i < N; i++)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    for (i = 0; i < N; i++)
		INTEGER(ans)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    for (i = 0; i < N; i++)
		REAL(ans)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    {
		Rcomplex na_cmplx;
		na_cmplx.r = NA_REAL;
		na_cmplx.i = 0;
		for (i = 0; i < N; i++)
		    COMPLEX(ans)[i] = na_cmplx;
	    }
	    break;
	case RAWSXP:
	    if (N) memset(RAW(ans), 0, N);
	    break;
	default:
	    /* don't fill with anything */
	    ;
	}
    }
    if(!isNull(dimnames) && length(dimnames) > 0)
	ans = dimnamesgets(ans, dimnames);
    UNPROTECT(1);
    return ans;
}


SEXP allocMatrix(SEXPTYPE mode, int nrow, int ncol)
{
    SEXP s, t;
    R_xlen_t n;

    if (nrow < 0 || ncol < 0)
	error(_("negative extents to matrix"));
#ifndef LONG_VECTOR_SUPPORT
    if (double(nrow) * double(ncol) > INT_MAX)
	error(_("allocMatrix: too many elements specified"));
#endif
    n = (R_xlen_t( nrow)) * ncol;
    PROTECT(s = allocVector(mode, n));
    PROTECT(t = allocVector(INTSXP, 2));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}

/**
 * Allocate a 3-dimensional array
 *
 * @param mode The R mode (e.g. INTSXP)
 * @param nrow number of rows
 * @param ncol number of columns
 * @param nface number of faces
 *
 * @return A 3-dimensional array of the indicated dimensions and mode
 */
SEXP alloc3DArray(SEXPTYPE mode, int nrow, int ncol, int nface)
{
    SEXP s, t;
    R_xlen_t n;

    if (nrow < 0 || ncol < 0 || nface < 0)
	error(_("negative extents to 3D array"));
#ifndef LONG_VECTOR_SUPPORT
    if (double(nrow) * double(ncol) * double(nface) > INT_MAX)
	error(_("alloc3Darray: too many elements specified"));
#endif
    n = (R_xlen_t( nrow)) * ncol * nface;
    PROTECT(s = allocVector(mode, n));
    PROTECT(t = allocVector(INTSXP, 3));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    INTEGER(t)[2] = nface;
    setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}


SEXP allocArray(SEXPTYPE mode, SEXP dims)
{
    SEXP array;
    int i;
    R_xlen_t n = 1;
    double dn = 1;

    for (i = 0; i < LENGTH(dims); i++) {
	dn *= INTEGER(dims)[i];
#ifndef LONG_VECTOR_SUPPORT
	if(dn > INT_MAX)
	    error(_("'allocArray': too many elements specified by 'dims'"));
#endif
	n *= INTEGER(dims)[i];
    }

    PROTECT(dims = duplicate(dims));
    PROTECT(array = allocVector(mode, n));
    setAttrib(array, R_DimSymbol, dims);
    UNPROTECT(2);
    return array;
}

/* DropDims strips away redundant dimensioning information. */
/* If there is an appropriate dimnames attribute the correct */
/* element is extracted and attached to the vector as a names */
/* attribute.  Note that this function mutates x. */
/* Duplication should occur before this is called. */

SEXP DropDims(SEXP x)
{
    // The following is a kludge.  It is possible that the object
    // pointed to by x is not actually a VectorBase; however, provided
    // its 'dims' and/or 'dimnames' attributes, if present, are
    // configured in the standard way, Subscripting::dropDimensions()
    // should behave correctly.
    VectorBase* vb = static_cast<VectorBase*>(x);
    Subscripting::dropDimensions(vb);
    return vb;
}

SEXP attribute_hidden do_drop(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_)
{
    GCStackRoot<> x;
    SEXP xdims;
    int i, n, shorten;

    x = x_;
    if ((xdims = getAttrib(x, R_DimSymbol)) != R_NilValue) {
	n = LENGTH(xdims);
	shorten = 0;
	for (i = 0; i < n; i++)
	    if (INTEGER(xdims)[i] == 1) shorten = 1;
	if (shorten) {
	    if (MAYBE_REFERENCED(x)) x = duplicate(x);
	    x = DropDims(x);
	}
    }
    return x;
}

/* Length of Primitive Objects */

SEXP attribute_hidden do_length(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP x = args[0];

    auto dispatched = op->InternalDispatch(call, rho, num_args, args, tags);
    if (dispatched.first) {
	RObject* ans = dispatched.second;
	if (length(ans) == 1 && TYPEOF(ans) == REALSXP) {
	    GCStackRoot<> ansrt(ans);
	    double d = REAL(ans)[0];
	    if (R_FINITE(d) && d >= 0. && d <= INT_MAX && floor(d) == d)
		return coerceVector(ans, INTSXP);
	}
	return(ans);
    }


#ifdef LONG_VECTOR_SUPPORT
    // or use IS_LONG_VEC
    R_xlen_t len = xlength(x);
    if (len > INT_MAX) return ScalarReal(double( len));
#endif
    return ScalarInteger(length(x));
}

R_xlen_t get_object_length(RObject* x, Environment* rho)
{
    static BuiltInFunction* length_op
	= BuiltInFunction::obtainPrimitive("length");
    // Create a call to length(x)
    static Symbol* length = Symbol::obtain("length");
    static Symbol* x_sym = Symbol::obtain("x");
    static GCRoot<Expression> call = new Expression(length, new PairList(x_sym));

    if (isObject(x))
    {
	auto dispatched = length_op->InternalDispatch(
	    call, rho, 1, &x, call->getArgs());
	if (dispatched.first) {
	    RObject* len = dispatched.second;
	    return (R_xlen_t)
		(TYPEOF(len) == REALSXP ? REAL(len)[0] : asInteger(len));
	}
    }
    return(xlength(x));
}

static SEXP dispatch_subset2(SEXP x, R_xlen_t i, SEXP call, SEXP rho) {
    static SEXP bracket_op = NULL;
    SEXP args, x_elt;
    if (isObject(x)) {
        if (bracket_op == NULL)
            bracket_op = R_Primitive("[[");
        PROTECT(args = list2(x, ScalarReal(i + 1)));
        x_elt = do_subset2(call, bracket_op, args, rho);
        UNPROTECT(1);
    } else {
        x_elt = VECTOR_ELT(x, i);
    }
    return(x_elt);
}

// auxiliary for do_lengths_*(), i.e., R's lengths()
static R_xlen_t getElementLength(SEXP x, R_xlen_t i, SEXP call,
				 Environment* rho) {
    SEXP x_elt = dispatch_subset2(x, i, call, rho);
    return(get_object_length(x_elt, rho));
}

static SEXP do_lengths_long(SEXP x, Expression* call, Environment* rho)
{
    SEXP ans;
    R_xlen_t x_len, i;
    double *ans_elt;

    x_len = get_object_length(x, rho);
    PROTECT(ans = allocVector(REALSXP, x_len));
    for (i = 0, ans_elt = REAL(ans); i < x_len; i++, ans_elt++) {
        *ans_elt = getElementLength(x, i, call, rho);
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_lengths(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP x = args[0], ans;
    R_xlen_t x_len, i;
    int *ans_elt;
    int useNames = asLogical(args[1]);
    if (useNames == NA_LOGICAL)
	error(_("invalid '%s' value"), "USE.NAMES");
    bool isList = isVectorList(x) || isS4(x);
    if(!isList) switch(TYPEOF(x)) {
	case NILSXP:
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    break;
	default:
	    error(_("'%s' must be a list or atomic vector"), "x");
    }
    x_len = get_object_length(x, rho);
    PROTECT(ans = allocVector(INTSXP, x_len));
    if(isList) {
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
	    R_xlen_t x_elt_len = getElementLength(x, i, call, rho);
#ifdef LONG_VECTOR_SUPPORT
	    if (x_elt_len > INT_MAX) {
		ans = do_lengths_long(x, call, rho);
		UNPROTECT(1);
		PROTECT(ans);
		break;
	    }
#endif
	    *ans_elt = (int)x_elt_len;
	}
    } else { // atomic: every element has length 1
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++)
	    *ans_elt = 1;
    }
    if(useNames) {
	SEXP names = getAttrib(x, R_NamesSymbol);
	if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_rowscols(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP x, ans;
    int i, j, nr, nc;

    /* This is the dimensions vector */
    x = args[0];
    if (!isInteger(x) || LENGTH(x) != 2)
	error(_("a matrix-like object is required as argument to '%s'"),
	      (op->variant() == 2) ? "col" : "row");

    nr = INTEGER(x)[0];
    nc = INTEGER(x)[1];

    ans = allocMatrix(INTSXP, nr, nc);

    R_xlen_t NR = nr;
    switch (op->variant()) {
    case 1:
	for (i = 0; i < nr; i++)
	    for (j = 0; j < nc; j++)
		INTEGER(ans)[i + j * NR] = i + 1;
	break;
    case 2:
	for (i = 0; i < nr; i++)
	    for (j = 0; j < nc; j++)
		INTEGER(ans)[i + j * NR] = j + 1;
	break;
    }
    return ans;
}

static void matprod(double *x, int nrx, int ncx,
		    double *y, int nry, int ncy, double *z)
{
    CXXRCONST char *transa = "N", *transb = "N";
    double one = 1.0, zero = 0.0;
    LDOUBLE sum;
    Rboolean have_na = FALSE;
    R_xlen_t NRX = nrx, NRY = nry;

    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	/* Don't trust the BLAS to handle NA/NaNs correctly: PR#4582
	 * The test is only O(n) here.
	 */
	for (R_xlen_t i = 0; i < NRX*ncx; i++)
	    if (ISNAN(x[i])) {have_na = TRUE; break;}
	if (!have_na)
	    for (R_xlen_t i = 0; i < NRY*ncy; i++)
		if (ISNAN(y[i])) {have_na = TRUE; break;}
	if (have_na) {
	    for (int i = 0; i < nrx; i++)
		for (int k = 0; k < ncy; k++) {
		    sum = 0.0;
		    for (int j = 0; j < ncx; j++)
			sum += x[i + j * NRX] * y[j + k * NRY];
		    z[i + k * NRX] = double( sum);
		}
	} else
	    F77_CALL(dgemm)(transa, transb, &nrx, &ncy, &ncx, &one,
			    x, &nrx, y, &nry, &zero, z, &nrx);
    } else /* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*ncy; i++) z[i] = 0;
}

static void cmatprod(Rcomplex *x, int nrx, int ncx,
		     Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    CXXRCONST char *transa = "N", *transb = "N";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &nrx, &ncy, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	R_xlen_t NRX = nrx;
	for(R_xlen_t i = 0; i < NRX*ncy; i++) z[i].r = z[i].i = 0;
    }
#else
    int i, j, k;
    double xij_r, xij_i, yjk_r, yjk_i;
    LDOUBLE sum_i, sum_r;

    R_xlen_t NRX = nrx, NRY = nry;
    for (i = 0; i < nrx; i++)
	for (k = 0; k < ncy; k++) {
	    z[i + k * NRX].r = NA_REAL;
	    z[i + k * NRX].i = NA_REAL;
	    sum_r = 0.0;
	    sum_i = 0.0;
	    for (j = 0; j < ncx; j++) {
		xij_r = x[i + j * NRX].r;
		xij_i = x[i + j * NRX].i;
		yjk_r = y[j + k * NRY].r;
		yjk_i = y[j + k * NRY].i;
		if (ISNAN(xij_r) || ISNAN(xij_i)
		    || ISNAN(yjk_r) || ISNAN(yjk_i))
		    goto next_ik;
		sum_r += (xij_r * yjk_r - xij_i * yjk_i);
		sum_i += (xij_r * yjk_i + xij_i * yjk_r);
	    }
	    z[i + k * NRX].r = sum_r;
	    z[i + k * NRX].i = sum_i;
	next_ik:
	    ;
	}
#endif
}

static void symcrossprod(double *x, int nr, int nc, double *z)
{
    CXXRCONST char *trans = "T", *uplo = "U";
    double one = 1.0, zero = 0.0;
    R_xlen_t NC = nc;
    if (nr > 0 && nc > 0) {
	F77_CALL(dsyrk)(uplo, trans, &nc, &nr, &one, x, &nr, &zero, z, &nc);
	for (int i = 1; i < nc; i++)
	    for (int j = 0; j < i; j++) z[i + NC *j] = z[j + NC * i];
    } else { /* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NC*NC; i++) z[i] = 0;
    }

}

static void crossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    CXXRCONST char *transa = "T", *transb = "N";
    double one = 1.0, zero = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(dgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
			x, &nrx, y, &nry, &zero, z, &ncx);
    } else { /* zero-extent operations should return zeroes */
	R_xlen_t NCX = ncx;
	for(R_xlen_t i = 0; i < NCX*ncy; i++) z[i] = 0;
    }
}

static void ccrossprod(Rcomplex *x, int nrx, int ncx,
		       Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    CXXRCONST char *transa = "T", *transb = "N";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
			x, &nrx, y, &nry, &zero, z, &ncx);
    } else { /* zero-extent operations should return zeroes */
	R_xlen_t NCX = ncx;
	for(R_xlen_t i = 0; i < NCX*ncy; i++) z[i].r = z[i].i = 0;
    }
}

static void symtcrossprod(double *x, int nr, int nc, double *z)
{
    CXXRCONST char *trans = "N", *uplo = "U";
    double one = 1.0, zero = 0.0;
    if (nr > 0 && nc > 0) {
	F77_CALL(dsyrk)(uplo, trans, &nr, &nc, &one, x, &nr, &zero, z, &nr);
	for (int i = 1; i < nr; i++)
	    for (int j = 0; j < i; j++) z[i + nr *j] = z[j + nr * i];
    } else { /* zero-extent operations should return zeroes */
	R_xlen_t NR = nr;
	for(R_xlen_t i = 0; i < NR*NR; i++) z[i] = 0;
    }

}

static void tcrossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    CXXRCONST char *transa = "N", *transb = "T";
    double one = 1.0, zero = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(dgemm)(transa, transb, &nrx, &nry, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	R_xlen_t NRX = nrx;
	for(R_xlen_t i = 0; i < NRX*nry; i++) z[i] = 0;
    }
}

static void tccrossprod(Rcomplex *x, int nrx, int ncx,
			Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    CXXRCONST char *transa = "N", *transb = "T";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &nrx, &nry, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	R_xlen_t NRX = nrx;
	for(R_xlen_t i = 0; i < NRX*nry; i++) z[i].r = z[i].i = 0;
    }
}

/* "%*%" (op = 0), crossprod (op = 1) or tcrossprod (op = 2) */
SEXP do_crossprod(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x, CXXR::RObject* y)
{
    int ldx, ldy, nrx, ncx, nry, ncy;
    SEXPTYPE mode;
    SEXP xdims, ydims, ans;
    Rboolean sym;

    sym = isNull(y);
    if (sym && (op->variant() > 0)) y = x;
    if ( !(isNumeric(x) || isComplex(x)) || !(isNumeric(y) || isComplex(y)) )
	errorcall(call, _("requires numeric/complex matrix/vector arguments"));

    xdims = getAttrib(x, R_DimSymbol);
    ydims = getAttrib(y, R_DimSymbol);
    ldx = length(xdims);
    ldy = length(ydims);

    if (ldx != 2 && ldy != 2) {		/* x and y non-matrices */
	// for crossprod, allow two cases: n x n ==> (1,n) x (n,1);  1 x n = (n, 1) x (1, n)
	if (op->variant() == 1 && LENGTH(x) == 1) {
	    nrx = ncx = nry = 1;
	    ncy = LENGTH(y);
	}
	else {
	    nry = LENGTH(y);
	    ncy = 1;
	    if (op->variant() == 0) {
		nrx = 1;
		ncx = LENGTH(x);
		if(ncx == 1) {	        // y as row vector
		    ncy = nry;
		    nry = 1;
		}
	    }
	    else {
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldx != 2) {		/* x not a matrix */
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
	nrx = 0;
	ncx = 0;
	if (op->variant() == 0) {
	    if (LENGTH(x) == nry) {	/* x as row vector */
		nrx = 1;
		ncx = nry; /* == LENGTH(x) */
	    }
	    else if (nry == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
	else if (op->variant() == 1) { /* crossprod() */
	    if (LENGTH(x) == nry) {	/* x is a col vector */
		nrx = nry; /* == LENGTH(x) */
		ncx = 1;
	    }
	    /* else if (nry == 1) ... not being too tolerant
	       to treat x as row vector, as t(x) *is* row vector */
	}
	else { /* tcrossprod */
	    if (LENGTH(x) == ncy) {	/* x as row vector */
		nrx = 1;
		ncx = ncy; /* == LENGTH(x) */
	    }
	    else if (ncy == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldy != 2) {		/* y not a matrix */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = 0;
	ncy = 0;
	if (op->variant() == 0) {
	    if (LENGTH(y) == ncx) {	/* y as col vector */
		nry = ncx;
		ncy = 1;
	    }
	    else if (ncx == 1) {	/* y as row vector */
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else if (op->variant() == 1) { /* crossprod() */
	    if (LENGTH(y) == nrx) {	/* y is a col vector */
		nry = nrx;
		ncy = 1;
	    } else if (nrx == 1) {	// y as row vector
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else { // tcrossprod
	    if (nrx == 1) {		// y as row vector
		nry = 1;
		ncy = LENGTH(y);
	    }
	    else {			// y is a col vector
		nry = LENGTH(y);
		ncy = 1;
	    }
	}
    }
    else {				/* x and y matrices */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
    }
    /* nr[ow](.) and nc[ol](.) are now defined for x and y */

    if (op->variant() == 0) {
	/* primitive, so use call */
	if (ncx != nry)
	    errorcall(call, _("non-conformable arguments"));
    }
    else if (op->variant() == 1) {
	if (nrx != nry)
	    error(_("non-conformable arguments"));
    }
    else {
	if (ncx != ncy)
	    error(_("non-conformable arguments"));
    }

    if (isComplex(x) || isComplex(y))
	mode = CPLXSXP;
    else
	mode = REALSXP;
    x = coerceVector(x, mode);
    y = coerceVector(y, mode);

    if (op->variant() == 0) {			/* op == 0 : matprod() */

	PROTECT(ans = allocMatrix(mode, nrx, ncy));
	if (mode == CPLXSXP)
	    cmatprod(COMPLEX(x), nrx, ncx,
		     COMPLEX(y), nry, ncy, COMPLEX(ans));
	else
	    matprod(REAL(x), nrx, ncx,
		    REAL(y), nry, ncy, REAL(ans));

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));
	    if (xdims != R_NilValue) {
		if (ldx == 2 || ncx == 1) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }

#define YDIMS_ET_CETERA							\
	    if (ydims != R_NilValue) {					\
		if (ldy == 2) {						\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 1));	\
		    dny = getAttrib(ydims, R_NamesSymbol);		\
		    if(!isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 1)); \
		} else if (nry == 1) {					\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));	\
		    dny = getAttrib(ydims, R_NamesSymbol);		\
		    if(!isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0)); \
		}							\
	    }								\
									\
	    /* We sometimes attach a dimnames attribute			\
	     * whose elements are all NULL ...				\
	     * This is ugly but causes no real damage.			\
	     * Now (2.1.0 ff), we don't anymore: */			\
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||			\
		VECTOR_ELT(dimnames,1) != R_NilValue) {			\
		if (dnx != R_NilValue || dny != R_NilValue)		\
		    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);	\
		setAttrib(ans, R_DimNamesSymbol, dimnames);		\
	    }								\
	    UNPROTECT(2)

	    YDIMS_ET_CETERA;
	}
    }

    else if (op->variant() == 1) {	/* op == 1: crossprod() */

	PROTECT(ans = allocMatrix(mode, ncx, ncy));
	if (mode == CPLXSXP)
	    if(sym)
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		crossprod(REAL(x), nrx, ncx,
			  REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));

	    if (xdims != R_NilValue) {
		if (ldx == 2) {/* not nrx==1 : .. fixed, ihaka 2003-09-30 */
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 1));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 1));
		}
	    }

	    YDIMS_ET_CETERA;
	}

    }
    else {					/* op == 2: tcrossprod() */

	PROTECT(ans = allocMatrix(mode, nrx, nry));
	if (mode == CPLXSXP)
	    if(sym)
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symtcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		tcrossprod(REAL(x), nrx, ncx,
			   REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));

	    if (xdims != R_NilValue) {
		if (ldx == 2) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }
	    if (ydims != R_NilValue) {
		if (ldy == 2) {
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));
		    dny = getAttrib(ydims, R_NamesSymbol);
		    if(!isNull(dny))
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0));
		}
	    }
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||
		VECTOR_ELT(dimnames,1) != R_NilValue) {
		if (dnx != R_NilValue || dny != R_NilValue)
		    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);
		setAttrib(ans, R_DimNamesSymbol, dimnames);
	    }

	    UNPROTECT(2);
	}
    }
    UNPROTECT(3);
    return ans;
}
#undef YDIMS_ET_CETERA

SEXP attribute_hidden do_matprod(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x = CAR(args), y = CADR(args);

    if ((IS_S4_OBJECT(x) || IS_S4_OBJECT(y))
	&& R_has_methods(op)) {
	SEXP s;
	/* Remove argument names to ensure positional matching */
	for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);
	std::pair<bool, SEXP> pr
	    = R_possible_dispatch(call, op, args, rho, FALSE);
	if (pr.first) return pr.second;
    }
    return do_crossprod(SEXP_downcast<Expression*>(call),
			SEXP_downcast<BuiltInFunction*>(op), x, y);
}

SEXP attribute_hidden do_transpose(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_)
{
    SEXP a, r, dims, dimnames, dimnamesnames = R_NilValue,
	ndimnamesnames, rnames, cnames;
    int ldim, ncol = 0, nrow = 0;
    R_xlen_t len = 0;

    a = x_;

    if (isVector(a)) {
	dims = getAttrib(a, R_DimSymbol);
	ldim = length(dims);
	rnames = R_NilValue;
	cnames = R_NilValue;
	switch(ldim) {
	case 0:
	    len = nrow = LENGTH(a);
	    ncol = 1;
	    rnames = getAttrib(a, R_NamesSymbol);
	    dimnames = rnames;/* for isNull() below*/
	    break;
	case 1:
	    len = nrow = LENGTH(a);
	    ncol = 1;
	    dimnames = getAttrib(a, R_DimNamesSymbol);
	    if (dimnames != R_NilValue) {
		rnames = VECTOR_ELT(dimnames, 0);
		dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
	    }
	    break;
	case 2:
	    ncol = ncols(a);
	    nrow = nrows(a);
	    len = XLENGTH(a);
	    dimnames = getAttrib(a, R_DimNamesSymbol);
	    if (dimnames != R_NilValue) {
		rnames = VECTOR_ELT(dimnames, 0);
		cnames = VECTOR_ELT(dimnames, 1);
		dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
	    }
	    break;
	default:
	    error(_("argument is not a matrix"));
	}
    }
    else
	error(_("argument is not a matrix"));
    PROTECT(r = allocVector(TYPEOF(a), len));
    R_xlen_t i, j, l_1 = len-1;
    switch (TYPEOF(a)) {
    case LGLSXP:
    case INTSXP:
	// filling in columnwise, "accessing row-wise":
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            INTEGER(r)[i] = INTEGER(a)[j];
        }
        break;
    case REALSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            REAL(r)[i] = REAL(a)[j];
        }
        break;
    case CPLXSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            COMPLEX(r)[i] = COMPLEX(a)[j];
        }
        break;
    case STRSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            SET_STRING_ELT(r, i, STRING_ELT(a,j));
        }
        break;
    case VECSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            SET_VECTOR_ELT(r, i, VECTOR_ELT(a,j));
        }
        break;
    case RAWSXP:
        for (i = 0, j = 0; i < len; i++, j += nrow) {
            if (j > l_1) j -= l_1;
            RAW(r)[i] = RAW(a)[j];
        }
        break;
    default:
        UNPROTECT(1);
        error(_("argument is not a matrix"));
    }
    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = ncol;
    INTEGER(dims)[1] = nrow;
    setAttrib(r, R_DimSymbol, dims);
    UNPROTECT(1);
    /* R <= 2.2.0: dropped list(NULL,NULL) dimnames :
     * if(rnames != R_NilValue || cnames != R_NilValue) */
    if(!isNull(dimnames)) {
	PROTECT(dimnames = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, cnames);
	SET_VECTOR_ELT(dimnames, 1, rnames);
	if(!isNull(dimnamesnames)) {
	    PROTECT(ndimnamesnames = allocVector(VECSXP, 2));
	    SET_VECTOR_ELT(ndimnamesnames, 1, STRING_ELT(dimnamesnames, 0));
	    SET_VECTOR_ELT(ndimnamesnames, 0,
			   (ldim == 2) ? STRING_ELT(dimnamesnames, 1):
			   R_BlankString);
	    setAttrib(dimnames, R_NamesSymbol, ndimnamesnames);
	    UNPROTECT(1);
	}
	setAttrib(r, R_DimNamesSymbol, dimnames);
	UNPROTECT(1);
    }
    copyMostAttrib(a, r);
    UNPROTECT(1);
    return r;
}

/*
 New version of aperm, using strides for speed.
 Jonathan Rougier <J.C.Rougier@durham.ac.uk>

 v1.0 30.01.01

 M.Maechler : expanded	all ../include/Rdefines.h macros
 */

/* this increments iip and sets j using strides */

#define CLICKJ						\
    for (itmp = 0; itmp < n; itmp++)			\
	if (iip[itmp] == isr[itmp]-1) iip[itmp] = 0;	\
	else {						\
	    iip[itmp]++;				\
	    break;					\
	}						\
    for (lj = 0, itmp = 0; itmp < n; itmp++)	       	\
	lj += iip[itmp] * stride[itmp];

/* aperm (a, perm, resize = TRUE) */
SEXP attribute_hidden do_aperm(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* a_, CXXR::RObject* perm_, CXXR::RObject* resize_)
{
    SEXP a, perm, r, dimsa, dimsr, dna;
    int i, j, n, itmp;

    a = a_;
    if (!isArray(a))
	error(_("invalid first argument, must be an array"));

    PROTECT(dimsa = getAttrib(a, R_DimSymbol));
    n = LENGTH(dimsa);
    int *isa = INTEGER(dimsa);

    /* check the permutation */

    int *pp = static_cast<int *>( CXXR_alloc(size_t( n), sizeof(int)));
    perm = perm_;
    if (length(perm) == 0) {
	for (i = 0; i < n; i++) pp[i] = n-1-i;
    } else {
	if (LENGTH(perm) != n)
	    error(_("'perm' is of wrong length %d (!= %d)"),
		  LENGTH(perm), n);
	if (isString(perm)) {
	    SEXP dna = getAttrib(a, R_DimNamesSymbol);
	    if (isNull(dna))
		error(_("'a' does not have named dimnames"));
	    SEXP dnna = getAttrib(dna, R_NamesSymbol);
	    if (isNull(dnna))
		error(_("'a' does not have named dimnames"));
	    for (i = 0; i < n; i++) {
		const char *thiss = translateChar(STRING_ELT(perm, i));
		for (j = 0; j < n; j++)
		    if (streql(translateChar(STRING_ELT(dnna, j)),
			       thiss)) {pp[i] = j; break;}
		if (j >= n)
		    error(_("'perm[%d]' does not match a dimension name"), i+1);
	    }
	} else {
	    PROTECT(perm = coerceVector(perm, INTSXP));
	    for (i = 0; i < n; i++) pp[i] = INTEGER(perm)[i] - 1;
	    UNPROTECT(1);
	}
    }

    R_xlen_t *iip = static_cast<R_xlen_t *>( CXXR_alloc(size_t( n), sizeof(R_xlen_t)));
    for (i = 0; i < n; iip[i++] = 0);
    for (i = 0; i < n; i++)
	if (pp[i] >= 0 && pp[i] < n) iip[pp[i]]++;
	else error(_("value out of range in 'perm'"));
    for (i = 0; i < n; i++)
	if (iip[i] == 0) error(_("invalid '%s' argument"), "perm");

    /* create the stride object and permute */

    R_xlen_t *stride = static_cast<R_xlen_t *>( CXXR_alloc(size_t( n), sizeof(R_xlen_t)));
    for (iip[0] = 1, i = 1; i<n; i++) iip[i] = iip[i-1] * isa[i-1];
    for (i = 0; i < n; i++) stride[i] = iip[pp[i]];

    /* also need to have the dimensions of r */

    PROTECT(dimsr = allocVector(INTSXP, n));
    int *isr = INTEGER(dimsr);
    for (i = 0; i < n; i++) isr[i] = isa[pp[i]];

    /* and away we go! iip will hold the incrementer */

    R_xlen_t len = XLENGTH(a);
    PROTECT(r = allocVector(TYPEOF(a), len));

    for (i = 0; i < n; iip[i++] = 0);

    R_xlen_t li, lj;
    switch (TYPEOF(a)) {

    case INTSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    INTEGER(r)[li] = INTEGER(a)[lj];
	    CLICKJ;
	}
	break;

    case LGLSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    LOGICAL(r)[li] = LOGICAL(a)[lj];
	    CLICKJ;
	}
	break;

    case REALSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    REAL(r)[li] = REAL(a)[lj];
	    CLICKJ;
	}
	break;

    case CPLXSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    COMPLEX(r)[li].r = COMPLEX(a)[lj].r;
	    COMPLEX(r)[li].i = COMPLEX(a)[lj].i;
	    CLICKJ;
	}
	break;

    case STRSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    SET_STRING_ELT(r, li, STRING_ELT(a, lj));
	    CLICKJ;
	}
	break;

    case VECSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    SET_VECTOR_ELT(r, li, VECTOR_ELT(a, lj));
	    CLICKJ;
	}
	break;

    case RAWSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    RAW(r)[li] = RAW(a)[lj];
	    CLICKJ;
	}
	break;

    default:
	UNIMPLEMENTED_TYPE("aperm", a);
    }

    /* handle the resize */
    int resize = asLogical(resize_);
    if (resize == NA_LOGICAL) error(_("'resize' must be TRUE or FALSE"));

    /* and handle names(dim(.)) and the dimnames if any */
    if (resize) {
	SEXP nmdm = getAttrib(dimsa, R_NamesSymbol);
	if(nmdm != R_NilValue) { // dimsr needs correctly permuted names()
	    PROTECT(nmdm);
	    SEXP nm_dr = PROTECT(allocVector(STRSXP, n));
	    for (i = 0; i < n; i++) {
		SET_STRING_ELT(nm_dr, i, STRING_ELT(nmdm, pp[i]));
	    }
	    setAttrib(dimsr, R_NamesSymbol, nm_dr);
	    UNPROTECT(2);
	}
	setAttrib(r, R_DimSymbol, dimsr);

	PROTECT(dna = getAttrib(a, R_DimNamesSymbol));
	if (dna != R_NilValue) {
	    SEXP dnna, dnr, dnnr;

	    PROTECT(dnr  = allocVector(VECSXP, n));
	    PROTECT(dnna = getAttrib(dna, R_NamesSymbol));
	    if (dnna != R_NilValue) {
		PROTECT(dnnr = allocVector(STRSXP, n));
		for (i = 0; i < n; i++) {
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
		    SET_STRING_ELT(dnnr, i, STRING_ELT(dnna, pp[i]));
		}
		setAttrib(dnr, R_NamesSymbol, dnnr);
		UNPROTECT(1);
	    } else {
		for (i = 0; i < n; i++)
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
	    }
	    setAttrib(r, R_DimNamesSymbol, dnr);
	    UNPROTECT(2);
	}
	UNPROTECT(1);
    }
    else // !resize
	setAttrib(r, R_DimSymbol, dimsa);

    UNPROTECT(3); /* dimsa, r, dimsr */
    return r;
}

/* colSums(x, n, p, na.rm) and friends */
SEXP attribute_hidden do_colsum(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* X_, CXXR::RObject* n_, CXXR::RObject* p_, CXXR::RObject* na_rm_)
{
    SEXP x, ans = R_NilValue;
    int type;
    Rboolean NaRm, keepNA;

    x = X_;
    R_xlen_t n = asVecSize(n_);
    R_xlen_t p = asVecSize(p_);
    NaRm = CXXRCONSTRUCT(Rboolean, asLogical(na_rm_));
    if (n == NA_INTEGER || n < 0)
	error(_("invalid '%s' argument"), "n");
    if (p == NA_INTEGER || p < 0)
	error(_("invalid '%s' argument"), "p");
    if (NaRm == NA_LOGICAL) error(_("invalid '%s' argument"), "na.rm");
    keepNA = CXXRCONSTRUCT(Rboolean, !NaRm);

    switch (type = TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP: break;
    default:
	error(_("'x' must be numeric"));
    }
    if (n * (double)p > XLENGTH(x))
    	error(_("'x' is too short")); /* PR#16367 */

    int OP = op->variant();
    if (OP == 0 || OP == 1) { /* columns */
	PROTECT(ans = allocVector(REALSXP, p));
#ifdef _OPENMP
	int nthreads;
	/* This gives a spurious -Wunused-but-set-variable error */
	if (R_num_math_threads > 0)
	    nthreads = R_num_math_threads;
	else
	    nthreads = 1; /* for now */
#pragma omp parallel for num_threads(nthreads) default(none) \
    firstprivate(x, ans, n, p, type, NaRm, keepNA, R_NaReal, R_NaInt, OP)
#endif
	for (R_xlen_t j = 0; j < p; j++) {
	    R_xlen_t  cnt = n, i;
	    LDOUBLE sum = 0.0;
	    switch (type) {
	    case REALSXP:
	    {
		double *rx = REAL(x) + R_xlen_t(n)*j;
		if (keepNA)
		    for (sum = 0., i = 0; i < n; i++) sum += *rx++;
		else {
		    for (cnt = 0, sum = 0., i = 0; i < n; i++, rx++)
			if (!ISNAN(*rx)) {cnt++; sum += *rx;}
			else if (keepNA) {sum = NA_REAL; break;} // unused
		}
		break;
	    }
	    case INTSXP:
	    {
		int *ix = INTEGER(x) + R_xlen_t(n)*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_INTEGER) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    }
	    case LGLSXP:
	    {
		int *ix = LOGICAL(x) + R_xlen_t(n)*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_LOGICAL) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    }
	    }
	    if (OP == 1) sum /= cnt; /* gives NaN for cnt = 0 */
	    REAL(ans)[j] = double( sum);
	}
    }
    else { /* rows */
	PROTECT(ans = allocVector(REALSXP, n));

	/* allocate scratch storage to allow accumulating by columns
	   to improve cache hits */
	int *Cnt = nullptr;
	LDOUBLE *rans;
	if(n <= 10000) {
	    R_CheckStack2(n * sizeof(LDOUBLE));
	    rans = static_cast<LDOUBLE *>( alloca(n * sizeof(LDOUBLE)));
	    Memzero(rans, n);
	} else rans = Calloc(n, LDOUBLE);
	if (!keepNA && OP == 3) Cnt = Calloc(n, int);

	for (R_xlen_t j = 0; j < p; j++) {
	    LDOUBLE *ra = rans;
	    switch (type) {
	    case REALSXP:
	    {
		double *rx = REAL(x) + R_xlen_t(n) * j;
		if (keepNA)
		    for (R_xlen_t i = 0; i < n; i++) *ra++ += *rx++;
		else
		    for (R_xlen_t i = 0; i < n; i++, ra++, rx++)
			if (!ISNAN(*rx)) {
			    *ra += *rx;
			    if (OP == 3) Cnt[i]++;
			}
		break;
	    }
	    case INTSXP:
	    {
		int *ix = INTEGER(x) + (R_xlen_t)n * j;
		for (R_xlen_t i = 0; i < n; i++, ra++, ix++)
		    if (keepNA) {
			if (*ix != NA_INTEGER) *ra += *ix;
			else *ra = NA_REAL;
		    }
		    else if (*ix != NA_INTEGER) {
			*ra += *ix;
			if (OP == 3) Cnt[i]++;
		    }
		break;
	    }
	    case LGLSXP:
	    {
		int *ix = LOGICAL(x) + (R_xlen_t)n * j;
		for (R_xlen_t i = 0; i < n; i++, ra++, ix++)
		    if (keepNA) {
			if (*ix != NA_LOGICAL) *ra += *ix;
			else *ra = NA_REAL;
		    }
		    else if (*ix != NA_LOGICAL) {
			*ra += *ix;
			if (OP == 3) Cnt[i]++;
		    }
		break;
	    }
	    }
	}
	if (OP == 3) {
	    if (keepNA)
		for (R_xlen_t i = 0; i < n; i++) rans[i] /= p;
	    else
		for (R_xlen_t i = 0; i < n; i++) rans[i] /= Cnt[i];
	}
	for (R_xlen_t i = 0; i < n; i++) REAL(ans)[i] = (double) rans[i];

	if (!keepNA && OP == 3) Free(Cnt);
	if(n > 10000) Free(rans);
    }

    UNPROTECT(1);
    return ans;
}

/*
{
    data <- as.vector(data)
    dim <- as.integer(dim)
    vl <- prod(dim)
    if (length(data) != vl) {
        if (vl > .Machine$integer.max)
            stop("'dim' specifies too large an array")
        data <- rep(data, length.out = vl)
    }
    if (length(dim))
        dim(data) <- dim
    if (is.list(dimnames) && length(dimnames))
        dimnames(data) <- dimnames
    data
}
*/

/* array(data, dim, dimnames) */
SEXP attribute_hidden do_array(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* data_, CXXR::RObject* dim_, CXXR::RObject* dimnames_)
{
    SEXP vals, ans, dims, dimnames;
    R_xlen_t lendat, i, nans;

    vals = data_;
    /* at least NULL can get here */
    switch(TYPEOF(vals)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case EXPRSXP:
	case VECSXP:
	    break;
	default:
	    error(_("'data' must be of a vector type, was '%s'"),
		type2char(TYPEOF(vals)));
    }
    lendat = XLENGTH(vals);
    dims = dim_;
    dimnames = dimnames_;
    PROTECT(dims = coerceVector(dims, INTSXP));
    int nd = LENGTH(dims);
    if (nd == 0) error(_("'dims' cannot be of length 0"));
    double d = 1.0;
    for (int j = 0; j < nd; j++) d *= INTEGER(dims)[j];
#ifndef LONG_VECTOR_SUPPORT
    if (d > INT_MAX) error(_("too many elements specified"));
#endif
    nans = R_xlen_t( d);

    PROTECT(ans = allocVector(TYPEOF(vals), nans));
    switch(TYPEOF(vals)) {
    case LGLSXP:
	if (nans && lendat)
	    xcopyLogicalWithRecycle(LOGICAL(ans), LOGICAL(vals), 0, nans,
				    lendat);
	else
	    for (i = 0; i < nans; i++) LOGICAL(ans)[i] = NA_LOGICAL;
	break;
    case INTSXP:
	if (nans && lendat)
	    xcopyIntegerWithRecycle(INTEGER(ans), INTEGER(vals), 0, nans,
				    lendat);
	else
	    for (i = 0; i < nans; i++) INTEGER(ans)[i] = NA_INTEGER;
	break;
    case REALSXP:
	if (nans && lendat)
	    xcopyRealWithRecycle(REAL(ans), REAL(vals), 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) REAL(ans)[i] = NA_REAL;
	break;
    case CPLXSXP:
	if (nans && lendat)
	    xcopyComplexWithRecycle(COMPLEX(ans), COMPLEX(vals), 0, nans,
				    lendat);
	else {
	    Rcomplex na_cmplx;
	    na_cmplx.r = NA_REAL;
	    na_cmplx.i = 0;
	    for (i = 0; i < nans; i++) COMPLEX(ans)[i] = na_cmplx;
	}
	break;
    case RAWSXP:
	if (nans && lendat)
	    xcopyRawWithRecycle(RAW(ans), RAW(vals), 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) RAW(ans)[i] = 0;
	break;
    /* Rest are already initialized */
    case STRSXP:
	if (nans && lendat)
	    xcopyStringWithRecycle(ans, vals, 0, nans, lendat);
	break;
    case VECSXP:
    case EXPRSXP:
#ifdef SWITCH_TO_REFCNT
	if (nans && lendat)
	    xcopyVectorWithRecycle(ans, vals, 0, nans, lendat);
#else
	if (nans && lendat) {
	    /* Need to guard against possible sharing of values under
	       NAMED.  This is not needed with reference
	       coutning. (PR#15919) */
	    Rboolean needsmark = Rboolean(lendat < nans
					  || MAYBE_REFERENCED(vals));
	    for (i = 0; i < nans; i++) {
		SEXP elt = VECTOR_ELT(vals, i % lendat);
		if (needsmark || MAYBE_REFERENCED(elt))
		    MARK_NOT_MUTABLE(elt);
		SET_VECTOR_ELT(ans, i, elt);
	    }
	}
#endif
	break;
    default:
	// excluded above
	break;
    }

    ans = dimgets(ans, dims);
    if(!isNull(dimnames) && length(dimnames) > 0)
	ans = dimnamesgets(ans, dimnames);

    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_diag(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* nrow_, CXXR::RObject* ncol_)
{
    SEXP ans, x, snr, snc;
    int nr = 1, nc = 1, nprotect = 1;

    x = x_;
    snr = nrow_;
    snc = ncol_;
    nr = asInteger(snr);
    if (nr == NA_INTEGER)
	error(_("invalid 'nrow' value (too large or NA)"));
    if (nr < 0)
	error(_("invalid 'nrow' value (< 0)"));
    nc = asInteger(snc);
    if (nc == NA_INTEGER)
	error(_("invalid 'ncol' value (too large or NA)"));
    if (nc < 0)
	error(_("invalid 'ncol' value (< 0)"));
    int mn = (nr < nc) ? nr : nc;
    if (mn > 0 && LENGTH(x) == 0)
	error(_("'x' must have positive length"));

#ifndef LONG_VECTOR_SUPPORT
   if ((double)nr * (double)nc > INT_MAX)
	error(_("too many elements specified"));
#endif

   if (TYPEOF(x) == CPLXSXP) {
       PROTECT(ans = allocMatrix(CPLXSXP, nr, nc));
       int nx = LENGTH(x);
       R_xlen_t NR = nr;
       Rcomplex *rx = COMPLEX(x), *ra = COMPLEX(ans), zero;
       zero.r = zero.i = 0.0;
       for (R_xlen_t i = 0; i < NR*nc; i++) ra[i] = zero;
       R_xlen_t i, i1;
       MOD_ITERATE1(mn, nx, i, i1, {
	   ra[i * (NR+1)] = rx[i1];
       });
  } else {
       if(TYPEOF(x) != REALSXP) {
	   PROTECT(x = coerceVector(x, REALSXP));
	   nprotect++;
       }
       PROTECT(ans = allocMatrix(REALSXP, nr, nc));
       int nx = LENGTH(x);
       R_xlen_t NR = nr;
       double *rx = REAL(x), *ra = REAL(ans);
       for (R_xlen_t i = 0; i < NR*nc; i++) ra[i] = 0.0;
       R_xlen_t i, i1;
       MOD_ITERATE1(mn, nx, i, i1, {
	   ra[i * (NR+1)] = rx[i1];
       });
   }
   UNPROTECT(nprotect);
   return ans;
}


/* backsolve(r, b, k, upper.tri, transpose) */
SEXP attribute_hidden do_backsolve(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* r_, CXXR::RObject* x_, CXXR::RObject* k_, CXXR::RObject* upper_tri_, CXXR::RObject* transpose_)
{
    int nprot = 1;

    SEXP r = r_;
    SEXP b = x_;
    int nrr = nrows(r), nrb = nrows(b), ncb = ncols(b);
    int k = asInteger(k_);
    /* k is the number of rows to be used: there must be at least that
       many rows and cols in the rhs and at least that many rows on
       the rhs.
    */
    if (k == NA_INTEGER || k <= 0 || k > nrr || k > ncols(r) || k > nrb)
	error(_("invalid '%s' argument"), "k");
    int upper = asLogical(upper_tri_);
    if (upper == NA_INTEGER) error(_("invalid '%s' argument"), "upper.tri");
    int trans = asLogical(transpose_);
    if (trans == NA_INTEGER) error(_("invalid '%s' argument"), "transpose");
    if (TYPEOF(r) != REALSXP) {PROTECT(r = coerceVector(r, REALSXP)); nprot++;}
    if (TYPEOF(b) != REALSXP) {PROTECT(b = coerceVector(b, REALSXP)); nprot++;}
    double *rr = REAL(r);

    /* check for zeros on diagonal of r: only k row/cols are used. */
    size_t incr = nrr + 1;
    for(int i = 0; i < k; i++) { /* check for zeros on diagonal */
	if (rr[i * incr] == 0.0)
	    error(_("singular matrix in 'backsolve'. First zero in diagonal [%d]"),
		  i + 1);
    }

    SEXP ans = PROTECT(allocMatrix(REALSXP, k, ncb));
    if (k > 0 && ncb > 0) {
       /* copy (part) cols of b to ans */
	for(R_xlen_t j = 0; j < ncb; j++)
	    memcpy(REAL(ans) + j*k, REAL(b) + j*nrb, size_t(k) *sizeof(double));
	double one = 1.0;
	F77_CALL(dtrsm)("L", upper ? "U" : "L", trans ? "T" : "N", "N",
			&k, &ncb, &one, rr, &nrr, REAL(ans), &k);
    }
    UNPROTECT(nprot);
    return ans;
}

/* max.col(m, ties.method) */
SEXP attribute_hidden do_maxcol(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* m_, CXXR::RObject* ties_method_)
{
    SEXP m = m_;
    int method = asInteger(ties_method_);
    int nr = nrows(m), nc = ncols(m), nprot = 1;
    if (TYPEOF(m) != REALSXP) {PROTECT(m = coerceVector(m, REALSXP)); nprot++;}
    SEXP ans = PROTECT(allocVector(INTSXP, nr));
    R_max_col(REAL(m), &nr, &nc, INTEGER(ans), &method);
    UNPROTECT(nprot);
    return ans;
}
