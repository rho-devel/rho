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
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1997, 1998  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2012  The R Core Team.
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
 *
 *  EXPORTS	printVector()
 *		printNamedVector()
 *		printRealVector()
 *		printIntegerVector()
 *		printComplexVector()
 *
 *  See ./printutils.c	 for remarks on Printing and the Encoding utils.
 *  See ./format.c	 for the formatXXXX functions used below.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdarg>
#include "Print.h"

#include <numeric>

using namespace std;
using namespace CXXR;

#define DO_first_lab			\
    if (indx) {				\
	labwidth = IndexWidth(n) + 2;	\
	/* labwidth may well be		\
	   one more than desired ..*/	\
	VectorIndex(1, labwidth);	\
	width = labwidth;		\
    }					\
    else width = 0

#define DO_newline			\
    Rprintf("\n");			\
    if (indx) {				\
	VectorIndex(i + 1, labwidth);	\
	width = labwidth;		\
    }					\
    else				\
	width = 0

static
void printLogicalVector(int *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatLogical(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%s", EncodeLogical(x[i], w));
	width += w;
    }
    Rprintf("\n");
}

attribute_hidden
void printIntegerVector(int *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatInteger(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%s", EncodeInteger(x[i], w));
	width += w;
    }
    Rprintf("\n");
}

// used in uncmin.c
attribute_hidden
void printRealVector(double *x, R_xlen_t n, int indx)
{
    int w, d, e, labwidth=0, width;

    DO_first_lab;
    formatReal(x, n, &w, &d, &e, 0);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%s", EncodeReal(x[i], w, d, e, OutDec));
	width += w;
    }
    Rprintf("\n");
}

attribute_hidden
void printComplexVector(Rcomplex *x, R_xlen_t n, int indx)
{
    int w, wr, dr, er, wi, di, ei, labwidth=0, width;

    DO_first_lab;
    formatComplex(x, n, &wr, &dr, &er, &wi, &di, &ei, 0);

    w = wr + wi + 2;	/* +2 for "+" and "i" */
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	if (ISNA(x[i].r) || ISNA(x[i].i))
	    Rprintf("%s", EncodeReal(NA_REAL, w, 0, 0, OutDec));
	else
	    Rprintf("%s", EncodeComplex(x[i], wr + R_print.gap , dr, er,
					wi, di, ei, OutDec));
	width += w;
    }
    Rprintf("\n");
}

static void printStringVector(const StringVector* sv, R_xlen_t n, int quote,
			      int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    StringVector::const_iterator beg = sv->begin();
    w = accumulate(beg, beg + n, 0, (quote ? stringWidthQuote : stringWidth));

    for (R_xlen_t i = 0; i < n; i++) {
	String* str = const_cast<String*>((*sv)[i].get());
	if (i > 0 && width + w + R_print.gap > R_print.width) {
	    DO_newline;
	}
	Rprintf("%*s%s", R_print.gap, "",
		EncodeString(str, w, quote, Rprt_adj(R_print.right)));
	width += w + R_print.gap;
    }
    Rprintf("\n");
}

static
void printRawVector(Rbyte *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatRaw(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%*s%s", R_print.gap, "", EncodeRaw(x[i], ""));
	width += w;
    }
    Rprintf("\n");
}

void printVector(SEXP x, int indx, int quote)
{
/* print R vector x[];	if(indx) print indices; if(quote) quote strings */
    R_xlen_t n;

    if ((n = XLENGTH(x)) != 0) {
	R_xlen_t n_pr = (n <= R_print.max +1) ? n : R_print.max;
	/* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printLogicalVector(LOGICAL(x), n_pr, indx);
	    break;
	case INTSXP:
	    printIntegerVector(INTEGER(x), n_pr, indx);
	    break;
	case REALSXP:
	    printRealVector(REAL(x), n_pr, indx);
	    break;
	case STRSXP:
	    {
		const StringVector* sv = static_cast<StringVector*>(x);
		printStringVector(sv, n_pr, (quote ? '"' : 0), indx);
		break;
	    }
	case CPLXSXP:
	    printComplexVector(COMPLEX(x), n_pr, indx);
	    break;
	case RAWSXP:
	    printRawVector(RAW(x), n_pr, indx);
	    break;
	default:  // -Wswitch
	    break;
	}
	if(n_pr < n)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			n - n_pr);
    }
    else
#define PRINT_V_0						\
	switch (TYPEOF(x)) {					\
	case LGLSXP:	Rprintf("logical(0)\n");	break;	\
	case INTSXP:	Rprintf("integer(0)\n");	break;	\
	case REALSXP:	Rprintf("numeric(0)\n");	break;	\
	case CPLXSXP:	Rprintf("complex(0)\n");	break;	\
	case STRSXP:	Rprintf("character(0)\n");	break;	\
	case RAWSXP:	Rprintf("raw(0)\n");		break;	\
	default:                                        break;  \
	}
	PRINT_V_0;
}

#undef DO_first_lab
#undef DO_newline


/* The following code prints vectors which have every element named.

 * Primitives for each type of vector are presented first, followed
 * by the main (dispatching) function.
 * 1) These primitives are almost identical... ==> use PRINT_N_VECTOR macro
 * 2) S prints a _space_ in the first column for named vectors; we dont.
 */

#define PRINT_N_VECTOR(INI_FORMAT, PRINT_1)				\
{									\
    int i, j, k, nlines, nperline, w, wn;				\
    INI_FORMAT;								\
									\
    {                                                                   \
        StringVector::const_iterator beg = names->begin();              \
        wn = accumulate(beg, beg + n, 0, stringWidth);                  \
    }                                                                   \
    if (w < wn) w = wn;							\
    nperline = R_print.width / (w + R_print.gap);			\
    if (nperline <= 0) nperline = 1;					\
    nlines = n / nperline;						\
    if (n % nperline) nlines += 1;					\
									\
    for (i = 0; i < nlines; i++) {					\
	if (i) Rprintf("\n");						\
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++)	\
	    Rprintf("%s%*s",						\
		    EncodeString((*names)[k], w, 0, Rprt_adj_right),	\
		    R_print.gap, "");					\
	Rprintf("\n");							\
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++)	\
	    PRINT_1;							\
    }									\
    Rprintf("\n");							\
}


static void printNamedLogicalVector(int * x, int n, StringVector* names)
    PRINT_N_VECTOR(formatLogical(x, n, &w),
		   Rprintf("%s%*s", EncodeLogical(x[k],w), R_print.gap,""))

static void printNamedIntegerVector(int * x, int n, StringVector* names)
    PRINT_N_VECTOR(formatInteger(x, n, &w),
		   Rprintf("%s%*s", EncodeInteger(x[k],w), R_print.gap,""))

#undef INI_F_REAL
#define INI_F_REAL	int d, e; formatReal(x, n, &w, &d, &e, 0)

static void printNamedRealVector(double * x, int n, StringVector* names)
    PRINT_N_VECTOR(INI_F_REAL,
		   Rprintf("%s%*s", EncodeReal(x[k],w,d,e, OutDec),R_print.gap,""))

#undef INI_F_CPLX
#define INI_F_CPLX					\
    int wr, dr, er, wi, di, ei;				\
    formatComplex(x, n, &wr, &dr, &er, &wi, &di, &ei, 0);	\
    w = wr + wi + 2

#undef P_IMAG_NA
#define P_IMAG_NA				\
	    if(ISNAN(x[k].i))			\
		Rprintf("+%si", "NaN");		\
	    else

static void printNamedComplexVector(Rcomplex * x, int n, StringVector* names)
    PRINT_N_VECTOR(INI_F_CPLX,
	{ /* PRINT_1 */
	    if(j) Rprintf("%*s", R_print.gap, "");
	    if (ISNA(x[j].r) || ISNA(x[j].i)) {
		Rprintf("%s", EncodeReal(NA_REAL, w, 0, 0, OutDec));
	    }
	    else {
		Rprintf("%s", EncodeReal(x[k].r, wr, dr, er, OutDec));
		P_IMAG_NA
		if (x[k].i >= 0)
		    Rprintf("+%si", EncodeReal(x[k].i, wi, di, ei, OutDec));
		else
		    Rprintf("-%si", EncodeReal(-x[k].i, wi, di, ei, OutDec));
	    }
	})

#undef INI_F_STRING
#define INI_F_STRING                                          \
    StringVector::const_iterator beg = sv->begin();           \
    w = accumulate(beg, beg + n, 0,                           \
                   (quote ? stringWidthQuote : stringWidth));

static void printNamedStringVector(StringVector* sv, int n, int quote,
				   StringVector* names)
    PRINT_N_VECTOR(INI_F_STRING,
		   Rprintf("%s%*s",
			   EncodeString((*sv)[k], w, quote, Rprt_adj_right),
			   R_print.gap, ""))

static void printNamedRawVector(Rbyte * x, int n, StringVector* names)
    PRINT_N_VECTOR(formatRaw(x, n, &w),
		   Rprintf("%s%*s", EncodeRaw(x[k], ""), R_print.gap,""))

attribute_hidden
void printNamedVector(SEXP x, SEXP names, int quote, const char *title)
{
    int n;

    if (title != NULL)
	 Rprintf("%s\n", title);

    StringVector* namesv = SEXP_downcast<StringVector*>(names);
    if ((n = LENGTH(x)) != 0) {
	int n_pr = (n <= R_print.max +1) ? n : R_print.max;
	/* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printNamedLogicalVector(LOGICAL(x), n_pr, namesv);
	    break;
	case INTSXP:
	    printNamedIntegerVector(INTEGER(x), n_pr, namesv);
	    break;
	case REALSXP:
	    printNamedRealVector(REAL(x), n_pr, namesv);
	    break;
	case CPLXSXP:
	    printNamedComplexVector(COMPLEX(x), n_pr, namesv);
	    break;
	case STRSXP:
	    {
		if(quote) quote = '"';
		StringVector* sv = static_cast<StringVector*>(x);
	        printNamedStringVector(sv, n_pr, quote, namesv);
		break;
	    }
	case RAWSXP:
	    printNamedRawVector(RAW(x), n_pr, namesv);
	    break;
	default:  // -Wswitch
	    break;
	}
	if(n_pr < n)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			n - n_pr);

    }
    else {
	Rprintf("named ");
	PRINT_V_0;
    }
}
