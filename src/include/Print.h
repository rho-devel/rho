/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2003   The R Core Team.
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

#ifndef PRINT_H_
#define PRINT_H_

#include "Defn.h"
#include <R_ext/PrtUtil.h>
#include <R_ext/Print.h>

#ifdef __cplusplus
namespace CXXR {
    unsigned int stringWidth(unsigned int minwidth,
                             const CXXR::String* string);
    unsigned int stringWidthQuote(unsigned int minwidth,
			          const CXXR::String* string);
}

extern "C" {
#endif

#define formatRaw           Rf_formatRaw
#define EncodeElement       Rf_EncodeElement
#define EncodeEnvironment   Rf_EncodeEnvironment
#define printArray          Rf_printArray
#define printMatrix         Rf_printMatrix
#define printNamedVector    Rf_printNamedVector
#define printVector         Rf_printVector

typedef struct {
    int width;
    int na_width;
    int na_width_noquote;
    int digits;
    int scipen;
    int gap;
    int quote;
    int right;
    int max;
    SEXP na_string;
    SEXP na_string_noquote;
    int useSource;
    int cutoff; // for deparsed language objects
} R_print_par_t;
extern R_print_par_t R_print;

/* Computation of printing formats */
void formatRaw(Rbyte *, R_xlen_t, int *);

/* Formating of values */
const char *EncodeElement(SEXP, int, int, char);
const char *EncodeEnvironment(SEXP);


/* In Rinternals.h (and MUST be there):
   CustomPrintValue,  PrintValue, PrintValueRec */
void printArray(SEXP, SEXP, int, int, SEXP);
void printMatrix(SEXP, int, SEXP, int, int, SEXP, SEXP,
		 const char*, const char*);
void printNamedVector(SEXP, SEXP, int, const char*);
void printVector(SEXP, int, int);
// void PrintClosure(SEXP, Rboolean);
// void PrintLanguage(SEXP, Rboolean);

/* Utilities for S compatibility and debuggging */
int F77_SYMBOL(dblepr0)(const char *, int *, double *, int *);
int F77_SYMBOL(intpr0) (const char *, int *, int *, int *);
int F77_SYMBOL(realpr0)(const char *, int *, float *, int *);
void R_PV(SEXP s);

/* Offset for rowlabels if there are named dimnames */
#define R_MIN_LBLOFF 2

#define R_MIN_WIDTH_OPT		10
#define R_MAX_WIDTH_OPT		10000
#define R_MIN_DIGITS_OPT	0
#define R_MAX_DIGITS_OPT	22

#ifdef __cplusplus
}
#endif

#endif /* PRINT_H_ */
