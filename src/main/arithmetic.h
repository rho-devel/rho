/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2007	    The R Core Team.
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

#ifndef ARITHMETIC_H
#define ARITHMETIC_H 1

#ifdef __cplusplus

#include <Internal.h>
CXXR::quick_builtin do_math3;

extern "C" {
#endif

SEXP do_math1(SEXP, SEXP, SEXP, SEXP);
SEXP do_math2(SEXP, SEXP, SEXP, SEXP);
SEXP do_math4(SEXP, SEXP, SEXP, SEXP);
#ifdef WHEN_MATH5_IS_THERE
 SEXP do_math5(SEXP, SEXP, SEXP, SEXP);
#endif
SEXP do_cmathfuns(SEXP, SEXP, SEXP, SEXP);

SEXP complex_math1(SEXP, SEXP, SEXP, SEXP);
SEXP complex_math2(SEXP, SEXP, SEXP, SEXP);
SEXP complex_unary(ARITHOP_TYPE, SEXP, SEXP);
SEXP complex_binary(ARITHOP_TYPE, SEXP, SEXP);

SEXP R_binary(SEXP, SEXP, SEXP, SEXP);
SEXP R_unary(SEXP, SEXP, SEXP);

#ifdef __cplusplus
}
#endif

#endif /* ARITHMETIC_H */
