/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2007  The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_ARITH_H_
#define R_ARITH_H_

/* Only for use where config.h has not already been included */
#if defined(HAVE_GLIBC2) && !defined(_BSD_SOURCE)
/* ensure that finite and isnan are declared */
# define _BSD_SOURCE 1
#endif

#include <R_ext/Boolean.h>
#include <R_ext/libextern.h>

#ifdef  __cplusplus
extern "C" {
#elif !defined(NO_C_HEADERS)
/* needed for isnan and isfinite, neither of which are used under C++ */
# include <math.h>
#endif

/* implementation of these : ../../main/arithmetic.c */
LibExtern double R_NaN;		/* IEEE NaN */
LibExtern double R_PosInf;	/* IEEE Inf */
LibExtern double R_NegInf;	/* IEEE -Inf */
LibExtern double R_NaReal;	/* NA_REAL: IEEE */
LibExtern int	 R_NaInt;	/* NA_INTEGER:= INT_MIN currently */
#ifdef __MAIN__
//#undef extern  /* 2007/06/03 arr */
#undef LibExtern
#endif

#define NA_LOGICAL	R_NaInt
#define NA_INTEGER	R_NaInt
/* #define NA_FACTOR	R_NaInt  unused */
#define NA_REAL		R_NaReal
/* NA_STRING is a SEXP, so defined in Rinternals.h */

int R_IsNA(double);		/* True for R's NA only */
Rboolean R_IsNaN(double);		/* True for special NaN, *not* for NA */
Rboolean R_finite(double);		/* True if none of NA, NaN, +/-Inf */

#define ISNA(x)	       R_IsNA(x)
/* True for *both* NA and NaN.
   NOTE: some systems do not return 1 for TRUE. 
   Also note that C++ math headers specifically undefine
   isnan if it is a macro (it is on OS X and in C99),
   hence the workaround.  This code also appears in Rmath.h
*/
#ifdef __cplusplus
  Rboolean R_isnancpp(double); /* in arithmetic.c */
#  define ISNAN(x)     R_isnancpp(x)
#else
#  define ISNAN(x)     ((Rboolean)(isnan(x)!=0))
#endif

/* The following is only defined inside R */
#ifdef HAVE_WORKING_ISFINITE
/* isfinite is defined in <math.h> according to C99 */
# define R_FINITE(x)    isfinite(x)
#else
# define R_FINITE(x)    R_finite(x)
#endif

#ifdef  __cplusplus
}
#endif

#endif /* R_ARITH_H_ */
