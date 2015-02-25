/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2007  The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/* Included by R.h: API */

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
#include <cmath>

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

/* ISNAN(): True for *both* NA and NaN.
   NOTE: some systems do not return 1 for TRUE.
   Also note that C++ math headers specifically undefine
   isnan if it is a macro (it is on OS X and in C99),
   hence the workaround.  This code also appears in Rmath.h
*/
#ifdef __cplusplus
    /* CXXR Notes:
     *
     * isnan() was introduced by C99, which prescribes that it shall
     * be a macro. ISO14882, even in its 2003 issue, requires only
     * that <cmath> offer the facilities of math.h as defined in
     * ISO/IEC 9899:1990.  TR1 proposed that <cmath> make isnan()
     * available to C++ programs as a templated function within the
     * std namespace.  On Linux, gcc appears to make the ordinary
     * isnan() macro available to C++ programs.  However, on Mac OS X
     * (at least as of 10.5.6) gcc appears to make isnan() available
     * to C++ programs only as std::isnan().
     *
     * The following covers these two cases.  Another possibility that
     * may need to be addressed is that C++'s isnan() is to be found
     * in namespace std::tr1.
     */
    inline Rboolean R_isnancpp(double x)
    {
#ifdef isnan
	return Rboolean(isnan(x)!=0);
#else
	return Rboolean(std::isnan(x)!=0);
#endif
    }

    /* This anticipates C++ 0x. */
    inline Rboolean R_finite(double x)
    {
	return Rboolean(std::isfinite(x) != 0);
    }

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
