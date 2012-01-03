/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-12 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  Modified version of code from the Cephes Math Library
 *  Modifications Copyright (C) 2002 The R Development Core Team.
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <math.h>
#include <R_ext/Arith.h>

/* based on
Cephes Math Library Release 2.8:  June, 2000
Copyright 1984, 1995, 2000 by Stephen L. Moshier

According to http://www.netlib.org/cephes/readme

  What you see here may be used freely but it comes with no support or
  guarantee.
*/

static double polevl(double x, double *coef, int N)
{
    double *p = coef, ans;
    int i = N;

    ans = *p++;
    do
	ans = ans * x + *p++;
    while( --i );
    return( ans );
}

static double p1evl(double x, double *coef, int N)
{
    double *p = coef, ans;
    int i = N - 1;

    ans = x + *p++;
    do
	ans = ans * x + *p++;
    while( --i );
    return( ans );
}

static double P[] = {
-4.33231683752342103572E-3,
-5.91750212056387121207E-1,
-4.37390226194356683570E0,
-9.09030533308377316566E0,
-5.56682227230859640450E0
};
static double Q[] = {
/* 1.00000000000000000000E0,*/
 1.28757002067426453537E1,
 4.86042483805291788324E1,
 6.95722521337257608734E1,
 3.34009336338516356383E1
};

#define LOGE2 1.4426950408889634073599

double asinh(double xx)
#ifdef __cplusplus
	throw ()
#endif
{
    double a, z, x;
    int sign;

    if(ISNAN(xx)) return xx;
    if( xx == 0.0 ) return xx;
    if( xx < 0.0 ) {
	sign = -1;
	x = -xx;
    } else {
	sign = 1;
	x = xx;
    }

    if( x > 1.0e8 ) {
	if( x == R_PosInf ) return xx;
	return( sign * (log(x) + LOGE2) );
    }
    z = x * x;
    if( x < 0.5 ) {
	a = ( polevl(z, P, 4)/p1evl(z, Q, 4) ) * z;
	a = a * x + x;
	if( sign < 0 ) a = -a;
	return a;
    }
    a = sqrt( z + 1.0 );
    return sign * log(x + a);
}
