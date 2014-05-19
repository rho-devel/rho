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
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, 2005 The R Core Team
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
 *
 *  DESCRIPTION
 *
 *    The density function of the F distribution.
 *    To evaluate it, write it as a Binomial probability with p = x*m/(n+x*m).
 *    For m >= 2, we use the simplest conversion.
 *    For m < 2, (m-2)/2 < 0 so the conversion will not work, and we must use
 *               a second conversion.
 *    Note the division by p; this seems unavoidable
 *    for m < 2, since the F density has a singularity as x (or p) -> 0.
 */

#include "nmath.h"
#include "dpq.h"

double df(double x, double m, double n, int give_log)
{
    double p, q, f, dens;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return x + m + n;
#endif
    if (m <= 0 || n <= 0) ML_ERR_return_NAN;
    if (x < 0.)  return(R_D__0);
    if (x == 0.) return(m > 2 ? R_D__0 : (m == 2 ? R_D__1 : ML_POSINF));
    if (!R_FINITE(m) && !R_FINITE(n)) { /* both +Inf */
	if(x == 1.) return ML_POSINF;
	/* else */  return R_D__0;
    }
    if (!R_FINITE(n)) /* must be +Inf by now */
	return(dgamma(x, m/2, 2./m, give_log));
    if (m > 1e14) {/* includes +Inf: code below is inaccurate there */
	dens = dgamma(1./x, n/2, 2./n, give_log);
	return give_log ? dens - 2*log(x): dens/(x*x);
    }

    f = 1./(n+x*m);
    q = n*f;
    p = x*m*f;

    if (m >= 2) {
	f = m*q/2;
	dens = dbinom_raw((m-2)/2, (m+n-2)/2, p, q, give_log);
    }
    else {
	f = m*m*q / (2*p*(m+n));
	dens = dbinom_raw(m/2, (m+n)/2, p, q, give_log);
    }
    return(give_log ? log(f)+dens : f*dens);
}
