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
 *	Copyright (C) 2000, 2001, 2006 The R Core Team
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
 *    Computes the geometric probabilities, Pr(X=x) = p(1-p)^x.
 */

#include "nmath.h"
#include "dpq.h"

double dgeom(double x, double p, int give_log)
{ 
    double prob;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(p)) return x + p;
#endif

    if (p <= 0 || p > 1) ML_ERR_return_NAN;

    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x) || p == 0) return R_D__0;
    x = R_D_forceint(x);

    /* prob = (1-p)^x, stable for small p */
    prob = dbinom_raw(0.,x, p,1-p, give_log);

    return((give_log) ? log(p) + prob : p*prob);
}
