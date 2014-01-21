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
 *  Copyright (C) 1997-2002   The R Core Team.
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

#include "Rinternals.h"

/* Double Centering for Classical Multidimensional Scaling */

/* NB: this does not duplicate A */
SEXP DoubleCentre(SEXP A)
{
    int n = nrows(A);
    double *a = REAL(A);
    size_t N = n; /* avoid integer overflow with long vectors */

    for(int i = 0; i < n; i++) {
	double sum = 0;
	for(int j = 0; j < n; j++) sum += a[i+j*N];
	sum /= n;
	for(int j = 0; j < n; j++) a[i+j*N] -= sum;
    }
    for(int j = 0; j < n; j++) {
	double sum = 0;
	for(int i = 0; i < n; i++) sum += a[i+j*N];
	sum /= n;
	for(int i = 0; i < n; i++) a[i+j*N] -= sum;
    }
    return A;
}
