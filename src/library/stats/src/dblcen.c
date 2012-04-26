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

#include <math.h>
#include "mva.h"

/* Double Centering for Classical Multidimensional Scaling */

void dblcen(double *a, int *na)
{
    double sum;
    int n, i, j;

    n = *na;
    for(i=0 ; i<n ; i++) {
	sum = 0;
	for(j=0 ; j<n ; j++)
	    sum += a[i+j*n];
	sum /= n;
	for(j=0 ; j<n ; j++)
	    a[i+j*n] -= sum;
    }
    for(j=0 ; j<n ; j++) {
	sum = 0;
	for(i=0 ; i<n ; i++)
	    sum += a[i+j*n];
	sum /= n;
	for(i=0 ; i<n ; i++)
	    a[i+j*n] -= sum;
    }
}
