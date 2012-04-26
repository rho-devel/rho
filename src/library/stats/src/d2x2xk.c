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

#include <R.h>
#include <Rmath.h>

#include "ctest.h"

void
d2x2xk(Sint *k, double *m, double *n, double *t, double *d)
{
    int i, j, l, w, y, z;
    double u, **c;

    c = (double **) R_alloc(*k + 1, sizeof(double *));
    l = y = z = 0;
    c[0] = (double *) R_alloc(1, sizeof(double));
    c[0][0] = 1;
    for(i = 0; i < *k; i++) {
	y = imax2(0, *t - *n);
	z = imin2(*m, *t);
	c[i + 1] = (double *) R_alloc(l + z - y + 1, sizeof(double));
	for(j = 0; j <= l + z - y; j++)
	    c[i + 1][j] = 0;
	for(j = 0; j <= z - y; j++) {
	    u = dhyper(j + y, *m, *n, *t, FALSE);
	    for(w = 0; w <= l; w++)
		c[i + 1][w + j] += c[i][w] * u;
	}
	l = l + z - y;
	m++;
	n++;
	t++;
    }

    u = 0;
    for(j = 0; j <= l; j++)
	u += c[*k][j];
    for(j = 0; j <= l; j++)
	d[j] = c[*k][j] / u;
}
