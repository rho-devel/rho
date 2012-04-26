/*
   nls functions used in predict.nls

   Copyright 1999 Saikat DebRoy

   This file is part of the nlme package for S and related languages
   and is made available under the terms of the GNU General Public
   License, version 2, or at your option, any later version,
   incorporated herein by reference.

   This program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, a copy is available at
   http://www.r-project.org/Licenses/

*/
#include "base.h"

#ifndef R_S_H

static  double xlower = 0.0;

static double
est_delta(double *x, longint i)
{
  double xx;
  if(!sqrt_eps) sqrt_eps = sqrt(DOUBLE_EPS);
  if(!xlower) xlower = 100.*DOUBLE_XMIN;
				/* should sometime use the strategy of */
				/* the grd routine in dmnf */
  xx = fabs(x[i]);
  if (xx < xlower) return sqrt_eps;
  else return xx*sqrt_eps;
}

void
nls_diff_gradient(longint *pnpar, longint *pnobs, double *theta, double
		  *base, double *gradient, longint *pneg aSEV)
{
  longint i, j, npar = *pnpar, nobs = *pnobs, neg = *pneg;
  double xx, *gcol, di;
  S_EVALUATOR

  for(i=0, gcol = gradient; i<npar; i++, gcol += nobs) {
    xx = theta[i];
    theta[i] = theta[i] + (di = est_delta(theta,i));
    spread(theta, npar SEV); eval_model(FALSE SEV);
    if(neg) di = -di;		/* want negative gradient? */
    for(j=0; j<nobs; j++)
      gcol[j] = (nl_results[0][j] - base[j])/di;
    theta[i] = xx;
  }
}
#endif /* R_S_H */
