/*
   Implementation of eval_model() and spread() for R.

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

#include "nlOptimizer.h"

#ifndef R_S_H
#include "nonlin.h"
#endif /* R_S_H */

int
evaluate(double *param, longint nParam aMOD, double **value aSEV)
{
#ifdef R_S_H
  SEXP newPars, call, result;
  int i, nResult;

  PROTECT(newPars = allocVector(REALSXP, nParam));
  PROTECT(model);
  for(i = 0; i < nParam; i++)
    REAL(newPars)[i] = param[i];
  PROTECT(call = lang2(model, newPars));
  PROTECT(result = eval(call, R_GlobalEnv));
  nResult = LENGTH(result);
  if(value[0] == (double *) 0) {
    UNPROTECT(4);
    return(nResult);
  }
  double *res = REAL(result);
  for(i = 0; i < nResult; i++)
    value[0][i] = res[i];
  UNPROTECT(4);
#else
  spread(param, nParam SEV);
  eval_model(TRUE SEV);
  value[0] = nl_results[0];
#endif
  return(-1);
}
