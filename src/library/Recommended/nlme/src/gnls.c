/*
   Routines for fitting gnls models

   Copyright 1997-2005 Douglas M. Bates <bates@stat.wisc.edu>,
		       Jose C. Pinheiro,
		       Saikat DebRoy

   This file is part of the nlme package for R and related languages
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
#include "matrix.h"
#include "nlmefit.h"

extern void corStruct_recalc(double *, longint *, longint *, double *);

/* gnls functions and variables     */

typedef struct gnls_struct {	/* Generalized nonlinear least squares structure */
  double *residuals, *gradient, *corFactor, *varWeights, minFactor,
    tolerance, *newtheta, *theta, *incr, *add_ons,
    new_objective, objective;
  double *result[1];
  longint corOpt, varOpt, npar, ncol, N, nrdof, maxIter, *corDims;
#ifdef R_S_H
  SEXP model;
#endif
  int conv_failure;
} *gnlsPtr;

static gnlsPtr
gnls_init(double *ptheta, longint *dims, double *corFactor, double *varWeights,
	  longint *corDims, double *settings, double *additional,
	  longint corOpt, longint varOpt aMOD)
{
  longint nResult;
  gnlsPtr gnls = Calloc(1, struct gnls_struct);
  gnls->theta = ptheta;
  gnls->corFactor = corFactor;
  gnls->varWeights = varWeights;
  gnls->corDims = corDims;
  gnls->npar = dims[0];
  gnls->N = dims[1];
  gnls->nrdof = gnls->N - gnls->npar;
  gnls->ncol = gnls->npar + 1;
  gnls->maxIter = (int) settings[0];
  gnls->minFactor = settings[1];
  gnls->tolerance = settings[2];
  gnls->newtheta = Calloc(gnls->npar, double);
  gnls->incr = Calloc(gnls->npar, double);
  gnls->varOpt = varOpt;
  gnls->corOpt = corOpt;
  gnls->add_ons = additional;
#ifdef R_S_H
  gnls->model = model;
  gnls->result[0] = DNULLP;
  nResult = evaluate(ptheta, gnls->npar MOD, gnls->result SEV);
  gnls->result[0] = Calloc(nResult, double);
#endif
  return gnls;
}

static void
gnlsFree( gnlsPtr gnls )
{
  Free(gnls->newtheta);
  Free(gnls->incr);
#ifdef R_S_H
  Free(gnls->result[0]);
#endif
  Free(gnls);
}

static double
gnls_objective(gnlsPtr gnls)
{
  longint i, j;
  if(gnls->varOpt) {			/* variance function correction */
    for(i = 0; i < gnls->N; i++) {
      for(j = 0; j < gnls->ncol; j++) {
	*(gnls->result[0] + i + j * gnls->N) *= gnls->varWeights[i];
      }
    }
  }
  if(gnls->corOpt) {			/* correlation structure correction */
    corStruct_recalc(gnls->result[0], gnls->corDims, &gnls->ncol, gnls->corFactor);
  }
  gnls->residuals = gnls->result[0] + gnls->npar * gnls->N;
  gnls->gradient = gnls->result[0];
  return(d_sum_sqr(gnls->residuals, gnls->N));
}

/*  static double */
/*  gnls_RegSS(gnlsPtr gnls) */
/*  { */
/*    longint i; */
/*    double regSS = 0, aux; */
/*    for(i = 0; i < gnls->N; i++) { */
/*      aux = d_dot_prod(gnls->gradient + i, gnls->N, gnls->incr, 1L, gnls->npar); */
/*      regSS += aux * aux; */
/*    } */
/*    return(regSS); */
/*  } */

static double
gnls_increment(gnlsPtr gnls)
{
  double regSS = 0, *auxRes;
  QRptr aQR;
  longint i;
  if (!sqrt_eps) sqrt_eps = sqrt(DOUBLE_EPS);
  auxRes = Calloc(gnls->N, double);
  Memcpy(auxRes, gnls->residuals, gnls->N);
  aQR = QR(gnls->gradient, gnls->N, gnls->N, gnls->npar);
  QRsolve(aQR, gnls->residuals, gnls->N, 1L, gnls->incr, gnls->npar);
  QRqty(aQR, auxRes, gnls->N, 1L);
  for(i=0; i < gnls->npar; i++) {
    regSS += auxRes[i] * auxRes[i];
  }
  QRfree(aQR);
  Free(auxRes);
  return(sqrt(((double) gnls->nrdof) * regSS /
	      ((double) gnls->npar) * (gnls->new_objective - regSS)));
}

static longint
gnls_iterate(gnlsPtr gnls aSEV)
{
  double factor, criterion;
  longint iteration;
#ifdef R_S_H
  SEXP model = gnls->model;
#endif
  S_EVALUATOR

  Memcpy(gnls->newtheta, gnls->theta, gnls->npar);
  evaluate(gnls->theta, gnls->npar MOD, gnls->result SEV);
  gnls->new_objective = gnls->objective = gnls_objective(gnls);
  gnls->conv_failure = 0;
  for (factor = 1.0, iteration = 1; iteration <= gnls->maxIter;
       iteration++) {		/* outer iteration loop */
				/* increment and convergence criterion */
    criterion = gnls_increment(gnls);
    if (gnls->conv_failure) return(iteration); /* Unable to make increment */
    if (criterion < gnls->tolerance) return(iteration); /* successful completion */
    do {			/* inner loop for acceptable step size */
      if (factor < gnls->minFactor) {
	gnls->conv_failure = 1;
	return(iteration);
      }
      Memcpy(gnls->newtheta, gnls->theta, gnls->npar);
      d_axpy(gnls->newtheta, factor, gnls->incr, gnls->npar);
      evaluate(gnls->newtheta, gnls->npar MOD, gnls->result SEV);
      gnls->new_objective = gnls_objective(gnls);
      if (gnls->conv_failure) return(iteration); /* unable to evaluate objective */
      factor /= 2.0;
    } while (gnls->new_objective >= gnls->objective);
    factor *= 4.0;
    if (factor > 1.0)
      factor = 1.0;
    gnls->objective = gnls->new_objective;
    Memcpy(gnls->theta, gnls->newtheta, gnls->npar);
  }
  gnls->conv_failure = 2;	/* Maximum number of iterations exceeded */
  return(iteration - 1);
}

static void
gnls_wrapup(gnlsPtr gnls aSEV)
{
#ifdef R_S_H
  SEXP model = gnls->model;
#endif
  S_EVALUATOR

  evaluate(gnls->theta, gnls->npar MOD, gnls->result SEV);
  Memcpy(gnls->add_ons, gnls->result[0] + gnls->npar * gnls->N, gnls->N);
  gnls->objective = gnls_objective(gnls);
}

void
fit_gnls(double *ptheta, longint *pdims, double *pcorFactor, double
	 *pvarWeights, longint *pcorDims, double *settings,
	 double *additional, longint *pcorOpt, longint *pvarOpt aMOD)
{
  gnlsPtr gnls;
  S_EVALUATOR

#ifdef R_S_H
  PROTECT(model);
#endif /* R_S_H */
  if(!sqrt_eps) sqrt_eps = sqrt(DOUBLE_EPS);
  gnls = gnls_init(ptheta, pdims, pcorFactor, pvarWeights, pcorDims,
    settings, additional, *pcorOpt, *pvarOpt MOD);
  settings[4] = (double) gnls_iterate(gnls SEV);
  gnls_wrapup(gnls SEV);
  settings[3] = gnls->conv_failure;
  settings[5] = gnls->objective;
  gnlsFree(gnls);
#ifdef R_S_H
  UNPROTECT(1);
#endif /* R_S_H */
}
