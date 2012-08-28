/*
   header file for the nlme package

   Copyright 1999-2001 Saikat DebRoy,
		       Douglas Bates <bates@stat.wisc.edu>

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

#ifndef NLME_NLMEFIT_H
#define NLME_NLMEFIT_H
#include "base.h"

typedef struct dim_struct {
  longint
    N,				/* number of observations in original data */
    ZXrows,			/* number of rows in ZXy  */
    ZXcols,			/* number of columns in ZXy */
    Q,				/* number of levels of random effects */
    Srows,			/* number of rows in decomposition */
    *q,				/* dimensions of the random effects */
    *ngrp,			/* numbers of groups at each level */
    *DmOff,			/* offsets into the DmHalf array */
    *ncol,			/* no. of columns decomposed at each level */
    *nrot,			/* no. of columns rotated at each level */
    **ZXoff,			/* offsets into ZXy */
    **ZXlen,			/* groups lengths */
    **SToff,			/* offsets into storage */
    **DecOff,			/* offsets into decomposition */
    **DecLen;			/* decomposition group lengths */
} *dimPTR;

typedef struct state_struct {
  dimPTR dd;
  double *ZXy;
  longint *pdClass,
    *RML;
} *statePTR;

extern dimPTR dims(longint *);
extern dimPTR dimS(SEXP);
extern int count_DmHalf_pars(dimPTR, longint *);
extern double * generate_theta(double *, dimPTR, longint *, double *);
extern double * generate_DmHalf(double *, dimPTR, longint *, double *);
extern void dimFree(dimPTR);
extern void mixed_decomp(double *, longint *);
extern void mixed_fcn(longint, double *, double *, void *);
extern void mixed_grad(longint, double *, double *, void *);
extern void internal_decomp(dimPTR, double *);
extern void mixed_loglik(double *, longint *, double *, longint *,
			 double *, double *);
extern double internal_loglik(dimPTR, double *, double *, longint *,
			      double *, double *);
extern void mixed_estimate(double *, longint *, double *, longint *,
			   double *, double *, longint *);
extern void internal_estimate(dimPTR, double *);
extern void mixed_EM(double *, longint *, double *, longint *,
		     longint *, longint *, double *, double *, double *);
extern void internal_EM(dimPTR, double *, double *, int, longint *,
			longint *, double *, double *, double *);
extern void mixed_combined(double *, longint *, double *, longint *,
			   longint *, longint *, double *, double *,
			   double *, longint *);
extern void mixed_calcf(longint *, double *, longint *, double *,
			longint *, double *, void (*)(void));
extern void mixed_calcgh(longint *, double *, longint *, double *,
			 double *, longint *, double *, void (*)(void));
extern void gls_loglik(double *, longint *, double *, double *);
extern void gls_estimate(double *, longint *, double *, double *,
			 double *, double *, longint *, longint *);

#endif /* NLME_NLMEFIT_H */
