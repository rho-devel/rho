/*
   Routines dealing with correlation structures.

   Copyright 1997-2005  Douglas M. Bates <bates@stat.wisc.edu>,
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

#include "matrix.h"

#ifndef USING_R
extern void F77_NAME(dqrdca)();
#else
#include <R_ext/Applic.h>
#endif /* USING_R */

/* Factor list and Recalc for general corStruct object */

void
corStruct_factList(double *mat, longint *pdims, double *FactorL, double *logdet)
{
    longint i, j, M = pdims[1], *len = pdims + 4, job = 11L, info;
#ifndef USING_R
    longint zero = 0L;
#endif
    double *work, *work1;

    for(i = 0; i < M; i++) {
	longint li = len[i], lisq = li * li, lip1 = li + 1;
	work = Calloc(li, double);
	work1 = Calloc(lisq, double);
#ifdef USING_R
	F77_CALL(chol) (mat, &li, &li, mat, &info);
#else
	F77_CALL(chol) (mat, &li, work, &zero, &zero, &info);
#endif
	for(j = 0; j < li; j++) {
	    work1[j * lip1] = 1;
	    F77_CALL(dtrsl) (mat, &li, &li, work1 + j * li, &job, &info);
	    *logdet -= log(fabs(mat[j * lip1]));
	}
	Memcpy(FactorL, work1, lisq);
	Free(work); Free(work1);
	FactorL += lisq;
	mat += lisq;
    }
}

void
corStruct_recalc(double *Xy, longint *pdims, longint *ZXcol, double *Factor)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4, *start = len + M, i;
    for(i = 0; i < M;  i++) {
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Factor += (len[i] * len[i]);
    }
}

/* symm class - unstructured correlation - based on spherical
   parametrization */

void
symm_fullCorr(double *par, longint *maxC, double *crr)
    /* calculates full correlation structure  */
{
    double *work, aux, aux1, *src = par, *src1, *dest;
    longint i, j, n = *maxC;

    /* first get upper-triangular factor */
    dest = work = Calloc(n * (n + 1) / 2, double);
    for(i = 0; i < n; i++) {
	aux = 1.0;
	for(j = 0; j < i; j++) {
	    aux1 = exp(*src);
	    aux1 = PI * aux1/(1 + aux1); /* untransforming */
	    *dest = aux * cos(aux1);
	    aux *= sin(aux1);
	    dest++; src++;
	}
	*dest = aux;
	dest++;
    }

    /* getting the correlations */
    for(i = 0, dest = crr, src = work; i < n - 1; i++) {
	longint ip1 = i + 1;
	src += i;
	for(j = ip1, src1 = src; j < n; j++) {
	    src1 += j;
	    *dest = d_dot_prod(src, 1L, src1, 1L, ip1);
	    dest++;
	}
    }
    Free(work);
}

static void
symm_mat(double *crr, longint *time, longint *n, longint *maxC, double *mat)
{
    longint i, j, k, np1 = *n + 1, n1, n2;
    for(i = 0; i < *n; i++) {
	mat[i * np1] = 1.0;
	for(j = i + 1; j < *n; j++) {
	    n1 = (time[i] < time[j]) ? time[i] : time[j];
	    n2 = time[i] + time[j] - 2 * n1 - 1;
	    k = n1 * *maxC - n1 * (n1 + 1) / 2 + n2;
	    *(mat + i + j * (*n)) = *(mat + j + i * (*n)) = crr[k];
	}
    }
}

void
symm_matList(double *pars, longint *time, longint *maxC,
	     longint *pdims, double *mat)
{
    double *crr = Calloc(*maxC * (*maxC - 1) / 2, double);
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameters assumed in unconstrained form */
    symm_fullCorr(pars, maxC, crr);
    for(i = 0; i < M;  i++) {
	symm_mat(crr, time, &len[i], maxC, mat);
	mat += len[i] * len[i];
	time += len[i];
    }
    Free(crr);
}

static void
symm_fact(double *crr, longint *time, longint *n, longint *maxC, double *mat,
	  double *logdet)
{
    longint job = 11L, info, i, nsq = *n * (*n), np1 = *n + 1;
#ifndef USING_R
    longint zero = 0L;
#endif
    double *work = Calloc(*n, double), *work1 = Calloc(nsq, double);

    symm_mat(crr, time, n, maxC, mat);
#ifdef USING_R
    F77_CALL(chol) (mat, n, n, mat, &info);
#else
    F77_CALL(chol) (mat, n, work, &zero, &zero, &info);
#endif
    for(i = 0; i < *n; i++) {
	work1[i * np1] = 1;
	F77_CALL(dtrsl) (mat, n, n, work1 + i * (*n), &job, &info);
	*logdet -= log(fabs(mat[i * np1]));
    }
    Memcpy(mat, work1, nsq);
    Free(work); Free(work1);
}

void
symm_factList(double *pars, longint *time, longint *maxC, longint *pdims,
	      double *FactorL, double *logdet)
{
    double *crr = Calloc(*maxC * (*maxC - 1L) / 2L, double);
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameters assumed in unconstrained form */
    symm_fullCorr(pars, maxC, crr);
    for(i = 0; i < M;  i++) {
	symm_fact(crr, time, &len[i], maxC, FactorL, logdet);
	FactorL += len[i] * len[i];
	time += len[i];
    }
    Free(crr);
}

void
symm_recalc(double *Xy, longint *pdims, longint *ZXcol, double *pars,
	    longint *time, longint *maxC, double *logdet)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4, *start = len + M, i;
    double *crr = Calloc(*maxC * (*maxC - 1) / 2, double);
    /* parameters assumed in unconstrained form */
    symm_fullCorr(pars, maxC, crr);
    for(i = 0; i < M;  i++) {
	double *Factor = Calloc((len[i] * len[i]), double);
	symm_fact(crr, time + start[i], &len[i], maxC, Factor, logdet);
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Free(Factor);
    }
    Free(crr);
}

/* nat class - unstructured correlation - natural parametrization */

void
nat_fullCorr(double *par, longint *maxC, double *crr)
    /* calculates full correlation structure  */
{
    double aux;
    longint i, npar = *maxC * (*maxC - 1) / 2;

    for(i = 0; i < npar; i++) {
	aux = exp(par[i]);
	crr[i] = (aux - 1)/(aux + 1);
    }
}

void
nat_matList(double *pars, longint *time, longint *maxC,
	    longint *pdims, double *mat)
{
    double *crr = Calloc(*maxC * (*maxC - 1) / 2, double);
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameters assumed in unconstrained form */
    nat_fullCorr(pars, maxC, crr);
    for(i = 0; i < M;  i++) {
	symm_mat(crr, time, &len[i], maxC, mat);
	mat += len[i] * len[i];
	time += len[i];
    }
    Free(crr);
}

void
nat_factList(double *pars, longint *time, longint *maxC, longint *pdims,
	     double *FactorL, double *logdet)
{
    double *crr = Calloc(*maxC * (*maxC - 1L) / 2L, double);
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameters assumed in unconstrained form */
    nat_fullCorr(pars, maxC, crr);
    for(i = 0; i < M;  i++) {
	symm_fact(crr, time, &len[i], maxC, FactorL, logdet);
	FactorL += len[i] * len[i];
	time += len[i];
    }
    Free(crr);
}

void
nat_recalc(double *Xy, longint *pdims, longint *ZXcol, double *pars,
	   longint *time, longint *maxC, double *logdet)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4, *start = len + M, i;
    double *crr = Calloc(*maxC * (*maxC - 1) / 2, double);
    /* parameters assumed in unconstrained form */
    nat_fullCorr(pars, maxC, crr);
    for(i = 0; i < M;  i++) {
	double *Factor = Calloc((len[i] * len[i]), double);
	symm_fact(crr, time + start[i], &len[i], maxC, Factor, logdet);
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Free(Factor);
    }
    Free(crr);
}

/* AR1 class */

static void
AR1_mat(double *par, longint *n, double *mat)
{
    longint i, j;
    double aux;
    for(i = 0; i < *n; i++) {
	*(mat + i * (*n + 1)) = 1.0;
	for(j = i + 1; j < *n; j++) {
	    aux = pow(*par, j - i);
	    *(mat + i + j * (*n)) = *(mat + j + i * (*n)) = aux;
	}
    }
}

static double
safe_phi(double x)		/* returns (exp(x) - 1)/(exp(x) + 1), x < 0 */
{				/* or (1 - exp(-x))/(1 + exp(-x)), x > 0 */
    double ex;
    if (x < 0.0) {
	ex = exp(x);
	return (ex - 1.0)/(ex + 1.0);
    }
    ex = exp(-x);
    return (1.0 - ex)/(1.0 + ex);
}

void
AR1_matList(double *par, longint *pdims, double *mat)
{
    longint i, M = pdims[1], *len = pdims + 4;
    /* par assumed in unconstrained form */

    *par = safe_phi( *par );
    for(i = 0; i < M;  i++) {
	AR1_mat(par, &len[i], mat);
	mat += len[i] * len[i];
    }
}

static void
AR1_fact(double *par, longint *n, double *mat, double *logdet)
{
    longint i, np1 = *n + 1;
    double aux = sqrt(1 - *par * (*par)), aux1 = - (*par)/aux;

    *logdet -= (*n - 1) * log(aux);
    aux = 1/aux;
    mat[0] = 1;
    for(i = 1; i < *n; i++) {
	mat[i * np1] = aux;
	mat[i + *n * (i - 1)] = aux1;
    }
}

void
AR1_factList(double *par, longint *pdims, double *FactorL,
	     double *logdet)
{
    longint i, M = pdims[1], *len = pdims + 4;
    /* par assumed in unconstrained form */

    *par = safe_phi( *par );
    for(i = 0; i < M;  i++) {
	AR1_fact(par, &len[i], FactorL, logdet);
	FactorL += len[i] * len[i];
    }
}

void
AR1_recalc(double *Xy, longint *pdims, longint *ZXcol, double *par,
	   double *logdet)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4,  *start = len + M, i;
    double *Factor;
    /* par assumed in unconstrained form */
    *par = safe_phi( *par );
    for(i = 0; i < M;  i++) {
	Factor = Calloc(len[i] * len[i], double);
	AR1_fact(par, &len[i], Factor, logdet);
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Free(Factor);
    }
}


/* Continuous AR1 class */

static void
CAR1_mat(double *par, double *time, longint *n, double *mat)
{
    longint i, j;
    double aux;
    for(i = 0; i < *n; i++) {
	*(mat + i * (*n + 1)) = 1.0;
	for(j = i + 1; j < *n; j++) {
	    aux = pow(*par, fabs(time[j] - time[i]));
	    *(mat + i + j * (*n)) = *(mat + j + i * (*n)) = aux;
	}
    }
}

void
CAR1_matList(double *par, double *time, longint *pdims, double *mat)
{
    longint i, M = pdims[1], *len = pdims + 4;
    double aux = exp(*par);
    /* parameter assumed in unconstrained form */
    *par = aux / (1.0 + aux);
    for(i = 0; i < M;  i++) {
	CAR1_mat(par, time, &len[i], mat);
	mat += len[i] * len[i];
	time += len[i];
    }
}

static void
CAR1_fact(double *par, double *time, longint *n, double *mat, double *logdet)
{
    longint job = 11L, info, i, nsq = *n * (*n), np1 = *n + 1;
    double *work = Calloc(*n, double), *work1 = Calloc(nsq, double);
    CAR1_mat(par, time, n, mat);
#ifdef USING_R
    F77_CALL(chol) (mat, n, n, mat, &info);
#else
    longint zero = 0L;
    F77_CALL(chol) (mat, n, work, &zero, &zero, &info);
#endif
    for(i = 0; i < *n; i++) {
	work1[i * np1] = 1;
	F77_CALL(dtrsl) (mat, n, n, work1 + i * (*n), &job, &info);
	*logdet -= log(fabs(mat[i * np1]));
    }
    Memcpy(mat, work1, nsq);
    Free(work); Free(work1);
}

void
CAR1_factList(double *par, double *time, longint *pdims,
	      double *FactorL, double *logdet)
{
    longint i, M = pdims[1], *len = pdims + 4;
    double aux = exp(*par);
    /* parameter assumed in unconstrained form */
    *par = aux / (1.0 + aux);
    for(i = 0; i < M;  i++) {
	CAR1_fact(par, time, &len[i], FactorL, logdet);
	FactorL += len[i] * len[i];
	time += len[i];
    }
}

void
CAR1_recalc(double *Xy, longint *pdims, longint *ZXcol,
	    double *par, double *time, double *logdet)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4, *start = len + M, i;
    double aux = exp(*par);
    /* parameter assumed in unconstrained form */
    *par = aux / (1.0 + aux);
    for(i = 0; i < M;  i++) {
	double *Factor = Calloc(len[i] * len[i], double);
	CAR1_fact(par, time + start[i], &len[i], Factor, logdet);
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Free(Factor);
    }
}

/* ARMA class */

static void
ARMA_transPar(longint N, double *pars, double sgn)
{
    longint i, j, n, n2;
    double ps, D, aux;
    for(n = N - 1; n > -1; n--) {
	if ((ps = pars[n] * pars[n]) >= 1.0)
	    error(_("All parameters must be less than 1 in absolute value"));
	if (n) {
	    D = 1 - ps;
	    n2 = (n - 1)/2;
	    for(i = 0; i <= n2; i++) {
		if ((j = n - i -1) > i) {
		    aux = (pars[i] + sgn * pars[j] * pars[n])/D;
		    pars[j] = (pars[j] + sgn * pars[i] * pars[n])/D;
		    pars[i] = aux;
		} else {
		    pars[i] /= (1 - sgn * pars[n]);
		}
	    }
	}
	pars[n] = log((1 + pars[n])/(1 - pars[n]));
    }
}

void
ARMA_unconstCoef(longint *p, longint *q, double *pars)
{
    ARMA_transPar(*p, pars, 1.0);
    ARMA_transPar(*q, pars + *p, -1.0);
}

static void
ARMA_untransPar(longint N, double *pars, double sgn)
{
    longint i, j;
    double *aux;
    if (N) {
	aux = Calloc(N, double);
	for(i = 0; i < N; i++) {
	    pars[i] = exp(-pars[i]);
	    aux[i] = pars[i] = (1 - pars[i])/(1 + pars[i]);
	    if (i) {
		for(j = 0; j < i; j++) {
		    pars[j] = aux[j] + sgn * pars[i] * aux[i - j - 1];
		}
		Memcpy(aux, pars, i);
	    }
	}
	Free(aux);
    }
}

void
ARMA_constCoef(longint *p, longint *q, double *pars)
{
    ARMA_untransPar(*p, pars, -1.0);
    ARMA_untransPar(*q, pars + *p, 1.0);
}

static void
ARMA_cross(longint *p, longint *q, double *pars, double *psi)
{
    longint i, j, M = *q + 1, PM;
    M = (*p > M ? *p : M);
    psi[0] = 1;
    for(i = 1; i < M; i++) {
	psi[i] = ((*q < i) ? 0 : pars[*p + i - 1]);
	PM = (*p < i ? *p : i);
	for(j = 0; j < PM; j++) {
	    psi[i] += pars[j] * psi[i - j - 1];
	}
    }
}

static void
ARMA_corr(longint *p, longint *q, longint *maxlag, double *pars, double *psi,
	  double *crr)
{
    longint P = *p + 1, Pp1 = P + 1, i, j, k, minPQ, Mlag, maxPQ,
	*pivot = Calloc(P, longint);
    double *coef = Calloc(P * P, double), *src, *qraux = Calloc(P, double),
	*work = Calloc(P * P, double), *work1;

    if (!sqrt_eps) sqrt_eps = sqrt(DOUBLE_EPS);
    if ((maxPQ = ((*p > *q) ? *p : *q))) {
	for(i = 0, src = coef; i < P; i++, src += Pp1) {
	    crr[i] = 0;
	    *src = 1;
	}
	Mlag = ((*maxlag > *q) ? *maxlag : *q);
	Mlag = ((Mlag > *p) ? Mlag : *p) + 1;
	work1 = Calloc(Mlag, double);
	for(i = P; i < Mlag; i++) {
	    crr[i] = 0;
	}
	crr[0] = 1;
	for(i = 1, src = pars + *p; i <= *q; i++, src++) {
	    crr[0] += (*src) * psi[i];
	}
	if (*p) {
	    if ((minPQ = ((*p < *q) ? *p : *q))) {
		for(i = 1, src = pars + *p - 1; i <= minPQ; i++) {
		    for(j = i; j <= *q; j++) {
			crr[i] += *(src + j) * psi[j - i];
		    }
		}
	    }
	    for(i = 0, src = coef; i < P; i++, src++) {
		for(j = 0; j < *p; j++) {
		    k = i - j - 1;
		    k = ((k > 0) ? k : -k);
		    *(src + (k * P)) -= pars[j];
		}
	    }
/*        F77_CALL(dqrdca) (coef, &P, &P, &P, qraux, pivot, work, &i, &sqrt_eps); */
	    F77_CALL(dqrdc2) (coef, &P, &P, &P,  &sqrt_eps, &i, qraux, pivot, work);
	    if (i < P)
		error(_("Coefficient matrix not invertible" ));
	    i = 100L;
	    F77_CALL(dqrsl) (coef, &P, &P, &P, qraux, crr, DNULLP, crr, work1, DNULLP,
			     DNULLP, &i, &j);
	    Memcpy(crr, work1, Mlag);
	}
	for(i = P; i <= *q; i++) {
	    for(j = 0; j < *p; j++) {
		crr[i] += pars[j] * crr[i - j - 1];
	    }
	    for(j = i, src = pars + i - 1; j <= *q; j++, src++) {
		crr[i] += *src * psi[j - i];
	    }
	}
	for(i = maxPQ + 1; i < Mlag; i++) {
	    for(j = 0; j < *p; j++) {
		crr[i] += pars[j] * crr[i - j - 1];
	    }
	}
	for(i = 1; i < Mlag; i++) {
	    crr[i] /= crr[0];
	}
	Free(qraux); Free(work); Free(coef); Free(pivot); Free(work1);
    }
    crr[0] = 1;
}

static void
ARMA_fullCorr(longint *p, longint *q, longint *maxlag, double *pars,
	      double *crr)
{
    longint M = *q + 1;
    double *psi;
    M = ((M < *p) ? *p : M);
    psi = Calloc(M, double);
    ARMA_cross(p, q, pars, psi);
    ARMA_corr(p, q, maxlag, pars, psi, crr);
    Free(psi);
}

static void
ARMA_mat(double *crr, longint *time, longint *n, double *mat)
{
    longint i, j, k;
    for(i = 0; i < *n; i++) {
	for(j = i; j < *n; j++) {
	    k = time[j] - time[i];
	    k = ((k < 0) ? -k : k);
	    *(mat + i + j * (*n)) = *(mat + j + i * (*n)) = crr[k];
	}
    }
}

void
ARMA_matList(double *pars, longint *p, longint *q, longint *time,
	     longint *maxlag, longint *pdims, double *mat)
{
    double *crr = Calloc(*maxlag + 1L, double);
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameters assumed in unconstrained form */
    ARMA_constCoef(p, q, pars);
    ARMA_fullCorr(p, q, maxlag, pars, crr);
    for(i = 0; i < M;  i++) {
	ARMA_mat(crr, time, &len[i], mat);
	mat += len[i] * len[i];
	time += len[i];
    }
    Free(crr);
}

static void
ARMA_fact(double *crr, longint *time, longint *n, double *mat, double *logdet)
{
    longint job = 11L, info, i, nsq = *n * (*n), np1 = *n + 1;
    double *work = Calloc(*n, double), *work1 = Calloc(nsq, double);
#ifndef USING_R
    longint zero = 0L;
#endif
    ARMA_mat(crr, time, n, mat);
#ifdef USING_R
    F77_CALL(chol) (mat, n, n, mat, &info);
#else
    F77_CALL(chol) (mat, n, work, &zero, &zero, &info);
#endif
    for(i = 0; i < *n; i++) {
	work1[i * np1] = 1;
	F77_CALL(dtrsl) (mat, n, n, work1 + i * (*n), &job, &info);
	*logdet -= log(fabs(mat[i * np1]));
    }
    Memcpy(mat, work1, nsq);
    Free(work); Free(work1);
}

void
ARMA_factList(double *pars, longint *p, longint *q, longint *time,
	      longint *maxlag, longint *pdims, double *FactorL,
	      double *logdet)
{
    double *crr = Calloc(*maxlag + 1L, double);
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameters assumed in unconstrained form */
    ARMA_constCoef(p, q, pars);
    ARMA_fullCorr(p, q, maxlag, pars, crr);
    for(i = 0; i < M;  i++) {
	ARMA_fact(crr, time, &len[i], FactorL, logdet);
	FactorL += len[i] * len[i];
	time += len[i];
    }
    Free(crr);
}

void
ARMA_recalc(double *Xy, longint *pdims, longint *ZXcol, double *pars,
	    longint *p, longint *q, longint *time, longint *maxlag,
	    double *logdet)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4, *start = len + M, i;
    double *crr = Calloc(*maxlag + 1L, double);
    /* parameters assumed in unconstrained form */
    ARMA_constCoef(p, q, pars);
    ARMA_fullCorr(p, q, maxlag, pars, crr);
    for(i = 0; i < M;  i++) {
	double *Factor = Calloc(len[i] * len[i], double);
	ARMA_fact(crr, time + start[i], &len[i], Factor, logdet);
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Free(Factor);
    }
}

/* Compound symmetry */

static void
compSymm_mat(double *par, longint *n, double *mat)
{
    longint i, j;
    for(i = 0; i < *n; i++) {
	mat[(*n + 1) * i] = 1.0;
	for(j = i + 1; j < *n; j++) {
	    *(mat + i + j * (*n)) = *(mat + j + i * (*n)) = *par;
	}
    }
}

void
compSymm_matList(double *par, double *inf, longint *pdims, double *mat)
{
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameter assumed in unconstrained form */
    double aux = exp(*par);
    *par = (aux + *inf)/(aux + 1.0);
    for(i = 0; i < M;  i++) {
	compSymm_mat(par, &len[i], mat);
	mat += len[i] * len[i];
    }
}

static void
compSymm_fact(double *par, longint *n, double *mat, double *logdet)
{
    longint i, j, np1 = *n + 1, nsq = *n * (*n);
    double aux, aux1, *work = Calloc(nsq, double);
    aux = 1 + (*n - 1) * (*par);
    *logdet -= log(aux)/2;
    aux = 1/sqrt(aux * (*n));
    for(i = 0; i < nsq; i += *n) {
	work[i] = aux;
    }
    aux = 1 - (*par);
    *logdet -= (*n - 1) * log(aux)/2;
    for(i = 1; i < *n; i++) {
	aux1 = -1/sqrt(aux * i * (i + 1));
	for(j = 0; j < i; j++) {
	    work[i + j * (*n)] = aux1;
	}
	work[i * np1] = -i * aux1;
    }
    Memcpy(mat, work, nsq);
    Free(work);
}

void
compSymm_factList(double *par, double *inf, longint *pdims,
		  double *FactorL, double *logdet)
{
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameter assumed in unconstrained form */
    double aux = exp(*par);
    *par = (aux + *inf)/(aux + 1.0);
    for(i = 0; i < M;  i++) {
	compSymm_fact(par, &len[i], FactorL, logdet);
	FactorL += len[i] * len[i];
    }
}

void
compSymm_recalc(double *Xy, longint *pdims, longint *ZXcol, double *par,
		double *inf, double *logdet)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4, *start = len + M, i;
    double aux = exp(*par);
    /* parameter assumed in unconstrained form */
    *par = (aux + *inf)/(aux + 1.0);
    for(i = 0; i < M;  i++) {
	double *Factor = Calloc(len[i] * len[i], double);
	compSymm_fact(par, &len[i], Factor, logdet);
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Free(Factor);
    }
}

/* Huyn-Feldt class */

static void
HF_mat(double *par, longint *time, longint *n, double *mat)
{
    longint i, j, np1 = *n + 1;
    for(i = 0; i < *n; i++) {
	mat[i * np1] = par[time[i]];
	for(j = i + 1; j < *n; j++) {
	    *(mat + i + j * (*n)) = *(mat + j + i * (*n)) =
		0.5 * (par[time[i]] + par[time[j]]) - 1.0;
	}
    }
}

void
HF_matList(double *par, longint *maxC, longint *time, longint *pdims,
	   double *mat)
{
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameter assumed in unconstrained form */
    double inf = -1.0/(2.0 * ((double) *maxC));
    for(i = 0; i < *maxC; i++) {
	par[i] = 2.0 * (exp(par[i]) + inf) + 1.0;
    }
    for(i = 0; i < M;  i++) {
	HF_mat(par, time, &len[i], mat);
	mat += len[i] * len[i];
	time += len[i];
    }
}

static void
HF_fact(double *par, longint *time, longint *n, double *mat, double *logdet)
{
    longint job = 11L, info, i, nsq = *n * (*n), np1 = *n + 1;
    double *work = Calloc(*n, double), *work1 = Calloc(nsq, double);
#ifndef USING_R
    longint zero = 0L;
#endif
    HF_mat(par, time, n, mat);
#ifdef USING_R
    F77_CALL(chol) (mat, n, n, mat, &info);
#else
    F77_CALL(chol) (mat, n, work, &zero, &zero, &info);
#endif
    for(i = 0; i < *n; i++) {
	work1[i * np1] = 1;
	F77_CALL(dtrsl) (mat, n, n, work1 + i * (*n), &job, &info);
	*logdet -= log(fabs(mat[i * np1]));
    }
    Memcpy(mat, work1, nsq);
    Free(work); Free(work1);
}

void
HF_factList(double *par, longint *maxC, longint *time, longint *pdims,
	    double *FactorL, double *logdet)
{
    longint i, M = pdims[1], *len = pdims + 4;
    /* parameter assumed in unconstrained form */
    double inf = -1.0/(2.0 * ((double) *maxC));
    for(i = 0; i < *maxC; i++) {
	par[i] = 2.0 * (exp(par[i]) + inf) + 1.0;
    }
    for(i = 0; i < M;  i++) {
	HF_fact(par, time, &len[i], FactorL, logdet);
	FactorL += len[i] * len[i];
	time += len[i];
    }
}

void
HF_recalc(double *Xy, longint *pdims, longint *ZXcol, double *par,
	  longint *time, longint *maxC, double *logdet)
{
    longint N = pdims[0], M = pdims[1], *len = pdims + 4,  *start = len + M, i;
    double inf = -1.0/(2.0 * ((double) *maxC));
    /* parameter assumed in unconstrained form */
    for(i = 0; i < *maxC; i++) {
	par[i] = 2.0 * (exp(par[i]) + inf) + 1.0;
    }
    for(i = 0; i < M;  i++) {
	double *Factor = Calloc(len[i] * len[i], double);
	HF_fact(par, time + start[i], &len[i], Factor, logdet);
	mult_mat(Xy + start[i], N, Factor, len[i], len[i], len[i],
		 Xy + start[i], N, *ZXcol);
	Free(Factor);
    }
}

/* Spatial correlation structures */

/* Spherical class */

static double
spher_corr(double val)
{
    if (val < 1) return(1.0 - 1.5 * val + 0.5 * pow(val, 3));
    else return(0.0);
}

/* Exponential class */

static double
exp_corr(double val)
{
    return(exp(-val));
}

/* Gaussian class */

static double
Gaus_corr(double val)
{
    return(exp(-(val * val)));
}

/* Linear class */

static double
lin_corr(double val)
{
    if (val < 1) return(1.0 - val);
    else return(0.0);
}

/* Rational class */

static double
ratio_corr(double val)
{
    double val2 = val * val;
    return(1/(1+val2));
}

/* Dummy class */
static double
dummy_corr(double val)
{
    error(_("Unknown spatial correlation class"));
    return(0.0);              /* -Wall */
}

/* methods for the virtual class */

static void
spatial_mat(double *par, double *dist, longint *n, longint *nug,
	    double (*corr)(double ), double *mat)
{
    longint i, j, np1 = *n + 1;
    double aux, *sdist, ratio = 1.0;
    sdist = dist;
    if (*nug) ratio = par[1];
    for(i = 0; i < *n; i++) {
	mat[i * np1] = 1.0;
	for(j = i + 1; j < *n; j++, sdist++) {
	    aux = *sdist / *par;
	    *(mat + i + j * (*n)) = *(mat + j + i * (*n)) = ratio * corr(aux);
	}
    }
}

void
spatial_matList(double *par, longint *nug, double *dist, longint *pdims,
		double *minD, double *mat)
{
    longint i, M = pdims[1], spClass = pdims[2], *len = pdims + 4,
	*start = len + M;
    double aux, (*corr)(double ) = dummy_corr;
    /* parameter assumed in unconstrained form */
    par[0] = exp(par[0]);
    if (*nug == 1) {
	aux = exp(par[1]);
	par[1] = 1 / (1.0 + aux);	/* 1 - nugget */
    }
    switch(spClass) {
    case 1:			/* spherical */
	corr = spher_corr;
	par[0] += *minD;
	break;
    case 2:			/* exponential */
	corr = exp_corr;
	break;
    case 3:			/* Gaussian */
	corr = Gaus_corr;
	break;
    case 4:			/* linear */
	corr = lin_corr;
	par[0] +=  *minD;
	break;
    case 5:			/* rational quadratic */
	corr = ratio_corr;
	break;
    default:
	error(_("Unknown spatial correlation class"));
	break;
    }
    for(i = 0; i < M;  i++) {
	spatial_mat(par, dist + start[i], &len[i], nug, corr, mat);
	mat += len[i] * len[i];
    }
}

static void
spatial_fact(double *par, double *dist, longint *n, longint *nug,
	     double (*corr) (double ), double *mat,
	     double *logdet)
{
    longint job = 11L, info, i, nsq = *n * (*n), np1 = *n + 1;
    double *work = Calloc(*n, double), *work1 = Calloc(nsq, double);
#ifndef USING_R
    longint zero = 0L;
#endif
    spatial_mat(par, dist, n, nug, corr, mat);
#ifdef USING_R
    F77_CALL(chol) (mat, n, n, mat, &info);
#else
    F77_CALL(chol) (mat, n, work, &zero, &zero, &info);
#endif
    for(i = 0; i < *n; i++) {
	work1[i * np1] = 1;
	F77_CALL(dtrsl) (mat, n, n, work1 + i * (*n), &job, &info);
	*logdet -= log(fabs(mat[i * np1]));
    }
    Memcpy(mat, work1, nsq);
    Free(work); Free(work1);
}

void
spatial_factList(double *par, longint *nug, double *dist, longint *pdims,
		 double *minD, double *FactorL, double *logdet)
{
    longint i, M = pdims[1], spClass = pdims[2], *len = pdims + 4,
	*start = len + M;
    double aux, (*corr)(double ) = dummy_corr;
    /* parameter assumed in unconstrained form */
    par[0] = exp(par[0]);
    if (*nug == 1) {
	aux = exp(par[1]);
	par[1] = 1 / (1.0 + aux);	/* 1 - nugget */
    }

    switch(spClass) {
    case 1:			/* spherical */
	corr = spher_corr;
	par[0] += *minD;
	break;
    case 2:			/* exponential */
	corr = exp_corr;
	break;
    case 3:			/* Gaussian */
	corr = Gaus_corr;
	break;
    case 4:			/* linear */
	corr = lin_corr;
	par[0] +=  *minD;
	break;
    case 5:			/* rational quadratic */
	corr = ratio_corr;
	break;
    default:
	error(_("Unknown spatial correlation class"));
	break;
    }
    for(i = 0; i < M;  i++) {
	spatial_fact(par, dist + start[i], &len[i], nug, corr, FactorL, logdet);
	FactorL += len[i] * len[i];
    }
}

void
spatial_recalc(double *Xy, longint *pdims, longint *ZXcol, double *par,
	       double *dist, double *minD, longint *nug, double *logdet)
{
    longint N = pdims[0], M = pdims[1], spClass = pdims[2],
	*len = pdims + 4, *start = len + M, i;
    double aux, (*corr)(double ) = dummy_corr, *sXy;
    /* parameter assumed in unconstrained form */
    par[0] = exp(par[0]);
    if (*nug == 1) {
	aux = exp(par[1]);
	par[1] = 1 / (1.0 + aux); /* 1 - nugget */
    }

    switch(spClass) {
    case 1:			/* spherical */
	corr = spher_corr;
	par[0] += *minD;
	break;
    case 2:			/* exponential */
	corr = exp_corr;
	break;
    case 3:			/* Gaussian */
	corr = Gaus_corr;
	break;
    case 4:			/* linear */
	corr = lin_corr;
	par[0] +=  *minD;
	break;
    case 5:			/* rational quadratic */
	corr = ratio_corr;
	break;
    default:
	error(_("Unknown spatial correlation class"));
	break;
    }

    for(i = 0, sXy = Xy; i < M;  i++) {
	double *Factor = Calloc(len[i] * len[i], double);
	spatial_fact(par, dist + start[i], &len[i], nug, corr, Factor, logdet);
	mult_mat(sXy, N, Factor, len[i], len[i], len[i], sXy, N, *ZXcol);
	sXy += len[i];
	Free(Factor);
    }
}
