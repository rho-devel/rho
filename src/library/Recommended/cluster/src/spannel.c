/* Compute the SPANNing ELlipsoid
 * ------------------------------ for clusplot.default(*, span = TRUE)

 * Original spannel.f -- translated by f2c (version 20010821).
 * and f2c-clean,v 1.10 2002/03/28 16:37:27 maechler
 */
#include <math.h>
#include "cluster.h"

#ifdef DEBUG_spannel
# include <R_ext/Print.h>
#endif

void spannel(int *ncas, /* = number of objects */
	     int *ndep, /* = number of variables */
	     double *dat,/* [ncas, 0:ndep] */
	     double *dstopt, /* = squared distances [1:ncas] */
	     double *cov,/* matrix [0:ndep, 0:ndep] */
	     double *varsum,	/* [1:ndep] */
	     double *varss,	/* [1:ndep] */
	     double *prob,	/* [1:ncas] */
	     double *work,	/* [0:ndep] */
	     double *eps,
	     int *maxit, /* = maximal # iterations (and returns #{iter.})*/
	     int *ierr)
{
    static int c__0 = 0;
    int it, i, j, k;
    double dmax, p, deter;

    int dat_dim1 = *ncas;
    int cov_dim1 = *ndep + 1;

#define COV(i,j) cov[i + j * cov_dim1]
#define X(i,j)	 dat[i + j * dat_dim1]
/*      X(i,j) : i = 0..(n-1),  j = 0,..p often 1..p */

    /* Parameter adjustments */
    --varss;
    --varsum;

/* When spannel() is called,  dat[i,0] are all == 1 -- left unchanged:
 * Scale Data dat[i,j] to mean = 0 and var{1/n} = 1  -- for j= 1:ndep (not j=0!)
 */
    for (j = 1; j <= *ndep; ++j) {
	varsum[j] = 0.;
	varss[j] = 0.;
    }
    for (i = 0; i < *ncas; ++i) {
	for (j = 1; j <= *ndep; ++j) {
	    p = X(i,j);
	    varsum[j] += p;
	    varss [j] += p * p;
	}
    }
    for (j = 1; j <= *ndep; ++j) {
	double aver = varsum[j] / *ncas,
	    scal = sqrt(varss[j] / *ncas - aver * aver);
#ifdef DEBUG_spannel
	Rprintf("j= %d, scal = %g\n", j, scal);
#endif
	for (i = 0; i < *ncas; ++i)
	    X(i,j) = (X(i,j) - aver) / scal;
    }
    p = 1. / (double) (*ncas);
    for (i = 0; i < *ncas; ++i)
	prob[i] = p;
    *ierr = 0;
    p = (double) (*ndep);

/* ---- Repeat { ... up to `maxit' times ] */
    for(it = 0; it < *maxit; it++) {

	/* Cov[,] = weighted covariance of dat[,]  {weights = prob[]} */
	for (j = 0; j <= *ndep; ++j)
	    for (k = 0; k <= j; ++k)
		COV(k,j) = 0.;
	for (i = 0; i < *ncas; ++i) {
	    for (j = 0; j <= *ndep; ++j) {
		work[j] = X(i,j);
		double tempo = prob[i] * work[j];
		for (k = 0; k <= j; ++k)
		    COV(k,j) += tempo * work[k];
	    }
	}
	for (j = 0; j <= *ndep; ++j)
	    for (k = 0; k <= j; ++k)
		COV(j,k) = COV(k,j);

	deter = 1.;
	for (i = 0; i <= *ndep; ++i) {
	    cl_sweep(cov, ndep, &c__0, &i, &deter);
	    if (deter <= 0.) { *ierr = 2; return; }
	}
#ifdef DEBUG_spannel
	Rprintf(" it= %d; after all sweep()s : deter = %g\n", it, deter);
#endif
	dmax = 0.;
	for (i = 0; i < *ncas; ++i) {
	    double dist = -1.;
	    for (j = 0; j <= *ndep; ++j) {
		/* work(j) = - sum_{k=0}^p  dat(i,k) * cov(k,j) { = cov(j,k) },
		 * i.e., work_j = - X[i,] %*% COV[,j] */
		double w_j = 0.;
		for (k = 0; k <= *ndep; ++k)
		    w_j -= COV(j,k) * X(i,k);
		dist += w_j * X(i,j);
	    }
	    dstopt[i] = dist;/* Dist{opt}_i = -1 - t(X[i,]) %*% COV %*% X[i,] */
	    if (dmax < dist)
		dmax = dist;
	}/* for() : now	  dmax == max{ dstopt[i] } */

	if (dmax <= p + *eps) { /* _converged_ */
	    *maxit = it;
	    return;
	}
	/* else not yet converged */
	for (i = 0; i < *ncas; ++i)
	    prob[i] *= (dstopt[i] / p);
    }
    return;/* with it == *maxit and no convergence */

} /* spannel */
#undef X

/* This is currently also called from R : ../tests/sweep-ex.R
 * ==> keep pointer args !*/
void cl_sweep(double *cov, int *nord, int *ixlo, int *nel, double *deter)
{
    int i, j, cov_dim1 = *nord + 1;
    double temp = COV(*nel,*nel);

    *deter *= temp;
    if (*deter <= 0.) return; /* singular case -- signaled via *deter */
    if (*nord <= 1) {
	COV(1,1) = 1. / temp;
    }
    else { /* nord > 1 */
	for (i = *ixlo; i <= *nord; ++i) if (i != *nel) {
	    for (j = *ixlo; j <= i; ++j) if (j != *nel) {
		COV(j,i) = COV(i,j) - COV(i,*nel) * COV(*nel,j) / temp;
		COV(i,j) = COV(j,i);
	    }
	}
	COV(*nel,*nel) = 1.;
	for (i = *ixlo; i <= *nord; ++i) {
	    COV(*nel,i) = -COV(i,*nel) / temp;
	    COV(i,*nel) = COV(*nel,i);
	}
    }
    return;
} /* cl_sweep */
#undef COV
