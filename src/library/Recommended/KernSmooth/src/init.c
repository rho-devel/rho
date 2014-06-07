/*
 *  Part of R package KernSmooth
 *  Copyright (C) 2005-2007  B. D. Ripley
 *
 *  Unlimited use and distribution (see LICENCE).
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void F77_SUB(blkest)(double *x, double *y, int *n, int *q, int *qq,
		     int *nval, double *xj, double *yj, double *coef,
		     double *xmat, double *wk, double *qraux,
		     double *sigsqe, double *th22e, double *th24e);

void F77_SUB(cp)(double *x, double *y, int *n,
		 int *qq, int *nmax, double *rss, double *xj,
		 double *yj, double *coef, double *xmat, double *wk,
		 double *qraux, double *cpvals);

void F77_SUB(linbin)(double *x, int *n, double *a,
		     double *b, int *m, int *trun, double *gcounts);

void F77_SUB(lbtwod)(double *x, int *n, double *a1,
		     double *a2, double *b1, double *b2, int *m1,
		     int *m2, double *gcounts);

void F77_SUB(locpol)(double *xcounts, double *ycounts,
		     int *idrv, double *delta, double *hdisc, int *lvec,
		     int *indic, int *midpts, int *m, int *iq, double *fkap,
		     int *ipp, int *ippp, double *ss, double *tt,
		     double *smat, double *tvec, int *ipvt, double *curvest);

void F77_SUB(rlbin)(double *x, double *y, int *n,
		    double *a, double *b, int *m, int *trun, double *xcounts,
		    double *ycounts);

void F77_SUB(sdiag)(double *xcounts, double *delta,
		    double *hdisc, int *lvec, int *indic, int *midpts,
		    int *m, int *iq, double *fkap, int *ipp, int *ippp,
		    double *ss, double *smat, double *work, double *et,
		    int *ipvt, double *sd);

void F77_SUB(sstdg)(double *xcounts, double *delta,
		    double *hdisc, int *lvec, int *indic, int *midpts,
		    int *m, int *iq, double *fkap, int *ipp, int *ippp,
		    double *ss, double *uu, double *smat, double *umat,
		    double *work, double *det, int *ipvt, double *sstd);

static const R_FortranMethodDef FortEntries[] = {
    {"blkest", (DL_FUNC) &F77_SUB(blkest), 15},
    {"cp",     (DL_FUNC) &F77_SUB(cp),     13},
    {"linbin", (DL_FUNC) &F77_SUB(linbin),  7},
    {"lbtwod", (DL_FUNC) &F77_SUB(lbtwod),  9},
    {"locpol", (DL_FUNC) &F77_SUB(locpol), 19},
    {"rlbin",  (DL_FUNC) &F77_SUB(rlbin),   9},
    {"sdiag",  (DL_FUNC) &F77_SUB(sdiag),  17},
    {"sstdg",  (DL_FUNC) &F77_SUB(sstdg),  19},
    {NULL, NULL, 0}
};


void R_init_KernSmooth(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
