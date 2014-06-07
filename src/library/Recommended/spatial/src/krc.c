/*
 *  spatial/src/krc.c by W. N. Venables and B. D. Ripley  Copyright (C) 1994-2002
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 or 3 of the License
 *  (at your option).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 *
 */

#include <R.h>

#include "spatial.h"

#ifndef max
#  define max(a,b) ((a) > (b) ? (a) : (b))
#endif

/* defines  gls ls fmat frset alset valn pred prvar correlogram variogram */

static void dscale(double, double, double *, double *);
static void cholcov(double *, double *, double *, int, Sint *);
static void householder(double *, double *, double *, double *,
			int, int, Sint *);
static void chols(int, double *, double *, Sint *);
static void solv(double *, double *, int, double *, double *);
static void fsolv(double *, double *, int, double *);
static void bsolv(double *, double *, int, double *);
static void house_rhs(double *, double *, double *, int, int,
		      double *, double *);
static double val(double, double, double *, Sint *);

static double *alph1 = NULL;
static double xl1, xu1, yl1, yu1;

void
VR_alset(Sfloat *alph, Sint *nalph)
{
    int   i;

    if (alph1 != NULL)
	alph1 = Realloc(alph1, *nalph, double);
    else
	alph1 = Calloc(*nalph, double);
    for (i = 0; i < *nalph; ++i)
	alph1[i] = alph[i];
}

static double 
powi(double x, int i)
{
    int   j;
    double tmp;

    tmp = 1.0;
    for (j = 1; j <= i; ++j)
	tmp *= x;
    return tmp;
}

static void
cov(int n, double *d, int pred)
{
    int   i;
    double p, dd;
    int   id;
    double mm;

    mm = alph1[0];
    for (i = 0; i < n; ++i) {
	dd = sqrt(d[i]);
	id = (int) (dd / mm);
	p = dd / mm - id;
	if (pred && id == 0)
	    p = 1;		/* Preserve nugget effect on prediction */
	d[i] = (1 - p) * alph1[id + 1] + p * alph1[id + 2];
    }
}

void
VR_gls(double *x, double *y, double *z, Sint *n, Sint *np,
       Sint *npar, double *f, double *l, double *r, double *bz,
       double *wz, double *yy, double *w, Sint *ifail, double *l1f)
{
    double b[28];
    Sint  i, j;
    Sint  i1;
    double *w1, *w2;
    double *lf, *nu;

/* Generalized least squares (GLS) estimation of the parameters of
   spatial regression for observations (X,Y,Z) and regression
   matrix F. On output L contains the Cholesky factor of the
   covariance matrix of the observations, BZ the coefficients of
   the trend surface fitted by GLS, WZ the residuals of the
   observations for this fitted surface, W the product of the
   inverse of the Choleski factor of the covariance matrix and WZ,
   YY is the product of the inverse of the covariance matrix and
   WZ, and R the square root of the covariance matrix of BZ.  NU
   and B hold the operations performed in the Householder
   decomposition of the transformed regression problem i.e. the
   decomposition of L1F = LF (the product of the inverse of the
   Cholesky factor L and the F matrix).

   W1 and W2 are used as workspace. Form the Cholesky decomposition
   of the covariance matrix */


    lf = Calloc(*n * *npar, double);
    nu = Calloc(*n * *npar, double);
    w1 = Calloc(*n, double);
    w2 = Calloc(*n, double);
    cholcov(x, y, l, *n, ifail);
    if (*ifail > 0)
	return;
/* Find the product of the inverse Cholesky factor and F */
    for (i = 0; i < *npar; ++i) {
	i1 = i * (*n);
	for (j = 0; j < *n; ++j)
	    w1[j] = f[j + i1];
	fsolv(w2, w1, *n, l);
	for (j = 0; j < *n; ++j) {
	    l1f[j + i1] = w2[j];
	    lf[j + i1] = w2[j];
	}
    }
/* Perform Householder orthogonalization on LF and solve
   transformed regression problem by least squares. */

    householder(lf, nu, b, r, *n, *npar, ifail);
    if (*ifail > 0)
	return;
    fsolv(w1, z, *n, l);
    house_rhs(nu, b, r, *n, *npar, w1, bz);
    for (i = 0; i < *n; ++i)
	wz[i] = z[i] - val(x[i], y[i], bz, np);
    solv(yy, wz, *n, l, l);
    fsolv(w, wz, *n, l);
    Free(lf);
    Free(nu);
    Free(w1);
    Free(w2);
}

void
VR_ls(double *x, double *y, double *z, Sint *n, Sint *np,
      Sint *npar, double *f, double *r, double *bz, double *wz,
      Sint *ifail)
{
    double b[28];
    Sint  i, j, k;
    double *fw, *nu;

/*   Least squares estimation of parameters of spatial regression for
     observations (X,Y,Z) and regression matrix F. On output BZ
     contains the regression coefficients fitted by LS,
     WZ the residuals of the observations for this fitted surface
     and R the covariance matrix of BZ. NU and B hold the operations
     performed in the Householder decomposition of the regression problem
     i.e. the decomposition of F.
     Copy F matrix to FW for use in householder */

    fw = Calloc(*n * *npar, double);
    nu = Calloc(*n * *npar, double);
    k = 0;
    for (i = 1; i <= *npar; ++i)
	for (j = 1; j <= *n; ++j) {
	    fw[k] = f[k];
	    ++k;
	}
    householder(fw, nu, b, r, *n, *npar, ifail);
    if (*ifail > 0)
	return;
    house_rhs(nu, b, r, *n, *npar, z, bz);
    for (i = 0; i < *n; ++i)
	wz[i] = z[i] - val(x[i], y[i], bz, np);
    Free(fw);
    Free(nu);
}

/* -------------------------------------------------------------------- */
void
VR_fmat(double *f, double *x, double *y, Sint *n, Sint *np)
{
    int   i, j, k, k1;
    double *xs, *ys;

/*  FMAT evaluates the 'regression' matrix F (scaled to [-1,+1]x[-1,+1] to
    avoid excessively large or small values) */

    xs = Calloc(*n, double);
    ys = Calloc(*n, double);
    for (i = 0; i < *n; ++i)
	dscale(x[i], y[i], &xs[i], &ys[i]);
    k1 = 0;
    for (i = 0; i <= *np; ++i)
	for (j = 0; j <= *np - i; ++j)
	    for (k = 0; k < *n; ++k)
		f[k1++] = powi(xs[k], j) * powi(ys[k], i);
    Free(xs);
    Free(ys);
}

static void
cholcov(double *x, double *y, double *l, int n, Sint *ifail)
{
    int   i, j;
    static double *w;
    int   i1;
    double t1, t2;

/*   Finds the Cholesky factor of the covariance matrix */

    w = Calloc(n * (n + 1) / 2, double);
    i1 = 0;
    for (i = 0; i < n; ++i)
	for (j = 0; j <= i; ++j) {
	    t1 = x[i] - x[j];
	    t2 = y[i] - y[j];
	    w[i1++] = t1 * t1 + t2 * t2;
	}
    cov(n * (n + 1) / 2, w, 0);
    chols(n, w, l, ifail);
    Free(w);
}

/* ---------------------------------------------------------------------- */
/*   GENERAL PURPOSE MATRIX ROUTINES */

static void
householder(double *f, double *nu, double *b, double *r, int n,
	    int m, Sint *ifail)
{
    int   i, j, k;
    double c1, c2;
    int   i1, i2, k1, k2, k3, i3;

/*  Finds the Householder decomposition of the NxM matrix F, F is reduced
    to the matrix P' = [R' 0] where R is MxM and upper triangular.
    The operations performed are stored in NU and B.
    Perform Householder decomposition (ref. Golub and Van Loan 'matrix
    Computations' p.40) */

    *ifail = 0;
    for (k = 0; k < m; ++k) {
	k1 = k * n;
	k3 = k1 + k;
	c2 = fabs(f[k3]);
	for (i = k + 1; i < n; ++i)
	    c2 = max(c2, fabs(f[i + k1]));
	if (c2 < 1e-6) {
	    *ifail = k + 1;
	    return;
	}
	c1 = 0.0;
	for (i = k; i < n; ++i) {
	    k2 = k1 + i;
	    nu[k2] = f[k2] / c2;
	    c1 += nu[k2] * nu[k2];
	}
	c1 = sqrt(c1);
	b[k] = c1 * (c1 + fabs(nu[k3]));
	if (nu[k3] >= 0.0)
	    nu[k3] += c1;
	else
	    nu[k3] -= c1;
	i1 = (k + 1) * (k + 2) / 2;
	i2 = k + 1;
	for (i = k; i < m; ++i) {
	    i3 = i * n;
	    c1 = 0.0;
	    for (j = k; j < n; ++j)
		c1 += nu[k1 + j] * f[i3 + j];
	    c1 /= b[k];
	    r[i1 - 1] = f[i3 + k] - c1 * nu[k3];
	    for (j = k; j < n; ++j)
		f[i3 + j] -= c1 * nu[k1 + j];
	    i1 += i2;
	    ++i2;
	}
    }
}

static void
house_rhs(double *nu, double *b, double *r, int n, int m,
	  double *z, double *beta)
{
    int   i, k;
    double *w;
    int   k1;
    double sum;

/*  house_rhs is used in conjunction with householder. Once the Householder
    decomposition has been performed on the matrix F and the operations stored
    in NU and B the same operations can be performed on any rhs vector Z to
    give least squares estimates beta. */

    w = Calloc(n, double);
    for (i = 0; i < n; ++i)
	w[i] = z[i];
    for (k = 0; k < m; ++k) {
	sum = 0.0;
	k1 = k * n;
	for (i = k; i < n; ++i)
	    sum += nu[i + k1] * w[i];
	sum /= b[k];
	for (i = k; i < n; ++i)
	    w[i] -= sum * nu[i + k1];
    }
    bsolv(beta, w, m, r);
    Free(w);
}

static void
chols(int n, double *a, double *l, Sint *ifail)
{
    double eps = 1e-9;

    int   icol, irow, i, j, k, m;
    double w = 0.0; /* -Wall */
    int   i1;

/*  Forms Cholesky Decomposition of symmetric matrix (lower
    triangle stored as a vector A). L is stored in vector form
    to save space. Algorithm from
    M.J.R. Healy (1968) Applied Statistics  pp. 195-197.
 */

    /* Parameter adjustments to Fortran addressing */
    --l;
    --a;

    *ifail = 1;
    j = 1;
    k = 0;
    for (icol = 1; icol <= n; ++icol) {
	i1 = 0;
	for (irow = 1; irow <= icol; ++irow) {
	    ++k;
	    w = a[k];
	    m = j;
	    for (i = 1; i <= irow; ++i) {
		++i1;
		if (i == irow)
		    continue;
		if (fabs(l[i1]) < eps && fabs(l[m]) < eps)
		    ++m;
		else {
		    w -= l[i1] * l[m];
		    ++m;
		}
	    }
	    if (irow == icol)
		break;
	    if (l[i1] == 0.0)
		l[k] = 0.0;
	    else
		l[k] = w / l[i1];
	}
	if (fabs(w) < fabs(a[k] * eps))
	    l[k] = 0.0;
	else {
	    if (w <= 0.0)
		return;
	    l[k] = sqrt(w);
	}
	j += icol;
    }
    for (i = 1; i <= n; ++i) {
	j = i * (i + 1) / 2;
	if (l[j] == 0.0)
	    return;
    }
    *ifail = 0;
    return;
}

static void
fsolv(double *x, double *y, int n, double *l)
{
    int   i, j, i1;
    double sum;

/*  Solves Lx = y for x by forward substitution - L lower triangular
    and stored as a vector. */

    i1 = 0;
    for (i = 0; i < n; ++i) {
	x[i] = y[i];
	sum = 0.0;
	for (j = 0; j < i; ++j)
	    sum += x[j] * l[i1++];
	x[i] = (x[i] - sum) / l[i1++];
    }
}

static void
bsolv(double *x, double *y, int n, double *u)
{
    int   i, j, i1, ic;
    double sum;

/*  Solves Ux = y for x by backward substitution -
    U upper triangular and stored  as a vector.
 */

    i1 = n * (n + 1) / 2 - 1;
    for (i = n - 1; i >= 0; --i) {
	x[i] = y[i];
	sum = 0.0;
	ic = i1;
	for (j = i + 1; j < n; ++j) {
	    ic += j;
	    sum += x[j] * u[ic];
	}
	x[i] = (x[i] - sum) / u[i1];
	i1 -= i + 1;
    }
}

static void
solv(double *x, double *y, int n, double *l, double *u)
{
    double *w;

/*   Solves Ax = y where A = LU  */

    w = Calloc(n, double);
    fsolv(w, y, n, l);
    bsolv(x, w, n, u);
    Free(w);
}

void
VR_frset(Sfloat *xl, Sfloat *xu, Sfloat *yl, Sfloat *yu)
{
    xl1 = *xl;
    yl1 = *yl;
    xu1 = *xu;
    yu1 = *yu;
}

static void
dscale(double xo, double yo, double *xs, double *ys)
{
    double xl2, yl2;

/*   Scales (xo, yo) to (xs, ys) ( -1 < xs, ys < 1 ) */

    xl2 = (xu1 + xl1) / 2;
    yl2 = (yu1 + yl1) / 2;
    *xs = (xo - xl2) / (xu1 - xl2);
    *ys = (yo - yl2) / (yu1 - yl2);
}

/* ----------------------------------------------------- */
/*   VARIANCE/COVARIANCE,PREDICTION AND STANDARD ERROR ROUTINES */

static double
val(double xp, double yp, double *beta, Sint *np)
{
    int   i, j, i1;
    double xs, ys, sum;

/*   Evaluates the fitted surface at the point (XP,YP) for coeffs BETA */

    dscale(xp, yp, &xs, &ys);
    sum = 0.0;
    i1 = 0;
    for (i = 0; i <= *np; ++i)
	for (j = 0; j <= *np - i; ++j)
	    sum += beta[i1++] * powi(xs, j) * powi(ys, i);
    return sum;
}

void
VR_valn(double *z, double *x, double *y, Sint *n, double *beta, Sint *np)
{
    int   i, j, i1;
    int   it;
    double xp, yp, xs, ys, sum;

/*   Evaluates the fitted surface at the point (XP,YP) for coeffs BETA */

    for (it = 0; it < *n; ++it) {
	xp = x[it];
	yp = y[it];
	dscale(xp, yp, &xs, &ys);
	sum = 0.0;
	i1 = 0;
	for (i = 0; i <= *np; ++i)
	    for (j = 0; j <= *np - i; ++j)
		sum += beta[i1++] * powi(xs, j) * powi(ys, i);
	z[it] = sum;
    }
}

void
VR_krpred(double *z, double *xs, double *ys, double *x, double *y,
	  Sint *npt, Sint *n, double *yy)
{
    double *d;
    int   i;
    double t1, t2;
    int   it;
    double xp, yp;
    double sum;

/*   Gives value of interpolator at the point (XP,YP) */

    d = Calloc(*n, double);
    for (it = 0; it < *npt; ++it) {
	xp = xs[it];
	yp = ys[it];
	sum = 0.0;
	for (i = 0; i < *n; ++i) {
	    t1 = x[i] - xp;
	    t2 = y[i] - yp;
	    d[i] = t1 * t1 + t2 * t2;
	}
	cov(*n, d, 1);
	for (i = 0; i < *n; ++i) {
	    sum += yy[i] * d[i];
	}
	z[it] = sum;
    }
    Free(d);
}

void
VR_prvar(double *z, double *xp, double *yp, Sint *npt,
	 double *x, double *y, double *l, double *r, Sint *n,
	 Sint *np, Sint *npar, double *l1f)
{
    int   i, j, k, i1, k1, it;

    double *w1, *w2;
    double xs, ys, xd, yd;
    double sum, sum1;

/*   Evaluates prediction variance at (XP,YP) */

    w1 = Calloc(*n, double);
    w2 = Calloc(*n, double);
    for (it = 0; it < *npt; ++it) {
	for (i = 0; i < *n; ++i) {
	    xd = x[i] - xp[it];
	    yd = y[i] - yp[it];
	    w1[i] = xd * xd + yd * yd;
	}
	cov(*n, w1, 1);
	fsolv(w2, w1, *n, l);
	sum1 = 0.0;
	for (i = 0; i < *n; ++i)
	    sum1 += w2[i] * w2[i];
	sum1 = alph1[1] - sum1;
	dscale(xp[it], yp[it], &xs, &ys);
	k1 = 0;
	i1 = 0;
	for (i = 0; i <= *np; ++i)
	    for (j = 0; j <= *np - i; ++j) {
		++i1;
		w1[i1 - 1] = powi(xs, j) * powi(ys, i);
		for (k = 0; k < *n; ++k)
		    w1[i1 - 1] -= l1f[k1++] * w2[k];
	    }
	fsolv(w2, w1, *npar, r);
	sum = 0.0;
	for (i = 0; i < *npar; ++i)
	    sum += w2[i] * w2[i];
	z[it] = sum1 + sum;
    }
    Free(w1);
    Free(w2);
}


/* --------------------------------------------------------------------- */
/*  CORRELOGRAM AND VARIOGRAM ROUTINES */

void
VR_correlogram(Sfloat *xp, Sfloat *yp, Sint *nint, double *x,
	       double *y, double *z, Sint *n, Sint *cnt)
{
    double xd, yd, d, sc, zb, xm, var, sum;
    int   i, j, i1, ibin;
    double *cp;

    int  *ncp;

/* Correlogram for the observations (X,Y,Z). CP holds the empirical correlation
   for the NINT intervals based on the NCP observations in each interval.
   XM defines the upper limit on the x axis. */

    cp = Calloc(*nint + 1, double);
    ncp = Calloc(*nint + 1, int);
    zb = 0.0;
    for (i = 0; i < *n; ++i) {
	zb += z[i];
    }
    zb /= *n;
    for (i = 0; i < *nint; ++i) {
	ncp[i] = 0;
	cp[i] = 0.0;
    }
    xm = 0.0;
    for (i = 0; i < *n; ++i) {
	for (j = 0; j < i; ++j) {
	    xd = x[i] - x[j];
	    yd = y[i] - y[j];
	    d = xd * xd + yd * yd;
	    xm = max(xm, d);
	}
    }
    sc = (*nint - 1) / sqrt(xm);
    for (i = 0; i < *n; ++i) {
	for (j = 0; j <= i; ++j) {
	    xd = x[i] - x[j];
	    yd = y[i] - y[j];
	    d = sqrt(xd * xd + yd * yd);
	    ibin = (int) (sc * d);
	    ++ncp[ibin];
	    cp[ibin] += (z[i] - zb) * (z[j] - zb);
	}
    }
    sum = 0.0;
    for (i = 0; i < *n; ++i) {
	xd = z[i] - zb;
	sum += xd * xd;
    }
    var = sum / *n;
/*  Only return values for fairly reliable intervals */
    i1 = 0;
    for (i = 0; i < *nint; ++i) {
	if (ncp[i] > 5) {
	    xp[i1] = i / sc;
	    yp[i1] = cp[i] / (var * ncp[i]);
	    cnt[i1] = ncp[i];
	    i1++;
	}
    }
    *nint = i1;
    Free(cp);
    Free(ncp);
}

void
VR_variogram(Sfloat *xp, Sfloat *yp, Sint *nint, double *x,
	     double *y, double *z, Sint *n, Sint *cnt)
{
    double xd, yd, d, sc, xm;

    int   ibin, i, j, i1;
    double *sv;
    int  *nsv;

/*  Variogram for the observations (X,Y,Z). SV holds the empirical
    semi-variance for the NINT intervals based on the NSV observations
    in each interval.
 */

    sv = Calloc(*nint + 1, double);
    nsv = Calloc(*nint + 1, int);
    for (i = 0; i < *nint; ++i) {
	nsv[i] = 0;
	sv[i] = 0.0;
    }
    xm = 0.0;
    for (i = 0; i < *n; ++i) {
	for (j = 0; j < i; ++j) {
	    xd = x[i] - x[j];
	    yd = y[i] - y[j];
	    d = xd * xd + yd * yd;
	    xm = max(xm, d);
	}
    }
    sc = (*nint - 1) / sqrt(xm);
    for (i = 0; i < *n; ++i) {
	for (j = 0; j < i; ++j) {
	    xd = x[i] - x[j];
	    yd = y[i] - y[j];
	    d = sqrt(xd * xd + yd * yd);
	    ibin = (int) (sc * d);
	    ++nsv[ibin];
	    xd = z[i] - z[j];
	    sv[ibin] += xd * xd;
	}
    }
    i1 = 0;
    for (i = 0; i < *nint; ++i) {
	if (nsv[i] > 5) {
	    xp[i1] = i / sc;
	    yp[i1] = sv[i] / (2 * nsv[i]);
	    cnt[i1] = nsv[i];
	    i1++;
	}
    }
    *nint = i1;
    Free(sv);
    Free(nsv);
}
