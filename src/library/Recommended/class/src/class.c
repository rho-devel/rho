/*
 *  class/src/class.c by W. N. Venables and B. D. Ripley  Copyright (C) 1994-2002
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
#include <math.h>
#include <float.h>

#define EPS 1e-4		/* relative test of equality of distances */

#define RANDIN  GetRNGstate()
#define RANDOUT PutRNGstate()
#define UNIF unif_rand()

void
VR_knn1(Sint *pntr, Sint *pnte, Sint *p, double *train, Sint *class,
	double *test, Sint *res, Sint *votes, Sint *nc, double *dists)
{
    int   npat, index, i, j, k, ntr = *pntr, nte = *pnte, nind=0, ntie, *ind;
    double dm, dist, tmp;

    RANDIN;
    ind = Calloc(ntr, int);
    for (npat = 0; npat < nte; npat++) {
	dm = DOUBLE_XMAX;
	for (j = 0; j < ntr; j++) {
	    dist = 0.0;
	    for (k = 0; k < *p; k++) {
		tmp = test[npat + k * nte] - train[j + k * ntr];
		dist += tmp * tmp;
	    }
	    if (dist <= dm * (1 + EPS)) {
		if (dist < dm * (1 - EPS))
		    nind = 0;
		else
		    nind++;
		dm = dist;
		ind[nind] = j;
	    }
	}
	for (i = 1; i <= *nc; i++)
	    votes[i] = 0;
/* nind is the number of tied minima, minus one */
	if (nind == 0)
	    index = class[ind[0]];
	else {
	    for (i = 0; i <= nind; i++)
		votes[class[ind[i]]]++;
	    j = votes[1];
/*
     This uses 'reservoir sampling' to choose amongst ties at random
     on a single pass.

 */
	    index = 1;
	    ntie = 1;
	    for (i = 2; i <= *nc; i++)
		if (votes[i] > j) {
		    ntie = 1;
		    index = i;
		    j = votes[i];
		} else if (votes[i] == j) {
		    if (++ntie * UNIF < 1.0)
			index = i;
		}
	}
	res[npat] = index;
	dists[npat] = dm;
    }
    RANDOUT;
    Free(ind);
}


#define MAX_TIES 1000
/* Not worth doing this dynamically -- limits k + # ties + fence, in fact */


void
VR_knn(Sint *kin, Sint *lin, Sint *pntr, Sint *pnte, Sint *p,
       double *train, Sint *class, double *test, Sint *res, double *pr,
       Sint *votes, Sint *nc, Sint *cv, Sint *use_all)
{
    int   i, index, j, k, k1, kinit = *kin, kn, l = *lin, mm, npat, ntie,
          ntr = *pntr, nte = *pnte, extras;
    int   pos[MAX_TIES], nclass[MAX_TIES];
    int   j1, j2, needed, t;
    double dist, tmp, nndist[MAX_TIES];

    RANDIN;
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
 */

    for (npat = 0; npat < nte; npat++) {
	kn = kinit;
	for (k = 0; k < kn; k++)
	    nndist[k] = 0.99 * DOUBLE_XMAX;
	for (j = 0; j < ntr; j++) {
	    if ((*cv > 0) && (j == npat))
		continue;
	    dist = 0.0;
	    for (k = 0; k < *p; k++) {
		tmp = test[npat + k * nte] - train[j + k * ntr];
		dist += tmp * tmp;
	    }
/* Use 'fuzz' since distance computed could depend on order of coordinates */
	    if (dist <= nndist[kinit - 1] * (1 + EPS))
		for (k = 0; k <= kn; k++)
		    if (dist < nndist[k]) {
			for (k1 = kn; k1 > k; k1--) {
			    nndist[k1] = nndist[k1 - 1];
			    pos[k1] = pos[k1 - 1];
			}
			nndist[k] = dist;
			pos[k] = j;
/* Keep an extra distance if the largest current one ties with current kth */
			if (nndist[kn] <= nndist[kinit - 1])
			    if (++kn == MAX_TIES - 1)
				error("too many ties in knn");
			break;
		    }
	    nndist[kn] = 0.99 * DOUBLE_XMAX;
	}

	for (j = 0; j <= *nc; j++)
	    votes[j] = 0;
	if (*use_all) {
	    for (j = 0; j < kinit; j++)
		votes[class[pos[j]]]++;
	    extras = 0;
	    for (j = kinit; j < kn; j++) {
		if (nndist[j] > nndist[kinit - 1] * (1 + EPS))
		    break;
		extras++;
		votes[class[pos[j]]]++;
	    }
	} else { /* break ties at random */
	    extras = 0;
	    for (j = 0; j < kinit; j++) {
		if (nndist[j] >= nndist[kinit - 1] * (1 - EPS))
		    break;
		votes[class[pos[j]]]++;
	    }
	    j1 = j;
	    if (j1 == kinit - 1) { /* no ties for largest */
		votes[class[pos[j1]]]++;
	    } else {
/* Use reservoir sampling to choose amongst the tied distances */
		j1 = j;
		needed = kinit - j1;
		for (j = 0; j < needed; j++)
		    nclass[j] = class[pos[j1 + j]];
		t = needed;
		for (j = j1 + needed; j < kn; j++) {
		    if (nndist[j] > nndist[kinit - 1] * (1 + EPS))
			break;
		    if (++t * UNIF < needed) {
			j2 = j1 + (int) (UNIF * needed);
			nclass[j2] = class[pos[j]];
		    }
		}
		for (j = 0; j < needed; j++)
		    votes[nclass[j]]++;
	    }
	}

/* Use reservoir sampling to choose amongst the tied votes */
	ntie = 1;
	if (l > 0)
	    mm = l - 1 + extras;
	else
	    mm = 0;
	index = 0;
	for (i = 1; i <= *nc; i++)
	    if (votes[i] > mm) {
		ntie = 1;
		index = i;
		mm = votes[i];
	    } else if (votes[i] == mm && votes[i] >= l) {
		if (++ntie * UNIF < 1.0)
		    index = i;
	    }
	res[npat] = index;
	pr[npat] = (double) mm / (kinit + extras);
    }
    RANDOUT;
}


#define min9(a,b) ((a < b)?a:b)

void
VR_olvq(double *alpha, Sint *pn, Sint *p, double *x, Sint *cl,
	Sint *pncodes, double *xc, Sint *clc, Sint *niter,
	Sint *iters)
{
    int   index=0, iter, j, k, n = *pn, ncodes = *pncodes, npat, s;
    double *al;
    double dist, dm, tmp;

    al = Calloc(ncodes, double);
    for (j = 0; j < ncodes; j++)
	al[j] = *alpha;
    for (iter = 0; iter < *niter; iter++) {
	npat = iters[iter];
	dm = DOUBLE_XMAX;
	for (j = 0; j < ncodes; j++) {
	    dist = 0.0;
	    for (k = 0; k < *p; k++) {
		tmp = x[npat + k * n] - xc[j + k * ncodes];
		dist += tmp * tmp;
	    }
	    if (dist < dm) {
		dm = dist;
		index = j;
	    }
	}
	s = 2 * (clc[index] == cl[npat]) - 1;
	for (k = 0; k < *p; k++)
	    xc[index + k * ncodes] += s * al[index] *
		(x[npat + k * n] - xc[index + k * ncodes]);
	al[index] = min9(*alpha, al[index] / (1 + s * al[index]));
    }
    Free(al);
}

void
VR_lvq1(double *alpha, Sint *pn, Sint *p, double *x, Sint *cl,
	Sint *pncodes, double *xc, Sint *clc, Sint *niter,
	Sint *iters)
{
    int   index = 0, iter, j, k, n = *pn, ncodes = *pncodes, npat, s;
    double alpha_t;
    double dist, dm, tmp;

    for (iter = 0; iter < *niter; iter++) {
	npat = iters[iter];
	alpha_t = *alpha * (*niter - iter) / (double) *niter;
	dm = DOUBLE_XMAX;
	for (j = 0; j < ncodes; j++) {
	    dist = 0.0;
	    for (k = 0; k < *p; k++) {
		tmp = x[npat + k * n] - xc[j + k * ncodes];
		dist += tmp * tmp;
	    }
	    if (dist < dm) {
		dm = dist;
		index = j;
	    }
	}
	s = 2 * (clc[index] == cl[npat]) - 1;
	for (k = 0; k < *p; k++)
	    xc[index + k * ncodes] += s * alpha_t *
		(x[npat + k * n] - xc[index + k * ncodes]);
    }
}

void
VR_lvq2(double *alpha, double *win, Sint *pn, Sint *p, double *x,
	Sint *cl, Sint *pncodes, double *xc, Sint *clc,
	Sint *niter, Sint *iters)
{
    int index = 0, iter, j, k, n = *pn, ncodes = *pncodes, nindex = 0,
	npat, ntmp;
    double alpha_t;
    double dist, dm, ndm, tmp;

    for (iter = 0; iter < *niter; iter++) {
	npat = iters[iter];
	alpha_t = *alpha * (*niter - iter) / (double) *niter;
	ndm = dm = DOUBLE_XMAX;

	/* Find two nearest codebook vectors */
	for (j = 0; j < ncodes; j++) {
	    dist = 0.0;
	    for (k = 0; k < *p; k++) {
		tmp = x[npat + k * n] - xc[j + k * ncodes];
		dist += tmp * tmp;
	    }
	    if (dist < dm) {
		ndm = dm;
		nindex = index;
		dm = dist;
		index = j;
	    } else if (dist < ndm) {
		ndm = dist;
		nindex = j;
	    }
	}
	if (clc[index] != clc[nindex]) {
	    if (((clc[index] == cl[npat]) || (clc[nindex] == cl[npat]))
		&& dm / ndm > (1 - *win) / (1 + *win)) {
		if (clc[nindex] == cl[npat]) {
		    ntmp = index;
		    index = nindex;
		    nindex = ntmp;
		}
		for (k = 0; k < *p; k++) {
		    xc[index + k * ncodes] += alpha_t *
			(x[npat + k * n] - xc[index + k * ncodes]);
		    xc[nindex + k * ncodes] -= alpha_t *
			(x[npat + k * n] - xc[nindex + k * ncodes]);
		}
	    }
	}
    }
}

void
VR_lvq3(double *alpha, double *win, double *epsilon, Sint *pn, Sint *p,
	double *x, Sint *cl, Sint *pncodes, double *xc, Sint *clc,
	Sint *niter, Sint *iters)
{
    int index = 0, iter, j, k, n = *pn, ncodes = *pncodes, nindex = 0,
	npat, ntmp;
    double alpha_t;
    double dist, dm, ndm, tmp;

    for (iter = 0; iter < *niter; iter++) {
	npat = iters[iter];
	alpha_t = *alpha * (*niter - iter) / (double) *niter;
	ndm = dm = DOUBLE_XMAX;
	/* Find two nearest codebook vectors */
	for (j = 0; j < ncodes; j++) {
	    dist = 0.0;
	    for (k = 0; k < *p; k++) {
		tmp = x[npat + k * n] - xc[j + k * ncodes];
		dist += tmp * tmp;
	    }
	    if (dist < dm) {
		ndm = dm;
		nindex = index;
		dm = dist;
		index = j;
	    } else if (dist < ndm) {
		ndm = dist;
		nindex = j;
	    }
	}
	if (clc[index] != clc[nindex]) {
	    if (((clc[index] == cl[npat]) || (clc[nindex] == cl[npat]))
		&& dm / ndm > (1 - *win) / (1 + *win)) {
		if (clc[nindex] == cl[npat]) {
		    ntmp = index;
		    index = nindex;
		    nindex = ntmp;
		}
		for (k = 0; k < *p; k++) {
		    xc[index + k * ncodes] += alpha_t *
			(x[npat + k * n] - xc[index + k * ncodes]);
		    xc[nindex + k * ncodes] -= alpha_t *
			(x[npat + k * n] - xc[nindex + k * ncodes]);
		}
	    }
	} else if (clc[index] == cl[npat]) {
	    for (k = 0; k < *p; k++) {
		xc[index + k * ncodes] += *epsilon * alpha_t *
		    (x[npat + k * n] - xc[index + k * ncodes]);
		xc[nindex + k * ncodes] += *epsilon * alpha_t *
		    (x[npat + k * n] - xc[nindex + k * ncodes]);
	    }
	}
    }
}

void
VR_onlineSOM(double *data, double *codes, double *nhbrdist,
	     double *alpha, double *radii,
	     Sint *pn, Sint *pp, Sint *pncodes, Sint *rlen)
{
    int n = *pn, p = *pp, ncodes = *pncodes;
    int i, j, k, nearest = 0 /* -Wall */, nind;
    double dm, dist, tmp;
    unsigned int cd; /* avoid spurious warning from gcc pre-4.3.0 */

    RANDIN;
    for (k = 0; k < *rlen; k++) {
	/* pick a random data point */
	i = (int)(n * UNIF);
	/* find the nearest code 'near' */
	nind = 0; dm = DOUBLE_XMAX;
	for (cd = 0; cd < ncodes; cd++) {
	    dist = 0.0;
	    for (j = 0; j < p; j++) {
		tmp = data[i + j*n] - codes[cd + j*ncodes];
		dist += tmp * tmp;
	    }
	    if (dist <= dm * (1 + EPS)) {
		if (dist < dm * (1 - EPS)) {
		    nind = 0;
		    nearest = cd;
		} else {
		    if(++nind * UNIF < 1.0) nearest = cd;
		}
		dm = dist;
	    }
	    /* update all codes within radii[k] of 'nearest' */
	    for (cd = 0; cd < ncodes; cd++) {
		if(nhbrdist[cd + ncodes*nearest] > radii[k]) continue;
		for(j = 0; j < p; j++)
		    codes[cd + j*ncodes] += alpha[k] *
			(data[i + j*n] - codes[cd + j*ncodes]);
	    }
	}
    }
    RANDOUT;
}

#include "R_ext/Rdynload.h"

static const R_CMethodDef CEntries[] = {
    {"VR_knn", (DL_FUNC) &VR_knn, 14},
    {"VR_knn1", (DL_FUNC) &VR_knn1, 10},
    {"VR_lvq1", (DL_FUNC) &VR_lvq1, 10},
    {"VR_lvq2", (DL_FUNC) &VR_lvq2, 11},
    {"VR_lvq3", (DL_FUNC) &VR_lvq3, 12},
    {"VR_olvq", (DL_FUNC) &VR_olvq, 10},
    {"VR_onlineSOM", (DL_FUNC) &VR_onlineSOM, 9},
    {NULL, NULL, 0}
};

void R_init_class(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
}
