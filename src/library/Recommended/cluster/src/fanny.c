/* FANNY : program for Fuzzy cluster ANalysis */

/* was $Id$
 * fanny.f -- translated by f2c (version 20020621).
 * and treated by  f2c-clean v 1.10, and manually by Martin Maechler
 */

#include <Rmath.h>
#include <R_ext/Print.h>/* for diagnostics */

#include "cluster.h"
/* dysta3() is in cluster.h ! */

static void
fuzzy(int nn, int k, double *p,
      double *dp, double *pt, double *dss, double *esp,
      double *ef, double *obj,
      double r, double tol, int *nit, int trace_lev);

static void
caddy(int nn, int k, double *p, int *ktrue,
      int *nfuzz, int *ncluv, double *rdraw, int trace_lev);

static void
fygur(int kk, int nn,
      int *ncluv, int *nsend, int *nelem, int *negbr,
      double *syl, double *srank, double *avsyl, double *ttsyl,
      double *dss, double *s, double *sylinf);


void cl_fanny(int *nn,  /* = number of objects */
	      int *jpp, /* = number of variables for clustering */
	      int *kk,  /* = number of clusters */
	      double *x, double *dss, int *jdyss, double *valmd,
	      int *jtmd, int *ndyst, int *nsend, int *nelem,
	      int *negbr, double *syl, double *p, double *dp,
	      double *pt, int *nfuzz, double *esp, double *ef,
	      double *dvec, double *ttsyl,
	      double *obj, /* input/output;  see fuzzy() below */
	      int *ncluv, double *sylinf,
	      double *r, double *tol, int *maxit)
{
    int ktrue, trace_lev = (int) obj[1];
    Rboolean all_stats = (obj[0] == 0.);/* TODO: consider *not* doing caddy() */

    if (*jdyss != 1) { /* compute dissimilarities from data */
	int jhalt = 0;
	dysta3(nn, jpp, x, dss, ndyst, jtmd, valmd, &jhalt);
	if (jhalt) {
	    *jdyss = -1; return;
	}
    }

    fuzzy(*nn, *kk, p, dp, pt, dss, esp,
	  ef, obj, *r, *tol, maxit, trace_lev);

    caddy(*nn, *kk, p, /* -> */ &ktrue, nfuzz, ncluv, pt, trace_lev);

    obj[0] = (double) ktrue;

    /*	Compute "silhouette": */
    if (all_stats && 2 <= ktrue && ktrue < *nn) {
	int i, nhalf = *nn * (*nn - 1) / 2;
	double s = 0.; /* s := max( dss[i,j] ) */
	for(i = 0; i < nhalf; i++)
	    if (s < dss[i])
		s = dss[i];
	fygur(ktrue, *nn, ncluv, nsend, nelem,
	      negbr, syl, dvec, pt, ttsyl, dss, &s, sylinf);
    }
    return;
} /* cl_fanny */


void dysta3(int *nn, int *p, double *x, double *dys,
	    int *ndyst, int *jtmd, double *valmd, int *jhalt)
{
    int k, l, nlk, x_d = *nn;

    nlk = 0;
    for (l = 0; l < (*nn - 1); ++l) {
	for (k = l + 1; k < *nn; ++k, ++nlk) {
	    double clk = 0.;
	    int j, jj, npres = 0;
	    for (j = 0, jj = 0; j < *p; j++, jj+=x_d) {
		double d;
		if (jtmd[j] < 0) {
		    if (x[l + jj] == valmd[j] ||
			x[k + jj] == valmd[j])

			continue; /* next j */
		}
		++npres;
		d = x[l + jj] - x[k + jj];
		if (*ndyst != 2) /* 1 or 3 */
		    clk += d * d;
		else /* if (*ndyst == 2) */
		    clk += fabs(d);
	    }
	    if (npres == 0) {
		dys[nlk] = -1.;	*jhalt = 1;
	    } else {
		clk *= (*p) / (double) npres;
		dys[nlk] = (*ndyst == 1) ? sqrt(clk) : /*ndyst = 2 or 3 */ clk;
	    }
	}
    }
} /* dysta3 */


static
void fuzzy(int nn, int k, double *p,
	   double *dp, double *pt, double *dss, double *esp, double *ef,
	   double *obj,/* of length 4;
			* in : (cluster_only, trace_lev, compute_p, 0)
			* out: (ktrue	    , cryt, PC ("dunn"), normalized_PC)
			*/
	   double r,  /* the exponent, > 1. -- was fixed to 2 originally */
	   double tol,/* the precision for the iterations */
	   int *nit,   /* the maximal number of iterations --
			  originally fixed to 500 */
	   int trace_lev)
{
    double dt, xx, ddd, crt, reen, cryt;
    int p_d = nn, dp_d = nn;
    int i, j, m, mi, it;
    Rboolean converged = FALSE, compute_p = (int)obj[2];

    if(trace_lev)
	Rprintf("fanny()'s fuzzy(n = %d, k = %d):\n", nn, k);

    if(compute_p) {
	/* Compute initial fuzzy clustering, i.e. membership matrix  p[,] */
	int nd, ndk;
	double p0 = 0.1 / (k - 1);
	for (m = 0; m < nn; ++m)
	    for (j = 0; j < k; ++j)
		p[m + j * p_d] = p0;

	ndk = nn / k;
	nd = ndk;
	j = 0;
	for (m = 0; m < nn; ++m) {
	    int jj;
	    p[m + j * p_d] = 0.9;
	    if (m+1 >= nd) {
		++j;
		if (j+1 == k) /* reset */
		    nd = nn;
		else nd += ndk;
	    }
	    for (jj = 0; jj < k; ++jj)
		p[m + jj * p_d] = pow(p[m + jj * p_d], r);
	}
    }
    else { /* p[,]  already contains memberships */

	for (m = 0; m < nn; ++m)
	    for (j = 0; j < k; ++j)
		p[m + j * p_d] = pow(p[m + j * p_d], r);
    }

/*     initial criterion value */

    cryt = 0.;
    for (j = 0; j < k; ++j) {
	esp[j] = 0.;
	ef[j] = 0.;
	for (m = 0; m < nn; ++m) {
	    esp[j] += p[m + j * p_d];
	    for (i = 0; i < nn; ++i) {
		if (i != m) {
		    mi = imin2(m,i);
		    mi = mi * nn - (mi + 1) * (mi + 2) / 2 + imax2(m,i);
		    dp[m + j * dp_d] += p[i + j * p_d] * dss[mi];
		    ef[j] += p[i + j * p_d] * p[m + j * p_d] * dss[mi];
		}
	    }
	}
	cryt += ef[j] / (esp[j] * 2.);
    }
    crt = cryt;

    if(trace_lev) {
	Rprintf("fuzzy(): initial obj = %g\n", cryt);
	if(trace_lev >= 2) {
	    Rprintf("	    ef[]= (");
	    for(j=0; j < k; j++) Rprintf(" %g%s", ef[j], ((j < k-1)? "," : ")\n"));
	    Rprintf("	    esp[]= (");
	    for(j=0; j < k; j++) Rprintf(" %g%s",esp[j], ((j < k-1)? "," : ")\n"));
	}
    }

    reen = 1. / (r - 1.);

    it = 0;
    while(++it <= *nit) { /*  . . . . .  iterations . . . . . . . . . . . . . */

	for(m = 0; m < nn; m++) {
	    /* the new membership coefficients of the objects are calculated,
	       and the resulting value of the criterion is computed. */
	    dt = 0.;
	    for (j = 0; j < k; ++j) {
		pt[j] = pow(esp[j] / (dp[m + j * dp_d] - ef[j] / (2 * esp[j])),
			    reen);
		dt += pt[j];
	    }
	    xx = 0.;
	    for (j = 0; j < k; ++j) {
		pt[j] /= dt;
		if (pt[j] < 0.)
		    xx += pt[j];
	    }
	    /* now: sum_j (pt[j]) == 1;	 xx := sum_{pt[j] < 0} pt[j] */
	    for (j = 0; j < k; ++j) {
		double d_mj;
		pt[j] = (pt[j] > 0.) ? pow(pt[j] / (1 - xx), r) : 0.;
		d_mj = pt[j] - p[m + j * p_d];
		esp[j] += d_mj;
		for (i = 0; i < nn; ++i) {
		    if (i != m) {
			mi = imin2(m,i);
			mi = mi * nn - (mi + 1) * (mi + 2) / 2 + imax2(m,i);
			ddd = d_mj * dss[mi];
			dp[i + j * dp_d] += ddd;
			ef[j] += p[i + j * p_d] * 2. * ddd;
		    }
		}
		p[m + j * p_d] = pt[j];
	    }

	    if(trace_lev >= 3) {
		Rprintf(" pt[m= %d, *]: ",m);
		for (j = 0; j < k; ++j)
		    Rprintf(" %g%s", pt[j], ((j < k-1)? "," : "\n"));
	    }
	}

	/* m == nn */
	cryt = 0.;
	for (j = 0; j < k; ++j)
	    cryt += ef[j] / (esp[j] * 2.);

	if(trace_lev >= 2) Rprintf("  m == n:  obj = %#20.14g", cryt);

	/* Convergence check */
	if((converged = (fabs(cryt - crt) <= tol * cryt)))
	    break;

	if(trace_lev >= 2) Rprintf("  not converged: it = %d\n", it);
	crt = cryt;

    } /* while */

    *nit = (converged)? it : -1;

    if(trace_lev) {
	Rprintf("%s%sonverged after %d iterations,  obj = %#20.*g\n",
		trace_lev >=2 ? "\n" : "", (converged) ? "C" : "NOT c",
		it, (int)((trace_lev >= 2)? 20 : 7), cryt);
    }

    /* obj[0] = (double) it; << no longer; return it via *nit ! */
    obj[1] = cryt;
    /* PC (partition coefficient), "non-fuzzyness index" of libert is computed
     * C = 1/n sum_{i,j} u_{i,j} ^ r fulfills
     *	    1 >= C >= sum_j (1/k)^r = k * k^-r = k^(1-r)
     * ==> normalization  (C - k^(1-r)) / (1 - k^(1-r)) = (k^(r-1) * C - 1) / (k^(r-1) - 1)
     */
    for (j = 0, crt = 0.; j < k; ++j)
	crt += esp[j];
    crt /= nn;
    obj[2] = crt; /* the PC */
    xx = pow((double)k, r - 1.);
    obj[3] = (xx * crt - 1.) / (xx - 1.);
    /* Note however, that for r != 2,  MM rather prefers to use
     * the "original definition"    C = 1/n sum_{i,j} u_{i,j} ^ 2, and its normalization */

    /* p[m,j] := (u_{m,j} ^ r) ^{1/r} == u_{m,j} : */
    xx = 1. / r;
    for (m = 0; m < nn; ++m)
	for (j = 0; j < k; ++j)
	    p[m + j * p_d] = pow(p[m + j * p_d], xx);

} /* fuzzy */


static
void caddy(int nn, int k, double *p, int *ktrue,
	   int *nfuzz, int *ncluv, double *rdraw, int trace_lev)
{
    Rboolean stay;
    int i, m, ktry, nbest;
    double pbest;

    if(trace_lev)
	Rprintf("fanny()'s caddy(*, k = %d):\n", k);

    pbest = p[0];
    nbest = 1;
    for (i = 1; i < k; ++i) {
	if (pbest < p[i * nn]) {
	    pbest = p[i * nn];
	    nbest = i+1;
	}
    }
    nfuzz[0] = nbest;
    ncluv[0] = 1;
    *ktrue = 1;
    for (m = 1; m < nn; ++m) {
	pbest = p[m];
	nbest = 1;
	for (i = 1; i < k; ++i) {
	    if (pbest < p[m + i * nn]) {
		pbest = p[m + i * nn];
		nbest = i+1;
	    }
	}
	stay = FALSE;
	for (ktry = 0; ktry < *ktrue; ++ktry) {
	    if (nfuzz[ktry] == nbest) {
		stay = TRUE;
		ncluv[m] = ktry+1;
		break;
	    }
	}
	if (! stay) {
	    nfuzz[*ktrue] = nbest;
	    (*ktrue)++;
	    ncluv[m] = *ktrue;
	}
    }

    if(trace_lev)
	Rprintf(" -> k_true (crisp) = %d", *ktrue);
    if (*ktrue < k) {
	int kwalk, kleft;
	if(trace_lev)
	    Rprintf(" < k (= %d) !!\n", k);

	for (kwalk = *ktrue; kwalk < k; ++kwalk) {
	    for (kleft = 1; kleft <= k; ++kleft) {
		stay = FALSE;
		for (ktry = 0; ktry < kwalk; ++ktry) {
		    if (nfuzz[ktry] == kleft) {
			stay = TRUE;
			break;
		    }
		}
		if (! stay) {
		    nfuzz[kwalk] = kleft;
		    break;
		}
	    }
	}
    } else if(trace_lev) Rprintf("\n");

    for (m = 0; m < nn; ++m) {
	for (i = 0; i < k; ++i)
	    rdraw[i] = p[m + (nfuzz[i]-1) * nn];
	for (i = 0; i < k; ++i)
	    p[m + i * nn] = rdraw[i];
    }
    return;
} /* caddy */

/* -----------------------------------------------------------

     Compute Silhouette Information :

 TODO  cleanup: this is almost identical to dark() in  ./pam.c
   -- difference : different  dys[] / dss[] indexing, but that
   -- dss[] indexing change needs to be "synchronized" in all functions here
*/
static
void fygur(int kk, int nn,
	   int *ncluv, int *nsend, int *nelem, int *negbr,
	   double *syl, double *srank, double *avsyl, double *ttsyl,
	   double *dss, double *s, double *sylinf)
{
    int sylinf_d = nn; /* sylinf[nn, 4] */
    int j, l, k, k_, nj, ntt, nsylr;
    double dysa, dysb;
    /* pointers to sylinf[] columns:*/
    double *sylinf_2, *sylinf_3, *sylinf_4;
    sylinf_2 = sylinf	+ sylinf_d;
    sylinf_3 = sylinf_2 + sylinf_d;
    sylinf_4 = sylinf_3 + sylinf_d;

    /* Parameter adjustments */
    --avsyl;
    --ncluv;

    --dss;

    nsylr = 0;
    *ttsyl = 0.;
    for (k = 1; k <= kk; ++k) {

	/* nelem[0:(ntt-1)] := indices (1-based) of obs. in cluster k : */
	ntt = 0;
	for (j = 1; j <= nn; ++j) {
	    if (ncluv[j] == k) {
		nelem[ntt] = j;
		++ntt;
	    }
	}

	for (j = 0; j < ntt; ++j) {/* (j+1)-th obs. in cluster k */
	    nj = nelem[j];
	    dysb = *s * 1.1 + 1.;
	    negbr[j] = -1;
	    /* for all clusters	 k_ != k : */
	    for (k_ = 1; k_ <= kk; ++k_) if (k_ != k) {
		int nbb = 0;
		double db = 0.;
		for (l = 1; l <= nn; ++l) {
		    if (ncluv[l] == k_) {
			++nbb;
			if (l < nj) {
			    db += dss[nn * (l - 1) + nj - l * (l + 1) / 2];
			} else if (l > nj) {
			    db += dss[nn * (nj - 1) + l - nj * (nj + 1) / 2];
			} /* else dss(.)=0 ; nothing to add */
		    }
		}
		db /= nbb; /* now  db(k_) := mean( d[j, l]; l in C_{k_} ) */
		if (dysb > db) {
		    dysb = db;
		    negbr[j] = k_;
		}
	    }/* negbr[j] := arg min_{k_} db(k_) */
	    if (ntt > 1) {
		dysa = 0.;
		for (l = 0; l < ntt; ++l) {
		    int nl = nelem[l];
		    if (nj < nl) {
			dysa += dss[nn * (nj - 1) + nl - nj * (nj + 1) / 2];
		    } else if (nj > nl) {
			dysa += dss[nn * (nl - 1) + nj - nl * (nl + 1) / 2];
		    }/* else dss(.)=0 ; nothing to add */
		}
		dysa /= ntt - 1;
		if (dysa > 0.) {
		    if (dysb > 0.) {
			if (dysb > dysa)
			    syl[j] = 1. - dysa / dysb;
			else if (dysb < dysa)
			    syl[j] = dysb / dysa - 1.;
			else /* dysb == dysa: */
			    syl[j] = 0.;

			if (syl[j] < -1.)
			    syl[j] = -1.;
			else if (syl[j] > 1.)
			    syl[j] = 1.;

		    } else {
			syl[j] = -1.;
		    }
		}
		else /* dysa == 0 */ if (dysb > 0.)
		    syl[j] = 1.;
		else
		    syl[j] = 0.;
	    }
	    else { /* ntt == 1: */
		syl[j] = 0.;
	    }
	} /* for( j ) */
	avsyl[k] = 0.;
	for (j = 0; j < ntt; ++j) {
	    int lang = 0 /* -Wall */;
	    double symax = -2.;
	    for (l = 0; l < ntt; ++l) {
		if (symax < syl[l]) {
		    symax = syl[l];
		    lang = l;
		}
	    }
	    nsend[j] = lang;
	    srank[j] = symax; /* = syl[lang] */
	    avsyl[k] += srank[j];
	    syl[lang] = -3.;
	}
	*ttsyl += avsyl[k];
	avsyl[k] /= (double) ntt;
	if (ntt < 2) {
	    sylinf  [nsylr] = (double) k;
	    sylinf_2[nsylr] = (double) negbr[0];
	    sylinf_3[nsylr] = 0.;
	    sylinf_4[nsylr] = (double) nelem[0];
	    ++nsylr;
	}
	else {
	    for (j = 0; j < ntt; ++j) {
		nj = nsend[j];
		sylinf	[nsylr] = (double) k;
		sylinf_2[nsylr] = (double) negbr[nj];
		sylinf_3[nsylr] = srank[j];
		sylinf_4[nsylr] = (double) nelem[nj];
		++nsylr;
	    }
	}
    } /* for (k) */
    *ttsyl /= nn;
} /* fygur */
