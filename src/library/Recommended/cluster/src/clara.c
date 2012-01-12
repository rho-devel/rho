/*   Clustering LARge Applications
     ~		~~~   ~
     Clustering program based upon the k-medoid approach,
     and suitable for data sets of at least 100 objects.
     (for smaller data sets, please use program pam.)
 */

/* original Id: clara.f,v 1.10 2002/08/27 15:43:58 maechler translated by
 * f2c (version 20010821) and run through f2c-clean,v 1.10 2002/03/28
 */

#include <math.h>

#include <R_ext/Print.h>/* for diagnostics */
#include <R_ext/Random.h>/* when R's RNG is used */
#include <R_ext/Utils.h>/* for interrupting */

#include "cluster.h"
#include "ind_2.h"

void cl_clara(int *n,  /* = number of objects */
	      int *jpp,/* = number of variables */
	      int *kk, /* = number of clusters, 1 <= kk <= n-1 */
	      double *x,	/* Input:  the data x[n, jpp] _rowwise_ (transposed)
				 * Output: the first `n' values are the `clustering'
				 *	   (integers in 1,2,..,kk) */
	      int *nran,	/* = #{random samples} drawn	   (= `samples' in R)*/
	      int *nsam,	/* = #{objects} drawn from data set (`sampsize' in R) */
	      double *dys,/* [1:(1 + (nsam * (nsam - 1))/2)]
			   * Output: to contain the distances */
	      int *mdata,	/*= {0,1}; 1: min(x) is missing value (NA);  0: no NA */
	      double *valmd,/*[j]= missing value code (instead of NA) for x[,j]*/
	      int *jtmd,	/* [j]= {-1,1};	 -1: x[,j] has NA; 1: no NAs in x[,j] */
	      int *diss_kind,/*= {1,2};  1 : euclidean;  2 : manhattan*/
	      int/*logical*/ *rng_R,/*= {0,1};  0 : use clara's internal weak RNG;
				     *	        1 : use R's RNG (and seed) */
	      int/*logical*/ *pam_like,/* if (1), we do "swap()" as in pam();*/
	      // otherwise use the code as it was in clara() "forever" upto 2011-04
 	      int *nrepr, /* logical (0/1): 1 = "is representative object"  */
	      int *nsel,
	      int *nbest,/* x[nbest[j],] : the j-th obs in the final sample */
	      int *nr, int *nrx,/* prov. and final "medoids" aka representatives */
	      double *radus, double *ttd, double *ratt,
	      double *ttbes, double *rdbes, double *rabes,
	      int *mtt, double *obj,
	      double *avsyl, double *ttsyl, double *sylinf,
	      int *jstop, int *trace_lev,
	      double *tmp, /* = double [ 3 * nsam ] */
	      int *itmp	/* = integer[ 6 * nsam ] */
    )
{

#define tmp1 tmp
#define tmp2 &tmp[*nsam]

#define ntmp1 itmp
#define ntmp2 &itmp[*nsam]
#define ntmp3 &itmp[nsamb]
#define ntmp4 &itmp[nsamb+ *nsam]
#define ntmp5 &itmp[2*nsamb]
#define ntmp6 &itmp[2*nsamb+ *nsam]

    /* Local variables */

    Rboolean nafs, kall, full_sample, lrg_sam, dyst_toomany_NA,
	has_NA = *mdata;
    int j, jk, jkk, js, jsm, jran, l, n_sam;
    int nsm, ntt, rand_k, nrun, n_dys, nsamb, nunfs;
    double rnn, sky, zb, s, sx = -1., zba = -1.;/* Wall */

    *jstop = 0;
    rnn = (double) (*n);

    /* n_dys := size of distance array dys[] */
    n_dys = *nsam * (*nsam - 1) / 2 + 1;/* >= 1 */
    full_sample = (*n == *nsam);/* only one sub sample == full data */
    nsamb = *nsam * 2;
    lrg_sam = (*n < nsamb);/* sample more than *n/2 */
    if (lrg_sam)/* generate indices for the other, smaller half */
	n_sam = *n - *nsam;
    else
	n_sam = *nsam;

    if(*trace_lev) Rprintf("C clara(): (nsam,nran,n) = (%d,%d,%d);%s\n",
			   *nsam, *nran, *n,
			   full_sample ? " 'full_sample',":
			   (lrg_sam ? " 'large_sample',": ""));
    if(*rng_R && !full_sample)
	GetRNGstate();
    else /* << initialize `random seed' of the very simple randm() below */
	nrun = 0;

#define NEW_rand_k_trace_print(_nr_)					\
	rand_k= 1+ (int)(rnn* ((*rng_R)? unif_rand(): randm(&nrun)));	\
	if (rand_k > *n) {/* should never happen */			\
	    REprintf("** C clara(): random k=%d > n **\n", rand_k);	\
	    rand_k = *n;						\
	}								\
	if(*trace_lev >= 4) {						\
	    Rprintf("... {" #_nr_ "}");					\
	    if(*rng_R) Rprintf("R unif_rand()");			\
	    else       Rprintf("nrun=%5d", nrun);			\
	    Rprintf(" -> k{ran}=%d\n", rand_k);				\
	}

/* __LOOP__ :  random subsamples are drawn and partitioned into kk clusters */

    kall = FALSE; /* kall becomes TRUE iff we've found a "valid sample",
		     i.e. one for which all d(j,k) can be computed */
    nunfs = 0;
    dyst_toomany_NA = FALSE;
    for (jran = 1; jran <= *nran; ++jran) {
	if(*trace_lev) Rprintf("C clara(): sample %d ", jran);
	if (!full_sample) {/* `real' case: sample size < n */
	    ntt = 0;
	    if (kall && nunfs+1 != jran && !lrg_sam) {
		/* Have had (at least) one valid sample; use its representatives
		 * nrx[] :  nsel[] := sort(nrx[])  for the first j=1:k */
		if(*trace_lev >= 2) Rprintf(" if (kall && nunfs...): \n");

		for (jk = 0; jk < *kk; ++jk)
		    nsel[jk] = nrx[jk];
		for (jk = 0; jk < *kk-1; ++jk) { /* sort(nsel[0:(kk-1)] */
		    /* FIXME: nsel[] is 0-indexed, but *contains* 1-indices*/
		    nsm = nsel[jk];
		    jsm = jk;
		    for (jkk = jk + 1; jkk < *kk; ++jkk) {
			if (nsm > nsel[jkk]) {
			    nsm = nsel[jkk];
			    jsm = jkk;
			}
		    }
		    nsel[jsm] = nsel[jk]; nsel[jk] = nsm;
		}
		ntt = *kk;
	    }
	    else { /* no valid sample  _OR_  lrg_sam */
		if(*trace_lev >= 2) Rprintf(" finding 1st... new k{ran}:\n");

		/* Loop finding random index `rand_k' not yet in nrx[0:(*kk-1)] : */
	    L180:
		NEW_rand_k_trace_print(180)

		if (kall) {
		    for (jk = 0; jk < *kk; ++jk)
			if (rand_k == nrx[jk])
			    goto L180;
		}
		/* end Loop */

		nsel[ntt] = rand_k;
		if (++ntt == n_sam)
		    goto L295;
	    }

	    if(*trace_lev >= 2) {
		Rprintf(".. kall: %s, ", (kall) ? "T" : "FALSE");
		if(*trace_lev == 2) {
		    Rprintf("nsel[ntt=%d] = %d\n", ntt, nsel[ntt]);
		} else { /* trace_lev >= 3 */
		    Rprintf("\n... nrx [0:%d]= ",*kk-1);
		    for (jk = 0; jk < *kk; jk++) Rprintf("%d ",nrx[jk]);
		    Rprintf("\n... nsel[0:%d]= ",ntt-1);
		    for (jk = 0; jk < ntt; jk++) Rprintf("%d ",nsel[jk]);
		    Rprintf("\n");
		}
	    }

	    do {
		/* Loop finding random index 'rand_k' in {1:n},
		 * not in nrx[0:(k-1)] nor nsel[1:ntt] : */
	    L210:
		NEW_rand_k_trace_print(210)

		if (kall && lrg_sam) {
		    for (jk = 0; jk < *kk; ++jk) {
			if (rand_k == nrx[jk])
			    goto L210;
		    }
		}
		/* insert rand_k into nsel[1:ntt] or after  and increase ntt : */
		for (int ka = 0; ka < ntt; ++ka)
		    if (nsel[ka] >= rand_k) {
			if (nsel[ka] == rand_k)
			    goto L210;
			else {// nsel[ka] > rand_k :
			    for (int na = ntt; na > ka; --na)
				nsel[na] = nsel[na-1];
			    nsel[ka] = rand_k;
			    /* continue _outer_ loop */ goto L290;
			}
		    }
		// else: rand_k > nsel[ka]  for all ka = 0:(ntt-1) :
		nsel[ntt] = rand_k;

	    L290:
		++ntt;
	    } while (ntt < n_sam);

	L295:
	    if(*trace_lev) Rprintf(" {295} [ntt=%d, nunfs=%d] ", ntt, nunfs);
	    if (lrg_sam) {
		/* have indices for smaller _nonsampled_ half; revert this: */
		for (j = 1, jk = 0, js = 0; j <= *n; j++) {
		    if (jk < n_sam && nsel[jk] == j)
			++jk;
		    else
			nrepr[js++] = j;
		}
		for (j = 0; j < *nsam; ++j)
		    nsel[j] = nrepr[j];
	    }
	    if(*trace_lev >= 3) {
		Rprintf(".. nsel[1:%d]= ", *nsam);
		for (jk = 0; jk < *nsam; jk++) Rprintf("%d ",nsel[jk]);
	    }
	    if(*trace_lev) Rprintf(" -> dysta2()\n");
	}
	else { /* full_sample : *n = *nsam -- one sample is enough ! */
	    for (j = 0; j < *nsam; ++j)
		nsel[j] = j+1;/* <- uses 1-indices for its *values*! */
	}

	dysta2(*nsam, *jpp, nsel, x, *n, dys, *diss_kind,
	       jtmd, valmd, has_NA, &dyst_toomany_NA);
	if(dyst_toomany_NA) {
	    if(*trace_lev)
		Rprintf("  dysta2() gave dyst_toomany_NA --> new sample\n");
	    dyst_toomany_NA = FALSE;
	    ++nunfs;
	    continue;/* random sample*/
	}

	s = 0.;
	for(l = 1; l < n_dys; l++) /* dys[0] is not used here */
	    if (s < dys[l])
		s = dys[l];
	if(*trace_lev >= 2)
	    Rprintf(". clara(): s:= max dys[1..%d] = %g;", l-1,s);

	bswap2(*kk, *nsam, s, dys, *pam_like, *trace_lev,
	       /* --> */ &sky, nrepr,
	       /* dysma */tmp1, /*dysmb*/tmp2,
	       /* beter[], only used here */&tmp[nsamb]);

	if(*trace_lev >= 2)
	    Rprintf("end{bswap}: sky = %g\n", sky);

	selec(*kk, *n, *jpp, *diss_kind, &zb, *nsam, has_NA, jtmd, valmd,
	      *trace_lev, nrepr, nsel, dys, x, nr, &nafs, ttd, radus, ratt,
	      ntmp1, ntmp2, ntmp3, ntmp4, ntmp5, ntmp6, tmp1, tmp2);

	if (nafs) { /* couldn't assign some observation (to a cluster)
		     * because of too many NA s */
	    ++nunfs;
	    if(*trace_lev >= 2)
		Rprintf(" selec() -> 'NAfs'");
	}
	else if(!kall || zba > zb) { /* 1st proper sample  or  new best */
	    kall = TRUE;
	    if(*trace_lev >= 2) Rprintf(" 1st proper or new best:");
	    zba = zb;
	    for (jk = 0; jk < *kk; ++jk) {
		ttbes[jk] = ttd	 [jk];
		rdbes[jk] = radus[jk];
		rabes[jk] = ratt [jk];
		nrx  [jk] = nr	 [jk];
	    }
	    for (js = 0; js < *nsam; ++js)
		nbest[js] = nsel[js];
	    sx = s;
	}
	if(*trace_lev >= 2) Rprintf(" obj= %g\n", zb/rnn);

	if(full_sample) break; /* out of resampling */
    }
/* --- end random sampling loop */
    if(*rng_R && !full_sample)
	PutRNGstate();

    if (nunfs >= *nran) { *jstop = 1; return; }
    /* else */
    if (!kall) { *jstop = 2; return; }

    if(*trace_lev) {
	Rprintf("C clara(): best sample _found_ ");
	if(*trace_lev >= 2) {
	    Rprintf("; nbest[1:%d] =\n c(", *nsam);
	    for (js = 0; js < *nsam; ++js) {
		Rprintf("%d", nbest[js]);
		if(js+1 < *nsam) Rprintf(",");
	    }
	    Rprintf(")\n");
	}
	Rprintf(" --> dysta2(nbest), resul(), end\n");
    }


/*     for the best subsample, the objects of the entire data set
     are assigned to their clusters */

    *obj = zba / rnn;
    dysta2(*nsam, *jpp, nbest, x, *n, dys, *diss_kind, jtmd, valmd,
	   has_NA, &dyst_toomany_NA);
    if(dyst_toomany_NA) {
	REprintf(" *** SHOULD NOT HAPPEN: clara() -> dysta2(nbest) gave toomany_NA\n");
	return;
    }
    resul(*kk, *n, *jpp, *diss_kind, has_NA, jtmd, valmd, x, nrx, mtt);

    if (*kk > 1)
	black(*kk, *jpp, *nsam, nbest, dys, sx, x,
	      /* compute --> */
	      avsyl, ttsyl, sylinf,
	      ntmp1, ntmp2, ntmp3, ntmp4, /* syl[] */ tmp1, tmp2);
    return;
} /* End clara() ---------------------------------------------------*/
#undef tmp1
#undef tmp2

#undef ntmp1
#undef ntmp2
#undef ntmp3
#undef ntmp4
#undef ntmp5
#undef ntmp6


void dysta2(int nsam, int jpp, int *nsel,
	    double *x, int n, double *dys, int diss_kind,
	    int *jtmd, double *valmd, Rboolean has_NA, Rboolean *toomany_NA)
{
/* Compute Dissimilarities for the selected sub-sample	---> dys[,] */

    int nlk = 0;
    dys[0] = 0.;/* very first index; *is* used because ind_2(i,i) |-> 0 ! */
    for (int l = 1; l < nsam; ++l) {
	int lsel = nsel[l];
	if(lsel <= 0 || lsel > n)
	    REprintf(" ** dysta2(): nsel[l= %d] = %d is OUT\n", l, lsel);
	for (int k = 0; k < l; ++k) { /* compute d(nsel[l], nsel[k]) {if possible}*/
	    int ksel = nsel[k];
	    if(ksel <= 0 || ksel > n)
		REprintf(" ** dysta2(): nsel[k= %d] = %d is OUT\n", k, ksel);
	    ++nlk;
	    int npres = 0, j, lj, kj;
	    double clk = 0.;
	    for (j = 0, lj = lsel-1, kj = ksel-1; j < jpp;
		 ++j, lj += n, kj += n) {
		if (has_NA && jtmd[j] < 0) { /* x[,j] has some Missing (NA) */
		    /* in the following line (Fortran!), x[-2] ==> seg.fault
		       {BDR to R-core, Sat, 3 Aug 2002} */
		    if (x[lj] == valmd[j] || x[kj] == valmd[j]) {
			continue /* next j */;
		    }
		}
		++npres;
		if (diss_kind == 1)
		    clk += (x[lj] - x[kj]) * (x[lj] - x[kj]);
		else
		    clk += fabs(x[lj] - x[kj]);
	    }
	    if (npres == 0) {/* cannot compute d(.,.) because of too many NA */
		*toomany_NA = TRUE;
		dys[nlk] = -1.;
	    } else {
		double d1 = clk * (jpp / (double) npres);
		dys[nlk] = (diss_kind == 1) ? sqrt(d1) : d1 ;
	    }
	} /* for( k ) */
    } /* for( l ) */
    return;
} /* End dysta2() -----------------------------------------------------------*/

double randm(int *nrun)
{
/* we programmed this generator ourselves because we wanted it
   to be machine independent. it should run on most computers
   because the largest int used is less than 2^30 . the period
   is 2^16=65536, which is good enough for our purposes. */
    /* MM: improved the original speed-wise only: */
    *nrun = (*nrun * 5761 + 999) & 0177777;
    /* Masking off all but the last 16 bits is equivalent to  % 65536 */
    return ((double) (*nrun) / 65536.);
} /* randm() */

/* bswap2() : called once [per random sample] from clara() : */
void bswap2(int kk, int n, /* == nsam == 'sampsize', here in clara */
	    double s, const double dys[],
	    Rboolean pam_like, int trace_lev,
	    // result:
	    double *sky, int *nrepr,
	    double *dysma, double *dysmb, double *beter)
{
    int i, j, ij, k,h, hbest = -1, nbest = -1;/* init for -Wall */
    double dzsky;

    /* Parameter adjustments */
    --nrepr;
    --beter;

    --dysma;	--dysmb;

    if(trace_lev >= 2) {
	if(trace_lev == 2)
	    Rprintf("\n bswap2():");
	else
	    Rprintf("\nclara()'s bswap2(*, s=%g): ", s);
    }

    s = s * 1.1 + 1.;/* value larger than all dissimilarities */

/* ====== first algorithm: BUILD. ====== */

    for (i = 1; i <= n; ++i) {
	nrepr[i] = 0;
	dysma[i] = s;
    }

    for(k = 0; k < kk; k++) {
	int nmax = -1; /* -Wall */
	double ammax = 0.;
	for (i = 1; i <= n; ++i) {
	    if (nrepr[i] == 0) {
		beter[i] = 0.;
		for (j = 1; j <= n; ++j) {
		    double cmd = dysma[j] - dys[ ind_2(i, j)];
		    if (cmd > 0.)
			beter[i] += cmd;
		}
		if (ammax <= beter[i]) {
		    /*	does < (instead of <= ) work too? -- NO! */
		    ammax = beter[i];
		    nmax = i;
		}
	    }
	}

	nrepr[nmax] = 1;/* = .true. : found new representative */
	if(trace_lev >= 2) {
	    if(trace_lev == 2)
		Rprintf(" %d", nmax);
	    else
		Rprintf("    new repr. %d\n", nmax);
	}

	/* update dysma[] : dysma[j] = D(j, nearest_representative) */
	for (j = 1; j <= n; ++j) {
	    ij = ind_2(nmax, j);
	    if (dysma[j] > dys[ij])
		dysma[j] = dys[ij];
	}
    }
    // output of the above loop:  nrepr[], dysma[], ...

    *sky = 0.;
    for (j = 1; j <= n; ++j)
	*sky += dysma[j];

    if(trace_lev >= 2) /* >= 2 (?) */ {
	Rprintf("  after build: medoids are");
	for (i = 1; i <= n; ++i)
	    if(nrepr[i] == 1) Rprintf(" %2d", i);
	if(trace_lev >= 3) {
	    Rprintf("\n  and min.dist dysma[1:n] are\n");
	    for (i = 1; i <= n; ++i) {
		Rprintf(" %6.3g", dysma[i]);
		if(i % 10 == 0) Rprintf("\n");
	    }
	    if(n % 10 != 0) Rprintf("\n");
	} else Rprintf("\n");
	Rprintf(" --> sky = sum_j D_j= %g\n", *sky);
    }

    if (kk == 1)
	return;

// asky = *sky / ((double) n);

/* ====== second algorithm: SWAP. ====== */

/* Big LOOP : */
L60:

    for (j = 1; j <= n; ++j) {
	/*  dysma[j] := D_j  d(j, <closest medi>)  [KR p.102, 104]
	 *  dysmb[j] := E_j  d(j, <2-nd cl.medi>)  [p.103] */
	dysma[j] = s;
	dysmb[j] = s;
	for (i = 1; i <= n; ++i) {
	    if (nrepr[i]) {
		ij = ind_2(i, j);
		if (dysma[j] > dys[ij]) {
		    dysmb[j] = dysma[j];
		    dysma[j] = dys[ij];
		} else if (dysmb[j] > dys[ij]) {
		    dysmb[j] = dys[ij];
		}
	    }
	}
    }

    dzsky = 1.; /* 1 is arbitrary > 0; only dzsky < 0 matters in the end */
    for (h = 1; h <= n; ++h) if (!nrepr[h]) {
        for (i = 1; i <= n; ++i) if (nrepr[i]) {
	    double dz = 0.;
	    /* dz := T_{ih} := sum_j C_{jih}  [p.104] : */
	    for (j = 1; j <= n; ++j) {
		int ij = ind_2(i, j),
		    hj = ind_2(h, j);
		if (dys[ij] == dysma[j]) {
		    double small;
		    if(pam_like)
			small = dysmb[j] > dys[hj] ? dys[hj] : dysmb[j];
		    else // old clara code which differs from pam()'s
			// and seems a bit illogical:
			small = dysmb[j] > dys[ij] ? dys[hj] : dysmb[j];
		    dz += (- dysma[j] + small);
		}
		else if (dys[hj] < dysma[j])
		    dz += (- dysma[j] + dys[hj]);
	    }
	    if (dzsky > dz) {
		dzsky = dz; // dzsky := min_{i,h} T_{i,h}
		hbest = h;
		nbest = i;
	    }
	}
    }

    /* once had some 64-bit compiler / data configuration that looped forever*/
    R_CheckUserInterrupt();

    if (dzsky < 0.) { /* found an improving swap */
	if(trace_lev >= 3)
	    Rprintf( "   swp new %d <-> %d old; decreasing diss. by %g\n",
		     hbest, nbest, dzsky);
	nrepr[hbest] = 1;
	nrepr[nbest] = 0;
	*sky += dzsky;
	goto L60;
    }
    if(trace_lev >= 2 && hbest != -1) // in my examples  hbest == -1 and it does not print:
	Rprintf( "  Last swap: new %d <-> %d old; decreasing diss. by %g\n",
		 hbest, nbest, dzsky);

} /* End of bswap2() -------------------------------------------------- */

/* selec() : called once [per random sample] from clara() */
void selec(int kk, int n, int jpp, int diss_kind,
	   double *zb, int nsam, Rboolean has_NA, int *jtmd, double *valmd,
	   int trace_lev,
	   int *nrepr, int *nsel, double *dys, double *x, int *nr,
	   Rboolean *nafs, /* := TRUE if a distance cannot be calculated */
	   double *ttd, double *radus, double *ratt,
	   int *nrnew, int *nsnew, int *npnew, int *ns, int *np, int *new,
	   double *ttnew, double *rdnew)
{

    /* Local variables */
    int j, jk, jj, jp, jnew, ka, kb, jkabc = -1/* -Wall */;
    int newf, nrjk,  npab, nstrt, na, nb, npa, npb, njk, nobs;

    double pp = (double) (jpp), tra;

/* Parameter adjustments */
    --nsel;    --nrepr;

    --ratt;
    --radus; --ttd;    --np;	--nr;	 --ns;

    --rdnew; --ttnew; --npnew; --nrnew; --nsnew;
    --new;

    /* nafs := TRUE  if a distance cannot be calculated (because of NA s)*/
    *nafs = FALSE;

    /* identification of representative objects, and initializations */
    jk = 0;
    for (j = 1; j <= nsam; ++j) {
	if (nrepr[j] != 0) {
	    ++jk;
	    nr	 [jk] = nsel[j];
	    ns	 [jk] = 0;
	    ttd	 [jk] = 0.;
	    radus[jk] = -1.;
	    np	 [jk] = j;
	}
    }

/* - assignment of the objects of the entire data set to a cluster,
 * - computation of some statistics,
 * - determination of the new ordering of the clusters */

    *zb = 0.;
    newf = 0;

    for(jj = 1; jj <= n; jj++) {
	double dsum, dnull = -9./* -Wall */;
	if (!has_NA) {
	    for (jk = 1; jk <= kk; ++jk) {
		dsum = 0.;
		nrjk = nr[jk];
		if (nrjk != jj) {
		    for (jp = 0; jp < jpp; ++jp) {
			na = (nrjk - 1) + jp * n;
			nb = (jj   - 1) + jp * n;
			tra = fabs(x[na] - x[nb]);
			if (diss_kind == 1)
			    tra *= tra;
			dsum += tra;
		    }
		    if (jk != 1 && dsum >= dnull)
			continue /* next jk */;
		}
		dnull = dsum;
		jkabc = jk;
	    }
	}
	else { /* _has_ missing data */
	    Rboolean pres = FALSE;
	    for (jk = 1; jk <= kk; ++jk) {
		dsum = 0.;
		nrjk = nr[jk];
		if (nrjk != jj) {
		    nobs = 0;
		    for (jp = 0; jp < jpp; ++jp) {
			na = (nrjk - 1) + jp * n;
			nb = (jj   - 1) + jp * n;
			if (jtmd[jp] < 0) {
			    if (x[na] == valmd[jp] || x[nb] == valmd[jp])
				continue /* next jp */;
			}
			nobs++;
			tra = fabs(x[na] - x[nb]);
			if (diss_kind == 1)
			    tra *= tra;
			dsum += tra;
		    }
		    if (nobs == 0) /* all pairs partially missing */
			continue /* next jk */;
		    dsum *= (nobs / pp);
		    /*  MM:  ^^^^^^^^^ fishy; rather * (pp/nobs) as in dysta2*/
		}
		if (!pres)
		    pres = TRUE;
		else if (dnull <= dsum)
		    continue /* next jk */;
		/* here : pres was FALSE {i.e. 1st time} or
		 *	  dnull > dsum	 {i.e. new best} */
		dnull = dsum;
		jkabc = jk;
	    }/* for(jk ..) */

	    if (!pres) { /* found nothing */
		*nafs = TRUE; return;
	    }
	} /* else: has_NA */

	if (diss_kind == 1)
	    dnull = sqrt(dnull);

	*zb += dnull;
	ttd[jkabc] += dnull;
	if (radus[jkabc] < dnull)
	    radus[jkabc] = dnull;

	++ns[jkabc];
	if (newf < kk) {
	    if (newf != 0) {
		for (jnew = 1; jnew <= newf; ++jnew) {
		    if (jkabc == new[jnew])
			goto L90;/* next jj */
		}
	    }
	    ++newf;
	    new[newf] = jkabc;
	}
    L90:
	;
    } /* for( jj = 1..n ) */


/*     a permutation is carried out on vectors nr,ns,np,ttd,radus
     using the information in vector new. */

    for (jk = 1; jk <= kk; ++jk) {
	njk = new[jk];
	nrnew[jk] = nr[njk];
	nsnew[jk] = ns[njk];
	npnew[jk] = np[njk];
	ttnew[jk] = ttd[njk];
	rdnew[jk] = radus[njk];
    }
    for (jk = 1; jk <= kk; ++jk) {
	nr[jk] = nrnew[jk];
	ns[jk] = nsnew[jk];
	np[jk] = npnew[jk];
	ttd[jk] = ttnew[jk];
	radus[jk] = rdnew[jk];
    }
    for (j = 1; j <= kk; ++j) {
	double rns = (double) ns[j];
	ttd[j] /= rns;
    }

    if (kk > 1) {

	/* computation of minimal distance of medoid ka to any
	   other medoid for comparison with the radius of cluster ka. */

	for (ka = 1; ka <= kk; ++ka) {
	    nstrt = 0;
	    npa = np[ka];
	    for (kb = 1; kb <= kk; ++kb) {
		if (kb == ka)
		    continue /* next kb */;

		npb = np[kb];
		npab = ind_2(npa, npb);
		if (nstrt == 0)
		    nstrt = 1;
		else if (dys[npab] >= ratt[ka])
		    continue /* next kb */;

		ratt[ka] = dys[npab];
		if (ratt[ka] == 0.)
		    ratt[ka] = -1.;
	    }
	    if (ratt[ka] > -0.5)
		ratt[ka] = radus[ka] / ratt[ka];
	}
    }
    return;
} /* End selec() -----------------------------------------------------------*/

void resul(int kk, int n, int jpp, int diss_kind, Rboolean has_NA,
	   int *jtmd, double *valmd, double *x, int *nrx, int *mtt)
{
    /* Local variables */
    int j, jk, jj, ka, na, nb, njnb, nrjk, nobs, jksky = -1/* Wall */;
    double pp = (double) (jpp), dsum, tra, dnull = -9./* Wall */;

/* clustering vector is incorporated into x, and ``printed''. */

    for(jj = 0; jj < n; jj++) {

	for (jk = 0; jk < kk; ++jk) {
	    if (nrx[jk] == jj + 1)/* 1-indexing */
		goto L220; /* continue next jj (i.e., outer loop) */
	}
	njnb = jj;

	if (!has_NA) {
	    for (jk = 0; jk < kk; ++jk) {
		dsum = 0.;
		nrjk = (nrx[jk] - 1);
		for (j = 0; j < jpp; ++j) {
		    tra = fabs(x[nrjk + j * n] - x[njnb + j * n]);
		    if (diss_kind == 1)
			tra *= tra;
		    dsum += tra;
		}
		if (diss_kind == 1)
		    dsum = sqrt(dsum);
		if (jk == 0 || dnull > dsum) {
		    dnull = dsum;
		    jksky = jk;
		}
	    }
	}
	else { /* _has_ missing data */
	    for (jk = 0; jk < kk; ++jk) {
		dsum = 0.;
		nrjk = (nrx[jk] - 1);
		nobs = 0;
		for (j = 0; j < jpp; ++j) {
		    na = nrjk + j * n;
		    nb = njnb + j * n;
		    if (jtmd[j] < 0) {
			if (x[na] == valmd[j] || x[nb] == valmd[j])
			    continue /* next j */;
		    }
		    nobs++;
		    tra = fabs(x[na] - x[nb]);
		    if (diss_kind == 1)
			tra *= tra;
		    dsum += tra;
		}
		if (diss_kind == 1)
		    dsum = sqrt(dsum);
		dsum *= (nobs / pp);
		/* MM:   ^^^^^^^^  fishy; rather * (pp/nobs) as in dysta2 */

		if (jk == 0 || dnull > dsum) {
		    dnull = dsum;
		    jksky = jk;
		}
	    }
	}
	x[njnb] = (double) jksky + 1;/* 1-indexing */

    L220:
	;
    } /* for(jj = 0; jj < n ..)*/


    for (jk = 0; jk < kk; ++jk)
	x[nrx[jk] - 1] = (double) jk + 1;/* 1-indexing */

    /* mtt[k] := size(k-th cluster) : */
    for (ka = 0; ka < kk; ++ka) {
	mtt[ka] = 0;
	for(j = 0; j < n; j++) {
	    if (((int) x[j]) == ka + 1)/* 1-indexing */
		++mtt[ka];
	}
    }
    return;
} /* end resul() -----------------------------------------------------------*/


void black(int kk, int jpp, int nsam, int *nbest,
	   double *dys, double s, double *x,
	   /* --> Output : */
	   double *avsyl, double *ttsyl, double *sylinf,
	   /* but the following output vectors are never by clara() : */
	   int *ncluv, int *nsend, int *nelem, int *negbr,
	   double *syl, double *srank)
{
/* Silhouettes computation and "drawing"  --> syl[] and sylinf[] */

    /* System generated locals */
    int sylinf_dim1, sylinf_offset;

    /* Local variables */

    double att, btt, db, dysa, dysb, symax;
    int lang = -1/* -Wall */;
    int j, l, lplac, nj, nl, nbb, ncase, nclu, numcl, nsylr, ntt;

/* Parameter adjustments */
    --avsyl;

    --srank;    --syl;
    --negbr; --nelem; --nsend;
    --ncluv;	--nbest;

    sylinf_dim1 = nsam;
    sylinf_offset = 1 + sylinf_dim1 * 1;
    sylinf -= sylinf_offset;

/*
     construction of clustering vector (ncluv)
     of selected sample (nbest).
*/

    /* Function Body */
    for (l = 1; l <= nsam; ++l) {
	ncase = nbest[l];
	ncluv[l] = (int) x[ncase - 1];
    }

/*     drawing of the silhouettes */

    nsylr = 0;
    *ttsyl = 0.;
    for (numcl = 1; numcl <= kk; ++numcl) {
	ntt = 0;
	for (j = 1; j <= nsam; ++j) {
	    if (ncluv[j] == numcl) {
		++ntt;
		nelem[ntt] = j;
	    }
	}
	for (j = 1; j <= ntt; ++j) {
	    nj = nelem[j];
	    dysb = s * 1.1 + 1.;
	    negbr[j] = -1;

	    for (nclu = 1; nclu <= kk; ++nclu) {
		if (nclu != numcl) {
		    nbb = 0;
		    db = 0.;
		    for (l = 1; l <= nsam; ++l) {
			if (ncluv[l] == nclu) {
			    ++nbb;
			    db += dys[ind_2(nj, l)];
			}
		    }
		    btt = (double) nbb;
		    db /= btt;
		    if (db < dysb) {
			dysb = db;
			negbr[j] = nclu;
		    }
		}
	    }

	    if (ntt == 1) {
		syl[j] = 0.;	    continue /* j */;
	    }
	    dysa = 0.;
	    for (l = 1; l <= ntt; ++l) {
		nl = nelem[l];
		dysa += dys[ind_2(nj, nl)];
	    }
	    att = (double) (ntt - 1);
	    dysa /= att;
	    if (dysa <= 0.) {
		if (dysb > 0.)
		    syl[j] = 1.;
		else
		    syl[j] = 0.;

		continue /* j */;
	    }

	    if (dysb > 0.) {
		if (dysb > dysa)
		    syl[j] = 1. - dysa / dysb;
		else if (dysb < dysa)
		    syl[j] = dysb / dysa - 1.;
		else /* (dysb == dysa) */
		    syl[j] = 0.;

		if (syl[j] < -1.)
		    syl[j] = -1.;
		else if (syl[j] > 1.)
		    syl[j] = 1.;
	    }
	    else {
		syl[j] = -1.;
	    }

	} /* for(j ..) */

	avsyl[numcl] = 0.;
	for (j = 1; j <= ntt; ++j) {
	    symax = -2.;
	    for (l = 1; l <= ntt; ++l) {
		if (syl[l] > symax) {
		    symax = syl[l];
		    lang = l;
		}
	    }
	    nsend[j] = lang;
	    srank[j] = syl[lang];
	    avsyl[numcl] += srank[j];
	    syl[lang] = -3.;
	}
	*ttsyl += avsyl[numcl];
	avsyl[numcl] /= ntt;

	if (ntt >= 2) {
	    for (l = 1; l <= ntt; ++l) {
		lplac = nsend[l];
		ncase = nelem[lplac];
		++nsylr;
		sylinf[nsylr + sylinf_dim1] = (double) numcl;
		sylinf[nsylr + (sylinf_dim1 << 1)] = (double) negbr[lplac];
		sylinf[nsylr + sylinf_dim1 * 3] = srank[l];
		sylinf[nsylr + (sylinf_dim1 << 2)] = (double) nbest[ncase];
	    }
	}
	else {
	    ncase = nelem[1];
	    ++nsylr;
	    sylinf[nsylr + sylinf_dim1] = (double) numcl;
	    sylinf[nsylr + (sylinf_dim1 << 1)] = (double) negbr[1];
	    sylinf[nsylr + sylinf_dim1 * 3] = 0.;
	    sylinf[nsylr + (sylinf_dim1 << 2)] = (double) nbest[ncase];
	}

    }
    *ttsyl /= (double) (nsam);
    return;
} /* black */
