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
				     *	     1 : use R's RNG (and seed) */
	      int *nrepr,
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
    int nsm, ntt, nad, nadv, rand_k, kans, nrun, n_dys, nsamb, nunfs;
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
	    rand_k = *n;							\
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
			if (nsel[jkk] < nsm) {
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
		for (kans = 0; kans < ntt; ++kans)
		    if (nsel[kans] >= rand_k) {
			if (nsel[kans] == rand_k)
			    goto L210;
			else {
			    for (nad = kans; nad <= ntt; ++nad) {
				nadv = ntt - nad + kans;
				nsel[nadv] = nsel[nadv-1];
			    }
			    nsel[kans] = rand_k;
			    /* continue _outer_ loop */ goto L290;
			}
		    }
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

	bswap2(*kk, *nsam, nrepr, dys, &sky, s,
	       /* dysma */tmp1, /*dysmb*/tmp2,
	       /* beter[], only used here */&tmp[nsamb]);

	if(*trace_lev >= 2) Rprintf(" bs2");

	selec(*kk, *n, *jpp, *diss_kind, &zb, *nsam, has_NA, jtmd, valmd,
	      nrepr, nsel, dys, x, nr, &nafs, ttd, radus, ratt,
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
