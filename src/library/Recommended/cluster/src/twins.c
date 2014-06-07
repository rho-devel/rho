/* Produced by
 * $Id$
 *
 * twins.f -- translated by f2c (version 20031025).
 */

#include <math.h>
#include <Rmath.h>
#include "cluster.h"
#include "ind_2.h"

// the auxiliary routines
static void averl_(int nn, int *kwan, int *ner, double *ban, double *dys, int method, double *alpha, int *merge);
static void splyt_(int nn, int *kwan, int *ner, double *ban, double *dys, int *merge);
static double min_dis(double dys[], int ka, int kb, int ner[]);

/*     This program performs agglomerative nesting (AGNES) using the */
/*     group average method (_or others_) of Sokal and Michener (1958), */
/*     as well as divisive analysis (DIANA) using the method of */
/*     Mcnaughton-Smith, Williams, Dale, and Mockett (1964). */

/*     Extended by Martin Maechler to allow the (flexible) */
/*     Lance-Williams clustering method (with parameters alpha[1:4]) */

void twins(int *nn, // = maximal number of objects
	   int *jpp,// = maximal number of variables used in the analysis
	   double *x,
	   double *dys,
	   double *dys2,// dys2(.) can have length 1, if(!keep.diss)
	   int *jdyss, /* jdyss (in/out): initially, jdyss mod 10 = 1 : <==> diss = TRUE
			* jdyss < 10 : don't save dissimilarities */
	   double *valmd,
	   int *jtmd, int *ndyst, int *jalg, int *method,
	   int *kwan, int *ner, double *ban, double *coef,
	   double *alpha, int *merge)
{
    if (*jdyss % 10 == 1) {
	*jpp = 1;
    } else { // compute distances
	int jhalt = 0;
	F77_CALL(dysta)(nn, jpp, x, dys, ndyst, jtmd, valmd, &jhalt);
	/*       ------ in ./dysta.f */
	if (jhalt != 0) { *jdyss = -1; return; }
    }
    if (*jdyss >= 10) { /*        save distances for S */
	Memcpy(dys2, dys, (*nn * (*nn - 1) / 2 + 1));
    }
    if (*jalg != 2) {
	// AGNES
	averl_(*nn, kwan, ner, ban, dys, *method, alpha, merge);
    } else {
	// DIANA
	splyt_(*nn, kwan, ner, ban, dys,                 merge);
    }
    // Compute agglomerative/divisive coefficient from banner:
    *coef = bncoef(*nn, ban);
    return;
} /* twins */

/*     ----------------------------------------------------------- */
/*     AGNES agglomeration */
static void
averl_(int nn, int *kwan, int *ner,
       double *ban, double *dys, int method, double *alpha, int *merge)
{

/* VARs */
    double akb, smald;
    int j, k, l1, l2, lq, nab, lka, nlj,
	nclu, lnum, lput, lenda, lendb, lnext, n_1 = nn - 1,
	la = -1, lb = -1, llast = -1, lfyrs = -1, // <- against (unnecessary) warnings [-Wall]
	nmerge;

    /* System generated locals */
    int merge_dim1 = n_1;
    int merge_offset = 1 + merge_dim1;

    /* Parameter adjustments */
    merge -= merge_offset;
    --ban;
    --ner;
    --kwan;
    --alpha;

/*     initialization: */
/*       Starting with nn clusters, kwan(j) = #{obj} in cluster j */
    for (j = 1; j <= nn; ++j) {
	kwan[j] = 1;
	ner[j] = j;
    }

/*     find closest clusters */
    nmerge = 1;
    for (nclu = n_1; nclu >= 1; --nclu) {
	j = 1;
L80:
	++j;
	if (kwan[j] == 0) goto L80;

	smald = dys[ind_2(1, j)] * 1.1f + 1.;
	for (k = 1; k <= n_1; ++k) if (kwan[k] > 0) {
		for (j = k + 1; j <= nn; ++j) if (kwan[j] > 0) {
			nlj = ind_2(k, j);
			if (smald >= dys[nlj]) { // Note: also when "==" !
			    smald =  dys[nlj];
			    la = k;
			    lb = j;
			}
		    }
	    }

/*     merge-structure for plotting tree in S */

	l1 = -la;
	l2 = -lb;
	for (j = 1; j <= (nmerge - 1); ++j) {
	    if (merge[j + merge_dim1] == l1 || merge[j + (merge_dim1 << 1)] == l1)  l1 = j;
	    if (merge[j + merge_dim1] == l2 || merge[j + (merge_dim1 << 1)] == l2)  l2 = j;
	}
	merge[nmerge + merge_dim1] = l1;
	merge[nmerge + (merge_dim1 << 1)] = l2;
	++nmerge;

/*     determine lfyrs and llast */

	for (k = 1; k <= nn; ++k) {
	    if (ner[k] == la) lfyrs = k;
	    if (ner[k] == lb) llast = k;
	}
	ban[llast] = smald;

/*     if the two clusters are next to each other, ner must not be changed */

	lnext = lfyrs + kwan[la];
	if (lnext != llast) {
	    /*     updating ner and ban */
	    lput = lfyrs + kwan[la];
	    lnum = llast - lput;
	    for (k = 1; k <= lnum; ++k) {
		lka = ner[lput];
		akb = ban[lput];
		lenda = llast + kwan[lb] - 2;
		lendb = lenda + 1;
		for (j = lput; j <= lenda; ++j) {
		    ner[j] = ner[j + 1];
		    ban[j] = ban[j + 1];
		}
		ner[lendb] = lka;
		ban[lendb] = akb;
/* L220: */
	    }
	}

/*     We will merge A & B into  A_{new} */

	// Calculate new dissimilarities d(q, A_{new})
	for (lq = 1; lq <= nn; ++lq) { //  for each other cluster 'q'

	    double dnew, ta, tb, tq, fa, fb, fc;
	    if (lq == la || lq == lb || kwan[lq] == 0)
		continue;

	    int naq = ind_2(la, lq);
	    int nbq = ind_2(lb, lq);

	    switch(method) {
	    case 1: /*     1: group average method */
		ta = (double) kwan[la];
		tb = (double) kwan[lb];
		fa = ta / (ta + tb);
		fb = tb / (ta + tb);
		dys[naq] = fa * dys[naq] + fb * dys[nbq];
		break;
	    case 2: /*     2: single linkage */
		dnew = dys[naq];
		if (dys[nbq] < dnew)
		    dnew = dys[nbq];
		dys[naq] = dnew;
		break;
	    case 3: /*     3: complete linkage */
		dnew = dys[naq];
		if (dnew < dys[nbq])
		    dnew = dys[nbq];
		dys[naq] = dnew;
		break;
            case 4: /*     4: ward's method */
		ta = (double) kwan[la];
		tb = (double) kwan[lb];
		tq = (double) kwan[lq];
		fa = (ta + tq) / (ta + tb + tq);
		fb = (tb + tq) / (ta + tb + tq);
		fc = -tq / (ta + tb + tq);
		nab = ind_2(la, lb);
		dys[naq] = sqrt(fa * dys[naq] * dys[naq] +
				fb * dys[nbq] * dys[nbq] +
				fc * dys[nab] * dys[nab]);
		break;

	    case 5: /*     5: weighted average linkage */
		dys[naq] = (dys[naq] + dys[nbq]) / 2.;
		break;
	    case 6: /*     6: "Flexible Strategy" (K+R p.236 f) extended to 'Lance-Williams' */
		dys[naq] = alpha[1] * dys[naq] + alpha[2] * dys[nbq] +
		    alpha[3] * dys[ind_2(la, lb)] +
		    alpha[4] * fabs(dys[naq] - dys[nbq]);
		/* Lance-Williams would allow alpha(1:2) to *depend* on |cluster|
		 * could also include the extensions of Jambu(1978) --
		 * See Gordon A.D. (1999) "Classification" (2nd ed.) p.78 ff */
		break;
	    default:
		error(_("invalid method (code %d"), method);
	    }
	}
	kwan[la] += kwan[lb];
	kwan[lb] = 0;
    }
    return;
} /* averl_ */

/*     ----------------------------------------------------------- */

/*       cf = ac := "Agglomerative Coefficient" from AGNES banner */
/*  or   cf = dc := "Divisive Coefficient"      from DIANA banner */

void R_bncoef(int *nn, double *ban, double *cf) {
    *cf = bncoef(*nn, ban);
}

double bncoef(int nn, double *ban)
{
/* VARs */
    int k, kafte, kearl;
    double sup, syze, cf;

    /* Parameter adjustments */
    --ban;

    // sup := max_k ban[k]
    for(sup = 0., k = 2; k <= nn; ++k) {
	if (sup < ban[k])
	    sup = ban[k];
    }
    cf = 0.;
    for (k = 1; k <= nn; ++k) {
	kearl = (k > 1 ) ? k : 2;
	kafte = (k < nn) ? (k + 1) : nn;
	syze = fmin2(ban[kearl], ban[kafte]);
	cf += (1. - syze / sup);
    }
    return cf / nn;
} /* bncoef */

/*     ----------------------------------------------------------- */
/*     DIANA "splitting" */

static void
splyt_(int nn, int *kwan, int *ner,
       double *ban, double *dys, int *merge)
{
    /* Local variables */
    int j, ja, jb, k, l;
    int jma, jmb, lmm, llq, lmz,
	lxx, lxy, lmma, lmmb, lner, nclu;
    int lchan, nhalf, nmerge, n_1 = nn - 1, splyn;
    /* against (unnecessary) warnings [-Wall]: */
    int jaway = -1, lndsd = -1;

    double da, db, cs, sd, dyff;

    /* System generated locals */
    int merge_dim1 = n_1;
    int merge_offset = 1 + merge_dim1;

    /* Parameter adjustments */
    merge -= merge_offset;
    --ban;
    --ner;
    --kwan;


    /*     initialization */
    nclu = 1;
    nhalf = nn * n_1 / 2 + 1;
    for (l = 1; l <= nn; ++l) {
	kwan[l] = 0;
	ban[l] = 0.;
	ner[l] = l;
    }
    kwan[1] = nn;
    ja = 1;

/*     cs :=  diameter of data set */

    cs = 0.f;
    k = 0;
L20:
    if (cs < dys[k])
	cs = dys[k];
    ++k;
    if (k < nhalf) {
	goto L20;
    }

/*     prepare for splitting */

//____________ Big Loop _________________________________________________
L30:
    jb = ja + kwan[ja] - 1;
    jma = jb;

/*     special case of a pair of objects */

    if (kwan[ja] == 2) {
	kwan[ja] = 1;
	kwan[jb] = 1;
	ban [jb] = dys[ind_2(ner[ja], ner[jb])];
    }
    else {
	/*     finding first object to be shifted */
	double bygsd = -1.;
	for (l = ja; l <= jb; ++l) {
	    lner = ner[l];
	    sd = 0.;
	    for (j = ja; j <= jb; ++j)
		sd += dys[ind_2(lner, ner[j])];
	    if (bygsd < sd) {
		bygsd = sd;
		lndsd = l;
	    }
	}

/*     shifting the first object */
	--kwan[ja];
	kwan[jb] = 1;
	if (jb != lndsd) {
	    lchan = ner[lndsd];
	    lmm = jb - 1;
	    for (lmma = lndsd; lmma <= lmm; ++lmma) {
		lmmb = lmma + 1;
		ner[lmma] = ner[lmmb];
	    }
	    ner[jb] = lchan;
	}
	splyn = 0;
	jma = jb - 1;

/*     finding the next object to be shifted */

	do {
	    splyn++;
	    int rest = (jma - ja);
	    double bdyff = -1.;
	    for (l = ja; l <= jma; ++l) {
		lner = ner[l];
		da = 0.;
		for (j = ja; j <= jma; ++j)
		    da += dys[ind_2(lner, ner[j])];
		da /= rest;
		db = 0.;
		for (j = jma + 1; j <= jb; ++j)
		    db += dys[ind_2(lner, ner[j])];
		db /= splyn;
		dyff = da - db;
		if (bdyff < dyff) {
		    bdyff = dyff;
		    jaway = l;
		}
	    } /* end for(l ..) */
	    jmb = jma + 1;

/*     shifting the next object when necessary */

	    if (bdyff <= 0.)
		break; // out of  "object shifting"  while(.) loop

	    if (jma != jaway) {
		lchan = ner[jaway];
		lmz = jma - 1;
		for (lxx = jaway; lxx <= lmz; ++lxx)
		    ner[lxx] = ner[lxx + 1];
		ner[jma] = lchan;
	    }
	    for (lxx = jmb; lxx <= jb; ++lxx) {
		lxy = lxx - 1;
		if (ner[lxy] < ner[lxx])
		    break;
		lchan = ner[lxy]; ner[lxy] = ner[lxx]; ner[lxx] = lchan;
	    }

	    --kwan[ja];
	    kwan[jma] = kwan[jmb] + 1;
	    kwan[jmb] = 0;
	    --jma;
	    jmb = jma + 1;

	} while (jma != ja);


// 200:     switch the two parts when necessary 

	if (ner[ja] >= ner[jmb]) {
	    int lxxa = ja;
	    for (int lgrb = jmb; lgrb <= jb; ++lgrb) {
		++lxxa;
		lchan = ner[lgrb];
		int lxg = -1;
		for (lxy = lxxa; lxy <= lgrb; ++lxy) {
		    int lxf = lgrb - lxy + lxxa;
		    lxg = lxf - 1;
		    ner[lxf] = ner[lxg];
		}
		ner[lxg] = lchan;
	    }
	    llq = kwan[jmb];
	    kwan[jmb] = 0;
	    jma = ja + jb - jma - 1;
	    jmb = jma + 1;
	    kwan[jmb] = kwan[ja];
	    kwan[ja] = llq;
	}

/* 300 :    compute level for banner */

	if (nclu == 1) {
	    ban[jmb] = cs;
	} else {
	    ban[jmb] = min_dis(dys, ja, jb, &ner[1]);
	}

    }

    if (++nclu < nn) { /* continue splitting until all objects are separated */
	if (jb != nn) {
 L420:
	    ja += kwan[ja];
	    if (ja <= nn) {
		if (kwan[ja] <= 1)
		    goto L420;
		else
		    goto L30;
	    }
	}
	ja = 1;
	if (kwan[ja] == 1)
	    goto L420;
	else
	    goto L30;
    }
//____________ End Big Loop _________________________________________________

/* 500 :  merge-structure for plotting tree in S */

    for (nmerge = 1; nmerge <= n_1; ++nmerge) {
	int nj = -1, l1, l2;
	double dmin = cs;
	for (j = 2; j <= nn; ++j) {
	    if (kwan[j] >= 0 && dmin >= ban[j]) {
		dmin = ban[j];
		nj = j;
	    }
	}
	kwan[nj] = -1;
	l1 = -ner[nj - 1];
	l2 = -ner[nj];
	for (j = 1; j <= (nmerge - 1); ++j) {
	    if (merge[j + merge_dim1] == l1 || merge[j + (merge_dim1 << 1)] == l1)  l1 = j;
	    if (merge[j + merge_dim1] == l2 || merge[j + (merge_dim1 << 1)] == l2)  l2 = j;
	}
	merge[nmerge + merge_dim1] = l1;
	merge[nmerge + (merge_dim1 << 1)] = l2;
    }
    return;
} /* splyt_ */

/*     ----------------------------------------------------------- */
/*     used in splyt() above */
static double min_dis(double dys[], int ka, int kb, int ner[])
{
    double dm = 0.;
    for(int k = ka -1; k < kb -1; ++k) {
	int ner_k = ner[k];
	for (int j = k+1; j < kb; ++j) {
	    int k_j = ind_2(ner_k, ner[j]);
	    if (dm < dys[k_j])
		dm = dys[k_j];
	}
    }
    return dm;
} /* min_dis */

