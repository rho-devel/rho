/* Donated by Francois Romain */

#include <Rmath.h>  /* fmax2() */
#include <R_ext/Boolean.h>

#include "cluster.h"

void sildist(double *d,		/* distance : in matrix or dist format; i.e.,
				   of length n^2 or n*(n-1)/2; see 'ismat' */
	     int    *n,		/* number of Subjects (attr(d,'Size')) */
	     int    *clustering,/* clustering vector, values from {1..k} */
	     int    *k,		/* number of clusters */
	     double *diC,	/* diC */
	     int    *counts,	/* counts[k] :=	 #{cluster k} */
	     double *si,	/* (a_i - b_i) / max(ai,bi) */
	     int    *neighbor,	/* neighbor */
	     int    *ismat)	/* boolean : is 'd' a
				   matrix (1) or a dist vector (0) */
{
    int i,j,l, ci;
    Rboolean computeSi ; /* do we compute si[i] */
    double ai, bi ;

    for(i = 0, l = 0; i < *n; i++) {
	ci = clustering[i] - 1;
	counts[ci]++;

	if(*ismat)
	    l = (*n)*i + i+1;

	for(j = i+1; j < *n; j++, l++) {
	    int cj = clustering[j]-1;

	    diC[(*k)*i + cj] += d[l];
	    diC[(*k)*j + ci] += d[l];
	}
    }


    for(i = 0; i < *n; i++) {
	int ki = (*k)*i;
	ci = clustering[i] - 1;
	computeSi = TRUE;

	for(j=0; j < *k; j++) {
	    if(j == ci) {
		if(counts[j] == 1) /* only one subject in the cluster */
		    computeSi = FALSE;
		else
		    diC[ki + j] /= (counts[j]-1);
	    }
	    else {
		diC[ki + j] /= counts[j];
	    }
	}

	ai = diC[ki+ci];

	/* bi = min_C diC : */

	if(ci == 0) { /* cluster #1 */
	    bi = diC[ki+1]; neighbor[i] = 2;
	}
	else {
	    bi = diC[ki];   neighbor[i] = 1;
	}

	for(j = 1; j < *k; j++)
	    if(j != ci) {
		if(bi > diC[ki + j]) {
		    bi = diC[ki + j];
		    neighbor[i] = j+1;
		}
	    }

	si[i] = (computeSi && (bi != ai)) ?
	    (bi - ai) / fmax2(ai, bi) : 0.;
    }
}
