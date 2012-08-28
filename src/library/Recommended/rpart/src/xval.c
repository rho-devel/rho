/* SCCS  @(#)xval.c	1.9 06/06/01 */
/*
** Cross validate a model.  This routine is responsible for filling in
**  two vectors -- xrisk = cross-validated risk estimate
**                 xstd  = std of xrisk
**
** Basic method is to use a stratified partitioning of the data (NOT random)
**  into n_xval subgroups.  One by one, each of these groups is left out of
**  the partitioning by setting 'which' to 0.  After partitioning, the risk
**  of each left out subject is determined, under each of the unique
**  complexity parameters.
** The x-groups are set by the calling S-routine, so they can actually be
**  random, non-random, or whatever, as far as this routine is concerned.
**
**  n_xval: number of cross-validation subsets
**  cptable: head of the complexity parameter table, were results will be
**              stored
**  x_grp(n): defines the groups.  Integers from 1 to n_xval
**  maxcat  : max # categories, in any given categorical variable
**  error   : possible error message
**  parms   : vector of input parameters, initializers for the splitting rule
*/
#include <math.h>
#include <stdio.h>
#include "rpart.h"
#include "node.h"
#include "rpartS.h"
#include "rpartproto.h"

#define DEBUG 0
#if DEBUG
static int debug =0;    /*if it is odd, print out every tree */
                        /*if >= 2, print out every risk value we see */
#endif

/* Next line only if mainline version */
#ifdef MAIN
extern char *xname[];
#endif

void xval(int n_xval,  struct cptable *cptable_head,  Sint *x_grp, 
	  int maxcat,  char **error,                  double * parms)
    {
    int i,j,k, jj;
    double *xtemp, *xpred;
    int    *savew;
    double *cp;
    double alphasave;
    struct node *xtree;
    struct cptable *cplist;
    double temp;
    double old_wt, total_wt;
    int *which;

    alphasave = rp.alpha;
    which = rp.which;
    /*
    ** Allocate a set of temporary arrays
    */
    xtemp = (double *) CALLOC(3*rp.num_unique_cp, sizeof(double));
    xpred = xtemp + rp.num_unique_cp;
    cp    = xpred + rp.num_unique_cp;
    savew = (int *)   CALLOC(rp.n, sizeof(int));
    for (i=0; i<rp.n; i++) savew[i] = rp.which[i];

    /*
    ** Make the list of CPs that I will compare against
    */
    cp[0] = 10* cptable_head->cp;    /*close enough to infinity */
    i=1;
    for (cplist= cptable_head; i<rp.num_unique_cp; cplist = cplist->forward) {
	cp[i] = sqrt(cplist->cp * (cplist->forward)->cp);
	i++;
	}
    total_wt =0;
    for (i=0; i<rp.n; i++) total_wt += rp.wt[i];
    old_wt = total_wt;

    /*
    ** do the validations
    */
    for (i=0; i<n_xval; i++) {
	/*
	** mark the "leave out" data as fictional node 0, the rest as node 1
	*/
	k=0;
	temp =0;
	for (j=0; j<rp.n; j++) {
	    if (x_grp[j]==(i+1)) {
		which[j] =0;
		}
	    else {
		which[j] =1;
		rp.ytemp[k] = rp.ydata[j];
		rp.wtemp[k] = rp.wt[j];
		k++;
		temp += rp.wt[j];
		}
	    }

	/* rescale the cp */
	for (j=0; j<rp.num_unique_cp; j++) cp[j] *= temp/old_wt;
	rp.alpha *= temp/old_wt;
	old_wt = temp;

	/*
	** partition the new tree
	*/
	xtree = (struct node *) CALLOC(1, nodesize);
	xtree->num_obs = k;
	(*rp_init)(k,rp.ytemp, maxcat, error, parms, &temp, 2, rp.wtemp);
	(*rp_eval)(k, rp.ytemp, xtree->response_est, &(xtree->risk),
		   rp.wtemp);
	xtree->complexity = xtree->risk;
	partition(1, xtree, &temp);
	fix_cp(xtree, xtree->complexity);
#ifdef MAIN
	if (debug%2 ==1) print_tree(xtree, 1, xname,0,0);
#endif
	/*
	** run the extra data down the new tree
	*/
	for (j=0; j<rp.n; j++) {
	    if (which[j]==0) {
		rundown(xtree, j, cp, xpred, xtemp);
#if DEBUG > 1
		if (debug >1) {
		   jj = j+1;
		   printf("\nObs %d, y=%f \n", jj, rp.ydata[j][0]);
		}
#endif
		/* add it in to the risk */
		cplist = cptable_head;
		for (jj = 0; jj<rp.num_unique_cp; jj++) {
		    cplist->xrisk += xtemp[jj] * rp.wt[j];
		    cplist->xstd  += xtemp[jj]*xtemp[jj] * rp.wt[j];
#if DEBUG > 1
		    if (debug>1) printf("  cp=%f, pred=%f, xtemp=%f\n",
					cp[jj]/old_wt, xpred[jj], xtemp[jj]);
#endif
		    cplist = cplist->forward;
		    }
		}
	    }
	free_tree(xtree, 1);
	}

    for (cplist = cptable_head; cplist!=0; cplist=cplist->forward) {
	cplist->xstd = sqrt( cplist->xstd -
				      cplist->xrisk* cplist->xrisk/total_wt);
	}
    rp.alpha=alphasave;
    for (i=0; i<rp.n; i++) rp.which[i] = savew[i];
    Free(savew);
    Free(xtemp);
    }
