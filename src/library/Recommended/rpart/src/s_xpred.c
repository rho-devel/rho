/*
**  SCCS  @(#)s_xpred.c	1.16 06/06/01
** An S interface to "cross validated predictions"
**    99% of this routine is a copy of s_to_rp and rpart.c
*/
#include "rpart.h"
#include "node.h"
#include "func_table.h"
#include "rpartS.h"
#include "rpartproto.h"

void s_xpred(Sint *sn, 	   Sint *nvarx,   Sint *ncat,    Sint *method, 
	     double *opt,  double *parms, Sint *xvals,   Sint *x_grp,
	     double *ymat, FLOAT *xmat,   Sint *missmat, double *predict,
	     Sint *ncp,    double *cp,    char **error,  double *wt,
	     Sint *ny,     double *cost)
    {
    int i,j,k;
    int maxcat;
    double temp;
    int n, nvar;
    int maxpri;
    struct node *xtree;
    double old_wt, total_wt;

    /*
    ** initialize the splitting functions from the function table
    */
    if (*method <= NUM_METHODS) {
	i = *method -1;
	rp_init   = func_table[i].init_split;
	rp_choose = func_table[i].choose_split;
	rp_eval   = func_table[i].eval;
	rp_error  = func_table[i].error;
	rp.num_y  = *ny;
	}
    else {
	*error = _("invalid value for 'method'");
	*sn = -1; 
	return;
	}

    /*
    ** Set other parameters
    **
    **  The opt string is in the order of control.rpart()
    **    minsplit, minbucket, cp, maxcomptete, maxsurrogate, usesurrogate,
    **    and xval
    */
    n = *sn;
    nvar = *nvarx;
    rp.nvar =  nvar;
    rp.numcat = ncat;
    rp.n = n;
    rp.num_unique_cp = *ncp;
    rp.wt = wt;

    rp.min_node =  opt[1];
    rp.min_split = opt[0];
    rp.complexity= opt[2];
    maxpri       = opt[3] +1;
    rp.maxpri = maxpri;
    if (maxpri <1) rp.maxpri =1;
    rp.maxsur = opt[4];
    rp.usesurrogate = opt[5];
    rp.sur_agree = opt[6];
    rp.maxnode  = pow((double)2.0, opt[7]) -1;
    rp.vcost    = cost;

    /*
    ** create the "ragged array" pointers to the matrix
    **   x and missmat are in column major order
    **   y is in row major order
    */
    rp.xdata = (FLOAT **) ALLOC(nvar, sizeof(FLOAT *));
    for (i=0; i<nvar; i++) {
	rp.xdata[i] = &(xmat[i*n]);
	}
    rp.ydata = (double **) ALLOC(n, sizeof(double *));
    for (i=0; i<n; i++)  rp.ydata[i] = &(ymat[i*rp.num_y]);

    /*
    ** allocate some scratch
    */
    rp.tempvec = (int *)ALLOC(2*n, sizeof(int));
    rp.which   = rp.tempvec +n;
    rp.xtemp = (FLOAT *)ALLOC(n, sizeof(FLOAT));
    rp.ytemp = (double **)ALLOC(n, sizeof(double *));
    rp.wtemp = (double *)ALLOC(n, sizeof(double));

    /*
    ** create a matrix of sort indices, one for each continuous variable
    **   This sort is "once and for all".  The result is stored on top
    **   of the 'missmat' array.
    ** I don't have to sort the categoricals.
    */
    rp.sorts  = (Sint**) ALLOC(nvar, sizeof(Sint *));
    maxcat=0;
    for (i=0; i<nvar; i++) {
	rp.sorts[i] = &(missmat[i*n]); 
	for (k=0; k<n; k++) {
	    if (rp.sorts[i][k]==1) {
		rp.tempvec[k] = -(k+1);
		rp.xdata[i][k] =0;     /* avoid weird numerics in S's NA */
	        }
	    else                   rp.tempvec[k] =  k;
	    }
	if (ncat[i]==0)  mysort(0, n-1, rp.xdata[i], rp.tempvec); 
	else if (ncat[i] > maxcat)  maxcat = ncat[i];
        for (k=0; k<n; k++) rp.sorts[i][k] = rp.tempvec[k];
	}

    /*
    ** And now the last of my scratch space
    */
    if (maxcat >0) {
	rp.csplit = (int *) ALLOC(3*maxcat, sizeof(int));
	rp.left = rp.csplit + maxcat;
	rp.right= rp.left   + maxcat;
        rp.lwt    = (double *) ALLOC(2*maxcat, sizeof(double));
        rp.rwt  = rp.lwt    + maxcat;
	}
    else rp.csplit = (int *)ALLOC(1, sizeof(int));

    (*rp_init)(n, rp.ydata, maxcat, error, parms, &rp.num_resp, 1, rp.wt);
    nodesize = sizeof(struct node) + (rp.num_resp-2)*sizeof(double);

    /*
    ** I need the risk of the full tree, to scale alpha
    */
    xtree = (struct node *) ALLOC(1, nodesize);
    (*rp_eval)(n, rp.ydata, xtree->response_est, &(xtree->risk), rp.wt);
    rp.alpha = rp.complexity * (xtree)->risk;

    /*
    ** do the validations
    */
    total_wt =0;
    for (i=0; i<rp.n; i++) total_wt += rp.wt[i];
    old_wt = total_wt;

    for (i=0; i< *xvals; i++) {
	/*
	** mark the "leave out" data as ficticious node 0
	*/
	k=0;
	temp =0;
	for (j=0; j<rp.n; j++) {
	    if (x_grp[j]==(i+1)) {
		rp.which[j] =0;
		}
	    else {
		rp.which[j] =1;
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

	/*
	** run the extra data down the new tree
	*/
	for (j=0; j<rp.n; j++) {
	    if (rp.which[j]==0) {
		rundown2(xtree, j, cp, (predict+ j* *ncp));
		}
	    }
	free_tree(xtree, 1);
	}
    }
