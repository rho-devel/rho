/* SCCS @(#)rpart.c	1.13 08/13/01    */
/*
** The main entry point for recursive partitioning routines.
**
** Input variables:
**      n       = # of observations
**      nvarx   = # of columns in xmat
**      ncat    = # categories for each var, 0 for continuous variables.
**      method  = 1 - anova
**                2 - exponential survival
**		  3 - classification
**	          4 - user defined callback
**      maxpri  = max number of primary variables to retain (must be >0)
**      parms   = extra parameters for the split function, e.g. poissoninit
**      ymat    = matrix or vector of response variables
**      xmat    = matrix of continuous variables
**      missmat = matrix that indicates missing x's.  1=missing.
**      cptable  = a pointer to the root of the complexity parameter table
**      tree     = a pointer to the root of the tree
**      error    = a pointer to an error message buffer
**      xvals    = number of cross-validations to do
**      xgrp     = indices for the cross-validations
**      wt       = vector of case weights
**      opt      = options, in the order of rpart.control()
**	ny       = number of columns in the input y matrix
**      cost     = vector of variable costs
**
** Returned variables
**      error    = text of the error message
**      which    = final node for each observation
**
** Return value: 0 if all was well, 1 for error
**
*/
#define MAINRP
#include <stdio.h>
#include <math.h>
#include "rpart.h"
#include "node.h"
#include "func_table.h"
#include "rpartS.h"
#include "rpartproto.h"

int rpart(int n,         int nvarx,      Sint *ncat,     int method, 
          int  maxpri,   double *parms,  double *ymat,   FLOAT *xmat,
          Sint *missmat, struct cptable *cptable,
	  struct node **tree,            char **error,   int *which,
	  int xvals,     Sint *x_grp,    double *wt,     double *opt,
	  int ny,        double *cost) {
    int i,k;
    int maxcat;
    double temp;

    /*
    ** initialize the splitting functions from the function table
    */
    if (method <= NUM_METHODS) {
	i = method -1;
	rp_init   = func_table[i].init_split;
	rp_choose = func_table[i].choose_split;
	rp_eval   = func_table[i].eval;
	rp_error  = func_table[i].error;
	rp.num_y  = ny;
	}
    else {
	*error = "Invalid value for 'method'";
	return(1);
	}

    /*
    ** set some other parameters
    */
    rp.min_node =  (int) opt[1];
    rp.min_split = (int) opt[0];
    rp.complexity= opt[2];
    rp.maxsur = (int) opt[4];
    rp.usesurrogate = (int) opt[5];
    rp.sur_agree = (int) opt[6];
    rp.maxnode  = (int) pow((double)2.0, opt[7]) -1;
    rp.nvar = nvarx;
    rp.numcat = ncat;
    rp.maxpri = maxpri;
    if (maxpri <1) rp.maxpri =1;
    rp.n = n;
    rp.which = which;
    rp.wt    = wt;
    rp.iscale = 0.0;
    rp.vcost  = cost;

    /*
    ** create the "ragged array" pointers to the matrix
    **   x and missmat are in column major order
    **   y is in row major order
    */
    rp.xdata = (FLOAT **) ALLOC(nvarx, sizeof(FLOAT *));
    for (i=0; i<nvarx; i++) {
	rp.xdata[i] = &(xmat[i*n]);
	}
    rp.ydata = (double **) ALLOC(n, sizeof(double *));
    for (i=0; i<n; i++)  rp.ydata[i] = &(ymat[i*rp.num_y]);
    /*
    ** allocate some scratch
    */
    rp.tempvec = (int *)ALLOC(n, sizeof(int));
    rp.xtemp = (FLOAT *)ALLOC(n, sizeof(FLOAT));
    rp.ytemp = (double **)ALLOC(n, sizeof(double *));
    rp.wtemp = (double *)ALLOC(n, sizeof(double));

    /*
    ** create a matrix of sort indices, one for each continuous variable
    **   This sort is "once and for all".  The result is stored on top
    **   of the 'missmat' array.
    ** I don't have to sort the categoricals.
    */
    rp.sorts  = (Sint**) ALLOC(nvarx, sizeof(Sint *));
    maxcat=0;
    for (i=0; i<nvarx; i++) {
	rp.sorts[i] = &(missmat[i*n]);
	for (k=0; k<n; k++) {
	    if (rp.sorts[i][k]==1) {
		rp.tempvec[k] = -(k+1);
		rp.xdata[i][k]=0;   /*weird numerics might destroy 'sort'*/
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
	rp.lwt    = (double *) ALLOC(2*maxcat, sizeof(double));
	rp.left = rp.csplit + maxcat;
	rp.right= rp.left   + maxcat;
	rp.rwt  = rp.lwt    + maxcat;
	}
    else rp.csplit = (int *)ALLOC(1, sizeof(int));

    /*
    ** initialize the top node of the tree
    */
    temp =0;
    for (i=0; i<n; i++) {
	which[i] =1;
	temp += wt[i];
	}
    i = rp_init(n, rp.ydata, maxcat, error, parms, &rp.num_resp, 1, wt);
    nodesize = sizeof(struct node) + (rp.num_resp-2)*sizeof(double);
    *tree = (struct node *) CALLOC(1, nodesize);
    (*tree)->num_obs = n;
    (*tree)->sum_wt  = temp;
    if (i>0) return(i);

    (*rp_eval)(n, rp.ydata, (*tree)->response_est, &((*tree)->risk), wt);
    (*tree)->complexity = (*tree)->risk;
    rp.alpha = rp.complexity * (*tree)->risk;

    /*
    ** Do the basic tree
    */
    partition(1, (*tree), &temp);
    cptable->cp = (*tree)->complexity;
    cptable->risk = (*tree)->risk;
    cptable->nsplit = 0;
    cptable->forward =0;
    cptable->xrisk =0;
    cptable->xstd =0;
    rp.num_unique_cp =1;

    if ((*tree)->rightson ==0) return(0); /* Nothing more needs to be done */
    make_cp_list((*tree), (*tree)->complexity, cptable);
    make_cp_table((*tree), (*tree)->complexity, 0);

    if (xvals >1 && (*tree)->rightson !=0) 
	xval(xvals, cptable, x_grp, maxcat, error, parms);
    /*
    ** all done
    */
    return(0);
    }
