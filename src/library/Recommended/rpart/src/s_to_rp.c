/* SCCS @(#)s_to_rp.c	1.17 06/06/01  */
/*
** An S interface to the the recursive partitioning routines.
*/
#include <stdio.h>
#include "rpart.h"
#include "node.h"
#include "rpartS.h"
#include "rpartproto.h"

static struct cptable cptab;
static struct node *tree;
static int *savewhich;

void s_to_rp(Sint *n, 	  Sint *nvarx, 	 Sint *ncat, 	Sint *method, 
	     double *opt, double *parms, Sint *xvals,   Sint *x_grp,
	     double *y,   FLOAT *xmat,   Sint *missmat, char **error,
	     double *wt,  Sint  *ny,     double *cost)
    {
    int itemp;
    int maxpri;
    int rval;      /* return value */
    savewhich = (int *) CALLOC((int)*n, sizeof(int));
    /*
    **  The opt string is in the order of control.rpart()
    **    minsplit, minbucket, cp, maxcomptete, maxsurrogate, usesurrogate,
    **    and xval
    */
    maxpri = opt[3] +1;
    rval = rpart( (int)*n,    (int)*nvarx,   ncat,        (int)*method,
	           maxpri,      parms,       y,            xmat,        
		  missmat,     &cptab,      &tree,        &(error[0]), 
		  savewhich,  (int)*xvals,  x_grp,         wt,         
		  opt,        (int)ny[0],   cost);
    /*
    ** count up the number of nodes, splits, categorical splits, and cp's
    */
    rpcountup(tree, n, nvarx, &itemp);
    ncat[0] = itemp;
    *method = rp.num_unique_cp;
    if (rval==1) *n= -1;   /* signal an error */
    }

/*
** The routine above returns the sizes of the objects, and saves the lists
**   (the list heads are static).  S then calls again with appropriately
**   sized arrays to this routine. This stuffs the arrays and frees the memory
*/
void s_to_rp2(Sint *n,         Sint *nsplit,    Sint *nnode,     Sint *ncat, 
	      Sint *numcat,    Sint *maxcat,    Sint *xvals,     Sint *which, 
	      double *cptable, double *dsplit,  Sint *isplit,    Sint *csplit,
	      double *dnode,   Sint *inode)
    {
    int i;
    int  nodenum, j;
    struct cptable *cp, *cp2;
    double **ddnode  , *ddsplit[3];
    Sint   *iinode[6], *iisplit[3];
    Sint   **ccsplit;
    double scale;
    /*
    ** create the "ragged array" pointers to the matrices
    */
    ddnode = (double **) ALLOC(3+rp.num_resp, sizeof(double *));
    for (i=0; i<(3+rp.num_resp); i++) {
	ddnode[i] = dnode;  dnode  += *nnode;
	}
    for (i=0; i<3; i++) {
	ddsplit[i]= dsplit; dsplit += *nsplit;
	}
    for (i=0; i<6; i++) {
	iinode[i] = inode;  inode  += *nnode;
	}
    for (i=0; i<3; i++) {
	iisplit[i]= isplit; isplit += *nsplit;
	}

    /* I don't understand this next line.  Even if I don't need ccsplit
    ** (maxcat=0), not allocating it makes S memory fault.  Not that
    **  4 extra bytes is any big deal....
    */
    if (*maxcat==0) i=1; else i = *maxcat;
    ccsplit = (Sint **)CALLOC(i, sizeof(Sint *));
    for (i=0; i<*maxcat; i++) {
	ccsplit[i] = csplit;   csplit += *ncat;
	}

    /* retrieve the complexity table */
    scale = 1/tree->risk;
    i=0;
    for (cp = &cptab; cp !=0; cp= cp->forward) {
	cptable[i++] = cp->cp * scale;
	cptable[i++] = cp->nsplit;
	cptable[i++] = cp->risk * scale;
	if (*xvals >1) {
	    cptable[i++] = cp->xrisk*scale;
	    cptable[i++] = cp->xstd *scale;
	    }
	}

    /* Now get the tree */
    *nnode=0; *nsplit=0; *ncat=0;   /*array starting points */
    rpmatrix(tree, nnode, nsplit, ncat, numcat,
		    ddsplit, iisplit, ccsplit, ddnode, iinode, 1);

    /*
    ** Now fix up the 'which' array
    **   It would be a simple S match(), except that nodes sometimes get cut
    */
    for (i=0; i<*n; i++) {
	nodenum = savewhich[i];
	do {
	    for (j=0; j< *nnode; j++)
		if (iinode[0][j] == nodenum) {
		    which[i] = j+1;
		    break;
		    }
	    nodenum /=2;
	    }  while (j >= *nnode);
	}

    /*
    ** restore the memory
    **  since the root was not calloced, I have to not free it (second arg
    **  of free_tree).
    */
    free_tree(tree, 0);
    for (cp=cptab.forward; cp!=0; ) {
	cp2 = cp->forward;
	Free(cp);
	cp = cp2;
	}
    Free(ccsplit);
    Free(savewhich);
    }

