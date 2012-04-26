/* SCCS @(#)rundown2.c	@(#)rundown2.c	1.5 12/13/99 */
/*
** Run an observation down the tree, and return the predicted value,
**    for several CP values at once.
** (A subset of rundown.c, which also returns the prediction error).
*/
#include <stdio.h>
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void rundown2(struct node *tree, int obs, double *cp, double *xpred)
    {
    int i;
    struct node *otree;

    /*
    ** Now, repeat the following: for the cp of interest, run down the tree
    **   until I find a node with smaller complexity.  The parent node will
    **   not have collapsed, but this split will have, so this is my
    **   predictor.
    */
    otree = tree;
    for (i=0; i<rp.num_unique_cp; i++) {
	while ( cp[i] < tree->complexity) {
	    tree = branch(tree, obs);
	    if (tree ==0) goto oops;
	    otree = tree;
	    }
	xpred[i] =  tree->response_est[0];
	}

    return;

oops:;
    if (rp.usesurrogate <2) { /*must have hit a missing value */
	for (; i<rp.num_unique_cp; i++)
	    xpred[i] = otree->response_est[0];
	return;
	}
    /*
    ** I never really expect to get to this code.  It can only happen if
    **  the last cp on my list is smaller than the terminal cp of the
    **  xval tree just built.  This is impossible (I think).  But just in
    **  case I put a message here.
    */
    REprintf("Warning message--see rundown2.c\n");
    }
