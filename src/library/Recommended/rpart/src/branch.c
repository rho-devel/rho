/* SCCS  @(#)branch.c	1.5 06/06/01 */
/*
** Walk an observation 'one more split' down the tree.  If there are no
**   more splits, return 0, otherwise return the address of the new node.
** A return of zero also comes about if surrogates aren't being used, and I
**   hit a missing value.
*/
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

struct node *branch(struct node *tree, int obs)
    {
    int i, j, dir;
    struct node *me;
    struct split *tsplit;
    FLOAT **xdata;
    Sint   **sorts;
    if (tree->leftson ==0) return(0);

    me = tree;
    xdata = rp.xdata;
    sorts = rp.sorts;
    /*
    ** choose left or right son
    **   this may use lots of surrogates before we're done
    */
    tsplit = me->primary;
    j= tsplit->var_num;
    if (rp.numcat[j]==0) { /* continuous */
	for (i=0; i<rp.n; i++) {
	    if (sorts[j][i] == obs) {  /* found the match */
		if (xdata[j][i] < tsplit->spoint)
			dir =  tsplit->csplit[0];
		   else dir = -tsplit->csplit[0];
		goto down;
		}
	    }
	}
    else {
	dir = (tsplit->csplit)[(int)xdata[j][obs] -1];
	if (dir !=0) goto down;
	}

    if (rp.usesurrogate==0) return(0);
    /*
    ** use the surrogates
    */
    for (tsplit= me->surrogate; tsplit !=0; tsplit= tsplit->nextsplit) {
	j = tsplit->var_num;
	if (rp.numcat[j]==0) {
	    for (i=0; i<rp.n; i++) {
		if (sorts[j][i] == obs) {
		    if (xdata[j][i] < tsplit->spoint)
			    dir =  tsplit->csplit[0];
		       else dir = -tsplit->csplit[0];
		    goto down;
		    }
		}
	    }
	else {
	    dir = (tsplit->csplit)[(int)xdata[j][obs] -1];
	    if (dir !=0) goto down;
	    }
	}


    if (rp.usesurrogate <2) return(0);
    /*
    ** split it by default
    */
    dir = me->lastsurrogate;

down:if (dir==LEFT) return(me->leftson);
     else           return(me->rightson);
    }
