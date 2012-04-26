/* SCCS  @(#)nodesplit.c	1.5 06/06/01 */
/*
** nodesplit -- Split the node in two, and keep a count as we do of how
**  many splits are determined by each surrogate variable.
*/
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void nodesplit(struct node *me, int nodenum)
    {
    int i,j, k;
    struct split *tsplit;
    int    var,
	   extra,
	   lastisleft,
	   someleft;
    FLOAT  psplit;
    int    *index;
    int    *which;
    Sint   **sorts;
    FLOAT  **xdata;

    which = rp.which;
    sorts = rp.sorts;
    xdata = rp.xdata;
    /*
    ** Walk through the variables (primary, then surrogate 1, then surr 2...)
    **   and reassign "which"
    */
    tsplit = me->primary;
    var = tsplit->var_num;
    someleft =0;
    if (rp.numcat[var] >0) {
	index = tsplit->csplit;
	for (i=0; i<rp.n; i++) {
	    if (which[i] != nodenum) continue;
	    if (sorts[var][i] <0) someleft++;
	    else switch (index[(int)xdata[var][i]-1]) {
		case LEFT:  which[i] = 2*nodenum;  break;
		case RIGHT: which[i] = 2*nodenum +1;  break;
		}
	    }
	}
    else {
	psplit = tsplit->spoint;
	extra = tsplit->csplit[0];
	for (i=0; i<rp.n; i++) {
	    j = sorts[var][i];
	    if ( j <0) {
		if (which[-(j+1)]==nodenum) someleft++;
		}
	    else if (which[j]==nodenum){
		if (xdata[var][i] < psplit)  k = extra;
		else                         k = -extra;
		if (k==LEFT) which[j] = 2*nodenum;
		else         which[j] = 2*nodenum +1;
		}
	    }
	}

    /* Now the surrogates */
    if (rp.usesurrogate==0) return;
    for (tsplit=me->surrogate;  tsplit!=0 && someleft>0;
				   tsplit=tsplit->nextsplit) {
	someleft=0;
	var = tsplit->var_num;
	if (rp.numcat[var] >0) {
	    index = tsplit->csplit;
	    for (i=0; i<rp.n; i++) {
		if (which[i]!=nodenum) continue;
		if (sorts[var][i] <0) someleft++;
		else {
		    k = xdata[var][i];    /*the value of the surrogate var */
		    /*
		    ** The need for the if stmt below may not be
		    **  obvious. The surrogate's value must not be missing,
		    **  AND there must have been at least 1 person with
		    **  both this level of the surrogate and a primary
		    **  split value somewhere in the node.  If everyone in
		    **  this node with level k of the surrogate also had a
		    **  missing value of the primary variable, then index[k-1]
		    **  will be zero.
		    */
		    if (index[k-1] !=0) {
			tsplit->count++;
			if (index[k-1] == LEFT) which[i] = 2*nodenum;
			else                    which[i] = 2*nodenum +1;
			}
		    else someleft++;
		    }
		}
	    }
	else {
	    psplit= tsplit->spoint;
	    extra = tsplit->csplit[0];
	    for (i=0; i<rp.n; i++) {
		j = sorts[var][i];
		if (j<0)  { /* missing */
		    if (which[-(j+1)]==nodenum) someleft++;
		    }
		else if (which[j]==nodenum){
		    tsplit->count++;
		    if (xdata[var][i] < psplit) k = extra;
		    else                        k = -extra;
		    if (k==LEFT)  which[j] = 2*nodenum;
		    else          which[j] = 2*nodenum +1;
		    }
		}
	    }
	}

    if (someleft>0 && rp.usesurrogate==2) {
	/* all surrogates missing, use the default */
	i = me->lastsurrogate;
	if (i !=0) {   /*50-50 splits are possible */
	    if (i < 0) lastisleft = 2*nodenum;
	    else       lastisleft = 2*nodenum +1;

	    for (i=0; i<rp.n; i++) {
		if (which[i] == nodenum) which[i] = lastisleft;
		}
	    }
	}
    }
