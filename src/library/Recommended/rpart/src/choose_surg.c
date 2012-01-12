/* SCCS @(#)choose_surg.c	1.6 06/06/01  */
/*
** A particular split routine, optimized for the surrogate variable
**  search.  The "goodness" of a split is the total weights of concordant
**  observations between the surrogate and the primary split.
**  Note that the CART folks use the %concordance, which factors missing
**  values into the equations somewhat differently.
**
**  y is coded as  +1=left, -1=right, 0=missing 
** 
*/
#include "rpart.h"
#include "rpartproto.h"

void choose_surg(int nodenum,    int *y,         FLOAT *x,     Sint *order, 
		 int ncat,    double *agreement, FLOAT *split, int *csplit,
		 double tleft,double tright,     double *adj)
    {
    int i,j;
    int agree;
    int lcount, rcount;
    int ll, lr, rr, rl;
    double llwt, lrwt, rrwt, rlwt;   /* sum of weights for each */
    int defdir;
    FLOAT lastx = 0.0;
    int  *which, *left, *right;
    double *lwt, *rwt;
    double majority, total_wt;

    which = rp.which;
    left = rp.left;
    right =rp.right;
    lwt   =rp.lwt;
    rwt   =rp.rwt;

    if (ncat==0) {  /* continuous case */
	/*
	** ll = y's that go left that are also sent left by my split
	** lr = y's that go left that I send right
	** rl= y's that go right that I send to the left
	** rr= y's that go right that I send to the right
	**
	** The agreement is max(ll+rr, lr+rl), if weights were =1;
	**   actually max(llwt + rrwt, lrwt + rlwt)/ denominator
	**
	** I enforce that at least 2 obs must go each way, to avoid having an
	**  uncorrelated surrogate beat the "null" surrogate too easily
	*/
	ll = rl =0;
	llwt =0; rlwt =0;
	for (i=rp.n-1; i>=0; i--) { /*start with me sending all to the left */
	    j = order[i];
	    if (j>=0 &&  which[j]==nodenum) {  
		lastx = x[i];        /*this is why I ran the loop backwards*/
		switch( y[j]) {
		    case LEFT : ll++;
			        llwt += rp.wt[j];
				break;
		    case RIGHT: rl++;
			        rlwt += rp.wt[j];
				break;
		    default   :;
		    }
		}
	    }
	lr = rr =0;
	lrwt =0; rrwt=0;
	if (llwt > rlwt) agree = llwt;
	else             agree = rlwt;

	majority   = agree;             /*worst possible agreement */
	total_wt  = llwt + rlwt;
	/*
	**  March across, moving things from the right to the left
	**    the "lastx" code is caring for ties in the x var
	**    (The loop above sets it to the first unique x value).
	*/
	for (i=0; (ll+rl)>=2; i++) {
	    j = order[i];
	    if (j >=0 &&  which[j]==nodenum) {       /* obs is in this node */
		if ((lr+rr)>=2  &&  x[i] != lastx) {
		    /* new x found, evaluate the split */
		    if ((llwt +rrwt) > agree) {
			agree = llwt + rrwt;
			csplit[0] = RIGHT;       /* < goes to the right */
			*split = (x[i] + lastx)/2;
			}
		    else if ((lrwt +rlwt) > agree) {
			agree = lrwt + rlwt;
			csplit[0] = LEFT;
			*split = (x[i] + lastx)/2;
			}
		    }

		switch (y[j]) {    /* update numbers */
		    case LEFT : ll--; lr++;
			        llwt -= rp.wt[j];
				lrwt += rp.wt[j];
				break;
		    case RIGHT: rl--; rr++;
			        rlwt -= rp.wt[j];
				rrwt += rp.wt[j];
				break;
		    default   : ;         /*ignore missing y's */
		    }
		lastx = x[i];
		}
	    }
	}

    else {     /* categorical predictor */
	for (i=0; i<ncat; i++) {
	    left[i] =0;
	    right[i]=0;
	    lwt[i] = 0;
	    rwt[i] = 0;
	    }

	/* First step:
	**  left = table(x[y goes left]), right= table(x[y goes right])
	**  so left[2] will be the number of x==2's that went left,
	**  and lwt[2] the sum of the weights for those observations.
	*/
	for (i=0; i<rp.n; i++) {
	    if (which[i] == nodenum &&  order[i]>=0) {
		j = (int)x[i] -1;
		switch( y[i]) {
		    case LEFT : left[j]++;
			        lwt[j] += rp.wt[i];
				break;
		    case RIGHT: right[j]++;
			        rwt[j] += rp.wt[i];
				break;
		    default:;
		    }
		}
	    }

	/*
	**  Compute which is better: everyone to the right, or all go left
	*/
	lcount=0; rcount=0;
	llwt =0;  rrwt =0;
	for (i=0; i<ncat; i++) {
	    lcount += left[i];
	    rcount += right[i];
	    llwt += lwt[i];
	    rrwt += rwt[i];
	    }
	if (llwt > rrwt) {
	    defdir= LEFT;
	    majority= llwt;
	    }
	else {
            defdir= RIGHT;
	    majority =rrwt;
	    }
	total_wt  = llwt + rrwt;

	/* 
	** We can calculate the best split category by category--- send each
	**  x value individually to its better direction
	*/
	agree =0;
	for (i=0; i<ncat; i++) {
	    if (left[i]==0 && right[i]==0) csplit[i]=0;
	    else {
		if (lwt[i]< rwt[i] || (lwt[i]==rwt[i] && defdir==RIGHT)) {
		    agree+= rwt[i];
		    csplit[i] = RIGHT;
		    }
		else {
		    agree += lwt[i];
		    csplit[i] = LEFT;
		    }
		}
	    }
	}

    /*
    **  Now we have the total agreement.  Calculate the %agreement and
    **    the adjusted agreement
    **  For both, do I use the total y vector as my denominator (my
    **    preference), or only the y's for non-missing x (CART book).
    */
    if (rp.sur_agree ==0) { /* use total table */
	total_wt = tleft + tright;
	if (tleft > tright) majority=tleft;
	else                majority=tright;
	}

    *agreement = agree/ total_wt;
    majority /= total_wt;
    *adj = (*agreement -majority)/ (1-majority);
    }
