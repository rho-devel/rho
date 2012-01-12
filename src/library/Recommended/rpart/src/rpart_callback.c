/*
**  SCCS @(#)rpart_callback.c	1.4 08/13/01
** callback routines for "user" splitting functions in rpart
*/
#include <R.h>
#include <Rinternals.h>
#define FLOAT double
/* don't include rpart.h: it conflicts */

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rpart", String)
#else
#define _(String) (String)
#endif


static int ysave;       /* number of columns of y  */
static int rsave;       /* the length of the returned "mean" from the
			      user's eval routine */
static SEXP expr1;  /* the evaluation expression for splits */
static SEXP expr2;  /* the evaluation expression for values */
static SEXP rho;

static double *ydata;   /* pointer to the data portion of yback */
static double *xdata;	/* pointer to the data portion of xback */
static double *wdata;	/* pointer to the data portion of wback */
static int   *ndata;	/* pointer to the data portion of nback */

/*
** The first routine saves away the parameters, the location
**   of the evaluation frame and the 2 expressions to be computed within it,
**   and ferrets away the memory location of the 4 "callback" objects.
*/
SEXP init_rpcallback(SEXP rhox, SEXP ny, SEXP nr,
		     SEXP expr1x, SEXP expr2x)
{
    SEXP stemp;

    rho = rhox;
    ysave  = asInteger(ny );
    rsave  = asInteger(nr);
    expr1  = expr1x;
    expr2  = expr2x;

    stemp = findVarInFrame(rho, install("yback"));
    if(!stemp) error(_("'yback' not found"));
    ydata = REAL(stemp);
    stemp = findVarInFrame(rho, install("wback"));
    if(!stemp) error(_("'wback' not found"));
    wdata = REAL(stemp);
    stemp = findVarInFrame(rho, install("xback"));
    if(!stemp) error(_("'xback' not found"));
    xdata = REAL(stemp);
    stemp = findVarInFrame(rho, install("nback"));
    if(!stemp) error(_("'nback' not found"));
    ndata = INTEGER(stemp);

    return(R_NilValue);
    }

/*
** This is called by the usersplit init function
**  For the "hardcoded" user routines, this is a constant written into
**  their init routine, but here we need to grab it from outside.
*/
void rpart_callback0(int *nr) {
    *nr = rsave;
    }

/*
** This is called by the evaluation function
*/
void rpart_callback1(int n, double *y[], double *wt, double *z) {
    int i,j, k;
    SEXP value;
    double *dptr;

    /* Copy n and wt into the parent frame */
    k=0;
    for (i=0; i<ysave; i++) {
	for (j=0; j<n; j++) {
	    ydata[k] = y[j][i];
	    k++;
	    }
	}

    for (i=0; i<n; i++) {
	wdata[i] = wt[i];
	}
    ndata[0] = n;

    /* 
    **  Evaluate the saved expression in the parent frame
    **   The result should be a vector of numerics containing the
    **   "deviance" followed by the "mean"
    */

    /* no need to protect as no memory allocation (or error) below */
    value = eval(expr2, rho);
    if (!isReal(value)) {
	error(_("return value not a vector"));
    }
    if (LENGTH(value) != (1 + rsave))
	error(_("returned value is the wrong length"));
    dptr = REAL(value);
    for (i=0; i<=rsave; i++) z[i] = dptr[i];
    }

/*
** This part is called by the rpart "split" function
**   It is expected to return an n-1 length vector of "goodness of split"
*/
void rpart_callback2(int n, int ncat, double *y[], double *wt, 
		     FLOAT *x, double *good) {
    int i, j, k;
    SEXP goodness;
    double *dptr;

    k=0;
    for (i=0; i<ysave; i++) {
	for (j=0; j<n; j++) {
	    ydata[k] = y[j][i];
	    k++;
	    }
	}

    for (i=0; i<n; i++) {
	wdata[i] = wt[i];
	xdata[i] = x[i];
	}
    if (ncat >0) {
	ndata[0] = -n;  /*the negative serves as a marker for rpart.s */
	}
    else ndata[0] =n;  

    /* no need to protect as no memory allocation (or error) below */
    goodness = eval(expr1, rho);
    if (!isReal(goodness))
	error(_("the expression expr1 did not return a vector!"));
    j = LENGTH(goodness);

    /* yes, the lengths have already been checked in the C code  ---
       call this extra documenation then */
    if (ncat==0) {
	if (j != 2*(n-1)) 
	    error(
		_("the expression expr1 returned a list of %d elements, %d required"),
		j, 2*(n-1));

	dptr = REAL(goodness);
	for (i=0; i<j; i++) good[i] = dptr[i];
	}
    else {
	/* 
	** If not all categories were present in X, then the return list
	**   will have 2(#categories present) -1 elements
	** The first element of "good" contains the number of groups found
	*/
	dptr = REAL(goodness);
	good[0] = (j+1)/2;
	for (i=0; i<j; i++) good[i+1] = dptr[i];
	}


    /* There is a memory growth here (yes?) -- should release the goodness 
    **  object right now.  There will be LOTS of them, and they won't
    **  go away until the parent routine is done. But there is no
    **  public macro to do it.
    */
    }
