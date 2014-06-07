#include "sparseQR.h"

SEXP sparseQR_validate(SEXP x)
{
    CSP V = AS_CSP__(GET_SLOT(x, install("V"))),
	R = AS_CSP__(GET_SLOT(x, install("R")));
    SEXP beta = GET_SLOT(x, install("beta")),
	p = GET_SLOT(x, Matrix_pSym),
	q = GET_SLOT(x, install("q"));
    R_CheckStack();

    if (LENGTH(p) != V->m)
	return mkString(_("length(p) must match nrow(V)"));
    if (LENGTH(beta) != V->n)
	return mkString(_("length(beta) must match ncol(V)"));
    int	lq = LENGTH(q);
    if (lq && lq != R->n)
	return mkString(_("length(q) must be zero or ncol(R)"));
    if (V->n != R->n)
	return mkString("ncol(V) != ncol(R)");
    /* FIXME: Check that the permutations are permutations */
    return ScalarLogical(1);
}

/**
 * Apply Householder transformations and the row permutation P to y
 *
 * @param V sparse matrix containing the vectors defining the
 *        Householder transformations
 * @param beta scaling factors for the Householder transformations
 * @param p 0-based permutation vector of length V->m
 * @param trans logical value - if TRUE create Q'y[p] otherwise Qy[p]
 * @param y contents of a V->m by nrhs, i.e. dim(y) == ydims[0:1], dense matrix
 *        Note that V->m = m2 : V may contain "spurious 0 rows" (structural rank deficiency)
 * @param ydims dimensions of y
 */
static
void sparseQR_Qmult(cs *V, double *beta, int *p, int trans,
		    double *y, int *ydims)
{
    int j, k, m = V->m, n = V->n;
    double *x = Alloca(m, double);	/* workspace */
    R_CheckStack();

    if (ydims[0] != m)
	error(_("sparseQR_Qmult(): nrow(y) = %d != %d = nrow(V)"), ydims[0], m);
    for (j = 0; j < ydims[1]; j++) {
	double *yj = y + j * m;
	if (trans) {
	    cs_pvec(p, yj, x, m);	/* x(0:m-1) = y(p(0:m-1), j) */
	    Memcpy(yj, x, m);	/* replace it */
	    for (k = 0 ; k < n ; k++)   /* apply H[1]...H[n] */
		cs_happly(V, k, beta[k], yj);
	} else {
	    for (k = n - 1 ; k >= 0 ; k--) /* apply H[n]...H[1] */
		cs_happly(V, k, beta[k], yj);
	    cs_ipvec(p, yj, x, m); /* inverse permutation */
	    Memcpy(yj, x, m);
	}
    }
}


/**
* Given a sparse QR decomposition and y,  compute  Q y  or  Q'y
*
* @param qr a "sparseQR" object
* @param y a (dense) Matrix
* @param trans logical, if TRUE compute   Q'y   else  Q y
* @return Q'y ("qty")  or   Qy ("qy")
*/
SEXP sparseQR_qty(SEXP qr, SEXP y, SEXP trans)
{

//--- will be prepended also to other  sparseQR_..() functions below -----------
#define INIT_sparseQR_							\
    CSP V = AS_CSP__(GET_SLOT(qr, install("V")));			\
    R_CheckStack();							\
    SEXP ans, aa;							\
    PROTECT_INDEX ipx;                                                  \
    PROTECT_WITH_INDEX(ans = dup_mMatrix_as_dgeMatrix(y), &ipx);	\
    int *ydims = INTEGER(GET_SLOT(ans, Matrix_DimSym)),			\
	m = ydims[0], n = ydims[1], M = V->m, *d_a;			\
    Rboolean rank_def = (m < M);					\
    if(rank_def) { /* must add 0-rows to y, i.e. ans, and remove them *before* return */ \
	aa = PROTECT(NEW_OBJECT(MAKE_CLASS("dgeMatrix")));		\
	d_a = INTEGER(GET_SLOT(aa, Matrix_DimSym)); d_a[0] = M; d_a[1] = n; \
	SEXP dn = GET_SLOT(aa, Matrix_DimNamesSym);			\
	SET_VECTOR_ELT(dn, 1,						\
		       duplicate(VECTOR_ELT(GET_SLOT(ans, Matrix_DimNamesSym), 1))); \
	SET_SLOT(aa, Matrix_DimNamesSym, dn);				\
	double *yy = REAL( GET_SLOT(ans, Matrix_xSym));     /* m * n */	\
	double *xx = REAL(ALLOC_SLOT(aa, Matrix_xSym, REALSXP, M * n));	\
	for(int j = 0; j < n; j++) { /* j-th column */			\
	    Memcpy(xx + j*M, yy + j*m, m); /* copy          x[   1:m , j ] := yy[,j] */	\
	    for(int i = m; i < M; i++) xx[i + j*M] = 0.;/*  x[(m+1):M, j ] := 0	*/ \
	}								\
	REPROTECT(ans = duplicate(aa), ipx); /* is  M x n  now */	\
	ydims = INTEGER(GET_SLOT(ans, Matrix_DimSym));			\
    }
//--- end {INIT_sparseQR_} -----------------------------------------------------

    INIT_sparseQR_

    sparseQR_Qmult(V, REAL(GET_SLOT(qr, install("beta"))),
		   INTEGER(GET_SLOT(qr, Matrix_pSym)),
		   asLogical(trans),
		   REAL(GET_SLOT(ans, Matrix_xSym)), ydims);
#define EXIT_sparseQR_							\
    /* remove the extra rows from ans */				\
	d_a[0] = m;/* -> @Dim is ok;  @Dimnames (i.e. colnames) still are */ \
	double *yy = REAL( GET_SLOT(ans, Matrix_xSym)); /* is  M  x n */ \
	double *xx = REAL(ALLOC_SLOT(aa, Matrix_xSym, REALSXP, m * n));	\
	for(int j = 0; j < n; j++) { /*  j-th column */			\
	    Memcpy(xx + j*m, yy + j*M, m); /* copy    x[ 1:m, j ] := yy[,j] */ \
	}								\
	ans = duplicate(aa); /*  m x n  finally */			\
	UNPROTECT(1)

    if(rank_def) {
	warning(_("%s(): structurally rank deficient case: possibly WRONG zeros"),
		"sparseQR_qty");
	EXIT_sparseQR_;
    }

    UNPROTECT(1);
    return ans;
}

// Compute  qr.coef(qr, y)  :=  R^{-1} Q' y   {modulo row and column permutations}
SEXP sparseQR_coef(SEXP qr, SEXP y)
{
    SEXP qslot = GET_SLOT(qr, install("q"));
    CSP	R = AS_CSP__(GET_SLOT(qr, install("R")));

    INIT_sparseQR_
	;
    double *ax = REAL(GET_SLOT(ans, Matrix_xSym)); // M x n  (M = V->m = R->m)

    /* apply row permutation and multiply by Q' */
    sparseQR_Qmult(V, REAL(GET_SLOT(qr, install("beta"))),
		   INTEGER(GET_SLOT(qr, Matrix_pSym)), /* trans = */ TRUE,
		   ax, ydims);

  // FIXME: check  n_R, M (= R->m)   vs  n, m
    int *q = INTEGER(qslot), lq = LENGTH(qslot), n_R = R->n;
    double *x = Alloca(M, double);
    R_CheckStack();
    for (int j = 0; j < n; j++) {
	double *aj = ax + j * M;
	cs_usolve(R, aj);
	if (lq) {
	    cs_ipvec(q, aj, x, n_R);
	    Memcpy(aj, x, n_R);
	}
    }

    if(rank_def) {
	warning(_("%s(): structurally rank deficient case: possibly WRONG zeros"),
		"sparseQR_coef");
	EXIT_sparseQR_;
    }

    UNPROTECT(1);
    return ans;
}

/** Compute  qr.resid(qr, y)   or   qr.fitted(qr, y)
*/
SEXP sparseQR_resid_fitted(SEXP qr, SEXP y, SEXP want_resid)
{
    int *p = INTEGER(GET_SLOT(qr, Matrix_pSym)),
	resid = asLogical(want_resid);
    double *beta = REAL(GET_SLOT(qr, install("beta")));

    INIT_sparseQR_

    double *ax = REAL(GET_SLOT(ans, Matrix_xSym));

    /* apply row permutation and multiply by Q' */
    sparseQR_Qmult(V, beta, p, /* trans = */ TRUE, ax, ydims);

// FIXME   (n,m) := dim(y)   vs  (N,M) := dim(V)  -- ok ??
    int N = V->n; // M = V->m  (in INIT_.. above)
    for (int j = 0; j < n; j++) {
	if (resid) // qr.resid(): zero first N rows
	    for (int i = 0; i < N; i++) ax[i + j * M] = 0;
	else // qr.fitted(): zero last M - N rows
	    for (int i = N; i < M; i++) ax[i + j * M] = 0;
    }
    /* multiply by Q and apply inverse row permutation */
    sparseQR_Qmult(V, beta, p, /* trans = */ FALSE, ax, ydims);

    if(rank_def) {
	warning(_("%s(): structurally rank deficient case: possibly WRONG zeros"),
		"sparseQR_resid_fitted");
	EXIT_sparseQR_;
    }

    UNPROTECT(1);
    return ans;
}
