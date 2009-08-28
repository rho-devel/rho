#include "sparseQR.h"

SEXP sparseQR_validate(SEXP x)
{
    CSP V = AS_CSP__(GET_SLOT(x, install("V"))),
	R = AS_CSP__(GET_SLOT(x, install("R")));
    SEXP beta = GET_SLOT(x, install("beta")),
	p = GET_SLOT(x, Matrix_pSym),
	q = GET_SLOT(x, install("q"));
    int	lq = LENGTH(q);
    R_CheckStack();

    if (LENGTH(p) != V->m)
	return mkString(_("length(p) must match nrow(V)"));
    if (LENGTH(beta) != V->m)
	return mkString(_("length(beta) must match nrow(V)"));
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
 * @param y contents of a V->m by nrhs dense matrix
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
	error(_("Dimensions of system are inconsistent"));
    for (j = 0; j < ydims[1]; j++) {
	double *yj = y + j * m;
	if (trans) {
	    cs_pvec(p, yj, x, m);	/* x(0:m-1) = y(p(0:m-1, j)) */
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


SEXP sparseQR_qty(SEXP qr, SEXP y, SEXP trans)
{
    SEXP ans = PROTECT(dup_mMatrix_as_dgeMatrix(y));
    CSP V = AS_CSP__(GET_SLOT(qr, install("V")));
    R_CheckStack();

    sparseQR_Qmult(V, REAL(GET_SLOT(qr, install("beta"))),
		   INTEGER(GET_SLOT(qr, Matrix_pSym)),
		   asLogical(trans),
		   REAL(GET_SLOT(ans, Matrix_xSym)),
		   INTEGER(GET_SLOT(ans, Matrix_DimSym)));
    UNPROTECT(1);
    return ans;
}

SEXP sparseQR_coef(SEXP qr, SEXP y)
{
    SEXP ans = PROTECT(dup_mMatrix_as_dgeMatrix(y)),
	qslot = GET_SLOT(qr, install("q"));
    CSP V = AS_CSP__(GET_SLOT(qr, install("V"))),
	R = AS_CSP__(GET_SLOT(qr, install("R")));
    int *ydims = INTEGER(GET_SLOT(ans, Matrix_DimSym)),
	*q = INTEGER(qslot),
	j, lq = LENGTH(qslot), m = R->m, n = R->n;
    double *ax = REAL(GET_SLOT(ans, Matrix_xSym)),
	*x = Alloca(m, double);
    R_CheckStack();
    R_CheckStack();

    /* apply row permutation and multiply by Q' */
    sparseQR_Qmult(V, REAL(GET_SLOT(qr, install("beta"))),
		   INTEGER(GET_SLOT(qr, Matrix_pSym)), 1,
		   REAL(GET_SLOT(ans, Matrix_xSym)), ydims);
    for (j = 0; j < ydims[1]; j++) {
	double *aj = ax + j * m;
	cs_usolve(R, aj);
	if (lq) {
	    cs_ipvec(q, aj, x, n);
	    Memcpy(aj, x, n);
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP sparseQR_resid_fitted(SEXP qr, SEXP y, SEXP resid)
{
    SEXP ans = PROTECT(dup_mMatrix_as_dgeMatrix(y));
    CSP V = AS_CSP__(GET_SLOT(qr, install("V")));
    int *ydims = INTEGER(GET_SLOT(ans, Matrix_DimSym)),
	*p = INTEGER(GET_SLOT(qr, Matrix_pSym)),
	i, j, m = V->m, n = V->n, res = asLogical(resid);
    double *ax = REAL(GET_SLOT(ans, Matrix_xSym)),
	*beta = REAL(GET_SLOT(qr, install("beta")));
    R_CheckStack();

    /* apply row permutation and multiply by Q' */
    sparseQR_Qmult(V, beta, p, 1, ax, ydims);
    for (j = 0; j < ydims[1]; j++) {
	if (res)		/* zero first n rows */
	    for (i = 0; i < n; i++) ax[i + j * m] = 0;
	else 			/* zero last m - n rows */
	    for (i = n; i < m; i++) ax[i + j * m] = 0;
    }
    /* multiply by Q and apply inverse row permutation */
    sparseQR_Qmult(V, beta, p, 0, ax, ydims);

    UNPROTECT(1);
    return ans;
}
