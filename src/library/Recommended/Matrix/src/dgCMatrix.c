#include "dgCMatrix.h"

/* for Csparse_transpose() : */
#include "Csparse.h"
#include "chm_common.h"
/* -> Mutils.h / SPQR ... */

/* FIXME -- we "forget" about dimnames almost everywhere : */

/* for dgCMatrix  _and_ lgCMatrix and others  (but *not*  ngC...) : */
SEXP xCMatrix_validate(SEXP x)
{
    /* Almost everything now in Csparse_validate ( ./Csparse.c )
     * *but* the checking of the 'x' slot : */
    if (length(GET_SLOT(x, Matrix_iSym)) !=
	length(GET_SLOT(x, Matrix_xSym)))
	return mkString(_("lengths of slots 'i' and 'x' must match"));

    return ScalarLogical(1);
}

/* for dgRMatrix  _and_ lgRMatrix and others  (but *not*  ngC...) : */
SEXP xRMatrix_validate(SEXP x)
{
    /* Almost everything now in Rsparse_validate ( ./Csparse.c )
     * *but* the checking of the 'x' slot : */
    if (length(GET_SLOT(x, Matrix_jSym)) !=
	length(GET_SLOT(x, Matrix_xSym)))
	return mkString(_("lengths of slots 'j' and 'x' must match"));

    return ScalarLogical(1);
}

/* This and the following R_to_CMatrix() lead to memory-not-mapped seg.faults
 * only with {32bit + R-devel + enable-R-shlib} -- no idea why */
SEXP compressed_to_TMatrix(SEXP x, SEXP colP)
{
    int col = asLogical(colP); /* 1 if "C"olumn compressed;  0 if "R"ow */
    /* however, for Csparse, we now effectively use the cholmod-based
     * Csparse_to_Tsparse() in ./Csparse.c ; maybe should simply write
     * an  as_cholmod_Rsparse() function and then do "as there" ...*/
    SEXP indSym = col ? Matrix_iSym : Matrix_jSym,
	ans,	indP = GET_SLOT(x, indSym),
	pP = GET_SLOT(x, Matrix_pSym);
    int npt = length(pP) - 1;
    char *ncl = strdup(class_P(x));
    char *valid[] = {
	"dgCMatrix", "dsCMatrix", "dtCMatrix", /* 0: 0:2 */
	"lgCMatrix", "lsCMatrix", "ltCMatrix", /* 1: 3:5 */
	"ngCMatrix", "nsCMatrix", "ntCMatrix", /* 2: 6:8 */
	"zgCMatrix", "zsCMatrix", "ztCMatrix", /* 3: 9:11 */

	"dgRMatrix", "dsRMatrix", "dtRMatrix", /* 4: 12:14 */
	"lgRMatrix", "lsRMatrix", "ltRMatrix", /* 5: 15:17 */
	"ngRMatrix", "nsRMatrix", "ntRMatrix", /* 6: 18:20 */
	"zgRMatrix", "zsRMatrix", "ztRMatrix", /* 7: 21:23 */
	""};
    int ctype = Matrix_check_class_etc(x, valid);

    if (ctype < 0)
	error(_("invalid class(x) '%s' in compressed_to_TMatrix(x)"), ncl);

    /* replace 'C' or 'R' with 'T' :*/
    ncl[2] = 'T';
    ans = PROTECT(NEW_OBJECT(MAKE_CLASS(ncl)));

    slot_dup(ans, x, Matrix_DimSym);
    if((ctype / 3) % 4 != 2) /* not n..Matrix */
	slot_dup(ans, x, Matrix_xSym);
    if(ctype % 3) { /* s(ymmetric) or t(riangular) : */
	slot_dup(ans, x, Matrix_uploSym);
	if(ctype % 3 == 2) /* t(riangular) : */
	    slot_dup(ans, x, Matrix_diagSym);
    }
    SET_DimNames(ans, x);
    SET_SLOT(ans, indSym, duplicate(indP));
    expand_cmprPt(npt, INTEGER(pP),
		  INTEGER(ALLOC_SLOT(ans, col ? Matrix_jSym : Matrix_iSym,
				     INTSXP, length(indP))));
    free(ncl);
    UNPROTECT(1);
    return ans;
}

SEXP R_to_CMatrix(SEXP x)
{
    SEXP ans, tri = PROTECT(allocVector(LGLSXP, 1));
    char *ncl = strdup(class_P(x));
    char *valid[] = {
	"dgRMatrix", "dsRMatrix", "dtRMatrix",
	"lgRMatrix", "lsRMatrix", "ltRMatrix",
	"ngRMatrix", "nsRMatrix", "ntRMatrix",
	"zgRMatrix", "zsRMatrix", "ztRMatrix",
	""};
    int ctype = Matrix_check_class_etc(x, valid);
    int *x_dims = INTEGER(GET_SLOT(x, Matrix_DimSym)), *a_dims;

    if (ctype < 0)
	error(_("invalid class(x) '%s' in R_to_CMatrix(x)"), ncl);

    /* replace 'R' with 'C' : */
    ncl[2] = 'C';
    ans = PROTECT(NEW_OBJECT(MAKE_CLASS(ncl)));

    a_dims = INTEGER(ALLOC_SLOT(ans, Matrix_DimSym, INTSXP, 2));
    /* reversed dim() since we will transpose: */
    a_dims[0] = x_dims[1];
    a_dims[1] = x_dims[0];

    /* triangular: */ LOGICAL(tri)[0] = 0;
    if((ctype / 3) != 2) /* not n..Matrix */
	slot_dup(ans, x, Matrix_xSym);
    if(ctype % 3) { /* s(ymmetric) or t(riangular) : */
	SET_SLOT(ans, Matrix_uploSym,
		 mkString((*uplo_P(x) == 'U') ? "L" : "U"));
	if(ctype % 3 == 2) { /* t(riangular) : */
	    LOGICAL(tri)[0] = 1;
	    slot_dup(ans, x, Matrix_diagSym);
	}
    }
    SET_SLOT(ans, Matrix_iSym, duplicate(GET_SLOT(x, Matrix_jSym)));
    slot_dup(ans, x, Matrix_pSym);
    ans = Csparse_transpose(ans, tri);
    SET_DimNames(ans, x);
    free(ncl);
    UNPROTECT(2);
    return ans;
}

SEXP compressed_non_0_ij(SEXP x, SEXP colP)
{
    int col = asLogical(colP); /* 1 if "C"olumn compressed;  0 if "R"ow */
    SEXP ans, indSym = col ? Matrix_iSym : Matrix_jSym;
    SEXP indP = GET_SLOT(x, indSym),
	pP = GET_SLOT(x, Matrix_pSym);
    int i, *ij;
    int ncol = INTEGER(GET_SLOT(x, Matrix_DimSym))[1],
	n_el = INTEGER(pP)[ncol]; /* is only == length(indP), if the
				     i-slot is not over-allocated */

    ij = INTEGER(ans = PROTECT(allocMatrix(INTSXP, n_el, 2)));
    /* expand the compressed margin to 'i' or 'j' : */
    expand_cmprPt(ncol, INTEGER(pP), &ij[col ? n_el : 0]);
    /* and copy the other one: */
    if (col)
	for(i = 0; i < n_el; i++)
	    ij[i] = INTEGER(indP)[i];
    else /* row compressed */
	for(i = 0; i < n_el; i++)
	    ij[i + n_el] = INTEGER(indP)[i];

    UNPROTECT(1);
    return ans;
}

#if 0				/* unused */
SEXP dgCMatrix_lusol(SEXP x, SEXP y)
{
    SEXP ycp = PROTECT((TYPEOF(y) == REALSXP) ?
		       duplicate(y) : coerceVector(y, REALSXP));
    CSP xc = AS_CSP__(x);
    R_CheckStack();

    if (xc->m != xc->n || xc->m <= 0)
	error(_("dgCMatrix_lusol requires a square, non-empty matrix"));
    if (LENGTH(ycp) != xc->m)
	error(_("Dimensions of system to be solved are inconsistent"));
    if (!cs_lusol(/*order*/ 1, xc, REAL(ycp), /*tol*/ 1e-7))
	error(_("cs_lusol failed"));

    UNPROTECT(1);
    return ycp;
}
#endif

SEXP dgCMatrix_qrsol(SEXP x, SEXP y, SEXP ord)
{
    SEXP ycp = PROTECT((TYPEOF(y) == REALSXP) ?
		       duplicate(y) : coerceVector(y, REALSXP));
    CSP xc = AS_CSP(x); /* <--> x  may be  dgC* or dtC* */
    int order = INTEGER(ord)[0];
    R_CheckStack();

    if (order < 0 || order > 3)
	warning(_("dgCMatrix_qrsol(., order) needs order in {0,..,3}"));
    /* --> cs_amd()  ---  order 0: natural, 1: Chol, 2: LU, 3: QR */
    if (LENGTH(ycp) != xc->m)
	error(_("Dimensions of system to be solved are inconsistent"));
    /* FIXME?  Note that qr_sol() would allow *under-determined systems;
               In general, we'd need  LENGTH(ycp) = max(n,m)
     */
    if (xc->m < xc->n || xc->n <= 0)
	error(_("dgCMatrix_qrsol(<%d x %d>-matrix) requires a 'tall' rectangular matrix"),
		xc->m, xc->n);

    /* cs_qrsol(): Tim Davis (2006) .. "8.2 Using a QR factorization", p.136f , calling
     * -------      cs_sqr(order, ..), see  p.76 */
    if (!cs_qrsol(order, xc, REAL(ycp)))
	/* return value really is 0 or 1 - no more info there */
	error(_("cs_qrsol failed"));

    /* Solution is only in the first part of ycp -- cut its length back to n : */
    {
	SEXP nms = getAttrib(ycp, R_NamesSymbol);
	SETLENGTH(ycp, xc->n);
	if(nms != R_NilValue) {
	    SETLENGTH(nms, xc->n);
	    setAttrib(ycp, R_NamesSymbol, nms);
	}
    }
    UNPROTECT(1);
    return ycp;
}

/* Modified version of Tim Davis's cs_qr_mex.c file for MATLAB */
SEXP dgCMatrix_QR(SEXP Ap, SEXP order)
{
    SEXP ans = PROTECT(NEW_OBJECT(MAKE_CLASS("sparseQR")));
    CSP A = AS_CSP__(Ap), D;
    css *S;
    csn *N;
    int m = A->m, n = A->n, ord = asLogical(order) ? 3 : 0, *p;
    int *dims = INTEGER(ALLOC_SLOT(ans, Matrix_DimSym, INTSXP, 2));
    R_CheckStack();

    if (m < n) error(_("A must have #{rows} >= #{columns}")) ;
    dims[0] = m; dims[1] = n;
    S = cs_sqr(ord, A, 1);	/* symbolic QR ordering & analysis*/
    if (!S) error(_("cs_sqr failed"));
    N = cs_qr(A, S);		/* numeric QR factorization */
    if (!N) error(_("cs_qr failed")) ;
    cs_dropzeros(N->L);		/* drop zeros from V and sort */
    D = cs_transpose(N->L, 1);
    cs_spfree(N->L);
    N->L = cs_transpose(D, 1);
    cs_spfree(D);
    cs_dropzeros(N->U);		/* drop zeros from R and sort */
    D = cs_transpose(N->U, 1);
    cs_spfree(N->U) ;
    N->U = cs_transpose(D, 1);
    cs_spfree(D);
    m = N->L->m;		/* m may be larger now */
    p = cs_pinv(S->pinv, m);	/* p = pinv' */
    SET_SLOT(ans, install("V"),
	     Matrix_cs_to_SEXP(N->L, "dgCMatrix", 0));
    Memcpy(REAL(ALLOC_SLOT(ans, install("beta"),
			   REALSXP, n)), N->B, n);
    Memcpy(INTEGER(ALLOC_SLOT(ans, Matrix_pSym,
			      INTSXP, m)), p, m);
    SET_SLOT(ans, install("R"),
	     Matrix_cs_to_SEXP(N->U, "dgCMatrix", 0));
    if (ord)
	Memcpy(INTEGER(ALLOC_SLOT(ans, install("q"),
				  INTSXP, n)), S->q, n);
    else
	ALLOC_SLOT(ans, install("q"), INTSXP, 0);
    cs_nfree(N);
    cs_sfree(S);
    cs_free(p);
    UNPROTECT(1);
    return ans;
}

#ifdef Matrix_with_SPQR
/**
 * Return a SuiteSparse QR factorization of the sparse matrix A
 *
 * @param Ap (pointer to) a [m x n] dgCMatrix
 * @param ordering integer SEXP specifying the ordering strategy to be used
 *	see SPQR/Include/SuiteSparseQR_definitions.h
 * @param econ integer SEXP ("economy"): number of rows of R and columns of Q
 *      to return. The default is m. Using n gives the standard economy form.
 *      A value less than the estimated rank r is set to r, so econ=0 gives the
 *      "rank-sized" factorization, where nrow(R)==nnz(diag(R))==r.
 * @param tol double SEXP: if tol <= -2 use SPQR's default,
 *                         if -2 < tol < 0, then no tol is used; otherwise,
 *      tol > 0, use as tolerance: columns with 2-norm <= tol treated as 0
 *
 *
 * @return SEXP  "SPQR" object with slots (Q, R, p, rank, Dim):
 *	Q: dgCMatrix; R: dgCMatrix  [subject to change to dtCMatrix FIXME ?]
 *	p: integer: 0-based permutation (or length 0 <=> identity);
 *	rank: integer, the "revealed" rank   Dim: integer, original matrix dim.
 */
SEXP dgCMatrix_SPQR(SEXP Ap, SEXP ordering, SEXP econ, SEXP tol)
{
/* SEXP ans = PROTECT(allocVector(VECSXP, 4)); */
    SEXP ans = PROTECT(NEW_OBJECT(MAKE_CLASS("SPQR")));

    CHM_SP A = AS_CHM_SP(Ap), Q, R;
    UF_long *E, rank;/* not always = int   FIXME  (Windows_64 ?) */

    if ((rank = SuiteSparseQR_C_QR(asInteger(ordering),
				   asReal(tol),/* originally had SPQR_DEFAULT_TOL */
				   (UF_long)asInteger(econ),/* originally had 0 */
				   A, &Q, &R, &E, &c)) == -1)
	error(_("SuiteSparseQR_C_QR returned an error code"));

    SET_SLOT(ans, Matrix_DimSym, duplicate(GET_SLOT(Ap, Matrix_DimSym)));
/*     SET_VECTOR_ELT(ans, 0, */
/* 		   chm_sparse_to_SEXP(Q, 0, 0, 0, "", R_NilValue)); */
    SET_SLOT(ans, install("Q"),
	     chm_sparse_to_SEXP(Q, 0, 0, 0, "", R_NilValue));

    /* Also gives a dgCMatrix (not a dtC* *triangular*) :
     * may make sense if to be used in the "spqr_solve" routines .. ?? */
/*     SET_VECTOR_ELT(ans, 1, */
/* 		   chm_sparse_to_SEXP(R, 0, 0, 0, "", R_NilValue)); */
    SET_SLOT(ans, install("R"),
	     chm_sparse_to_SEXP(R, 0, 0, 0, "", R_NilValue));
    cholmod_l_free_sparse(&R, &c);
    cholmod_l_free_sparse(&Q, &c);
    if (E) {
	/* FIXME when UF_long != int  ?! */
	Memcpy(INTEGER(ALLOC_SLOT(ans, Matrix_pSym, /* "p" */
				  INTSXP, A->ncol)), E, A->ncol);
/* 	SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, A->ncol)); */
/* 	Memcpy(INTEGER(VECTOR_ELT(ans, 2)), E, A->ncol); */
	Free(E);
    } /* else SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, 0)); */

    SET_SLOT(ans, install("rank"), ScalarInteger((int)rank));
    UNPROTECT(1);
    return ans;
}
#endif
/* Matrix_with_SPQR */

/* Modified version of Tim Davis's cs_lu_mex.c file for MATLAB */
void install_lu(SEXP Ap, int order, double tol)
{
    SEXP ans;
    css *S;
    csn *N;
    int n, *p, *dims;
    CSP A = AS_CSP__(Ap), D;
    R_CheckStack();

    n = A->n;
    if (A->m != n)
	error(_("LU decomposition applies only to square matrices"));
    if (order) {		/* not using natural order */
	order = (tol == 1) ? 2	/* amd(S'*S) w/dense rows or I */
	    : 1;		/* amd (A+A'), or natural */
    }
    S = cs_sqr(order, A, 0);	/* symbolic ordering, no QR bound */
    N = cs_lu(A, S, tol);	/* numeric factorization */
    if (!N) 
	error(_("cs_lu(A) failed: near-singular A (or out of memory)"));
    cs_dropzeros(N->L);		/* drop zeros from L and sort it */
    D = cs_transpose(N->L, 1);
    cs_spfree(N->L);
    N->L = cs_transpose(D, 1);
    cs_spfree(D);
    cs_dropzeros(N->U);		/* drop zeros from U and sort it */
    D = cs_transpose(N->U, 1);
    cs_spfree(N->U);
    N->U = cs_transpose(D, 1);
    cs_spfree(D);
    p = cs_pinv(N->pinv, n);	/* p=pinv' */
    ans = PROTECT(NEW_OBJECT(MAKE_CLASS("sparseLU")));
    dims = INTEGER(ALLOC_SLOT(ans, Matrix_DimSym, INTSXP, 2));
    dims[0] = n; dims[1] = n;
    SET_SLOT(ans, install("L"),
	     Matrix_cs_to_SEXP(N->L, "dtCMatrix", 0));
    SET_SLOT(ans, install("U"),
	     Matrix_cs_to_SEXP(N->U, "dtCMatrix", 0));
    Memcpy(INTEGER(ALLOC_SLOT(ans, Matrix_pSym, /* "p" */
			      INTSXP, n)), p, n);
    if (order)
	Memcpy(INTEGER(ALLOC_SLOT(ans, install("q"),
				  INTSXP, n)), S->q, n);
    cs_nfree(N);
    cs_sfree(S);
    cs_free(p);
    UNPROTECT(1);
    set_factors(Ap, ans, "LU");
}

SEXP dgCMatrix_LU(SEXP Ap, SEXP orderp, SEXP tolp)
{
    SEXP ans;
    /* FIXME: dgCMatrix_LU should check ans for consistency in
     * permutation type with the requested value - Should have two
     * classes or two different names in the factors list for LU with
     * permuted columns or not.
     * OTOH, currently  (order, tol) === (1, 1) always.
     * It is true that length(LU@q) does flag the order argument.
     */
    if (!isNull(ans = get_factors(Ap, "LU")))
	return ans;
    install_lu(Ap, asInteger(orderp), asReal(tolp));
    return get_factors(Ap, "LU");
}

SEXP dgCMatrix_matrix_solve(SEXP Ap, SEXP b)
{
    SEXP ans = PROTECT(dup_mMatrix_as_dgeMatrix(b)),
	lu, qslot;
    CSP L, U;
    int *bdims = INTEGER(GET_SLOT(ans, Matrix_DimSym)), *p, *q;
    int j, n = bdims[0], nrhs = bdims[1];
    double *ax = REAL(GET_SLOT(ans, Matrix_xSym)),
	*x = Alloca(n, double);
    R_CheckStack();

    if (isNull(lu = get_factors(Ap, "LU"))) {
	install_lu(Ap, 1, 1.0);
	lu = get_factors(Ap, "LU");
    }
    qslot = GET_SLOT(lu, install("q"));
    L = AS_CSP__(GET_SLOT(lu, install("L")));
    U = AS_CSP__(GET_SLOT(lu, install("U")));
    R_CheckStack();

    p = INTEGER(GET_SLOT(lu, Matrix_pSym));
    q = LENGTH(qslot) ? INTEGER(qslot) : (int *) NULL;

    if (U->n != n || nrhs < 1 || n < 1)
	error(_("Dimensions of system to be solved are inconsistent"));
    for (j = 0; j < nrhs; j++) {
	cs_pvec(p, ax + j * n, x, n);  /* x = b(p) */
	cs_lsolve(L, x);	       /* x = L\x */
	cs_usolve(U, x);	       /* x = U\x */
	if (q)			       /* b(q) = x */
	    cs_ipvec(q, x, ax + j * n, n);
	else
	    Memcpy(ax + j * n, x, n);
    }
    UNPROTECT(1);
    return ans;
}

SEXP dgCMatrix_cholsol(SEXP x, SEXP y)
{
    /* Solve Sparse Least Squares X %*% beta ~= y  with dense RHS y,
     * where X = t(x) i.e. we pass  x = t(X)  as argument,
     * via  "Cholesky(X'X)" .. well not really:
     * cholmod_factorize("x", ..) finds L in  X'X = L'L directly */
    CHM_SP cx = AS_CHM_SP(x);
    CHM_DN cy = AS_CHM_DN(coerceVector(y, REALSXP)), rhs, cAns;
    CHM_FR L;
    double one[] = {1,0}, zero[] = {0,0};
    SEXP ans = PROTECT(allocVector(VECSXP, 3));
    R_CheckStack();

    if (cx->ncol < cx->nrow || cx->ncol <= 0)
	error(_("dgCMatrix_cholsol requires a 'short, wide' rectangular matrix"));
    if (cy->nrow != cx->ncol)
	error(_("Dimensions of system to be solved are inconsistent"));
    rhs = cholmod_l_allocate_dense(cx->nrow, 1, cx->nrow, CHOLMOD_REAL, &c);
    if (!(cholmod_l_sdmult(cx, 0 /* trans */, one, zero, cy, rhs, &c)))
	error(_("cholmod_l_sdmult error"));
    L = cholmod_l_analyze(cx, &c);
    if (!cholmod_l_factorize(cx, L, &c))
	error(_("cholmod_l_factorize failed: status %d, minor %d from ncol %d"),
	      c.status, L->minor, L->n);
/* FIXME: Do this in stages so an "effects" vector can be calculated */
    if (!(cAns = cholmod_l_solve(CHOLMOD_A, L, rhs, &c)))
	error(_("cholmod_l_solve (CHOLMOD_A) failed: status %d, minor %d from ncol %d"),
	      c.status, L->minor, L->n);
    SET_VECTOR_ELT(ans, 0, chm_factor_to_SEXP(L, 0));
    SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, cx->nrow));
    Memcpy(REAL(VECTOR_ELT(ans, 1)), (double*)(cAns->x), cx->nrow);
/* FIXME: Change this when the "effects" vector is available */
    SET_VECTOR_ELT(ans, 2, allocVector(REALSXP, cx->nrow));
    Memcpy(REAL(VECTOR_ELT(ans, 1)), (double*)(rhs->x), cx->nrow);

    cholmod_l_free_factor(&L, &c);
    cholmod_l_free_dense(&rhs, &c);
    cholmod_l_free_dense(&cAns, &c);
    UNPROTECT(1);
    return ans;
}


/* Define all of
 *  dgCMatrix_colSums(....)
 *  igCMatrix_colSums(....)
 *  lgCMatrix_colSums_d(....)
 *  lgCMatrix_colSums_i(....)
 *  ngCMatrix_colSums_d(....)
 *  ngCMatrix_colSums_i(....)
 */
#define _dgC_
#include "t_gCMatrix_colSums.c"

#define _igC_
#include "t_gCMatrix_colSums.c"

#define _lgC_
#include "t_gCMatrix_colSums.c"

#define _ngC_
#include "t_gCMatrix_colSums.c"

#define _lgC_mn
#include "t_gCMatrix_colSums.c"

#define _ngC_mn
#include "t_gCMatrix_colSums.c"


SEXP lgCMatrix_colSums(SEXP x, SEXP NArm, SEXP spRes, SEXP trans, SEXP means)
{
    if(asLogical(means)) /* ==> result will be "double" / "dsparseVector" */
	return lgCMatrix_colSums_d(x, NArm, spRes, trans, means);
    else
	return lgCMatrix_colSums_i(x, NArm, spRes, trans, means);
}

SEXP ngCMatrix_colSums(SEXP x, SEXP NArm, SEXP spRes, SEXP trans, SEXP means)
{
    if(asLogical(means)) /* ==> result will be "double" / "dsparseVector" */
	return ngCMatrix_colSums_d(x, NArm, spRes, trans, means);
    else
	return ngCMatrix_colSums_i(x, NArm, spRes, trans, means);
}
