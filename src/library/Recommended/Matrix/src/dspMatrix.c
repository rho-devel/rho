#include "dspMatrix.h"

/* Note:  also used for lspMatrix */
SEXP dspMatrix_validate(SEXP obj)
{
    SEXP val = symmetricMatrix_validate(obj);
    if(isString(val))
	return(val);
    else { /* identical to the test in dtpMatrix_validate() : */
	int d = INTEGER(GET_SLOT(obj, Matrix_DimSym))[0],
	    lx = length(GET_SLOT(obj, Matrix_xSym));
	if(lx * 2 != d*(d+1))
	    return(mkString(_("Incorrect length of 'x' slot")));
	return ScalarLogical(1);
    }
}

double get_norm_sp(SEXP obj, const char *typstr)
{
    char typnm[] = {'\0', '\0'};
    int *dims = INTEGER(GET_SLOT(obj, Matrix_DimSym));
    double *work = (double *) NULL;

    typnm[0] = La_norm_type(typstr);
    if (*typnm == 'I' || *typnm == 'O') {
	work = (double *) R_alloc(dims[0], sizeof(double));
    }
    return F77_CALL(dlansp)(typnm, uplo_P(obj), dims,
			    REAL(GET_SLOT(obj, Matrix_xSym)), work);
}

SEXP dspMatrix_norm(SEXP obj, SEXP type)
{
    return ScalarReal(get_norm_sp(obj, CHAR(asChar(type))));
}

SEXP dspMatrix_rcond(SEXP obj, SEXP type)
{
    SEXP trf = dspMatrix_trf(obj);
    char typnm[] = {'\0', '\0'};
    int *dims = INTEGER(GET_SLOT(obj, Matrix_DimSym)), info;
    double anorm = get_norm_sp(obj, "O"), rcond;

    typnm[0] = La_rcond_type(CHAR(asChar(type)));
    F77_CALL(dspcon)(uplo_P(trf), dims,
		     REAL   (GET_SLOT(trf, Matrix_xSym)),
		     INTEGER(GET_SLOT(trf, Matrix_permSym)),
		     &anorm, &rcond,
		     (double *) R_alloc(2*dims[0], sizeof(double)),
		     (int *) R_alloc(dims[0], sizeof(int)), &info);
    return ScalarReal(rcond);
}

SEXP dspMatrix_solve(SEXP a)
{
    SEXP trf = dspMatrix_trf(a);
    SEXP val = PROTECT(NEW_OBJECT(MAKE_CLASS("dspMatrix")));
    int *dims = INTEGER(GET_SLOT(trf, Matrix_DimSym)), info;

    slot_dup(val, trf, Matrix_uploSym);
    slot_dup(val, trf, Matrix_xSym);
    slot_dup(val, trf, Matrix_DimSym);
    F77_CALL(dsptri)(uplo_P(val), dims, REAL(GET_SLOT(val, Matrix_xSym)),
		     INTEGER(GET_SLOT(trf, Matrix_permSym)),
		     (double *) R_alloc((long) dims[0], sizeof(double)),
		     &info);
    UNPROTECT(1);
    return val;
}

SEXP dspMatrix_matrix_solve(SEXP a, SEXP b)
{
    SEXP trf = dspMatrix_trf(a),
	val = PROTECT(dup_mMatrix_as_dgeMatrix(b));
    int *adims = INTEGER(GET_SLOT(a, Matrix_DimSym)),
	*bdims = INTEGER(GET_SLOT(val, Matrix_DimSym));
    int n = bdims[0], nrhs = bdims[1], info;

    if (adims[0] != n || nrhs < 1 || n < 1)
	error(_("Dimensions of system to be solved are inconsistent"));
    F77_CALL(dsptrs)(uplo_P(trf),
		     &n, &nrhs, REAL(GET_SLOT(trf, Matrix_xSym)),
		     INTEGER(GET_SLOT(trf, Matrix_permSym)),
		     REAL(GET_SLOT(val, Matrix_xSym)), &n, &info);
    UNPROTECT(1);
    return val;
}

SEXP dspMatrix_as_dsyMatrix(SEXP from)
{
    SEXP val = PROTECT(NEW_OBJECT(MAKE_CLASS("dsyMatrix"))),
	uplo = GET_SLOT(from, Matrix_uploSym),
	dimP = GET_SLOT(from, Matrix_DimSym),
	dmnP = GET_SLOT(from, Matrix_DimNamesSym);
    int n = *INTEGER(dimP);

    SET_SLOT(val, Matrix_DimSym, duplicate(dimP));
    SET_SLOT(val, Matrix_DimNamesSym, duplicate(dmnP));
    SET_SLOT(val, Matrix_uploSym, duplicate(uplo));
    packed_to_full_double(REAL(ALLOC_SLOT(val, Matrix_xSym, REALSXP, n*n)),
			  REAL(GET_SLOT(from, Matrix_xSym)), n,
			  *CHAR(STRING_ELT(uplo, 0)) == 'U' ? UPP : LOW);
    UNPROTECT(1);
    return val;
}

SEXP dspMatrix_matrix_mm(SEXP a, SEXP b)
{
    SEXP val = PROTECT(dup_mMatrix_as_dgeMatrix(b));
    int *bdims = INTEGER(GET_SLOT(val, Matrix_DimSym));
    int i, ione = 1, n = bdims[0], nrhs = bdims[1];
    const char *uplo = uplo_P(a);
    double *ax = REAL(GET_SLOT(a, Matrix_xSym)), one = 1., zero = 0.,
	*vx = REAL(GET_SLOT(val, Matrix_xSym));
    double *bx = Alloca(n * nrhs, double);
    R_CheckStack();

    Memcpy(bx, vx, n * nrhs);
    if (bdims[0] != n)
	error(_("Matrices are not conformable for multiplication"));
    if (nrhs < 1 || n < 1) {
/* 	error(_("Matrices with zero extents cannot be multiplied")); */
    } else
	for (i = 0; i < nrhs; i++)
	    F77_CALL(dspmv)(uplo, &n, &one, ax, bx + i * n, &ione,
			    &zero, vx + i * n, &ione);
    UNPROTECT(1);
    return val;
}

SEXP dspMatrix_trf(SEXP x)
{
    SEXP val = get_factors(x, "pBunchKaufman"),
	dimP = GET_SLOT(x, Matrix_DimSym),
	uploP = GET_SLOT(x, Matrix_uploSym);
    int *dims = INTEGER(dimP), *perm, info;
    int n = dims[0];
    const char *uplo = CHAR(STRING_ELT(uploP, 0));

    if (val != R_NilValue) return val;
    dims = INTEGER(dimP);
    val = PROTECT(NEW_OBJECT(MAKE_CLASS("pBunchKaufman")));
    SET_SLOT(val, Matrix_uploSym, duplicate(uploP));
    SET_SLOT(val, Matrix_diagSym, mkString("N"));
    SET_SLOT(val, Matrix_DimSym, duplicate(dimP));
    slot_dup(val, x, Matrix_xSym);
    perm = INTEGER(ALLOC_SLOT(val, Matrix_permSym, INTSXP, n));
    F77_CALL(dsptrf)(uplo, dims, REAL(GET_SLOT(val, Matrix_xSym)), perm, &info);
    if (info) error(_("Lapack routine %s returned error code %d"), "dsptrf", info);
    UNPROTECT(1);
    return set_factors(x, val, "pBunchKaufman");
}

