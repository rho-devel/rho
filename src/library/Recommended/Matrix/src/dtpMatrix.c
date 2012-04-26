/* double (precision) Triangular Packed Matrices
 * Note: this means *square* {n x n} matrices
*/

#include "dtpMatrix.h"

SEXP dtpMatrix_validate(SEXP obj)
{
    SEXP val = triangularMatrix_validate(obj);
    if(isString(val))
	return(val);
    else {
	int d = INTEGER(GET_SLOT(obj, Matrix_DimSym))[0],
	    lx = length(GET_SLOT(obj, Matrix_xSym));
	/* packed_ncol() [Mutils.h] checks, but gives *error* .. need string: */
	if(lx * 2 != d*(d+1))
	    return(mkString(_("Incorrect length of 'x' slot")));
	return ScalarLogical(1);
    }
}

static
double get_norm(SEXP obj, const char *typstr)
{
    char typnm[] = {'\0', '\0'};
    int *dims = INTEGER(GET_SLOT(obj, Matrix_DimSym));
    double *work = (double *) NULL;

    typnm[0] = La_norm_type(typstr);
    if (*typnm == 'I') {
	work = (double *) R_alloc(dims[0], sizeof(double));
    }
    return F77_CALL(dlantp)(typnm, uplo_P(obj), diag_P(obj), dims,
			    REAL(GET_SLOT(obj, Matrix_xSym)), work);
}

SEXP dtpMatrix_norm(SEXP obj, SEXP type)
{
    return ScalarReal(get_norm(obj, CHAR(asChar(type))));
}

SEXP dtpMatrix_rcond(SEXP obj, SEXP type)
{
    int *dims = INTEGER(GET_SLOT(obj, Matrix_DimSym)), info;
    char typnm[] = {'\0', '\0'};
    double rcond;

    typnm[0] = La_rcond_type(CHAR(asChar(type)));
    F77_CALL(dtpcon)(typnm, uplo_P(obj), diag_P(obj), dims,
		     REAL(GET_SLOT(obj, Matrix_xSym)), &rcond,
		     (double *) R_alloc(3*dims[0], sizeof(double)),
		     (int *) R_alloc(dims[0], sizeof(int)), &info);
    return ScalarReal(rcond);
}

SEXP dtpMatrix_solve(SEXP a)
{
    SEXP val = PROTECT(duplicate(a));
    int info, *Dim = INTEGER(GET_SLOT(val, Matrix_DimSym));
    F77_CALL(dtptri)(uplo_P(val), diag_P(val), Dim,
		     REAL(GET_SLOT(val, Matrix_xSym)), &info);
    UNPROTECT(1);
    return val;
}


SEXP dtpMatrix_getDiag(SEXP x)
{
    int n = *INTEGER(GET_SLOT(x, Matrix_DimSym));
    SEXP val = PROTECT(allocVector(REALSXP, n));

    d_packed_getDiag(REAL(val), x, n);
    UNPROTECT(1);
    return val;
}

SEXP ltpMatrix_getDiag(SEXP x)
{
    int n = *INTEGER(GET_SLOT(x, Matrix_DimSym));
    SEXP val = PROTECT(allocVector(LGLSXP, n));

    l_packed_getDiag(LOGICAL(val), x, n);
    UNPROTECT(1);
    return val;
}


SEXP dtpMatrix_matrix_mm(SEXP x, SEXP y)
{
    SEXP val = PROTECT(dup_mMatrix_as_dgeMatrix(y));
    /* Since 'x' is square (n x n ),   dim(x %*% y) = dim(y) */
    int *xDim = INTEGER(GET_SLOT(x, Matrix_DimSym)),
	*yDim = INTEGER(GET_SLOT(val, Matrix_DimSym));
    int ione = 1, j;
    const char *uplo = uplo_P(x), *diag = diag_P(x);
    double *xx = REAL(GET_SLOT(x, Matrix_xSym)),
	*vx = REAL(GET_SLOT(val, Matrix_xSym));

    if (yDim[0] != xDim[1])
	error(_("Dimensions of a (%d,%d) and b (%d,%d) do not conform"),
	      xDim[0], xDim[1], yDim[0], yDim[1]);
    for (j = 0; j < yDim[1]; j++) /* X %*% y[,j]  via BLAS 2 DTPMV(.) */
	F77_CALL(dtpmv)(uplo, "N", diag, yDim, xx,
			vx + j * yDim[0], &ione);
    UNPROTECT(1);
    return val;
}

SEXP dtpMatrix_matrix_solve(SEXP a, SEXP b)
{
    SEXP val = PROTECT(dup_mMatrix_as_dgeMatrix(b));
    /* Since 'x' is square (n x n ),   dim(x %*% b) = dim(b) */
    int *aDim = INTEGER(GET_SLOT(a, Matrix_DimSym)),
	*bDim = INTEGER(GET_SLOT(val, Matrix_DimSym));
    int ione = 1, j;
    const char *uplo = uplo_P(a), *diag = diag_P(a);
    double *ax = REAL(GET_SLOT(a, Matrix_xSym)),
	*vx = REAL(GET_SLOT(val, Matrix_xSym));

    if (bDim[0] != aDim[1])
	error(_("Dimensions of a (%d,%d) and b (%d,%d) do not conform"),
	      aDim[0], aDim[1], bDim[0], bDim[1]);
    for (j = 0; j < bDim[1]; j++) /* a^{-1} %*% b[,j]  via BLAS 2 DTPSV(.) */
	F77_CALL(dtpsv)(uplo, "N", diag, bDim, ax,
			vx + j * bDim[0], &ione);
    UNPROTECT(1);
    return val;
}

/* FIXME: This function should be removed and a rtP argument added to
 * dtpMatrix_matrix_mm */
SEXP dgeMatrix_dtpMatrix_mm(SEXP x, SEXP y)
{
    SEXP val = PROTECT(duplicate(x));
    /* Since 'y' is square (n x n ),   dim(x %*% y) = dim(x) */
    int *xDim = INTEGER(GET_SLOT(x, Matrix_DimSym)),
	*yDim = INTEGER(GET_SLOT(y, Matrix_DimSym));
    int i;
    const char *uplo = uplo_P(y), *diag = diag_P(y);
    double *yx = REAL(GET_SLOT(y, Matrix_xSym)),
 	*vx = REAL(GET_SLOT(val, Matrix_xSym));

    if (yDim[0] != xDim[1])
	error(_("Dimensions of a (%d,%d) and b (%d,%d) do not conform"),
	      xDim[0], xDim[1], yDim[0], yDim[1]);
    for (i = 0; i < xDim[0]; i++)/* val[i,] := Y' %*% x[i,]  */
	F77_CALL(dtpmv)(uplo, "T", diag, yDim, yx,
			vx + i, /* incr = */ xDim);
    UNPROTECT(1);
    return val;
}

SEXP dtpMatrix_as_dtrMatrix(SEXP from)
{
    SEXP val = PROTECT(NEW_OBJECT(MAKE_CLASS("dtrMatrix"))),
	uplo = GET_SLOT(from, Matrix_uploSym),
	diag = GET_SLOT(from, Matrix_diagSym),
	dimP = GET_SLOT(from, Matrix_DimSym),
	dmnP = GET_SLOT(from, Matrix_DimNamesSym);
    int n = *INTEGER(dimP);

    SET_SLOT(val, Matrix_DimSym, duplicate(dimP));
    SET_SLOT(val, Matrix_DimNamesSym, duplicate(dmnP));
    SET_SLOT(val, Matrix_diagSym, duplicate(diag));
    SET_SLOT(val, Matrix_uploSym, duplicate(uplo));
    packed_to_full_double(REAL(ALLOC_SLOT(val, Matrix_xSym, REALSXP, n*n)),
			  REAL(GET_SLOT(from, Matrix_xSym)), n,
			  *CHAR(STRING_ELT(uplo, 0)) == 'U' ? UPP : LOW);
    UNPROTECT(1);
    return val;
}

