#include "factorizations.h"

SEXP LU_validate(SEXP obj)
{
    return ScalarLogical(1);
}

SEXP BunchKaufman_validate(SEXP obj)
{
    return ScalarLogical(1);
}

SEXP pBunchKaufman_validate(SEXP obj)
{
    return ScalarLogical(1);
}

SEXP Cholesky_validate(SEXP obj)
{
    return ScalarLogical(1);
}

SEXP pCholesky_validate(SEXP obj)
{
    return ScalarLogical(1);
}

SEXP SVD_validate(SEXP obj)
{
    return ScalarLogical(1);
}

SEXP LU_expand(SEXP x)
{
    const char *nms[] = {"L", "U", "P", ""};
    SEXP L, U, P, val = PROTECT(Matrix_make_named(VECSXP, nms)),
	lux = GET_SLOT(x, Matrix_xSym),
	dd = GET_SLOT(x, Matrix_DimSym);
    int *iperm, *perm, *pivot = INTEGER(GET_SLOT(x, Matrix_permSym)),
	i, n = INTEGER(dd)[0];

    SET_VECTOR_ELT(val, 0, NEW_OBJECT(MAKE_CLASS("dtrMatrix")));
    L = VECTOR_ELT(val, 0);
    SET_VECTOR_ELT(val, 1, NEW_OBJECT(MAKE_CLASS("dtrMatrix")));
    U = VECTOR_ELT(val, 1);
    SET_VECTOR_ELT(val, 2, NEW_OBJECT(MAKE_CLASS("pMatrix")));
    P = VECTOR_ELT(val, 2);
    SET_SLOT(L, Matrix_xSym, duplicate(lux));
    SET_SLOT(L, Matrix_DimSym, duplicate(dd));
    SET_SLOT(L, Matrix_uploSym, mkString("L"));
    SET_SLOT(L, Matrix_diagSym, mkString("U"));
    make_d_matrix_triangular(REAL(GET_SLOT(L, Matrix_xSym)), L);
    SET_SLOT(U, Matrix_xSym, duplicate(lux));
    SET_SLOT(U, Matrix_DimSym, duplicate(dd));
    SET_SLOT(U, Matrix_uploSym, mkString("U"));
    SET_SLOT(U, Matrix_diagSym, mkString("N"));
    make_d_matrix_triangular(REAL(GET_SLOT(U, Matrix_xSym)), U);
    SET_SLOT(P, Matrix_DimSym, duplicate(dd));
    iperm = Alloca(n, int);
    R_CheckStack();
    perm = INTEGER(ALLOC_SLOT(P, Matrix_permSym, INTSXP, n));

    for (i = 0; i < n; i++) iperm[i] = i + 1; /* initialize permutation*/
    for (i = 0; i < n; i++) {	/* generate inverse permutation */
	int newpos = pivot[i] - 1;
	if (newpos != i) {
	    int tmp = iperm[i];

	    iperm[i] = iperm[newpos];
	    iperm[newpos] = tmp;
	}
    }
				/* invert the inverse */
    for (i = 0; i < n; i++) perm[iperm[i] - 1] = i + 1;
    UNPROTECT(1);
    return val;
}
