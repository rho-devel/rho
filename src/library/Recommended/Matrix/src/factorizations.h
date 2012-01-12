#ifndef MATRIX_FACTORS_H
#define MATRIX_FACTORS_H

#include "Mutils.h"

SEXP LU_validate(SEXP obj);
SEXP BunchKaufman_validate(SEXP obj);
SEXP pBunchKaufman_validate(SEXP obj);
SEXP Cholesky_validate(SEXP obj);
SEXP pCholesky_validate(SEXP obj);
SEXP SVD_validate(SEXP obj);
SEXP LU_expand(SEXP x);

#endif
