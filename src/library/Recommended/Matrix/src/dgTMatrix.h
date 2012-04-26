#ifndef MATRIX_TRIPLET_H
#define MATRIX_TRIPLET_H

#include "Mutils.h"

SEXP xTMatrix_validate(SEXP x);
SEXP dgTMatrix_to_dgeMatrix(SEXP x);
SEXP lgTMatrix_to_lgeMatrix(SEXP x);
SEXP dgTMatrix_to_matrix(SEXP x);
SEXP lgTMatrix_to_matrix(SEXP x);
#ifdef _valid_only_for_old_graph_package
SEXP graphNEL_as_dgTMatrix(SEXP x, SEXP symmetric);
#endif


#endif
