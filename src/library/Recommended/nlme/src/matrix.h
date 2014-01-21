/*
   header file for the nlme package

   Copyright 1999-2001  Saikat DebRoy

   This file is part of the nlme package for S and related languages
   and is made available under the terms of the GNU General Public
   License, version 2, or at your option, any later version,
   incorporated herein by reference.

   This program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, a copy is available at
   http://www.r-project.org/Licenses/

*/

#ifndef NLME_MATRIX_H
#define NLME_MATRIC_H
#include "base.h"
void F77_NAME(chol)(double *a, int *lda, int *n, double *v, int *info);
int F77_NAME(rs)(int *nm, int *n, double *a, double *w,
		 int *matz, double *z, double *fv1, double *fv2, int *ierr);
#include <R_ext/Linpack.h>

typedef struct QR_struct {
  double *mat, *qraux;
  longint *pivot, rank, ldmat, nrow, ncol;
} *QRptr;

extern void d_axpy(double *, double, double *, longint);
extern double d_dot_prod(double *, longint, double *, longint, longint);
extern double d_sum_sqr( double *, longint);
extern double *copy_mat(double *, longint, double *, longint, longint,
			longint);
extern double *copy_trans(double *, longint, double *, longint,
			  longint, longint);
extern double *mult_mat(double *, longint, double *, longint, longint,
			longint, double *, longint, longint);
extern QRptr QR(double *, longint, longint, longint);
extern void QRfree(QRptr);
extern longint QRqty(QRptr, double *, longint, longint);
extern longint QRsolve(QRptr, double *, longint, longint, double *, longint);
extern double QRlogAbsDet(QRptr);
extern void QRstoreR(QRptr, double *, longint);
extern longint QR_and_rotate(double *, longint, longint, longint,
			     double *, longint, longint, double *,
			     double *, longint);
#endif /* NLME_MATRIX_H */
