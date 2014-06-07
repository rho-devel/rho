/*
   Routines for dealing with pdMat objects

   Copyright 1997-2005 Douglas M. Bates <bates@stat.wisc.edu>,
		       Jose C. Pinheiro,
		       Saikat DebRoy

   This file is part of the nlme package for R and related languages
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

#include "pdMat.h"
#include "matrix.h"

/* Positive definite matrices */

static void
Chol_pd(double *L, longint *q, double *l)
{
  longint i, qq = *q;
  for(i = 0; i < qq; i++) {
    Memcpy(L + i * qq, l, i + 1);
    l += i + 1;
  }
}

void				/* general pd, logChol parametrization */
logChol_pd(double *L, longint *q, double *l)
{
  longint i, qq = *q;
  double *ll = l + qq;
  L[0] = exp(*l);
  for(i = 1; i < qq; i++) {
    L[i * (qq + 1)] = exp(l[i]);
    Memcpy(L + i * qq, ll, i);
    ll += i;
  }
}

void
matrixLog_pd(double *L, longint *q, double *l)
{
  longint i, j, qq = *q, one = 1L, info = 0L;
  if ( qq == 1 ) {
    *L = exp( *l );
  } else {
    double *vectors = Calloc((size_t) qq * qq, double),
      *work1 = Calloc((size_t) qq, double), *work2 = Calloc((size_t) qq, double),
      *values = Calloc((size_t) qq, double);
    Chol_pd(L, q, l);
    for(i = 0; i < qq - 1; i++) {
      copy_mat(L + (i * (qq + 1) + 1), 1L, L + i * (qq + 1) + qq, qq, 1L,
	       qq - (i + 1));
    }
    F77_CALL(rs) (q, q, L, values, &one, vectors, work1, work2, &info);
    for(i = 0; i < qq; i++) {
      values[i] = exp(values[i]);
      for(j = 0; j < qq; j++) {
	vectors[i * qq + j] *= values[i];
      }
    }
    copy_trans(L, qq, vectors, qq, qq, qq);
    Free(vectors); Free(work1); Free(work2); Free(values);
  }
}


void
natural_pd(double *L, longint *q, double *l) /* natural parametrization  */
{
  longint i, j, qp1 = *q + 1, info;
  double *std = l, *corr = l + *q, *work = Calloc(*q, double);

  for(i = 0; i < *q; i++) std[i] = exp(std[i]);

  for(i = 0; i < *q; i++) {
    L[i * qp1] = std[i] * std[i];
    for(j = i + 1; j < *q; j++) {
      *corr = exp(*corr);
      *corr = (*corr - 1)/(*corr + 1);
      L[i * (*q) + j] = L[j * (*q) + i] = std[i] * std[j] * (*corr);
      corr++;
    }
  }
#ifdef R_S_H
  F77_CALL(chol) (L, q, q, L, &info);
#else
  zero = 0L;
  F77_CALL(chol) (L, q, work, &zero, &zero, &info);
#endif /* R_S_H */
  Free(work);
}

void
compSymm_pd(double *L, longint *q, double *l) /* compound symmetry */
{
  longint i, j, qp1 = *q + 1;
  double aux = exp(l[0]), aux1 = exp(l[1]), aux2;

  aux1 = (aux1 - 1.0/((double) *q - 1.0))/(aux1 + 1.0);
  aux2 = aux * sqrt(1.0 - aux1);
  aux1 = aux * sqrt((1.0 + (*q - 1.0) * aux1) / ((double) *q));

  for(i = 0; i < *q; i++) {
    L[i * (*q)] = aux1;
  }
  for(i = 1; i < *q; i++) {
    aux = -aux2/sqrt(i * (i + 1));
    for(j = 0; j < i; j++) {
      L[i + (j * (*q))] = aux;
    }
    L[i * qp1] = -aux * i;
  }
}
