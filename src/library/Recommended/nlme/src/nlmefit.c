/*
   Routines for calculation of the log-likelihood or restricted
   log-likelihood with mixed-effects models.

   Copyright 1997-2005  Douglas M. Bates <bates@stat.wisc.edu>,
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

#include "nlmefit.h"
#include "matrix.h"
#include "pdMat.h"

#ifndef SPLUS_VERSION
#ifdef S_VERSION
#define msmnh    dmnh
#endif /* S_VERSION */
#endif /* SPLUS_VERSION */

extern void F77_NAME(msmnh)();

static longint **
setOffsets(longint ** base, longint * ngrp, longint Qp2)
{
    longint i, **ptrVec = Calloc((size_t) Qp2, longint *);
    for (i = 0; i < Qp2; i++) {
	ptrVec[i] = *base;
	*base += ngrp[i];
    }
    return ptrVec;
}

dimPTR
dims(longint *pdims)
{				/* constructor for a dims object */
    dimPTR value = Calloc((size_t) 1, struct dim_struct);
    longint *base, Qp2, *ngrp;

    value->N = (int) pdims[0];
    value->ZXrows = pdims[1];
    value->ZXcols = pdims[2];
    value->Q = pdims[3];
    Qp2 = (value->Q) + 2;
    value->Srows = pdims[4];
    value->q = pdims + 5;
    ngrp = value->ngrp = value->q + Qp2;
    value->DmOff = value->ngrp + Qp2;
    value->ncol = value->DmOff + Qp2;
    value->nrot = value->ncol + Qp2;
    base = value->nrot + Qp2;
    value->ZXoff = setOffsets(&base, ngrp, Qp2);
    value->ZXlen = setOffsets(&base, ngrp, Qp2);
    value->SToff = setOffsets(&base, ngrp, Qp2);
    value->DecOff = setOffsets(&base, ngrp, Qp2);
    value->DecLen = setOffsets(&base, ngrp, Qp2);
    return value;
}

SEXP getListElement(SEXP list, char *str)
{
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++)
	if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	    elmt = VECTOR_ELT(list, i);
	    break;
	}
    return elmt;
}

dimPTR				/* create a dimensions object directly */
dimS(SEXP d)			/* from an SEXP */
{
    int i, Qp2;  SEXP tmp;
    dimPTR value = Calloc((size_t) 1, struct dim_struct);
    value->N = INTEGER(coerceVector(getListElement(d, "N"), INTSXP))[0];
    value->ZXrows =
	INTEGER(coerceVector(getListElement(d, "ZXrows"), INTSXP))[0];
    value->ZXcols =
	INTEGER(coerceVector(getListElement(d, "ZXcols"), INTSXP))[0];
    value->Q =
	INTEGER(coerceVector(getListElement(d, "Q"), INTSXP))[0];
    value->Srows =
	INTEGER(coerceVector(getListElement(d, "Srows"), INTSXP))[0];
    Qp2 = value->Q + 2;
    value->q = INTEGER(coerceVector(getListElement(d, "q"), INTSXP));
    value->ngrp = INTEGER(coerceVector(getListElement(d, "ngrp"), INTSXP));
    value->DmOff = INTEGER(coerceVector(getListElement(d, "DmOff"), INTSXP));
    value->ncol = INTEGER(coerceVector(getListElement(d, "ncol"), INTSXP));
    value->nrot = INTEGER(coerceVector(getListElement(d, "nrot"), INTSXP));
    value->ZXoff = Calloc(Qp2, int *);
    tmp = coerceVector(getListElement(d, "ZXoff"), VECSXP);
    for (i = 0; i < Qp2; i++) {
	(value->ZXoff)[i] = INTEGER(coerceVector(VECTOR_ELT(tmp, i), INTSXP));
    }
    value->ZXlen = Calloc(Qp2, int *);
    tmp = coerceVector(getListElement(d, "ZXlen"), VECSXP);
    for (i = 0; i < Qp2; i++) {
	(value->ZXlen)[i] = INTEGER(coerceVector(VECTOR_ELT(tmp, i), INTSXP));
    }
    value->SToff = Calloc(Qp2, int *);
    tmp = coerceVector(getListElement(d, "SToff"), VECSXP);
    for (i = 0; i < Qp2; i++) {
	(value->SToff)[i] = INTEGER(coerceVector(VECTOR_ELT(tmp, i), INTSXP));
    }
    value->DecOff = Calloc(Qp2, int *);
    tmp = coerceVector(getListElement(d, "DecOff"), VECSXP);
    for (i = 0; i < Qp2; i++) {
	(value->DecOff)[i] = INTEGER(coerceVector(VECTOR_ELT(tmp, i), INTSXP));
    }
    value->DecLen = Calloc(Qp2, int *);
    tmp = coerceVector(getListElement(d, "DecLen"), VECSXP);
    for (i = 0; i < Qp2; i++) {
	(value->DecLen)[i] = INTEGER(coerceVector(VECTOR_ELT(tmp, i), INTSXP));
    }
    return value;
}
void
dimFree(dimPTR this)
{
    Free(this->DecOff);
    Free(this->DecLen);
    Free(this->SToff);
    Free(this->ZXlen);
    Free(this->ZXoff);
    Free(this);
}

size_t
count_DmHalf_pars( dimPTR dd, longint *pdClass )
{
    int i, result;
    for ( i = 0, result = 0; i < dd->Q; i++ ) {
	switch( pdClass[ i ] ) {
	case 0: case 4: result += ( (dd->q)[ i ] * ( (dd->q)[ i ] + 1 ) ) / 2;
	    break;
	case 1: result += (dd->q)[ i ];
	    break;
	case 2: result += 1;
	    break;
	case 3: result += 2;
	}
    }
    return (size_t) result;
}

double *
generate_DmHalf( double *DmHalf, dimPTR dd, longint *pdClass, double *pars )
{				/* Expand parameters to DmHalf arrays */
    int i, j, q, Q = dd->Q; double diag;
    for (i = 0; i < Q; i++) {
	q = (dd->q)[ i ];
	switch (pdClass[i]) {
	case 0:			/* default: unstructured */
	    matrixLog_pd( DmHalf + (dd->DmOff)[ i ], dd->q + i, pars );
	    pars += (q * (q + 1))/2;
	    break;
	case 1:			/* diagonal */
	    for (j = 0; j < q; j++) {
		DmHalf[ (dd->DmOff)[i] + j * (q + 1) ] = exp( *pars++ );
	    }
	    break;
	case 2:			/* multiple of identity */
	    diag = exp( *pars );
	    for (j = 0; j < q; j++) {
		DmHalf[ (dd->DmOff)[i] + j * (q + 1) ] = diag;
	    }
	    pars++;
	    break;
	case 3:			/* compound symmetry */
	    compSymm_pd( DmHalf + (dd->DmOff)[ i ], dd->q + i, pars );
	    pars += 2;
	    break;
	case 4:                 /* unstructured with log-cholesky
				   parametrization */
	    logChol_pd(DmHalf + (dd->DmOff)[ i ], dd->q + i, pars );
	    pars += (q * (q + 1))/2;
	    break;
	}
    }
    return DmHalf;
}

#ifdef Debug
static void
print_mat( char *msg, double *x, longint ldx, longint nrow,
	   longint ncol )
{				/* print matrix and message */
    int i, j;
    printf( "%s\n", msg );
    for (i = 0; i < nrow; i++) {
	for (j = 0; j < ncol; j++) {
	    printf( " %10.5g", x[i + j * ldx ] );
	}
	printf( "\n" );
    }
    printf( "\n" );
}
#endif /* Debug */

static double *
scale_mat(double *y, longint ldy, double a,
	  double *x, longint ldx, longint nrow, longint ncol)
{				/* y <- a * x */
    int i, j;
    double * ret = y;

    for (j = 0; j < ncol; j++) {
	for (i = 0; i < nrow; i++) { y[i] = a * x[i]; }
	y += ldy; x += ldx;
    }
    return ret;
}

static double *
plus_equals_mat(double *y, longint ldy, double *x, longint ldx,
		longint nrow, longint ncol)
{				/* y <- y + x */
    double * ret = y;
    int i, j;

    for (j = 0; j < ncol; j++) {
	for (i = 0; i < nrow; i++) { y[i] += x[i]; }
	y += ldy; x += ldx;
    }
    return ret;
}

static longint			/* backsolve and update */
backsolve(double *mat, longint ldmat, longint nupdate, longint ncol,
	  longint nrot, longint ny)
{
    longint i, j, ONE = 1L, info;
    double *y = mat + (int) ((ncol + nrot - ny) * ldmat);

    mat = mat - (int) nupdate;
    for (i = 0; i < ny; i++) {	/* usually ny = 1 but just in case ... */
	F77_CALL(dtrsl) (mat + (int) nupdate, &ldmat, &ncol, y, &ONE, &info);
	if (info != 0) {
	    return info;
	}
	for (j = 0; j < ncol; j++) {
	    d_axpy(y - (int) nupdate, - y[j], mat + (int) (j * ldmat), nupdate);
	}
	y += (int) ldmat;
    }
    return info;
}

static longint			/* invert an upper-triangular matrix in place*/
invert_upper(double *mat, longint ldmat, longint ncol)
{
    longint i, j, ONE = 1L, info = 0L;
    double *b = Calloc((size_t) ncol, double);

    for (i = ncol; i > 1L; i--) {
	for (j = 0; j < (i - 1L); j++) { b[j] = 0.0; }
	b[((int) i) - 1] = 1.0;
	F77_CALL(dtrsl) (mat, &ldmat, &i, b, &ONE, &info);
	if (info != 0) { Free(b); return info; }
	Memcpy(mat + (i - 1) * ldmat, b, (int) i);
    }
    if (*mat == 0.0) { Free(b); return 1L; }
    *mat = 1.0 / (*mat);
    Free(b); return 0L;
}

static longint			/* invert a block in the virtual R array */
invert_block(double *mat, longint ldmat, longint nabove,
	     longint ncol, longint nright)
{
    double * tpblk = mat - (int) nabove;
    longint info = invert_upper(mat, ldmat, ncol);

    if (info != 0L) return info;
    if (nright > 0) {
	double *ntri = Calloc((size_t) (ncol * ncol), double),
	    *rtblk = mat + ncol * ldmat;
	scale_mat(ntri, ncol, -1.0, mat, ldmat, ncol, ncol);
	mult_mat(rtblk, ldmat, ntri, ncol, ncol, ncol, rtblk, ldmat, nright);
	Free(ntri);
	if (nabove > 0L) {
	    double *tmp = Calloc((size_t)(nabove * nright), double);
	    plus_equals_mat(rtblk - (size_t)nabove, ldmat,
			    mult_mat(tmp, nabove, tpblk, ldmat, nabove, ncol,
				     rtblk, ldmat, nright),
			    nabove, nabove, nright);
	    Free(tmp);
	}
    }
    if (nabove > 0L) {
	mult_mat(tpblk, ldmat, tpblk, ldmat, nabove, ncol, mat, ldmat, ncol);
    }
    return 0L;
}


void				/* return the decomposition for ZXy */
mixed_decomp(double *ZXy, longint *pdims)
{
    dimPTR dd = dims(pdims);	/* Create a dimensions structure */
    internal_decomp(dd, ZXy);
    dimFree(dd);
}

void
internal_decomp(dimPTR dd, double *ZXy)
{				/* decompose ZXy and re-write the dims */
    longint i, j, Qp2 = (dd->Q) + 2;
    double *dc;

    if ((dd->Srows) >= (dd->ZXrows)) /* decomposition is not worthwhile */
	return;
    dc = Calloc((size_t) ((dd->Srows) * (dd->ZXcols)), double);
    for (i = 0; i < Qp2; i++) {
	for(j = 0; j < (dd->ngrp)[i]; j++) {
	    QR_and_rotate(ZXy + (dd->ZXoff)[i][j], dd->ZXrows, (dd->ZXlen)[i][j],
			  (dd->ncol)[i] + (dd->nrot)[i], DNULLP, 0L,
			  (dd->ncol)[i], DNULLP, dc + (dd->SToff)[i][j],
			  dd->Srows);
	}
    }
    Memcpy(ZXy, dc, dd->Srows * dd->ZXcols);
    for (i = 0; i < Qp2; i++) {	/* re-write the offsets and lengths */
	for (j = 0; j < (dd->ngrp)[i]; j++) {
	    (dd->ZXoff)[i][j] = (dd->DecOff)[i][j];
	    (dd->ZXlen)[i][j] = (dd->DecLen)[i][j];
	}
    }
    dd->ZXrows = dd->Srows;	/* and the total number of rows */
    Free(dc);
}

double			/* evaluate the log-likelihood pieces */
internal_loglik(dimPTR dd, double *ZXy, double *DmHalf, longint *RML,
		double *dc, double *lRSS)
{				/* if dc is NULL, don't attempt storage */
    longint i, j, Q = dd->Q,  Qp2 = Q + 2, qi,
	ldstr = (dc != DNULLP) ? (dd->Srows) : 0L;
    double accum, *dmHlf, *lglk = Calloc( Qp2, double );
    QRptr dmQR;

    for (i = 0; i < Qp2; i++) {
	qi = (dd->q)[i];
	for (j = 0; j < (dd->ngrp)[i]; j++) {
	    if (qi >
		QR_and_rotate(ZXy + (dd->ZXoff)[i][j], dd->ZXrows,
			      (dd->ZXlen)[i][j], (dd->ncol)[i] + (dd->nrot)[i],
			      DmHalf + (dd->DmOff)[i], qi, (dd->ncol)[i],
			      lglk + i, dc + (dd->SToff)[i][j], ldstr))
	    {
		warning("Singular precision matrix in level %ld, block %ld",
			(long int) (i - (dd->Q)), j + 1L);
		return -DBL_MAX;
	    }
	}
    }
    for(i = 0, accum = 0; i < Q; i++) {
	qi = (dd->q)[i];
	dmHlf = Calloc( (size_t) qi * qi, double );
	dmQR = QR( copy_mat( dmHlf, qi, DmHalf + (dd->DmOff)[i],
			     qi, qi, qi ), qi, qi, qi);
	accum += (dd->ngrp)[i] * QRlogAbsDet( dmQR ) - lglk[i];
	QRfree( dmQR ); Free( dmHlf );
    }
    accum -= *RML * lglk[ Q ] + (dd->N - *RML * dd->ncol[ Q ]) * lglk[Q + 1];
    if (lRSS != DNULLP) *lRSS = lglk[Q + 1]; /* return log(RSS)/2 */
    Free( lglk );
    return accum;
}

void
internal_estimate(dimPTR dd, double *dc)
{				/* solve for Beta and b_i estimates */
    longint i, j, Qp1 = (dd->Q) + 1L;

    for (i = (dd->Q); i >= 0; i--) {
	for (j = 0; j < (dd->ngrp)[i]; j++) {
	    if (backsolve(dc + (dd->SToff)[i][j], dd->Srows,
			  (dd->SToff)[i][j] - (dd->DecOff)[i][j],
			  (dd->ncol)[i], (dd->nrot)[i], (dd->ncol)[Qp1]) != 0)
	    {
		error(_("Singularity in backsolve at level %ld, block %ld"),
		      (long int) (i - (dd->Q)), j + 1L);
	    }
	}
    }
}

static void
internal_R_invert(dimPTR dd, double *dc)
{				/* Invert the virtual R matrix in place */
    int i, j;
    for (i = (dd->Q); i >= 0; i--) {
	for (j = 0; j < (dd->ngrp)[i]; j++) {
	    invert_block(dc + (dd->SToff)[i][j], dd->Srows,
			 (dd->SToff)[i][j] - (dd->DecOff)[i][j],
			 (dd->ncol)[i], (dd->nrot)[i] - 1L);
	}
    }
}

static double cube_root_eps = 0.;

static double *
pt_prod( double *prod, double *a, double *b, longint len )
{				/* prod <- a * b */
    longint i; double *ret = prod;
    for (i = 0; i < len; i++) {
	*prod++ = *a++ * *b++;
    }
    return ret;
}

static void
finite_diff_Hess( double (*func)(double*), double *pars, int npar,
		  double *vals )
{				/* use Koshal design for finite-differences */
    int i, j, nTot = 1 + npar + ( npar * ( npar + 1 ) ) / 2;
    double *incr = Calloc( npar, double), *ppt, *xpt, *dpt,
	*parray = Calloc( nTot * npar, double ), /* array of parameters */
	*div = Calloc( nTot, double ), /* divisors */
	*Xmat = Calloc( nTot * nTot, double ); /* regressor matrix */
    QRptr xQR;

    if (!cube_root_eps) cube_root_eps = exp( log( DOUBLE_EPS ) / 3.);
    div[ 0 ] = 1.0;
    ppt = parray + npar * ( 2 * npar + 1 ); /* location of first cross term */
    xpt = Xmat + nTot * ( 2 * npar + 1 );	/* location of first cross column */
    dpt = div + 2 * npar + 1;
    for (i = 0; i < npar; i++) {
	incr[i] = (pars[ i ] != 0.0) ? cube_root_eps * pars[ i ] : cube_root_eps;
	div[ i + 1 ] = 1.0 / incr[ i ];
	div[ npar + i + 1 ] = 2.0 / ( incr[ i ] * incr[ i ] );
	parray[ npar + i * (npar + 1) ] = 1.0;
	parray[ (npar + i) * (npar + 1) ] = -1.0;
	for (j = i + 1; j < npar; j++) {
	    ppt[ i ] = ppt[ j ] = 1;
	    ppt += npar;
	}
	for (j = 0; j < nTot; j++) {
	    Xmat[ j + (i + 1) * nTot ] = parray[ i + j * npar ];
	}
	pt_prod( Xmat + (npar + i + 1) * nTot, Xmat + (i + 1) * nTot,
		 Xmat + (i + 1) * nTot, nTot );
	for (j = 0; j < i; j++) {
	    pt_prod( xpt, Xmat + (i + 1) * nTot, Xmat + (j + 1) * nTot, nTot );
	    xpt += nTot;
	    *dpt++ = 1.0 / ( incr[ i ] * incr[ j ] );
	}
    }
#ifdef Debug
    print_mat( "parray", parray, npar, npar, nTot );
#endif /* Debug */
    vals[ 0 ] = (*func)( pars );
    Xmat[ 0 ] = 1.0;
    for (i = 1; i < nTot; i++) {
	Xmat[i] = 1.0;		/* column of 1's for constant */
	Memcpy( parray, pars, npar );
	for (j = 0; j < npar; j++) {
	    parray[ j ] += parray[ j + i * npar ] * incr[ j ];
	}
	vals[i] = (*func)( parray );
    }
#ifdef Debug
    print_mat( "Xmat", Xmat, nTot, nTot, nTot );
#endif /* Debug */
    xQR = QR( Xmat, (longint) nTot, (longint) nTot, (longint) nTot );
    QRsolve( xQR, vals, (longint) nTot, 1L, vals, (longint) nTot );
    pt_prod( vals, vals, div, nTot );
    /* re-arrange the Hessian terms */
    xpt = vals + npar + 1;
    Memcpy( div, vals + npar + 1, nTot - ( npar + 1 ) );
    dpt = div + npar;		/* first off-diagonal */
    for (i = 0; i < npar; i++) {
	xpt[ i * ( npar + 1 ) ] = div[ i ];	/* diagonals */
	for (j = 0; j < i; j++) {
	    xpt[ i + j * npar ] = xpt[ j + i * npar ] = *dpt++;
	}
    }
    QRfree( xQR ); Free( incr ); Free( parray ); Free( div ); Free( Xmat );
    return;
}

void				/* For optif0 or optif9 */
mixed_fcn(longint n, double *pars, double *g, void *state)
{
    statePTR st = (statePTR) state;
    double *zxcopy = Calloc(st->dd->ZXrows * st->dd->ZXcols, double),
	*Delta = Calloc(st->dd->DmOff[st->dd->Q], double);

    Memcpy(zxcopy, st->ZXy, st->dd->ZXrows * st->dd->ZXcols);
    *g = -internal_loglik(st->dd, zxcopy,
			  generate_DmHalf(Delta, st->dd, st->pdClass, pars),
			  st->RML, DNULLP, DNULLP);
    Free(Delta); Free(zxcopy);
}

void				/* For optif0 or optif9 */
mixed_grad(longint n, double *pars, double *g, void *state)
{
    statePTR st = (statePTR) state;
    double *zxcopy = Calloc(st->dd->ZXrows * st->dd->ZXcols, double),
	*Delta = Calloc(st->dd->DmOff[st->dd->Q], double),
	*dc = Calloc((size_t) ((st->dd->Srows) * (st->dd->ZXcols)), double),
	*DmHalf, sigmainv, *pt, *res;
    double  sqrtDF = sqrt((double) (st->dd->N -
				    *(st->RML)*(st->dd->ncol[st->dd->Q])));
    longint i, j, offset;

    DmHalf = generate_DmHalf(Delta, st->dd, st->pdClass, pars),
	Memcpy(zxcopy, st->ZXy, st->dd->ZXrows * st->dd->ZXcols);
    /* needed ? */
    internal_loglik(st->dd, zxcopy, DmHalf, st->RML, dc, DNULLP);
    internal_estimate(st->dd, dc);
    internal_R_invert(st->dd, dc);
    sigmainv = *(dc + (size_t)((st->dd->Srows) * (st->dd->ZXcols)) - 1)/sqrtDF;
    sigmainv = 1.0/((sigmainv < 0.0) ? - sigmainv : sigmainv);
    offset = ((st->dd->ZXcols) - 1L) * (st->dd->Srows);
    for (i = 0L; i < (st->dd->Q); i++) {
	longint ncol = (st->dd->q)[i],
	    nright = (st->dd->nrot)[i] - (st->dd->nrot)[(st->dd->Q) - ( (*(st->RML)) ? 0 : 1 )];
	longint nrow = (ncol + nright + 1L) * (st->dd->ngrp)[i];
	QRptr qq;
	pt = res = Calloc((size_t) (ncol * nrow), double);
	for (j = 0L; j < (st->dd->ngrp)[i]; j++) {
	    copy_trans(pt, nrow, dc + (st->dd->SToff)[i][j], st->dd->Srows,
		       ncol, ncol + nright);
	    pt += ncol + nright;
	    scale_mat(pt++, nrow, sigmainv, dc + offset + (st->dd->SToff)[i][j],
		      1L, 1L, ncol);
	}
	offset -= (st->dd->Srows) * ncol;
	qq = QR(res, nrow, nrow, ncol);
	QRstoreR(qq, res, ncol);
	QRfree(qq);
	switch (st->pdClass[i]) {
	case 0:			/* unstructured with matrix-logarithm
				   parametrization */
	    error(_("analytic gradient is not available with matrix logarithm"));
	    break;
	case 1:			/* diagonal */
	    for (j = 0; j < ncol; j++) {
		double tmp = DmHalf[ (st->dd->DmOff)[i] + j * (ncol + 1)];
		*g++ = st->dd->ngrp[i] - tmp*tmp*d_sum_sqr(res + j * ncol, j + 1L);
	    }
	    break;
	case 2:			/* multiple of identity */
	{
	    double tmp = 0.0;
	    for(j = 0; j < ncol; j++) {
		tmp += d_sum_sqr( res + j * nrow, j + 1L );
	    }
	    *g = tmp;
	    tmp = DmHalf[ (st->dd->DmOff)[i] + j * (ncol + 1)];
	    *g *= tmp * tmp;
	    *g = ncol*st->dd->ngrp[i] - *g;
	    g++;
	    break;
	}
	case 3:			/* compound symmetry */
	{
	    error(_("analytic gradient is not available with compound symmetry"));
	    break;
	}
	case 4:			/* unstructured with log-cholesky
				   parametrization */
	{
	    int j1;
	    double *col_j = Calloc(ncol, double);
	    for (j1 = 0; j1 < ncol; j1++) {
		int i1;
		for(i1 = 0; i1 < j1; i1++)
		    col_j[i1] = d_dot_prod(res + i1*ncol, 1, res +
					   j1*ncol, 1, 1+i1);
		for(i1 = j1; i1 < ncol; i1++)
		    col_j[i1] = d_dot_prod(res + i1*ncol, 1, res +
					   j1*ncol, 1, 1+j1);
		for (i1 = 0; i1 <= j1; i1++) {
		    int k1;
		    double sum = 0.0;

		    for (k1 = i1; k1 < ncol; k1++) {
			sum += DmHalf[(st->dd->DmOff)[i] + i1*ncol + k1] *
			    col_j[k1];
		    }
		    if (i1 == j1)
			*g++ = st->dd->ngrp[i] -
			    sum*DmHalf[(st->dd->DmOff)[i] + i1*(ncol + 1)];
		    else
			*g++ = -sum;
		}
	    }
	    break;
	}
	}
	Free(res);
    }
    Free(dc); Free(Delta); Free(zxcopy);
}

/* In gcc we can use nested function
   definitions but not for other compilers */
static double *zxcopy, *zxcopy2, *Delta, *values;
static dimPTR dd;
static longint *setngs, *pdC;
size_t zxdim;

static double
logLik_fun( double *pars )
{				/* defined for finite differences */
    Memcpy( zxcopy2, zxcopy, zxdim );
    return internal_loglik(dd, zxcopy2, generate_DmHalf( Delta, dd, pdC, pars ),
			   setngs, DNULLP, DNULLP );
}

static double
negLogLik_fun( double *pars )
{				/* defined for finite differences */
    Memcpy( zxcopy2, zxcopy, zxdim );
    return - internal_loglik(dd, zxcopy2, generate_DmHalf( Delta, dd, pdC, pars ),
			     setngs, DNULLP, DNULLP );
}

void
mixed_loglik(double *ZXy, longint *pdims, double *pars, longint *settings,
	     double *logLik, double *lRSS)
{				/* evaluate the log-likelihood */
    dd = dims(pdims);
    /* settings gives RML, asDelta, gradHess, and pdClass in that order */
    if (settings[ 1 ]) {		/* gradHess not used and pdClass ignored */
	*logLik = internal_loglik( dd, ZXy, pars, settings, DNULLP, lRSS);
    } else {			/* generate the Delta arrays from pars */
	setngs = settings;
	pdC = setngs + 3;
	Delta = Calloc( (dd->DmOff)[ dd->Q ], double );

	if (settings[ 2 ] == 0) {	/* no gradient or Hessian */
	    *logLik =
		internal_loglik( dd, ZXy, generate_DmHalf( Delta, dd, pdC, pars ),
				 settings, DNULLP, lRSS );
	} else {
	    int npar = count_DmHalf_pars( dd, pdC );
	    zxdim = (dd->ZXrows) * (dd->ZXcols);
	    zxcopy = Calloc( zxdim, double );
	    zxcopy2 = ZXy;

	    Memcpy( zxcopy, ZXy, zxdim );
	    finite_diff_Hess( logLik_fun, pars, npar, logLik);
	    Free( zxcopy );
	}
	Free( Delta );
    }
    dimFree( dd );
}

void				/* loglikelihood and parameter estimates */
mixed_estimate(double *ZXy, longint *pdims, double *DmHalf, longint *RML,
	       double *logLik, double *dc, longint *invert)
{				/* dc receives the decomposed ZXy array */
    dimPTR dd = dims(pdims);

    *logLik = internal_loglik(dd, ZXy, DmHalf, RML, dc, DNULLP);
    internal_estimate(dd, dc);
    if (*invert != 0) { internal_R_invert( dd, dc ); }
    dimFree(dd);
}

void				/* EM iterations for mixed-effects models */
internal_EM(dimPTR dd, double *ZXy, double *DmHalf, int nn,
	    longint *pdClass, longint *RML, double *logLik, double *Ra,
	    double *lRSS)
{
    double sigmainv, *res, *pt,
	*dc = Calloc((size_t) ((dd->Srows) * (dd->ZXcols)), double),
	*zxcopy = Calloc((size_t) ((dd->ZXrows) * (dd->ZXcols)), double);
    double  sqrtDF = sqrt((double) (dd->N - *RML * (dd->ncol[dd->Q])));
    longint i, j, k, offset;

    while (nn-- > 0) {
	copy_mat(zxcopy, dd->ZXrows, ZXy, dd->ZXrows, dd->ZXrows, dd->ZXcols);
	*logLik = internal_loglik(dd, zxcopy, DmHalf, RML, dc, DNULLP);
	internal_estimate( dd, dc );
	internal_R_invert( dd, dc );
	sigmainv = *(dc + (size_t)((dd->Srows) * (dd->ZXcols)) - 1)/sqrtDF;
	sigmainv = 1.0/((sigmainv < 0.0) ? - sigmainv : sigmainv);
	offset = ((dd->ZXcols) - 1L) * (dd->Srows);
	for (i = 0L; i < (dd->Q); i++) {
	    longint ncol = (dd->q)[i],
		nright = (dd->nrot)[i] - (dd->nrot)[(dd->Q) - ( (*RML) ? 0 : 1 )];
	    longint nrow = (ncol + nright + 1L) * (dd->ngrp)[i];
	    QRptr qq;
	    pt = res = Calloc((size_t) (ncol * nrow), double);
	    for (j = 0L; j < (dd->ngrp)[i]; j++) {
		copy_trans(pt, nrow, dc + (dd->SToff)[i][j], dd->Srows,
			   ncol, ncol + nright);
		pt += ncol + nright;
		scale_mat(pt++, nrow, sigmainv, dc + offset + (dd->SToff)[i][j],
			  1L, 1L, ncol);
	    }
	    offset -= (dd->Srows) * ncol;
	    qq = QR(res, nrow, nrow, ncol);
	    QRstoreR(qq, Ra + (dd->DmOff)[i], ncol);
	    QRfree(qq);
	    scale_mat(res, nrow, sqrt(1.0/((dd->ngrp)[i])),
		      Ra + (dd->DmOff)[i], ncol, ncol, ncol);
	    switch (pdClass[i]) {
	    case 0: case 4:			/* default: unstructured */
		invert_upper(res, nrow, ncol);
		copy_trans(DmHalf + (dd->DmOff)[i], ncol, res, nrow, ncol, ncol);
		break;
	    case 1:			/* diagonal */
		for (j = 0; j < ncol; j++) {
		    DmHalf[ (dd->DmOff)[i] + j * (ncol + 1)] =
			1. / sqrt( d_sum_sqr( res + j * nrow, j + 1L ) );
		}
		break;
	    case 2:			/* multiple of identity */
	    {
		double aux = 0.0;
		for(j = 0; j < ncol; j++) {
		    aux += d_sum_sqr( res + j * nrow, j + 1L );
		}
		aux = sqrt(ncol / aux);
		for(j = 0; j < ncol; j++) {
		    DmHalf[(dd->DmOff)[i] + j * (ncol + 1)] = aux;
		}
	    }
	    break;
	    case 3:			/* compound symmetry */
	    {
		double trA = 0.0, trAJ = 0.0, *auxRes;
		longint l;
		for(j = 0; j < ncol; j++) {
		    for(k = 0; k <= j; k++) {
			trA += res[k + j * nrow] * res[k + j * nrow];
			for(l = j + 1; l < ncol; l++) {
			    trAJ += res[k + j * nrow] * res[k + l * nrow];
			}
		    }
		}
		trAJ = 2 * trAJ + trA;
		trA = (ncol - 1) / (ncol * trA - trAJ);
		trAJ = 1/trAJ - trA;
		trA = ncol * trA + trAJ;
		auxRes = DmHalf + (dd->DmOff[i]);
		for(j = 0; j < ncol; j++) {
		    auxRes[j * (ncol + 1)] = trA;
		    for(k = (j + 1); k < ncol; k++) {
			auxRes[j * ncol + k] = auxRes[j + k * ncol] = trAJ;
		    }
		}
#ifdef USING_R
		F77_CALL(chol)(auxRes, &ncol, &ncol, auxRes, &l);
#else
		zero = 0L;
		F77_CALL(chol)(auxRes, &ncol, res, &zero, &zero, &l);
#endif /* USING_R */
	    }
	    break;
	    }
	    Free(res);
	}
    }
    copy_mat(zxcopy, dd->ZXrows, ZXy, dd->ZXrows, dd->ZXrows, dd->ZXcols);
    *logLik = internal_loglik(dd, zxcopy, DmHalf, RML, dc, lRSS);
    Free(dc); Free(zxcopy);
}

void
mixed_EM(double *ZXy, longint *pdims, double *DmHalf, longint *nIter,
	 longint *pdClass, longint *RML, double *logLik, double *Ra,
	 double *lRSS)
{
    dimPTR dd = dims(pdims);

    internal_EM(dd, ZXy, DmHalf, *nIter, pdClass, RML, logLik, Ra, lRSS);
    dimFree(dd);
}

void				/* to be called by Fortran msmnh */
mixed_calcf(longint *n, double *theta, longint *nf,
	    double *f, longint *uiparm, double *urparm,
	    void (*ufparm)(void))
{
    Memcpy( zxcopy2, zxcopy, zxdim );
    *f = - internal_loglik(dd, zxcopy2, generate_DmHalf( Delta, dd, pdC, theta ),
			   setngs, DNULLP, DNULLP );
}

void				/* to be called by Fortran msmnh */
mixed_calcgh(longint *n, double *theta, longint *nf,
	     double *g, double *h, longint *uiparm,
	     double *urparm, void (*ufparm)(void))
{
    longint i, nn = *n;
    double *hpt = values + nn + 1;

    finite_diff_Hess( negLogLik_fun, theta, (int) nn, values );
    Memcpy( g, values + 1, nn );
    for( i = 1; i <= nn; i++ ) {	/* copy upper triangle of Hessian */
	Memcpy( h, hpt, i );
	h += i;
	hpt += nn;
    }
}

static double *
crossprod_mat(double *y, longint ldy, double *x, longint ldx,
	      longint nrow, longint ncol) /* y <- t(x) %*% x */
{
    longint i, j;

    for( i = 0; i < ncol; i++ ) {
	y[ i * ldy + i ] = d_dot_prod( x + i * ldx, 1L, x + i * ldx, 1L, nrow );
	for( j = 0; j < i; j++) {
	    y[ i * ldy + j ] = y[ j * ldy + i ] =
		d_dot_prod( x + i * ldx, 1L, x + j * ldx, 1L, nrow );
	}
    }
    return y;
}

/* Forming the parameter structure from the Delta matrix */
/*  Not sure if these will ever be called from S. */
/*  Will leave open the possibility. */

static void
Delta2MatrixLog( double *theta, longint *q, double *Delta )
{
    longint i, j, qq = *q, one = 1L, info = 0L;
    if ( qq == 1 ) {
	*theta = log(*Delta * *Delta)/2.;
    } else {
	double *vectors = Calloc((size_t) qq * qq, double),
	    *DtransD = Calloc((size_t) qq * qq, double),
	    *workmat = Calloc((size_t) qq * qq, double),
	    *work2 = Calloc((size_t) qq, double),
	    *values = Calloc((size_t) qq, double), *pt;
	crossprod_mat(DtransD, qq, Delta, qq, qq, qq); /* form t(Delta) %*% Delta */
	F77_CALL(rs) (q, q, DtransD, values, &one, vectors, workmat, work2, &info);
	if (info != 0L) {
	    error(_("Unable to form eigenvalue-eigenvector decomposition"));
	}
	copy_mat(workmat, qq, vectors, qq, qq, qq);
	for(i = 0; i < qq; i++) {
	    values[i] = log(values[i])/2;
	    for(j = 0; j < qq; j++) {
		workmat[i * qq + j] *= values[i];
	    }
	}
	copy_trans(DtransD, qq, workmat, qq, qq, qq);
	mult_mat(workmat, qq, vectors, qq, qq, qq, DtransD, qq, qq);
	for( i = 0, pt = theta; i < qq; i++ ) {
	    for( j = 0; j <= i; j++ ) {
		*pt++ = workmat[ i * qq + j ];
	    }
	}
	Free(vectors); Free(DtransD); Free(workmat), Free(work2); Free(values);
    }
}

static void
Delta2LogCholesky(double *theta, longint *q, double *Delta )
{
    longint i, qq = *q, info = 0L;
    if ( qq == 1 ) {
	*theta = log(*Delta * *Delta)/2.;
    } else {
	double *ll = theta + qq,
	    *DtransD = Calloc((size_t) qq * qq, double);
	crossprod_mat(DtransD, qq, Delta, qq, qq, qq); /* form t(Delta) %*% Delta */
	F77_CALL(chol) (DtransD, &qq, &qq, Delta, &info); /* re-writes Delta */
	if (info != 0L)
	    error(_("Unable to form Cholesky decomposition"));
	*theta = log(Delta[0]);
	for(i = 1; i < qq; i++) {
	    theta[i] = log(Delta[i * (qq + 1)]);
	    Memcpy(ll, Delta + i * qq, i);
	    ll += i;
	}
	Free(DtransD);
    }
}

double *
generate_theta( double *theta, dimPTR dd, longint *pdClass, double *DmHalf )
{				/* Expand parameters to DmHalf arrays */
    int i, j, q, Q = dd->Q;
    for (i = 0; i < Q; i++) {
	q = (dd->q)[ i ];
	switch (pdClass[i]) {
	case 0:			/* default: unstructured */
	    Delta2MatrixLog( theta, dd->q + i, DmHalf + (dd->DmOff)[ i ] );
	    theta += (q * (q + 1))/2;
	    break;
	case 1:			/* diagonal */
	    for (j = 0; j < q; j++) {
		*theta++ = log( DmHalf[ (dd->DmOff)[i] + j * (q + 1) ] );
	    }
	    break;
	case 2:			/* multiple of identity */
	    *theta++ = log( DmHalf[(dd->DmOff)[i]] );
	    break;
	case 3:			/* compound symmetry */
	    error(_("Haven't written the compound symmetry case for this yet"));
	    break;
	case 4:			/* default: unstructured */
	    Delta2LogCholesky( theta, dd->q + i, DmHalf + (dd->DmOff)[ i ] );
	    theta += (q * (q + 1))/2;
	    break;
	}
    }
    return theta;
}

void				/* both EM and Newton-Raphson iterations */
mixed_combined(double *ZXy, longint *pdims, double *DmHalf, longint *nIter,
	       longint *pdClass, longint *RML, double *logLik, double *R0,
	       double *lRSS, longint *info)
{
    longint i, j;
    double *Ra, *dc, *work;

    dd = dims(pdims);		/* Using global dd, pdC, setngs, and Delta */
    pdC = pdClass;
    setngs = RML;

    dc = Calloc((size_t) ((dd->Srows) * (dd->ZXcols)), double);

    Ra = Calloc( dd->DmOff[dd->Q], double);
    internal_decomp( dd, ZXy );	/* take decomp if useful */

				/* check for non-zero entries in DmHalf */
    if ( d_sum_sqr( DmHalf, dd->DmOff[ dd->Q ]) == 0. ) {
	work = ZXy;			/* create starting estimates */
	Delta = DmHalf;
	for( i = 0; i < dd->Q; i++ ) {
	    for ( j = 0; j < (dd->q)[i]; j++ ) {
		*Delta = 0.375 * sqrt( d_sum_sqr( work, dd->ZXrows ) / (dd->ngrp)[i]);
		Delta += (dd->q)[i] + 1;
		work += dd->ZXrows;
	    }
	    Delta -= (dd->q)[i];	/* have moved too far - step back */
	}
    }
    internal_EM(dd, ZXy, DmHalf, *nIter, pdClass, RML, logLik, Ra, lRSS);
#ifdef USING_R
    {
	statePTR st = Calloc(1, struct state_struct);
	int ntheta = count_DmHalf_pars( dd, pdC ), itrmcd, itncnt,
	    p = dd->ncol[dd->Q], iagflg;
	double
	    *theta = Calloc(ntheta, double),
	    *typsiz = Calloc(ntheta, double),
	    *grad = Calloc(ntheta, double),
	    *newtheta = Calloc(ntheta, double),
	    *a = Calloc(ntheta * ntheta, double),
	    *work = Calloc(ntheta * 9, double);

	st->dd = dd;
	st->ZXy = ZXy;
	st->pdClass = pdClass;
	st->RML = RML;

	generate_theta(theta, dd, pdClass, DmHalf);

	*info = 9;			/* don't inhibit checks but suppress output */
	for (i = 0; i < ntheta; i++) { typsiz[i] = 1.0; }
/*      iagflg = 1; */
	iagflg = 0;
	for (i = 0; i < dd->Q; i++) {
	    if (pdClass[i] < 1 || pdClass[i] == 3 || pdClass[i] > 4) {
		iagflg = 0;
		break;
	    }
	}

	optif9(ntheta, ntheta, theta, (fcn_p) mixed_fcn, (fcn_p)
	       mixed_grad, (d2fcn_p) 0, st, typsiz, 1.0 /*fscale*/,
	       1 /*method*/,1 /*iexp*/, info, -1 /*ndigit*/, 50 /*itnlim*/,
	       iagflg, 0 /*iahflg*/, 1. /*dlt*/, pow(DBL_EPSILON, 0.25)
	       /*gradtl*/, 0. /*stepmx*/, sqrt(DBL_EPSILON) /*steptl*/,
	       newtheta, logLik, grad, &itrmcd, a, work, &itncnt);
	if (*info == 0) {
	    *logLik = internal_loglik( dd, ZXy,
				       generate_DmHalf( DmHalf, dd, pdC, theta ),
				       setngs, dc, lRSS );
	    copy_mat(R0, p, dc + (dd->SToff)[(dd->Q)][0], (dd->Srows), p, p + 1);
	}
	Free(work); Free(a); Free(newtheta); Free(grad); Free(typsiz); Free(theta);
	Free(st);
    }
#else  /* USING_R */
    {
	int ntheta = count_DmHalf_pars( dd, pdC );
	longint p, *iv, liv, lv, uiparm[1]; /* for msmnh */
	double *theta, *scale, ufparm[1];

	Delta = DmHalf;
	p = (dd->ncol)[(dd->Q)];
	zxdim = (dd->ZXrows) * (dd->ZXcols); /* global zxdim, zxcopy, and zxcopy2 */
	zxcopy = Calloc( zxdim, double );
	zxcopy2 = ZXy;
	Memcpy( zxcopy, ZXy, zxdim );	/* keep a copy before we mess it up */
	theta = Calloc((size_t) ntheta, double);
	generate_theta( theta, dd, pdClass, DmHalf );
	values = Calloc( (size_t) ntheta * (ntheta + 1) + 1, double ); /* global */
	liv = 60;
	iv = Calloc( (size_t) liv, longint );
	lv = 78 + ntheta * (ntheta + 12);
	work = Calloc( (size_t) lv, double );
	scale = Calloc( (size_t) ntheta, double );
	for( i = 0; i < ntheta; i++ ) { scale[i] = 1.; }
	F77_CALL(msmnh) (&ntheta, scale, theta, mixed_calcf, mixed_calcgh,
			 iv, &liv, &lv, work, uiparm, ufparm, abort);
	*info = iv[0];
	Memcpy( zxcopy2, ZXy, zxdim );
	*logLik = internal_loglik( dd, zxcopy2,
				   generate_DmHalf( Delta, dd, pdC, theta ),
				   setngs, dc, lRSS );
	copy_mat(R0, p, dc + (dd->SToff)[(dd->Q)][0], (dd->Srows), p, p + 1);
	Free(scale); Free(work); Free(iv); Free(values); Free(theta); Free(zxcopy);
    }
#endif  /* USING_R */
    dimFree( dd ); Free( dc ); Free( Ra );
}

/* functions for calculating df's for fixed effects tests */

static double
inner_perc(double *x, longint *grp, longint n)
    /* percentage of groups for which x is inner */
{
    /* x - column of X matrix to be assessed
       grp - integer vector with groups
       n - length of x and grp
       data are assumed to be ordered by grp */

    longint currGrp, nn = 0, isInner;
    double nInner = 0., nGrp = 0., currVal;

    while (nn < n) {
	currGrp = grp[nn];
	currVal = x[nn];
	nGrp++;
	isInner = 0;
	do {
	    if (isInner == 0 && x[nn] != currVal) {
		nInner++;
		isInner = 1;
	    }
	    nn++;
	} while (nn < n && currGrp == grp[nn]);
    }
    return(nInner/nGrp);
}

void
inner_perc_table(double *X, longint *grps, longint *p, longint *Q,
		 longint *n, double *pTable)
    /* constructs an p x Q "inner-percentage" table for a fixed effects
       matrix X and a set of grouping vectors grps */
{
    longint i, j, pp = *p, nn = *n, ipp = 0, inn = 0;
    for(i = 0; i < *Q; i++) {
	for(j = 0; j < pp; j++) {
	    pTable[j + ipp] = inner_perc(X + j * nn, grps + inn, nn);
	}
	ipp += pp;
	inn += nn;
    }
}

/* gls functions */
void
gls_loglik(double *Xy, longint *pdims, double *logLik, double *lRSS)
{
    longint i, N = pdims[0], p = pdims[1], RML = pdims[2],
	Np1 = N + 1, Nr = N - RML * p, rnkm1;
    QRptr dmQR;

    dmQR = QR(Xy, N, N, p + 1);
    rnkm1 = (dmQR->rank) - 1;
    if(rnkm1 != p) {
	*logLik = -DBL_MAX;
    } else {
	*lRSS = log(fabs (dmQR->mat[p * Np1]));
	*logLik -= Nr * (*lRSS);
	if (RML == 1) {
	    for(i = 0; i < p; i++) {
		*logLik -= log(fabs(dmQR->mat[i * Np1]));
	    }
	}
    }
    QRfree(dmQR);
}

#if 0
/* gls functions */
void
gls_loglik(double *Xy, longint *pdims, double *logLik, double *lRSS)
{
    longint i, N = pdims[0], p = pdims[1], RML = pdims[2],
	Np1 = N + 1, Nr = N - RML * p;
    QRptr dmQR;

    dmQR = QR(Xy, N, N, p + 1);
    *lRSS = log(fabs(dmQR->mat[p * Np1]));
    *logLik -= Nr * (*lRSS);
    if (RML == 1) {
	for(i = 0; i < p; i++) {
	    *logLik -= log(fabs(dmQR->mat[i * Np1]));
	}
    }
    QRfree(dmQR);
}
#endif

void
gls_estimate(double *Xy, longint *pdims, double *beta, double *sigma,
	     double *logLik, double *varBeta, longint *rank, longint *pivot)
{
    longint i, N = pdims[0], p = pdims[1], RML = pdims[2], pp1 = p + 1,
	Nr = N - RML * p, rk, rkm1, rkp1;
    QRptr dmQR;
    double *R = Calloc((size_t) (pp1 * pp1), double);

    dmQR = QR(Xy, N, N, pp1);
    *rank = rk = dmQR->rank;
    rkm1 = rk - 1;
    rkp1 = rk + 1;
    Memcpy(pivot, dmQR->pivot, pp1);
    for(i = 0; i < rk; i++) {
	Memcpy(R + i * rk, dmQR->mat + i * N, i + 1);
    }
    *sigma = fabs(R[rk * rk - 1]);
    *logLik -= Nr * log(*sigma);
    *sigma /= sqrt(((double) Nr));
    if (RML == 1) {
	for(i = 0; i < rkm1; i++) {
	    *logLik -= log(fabs(R[i * (rkp1)]));
	}
    }
    copy_mat(varBeta, rkm1, R, rk, rkm1, rkm1);
    invert_upper(varBeta, rkm1, rkm1);
    mult_mat(beta, rkm1, varBeta, rkm1, rkm1, rkm1, R + rkm1 * rk, rk,  1L);
    QRfree(dmQR);
    Free(R);
}
