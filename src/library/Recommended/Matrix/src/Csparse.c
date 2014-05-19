			/* Sparse matrices in compressed column-oriented form */

#include "Csparse.h"
#include "Tsparse.h"
#include "chm_common.h"

/** "Cheap" C version of  Csparse_validate() - *not* sorting : */
Rboolean isValid_Csparse(SEXP x)
{
    /* NB: we do *NOT* check a potential 'x' slot here, at all */
    SEXP pslot = GET_SLOT(x, Matrix_pSym),
	islot = GET_SLOT(x, Matrix_iSym);
    int *dims = INTEGER(GET_SLOT(x, Matrix_DimSym)), j,
	nrow = dims[0],
	ncol = dims[1],
	*xp = INTEGER(pslot),
	*xi = INTEGER(islot);

    if (length(pslot) != dims[1] + 1)
	return FALSE;
    if (xp[0] != 0)
	return FALSE;
    if (length(islot) < xp[ncol]) /* allow larger slots from over-allocation!*/
	return FALSE;
    for (j = 0; j < xp[ncol]; j++) {
	if (xi[j] < 0 || xi[j] >= nrow)
	    return FALSE;
    }
    for (j = 0; j < ncol; j++) {
	if (xp[j] > xp[j + 1])
	    return FALSE;
    }
    return TRUE;
}

SEXP Csparse_validate(SEXP x) {
    return Csparse_validate_(x, FALSE);
}


#define _t_Csparse_validate
#include "t_Csparse_validate.c"

#define _t_Csparse_sort
#include "t_Csparse_validate.c"

// R: .validateCsparse(x, sort.if.needed = FALSE) :
SEXP Csparse_validate2(SEXP x, SEXP maybe_modify) {
    return Csparse_validate_(x, asLogical(maybe_modify));
}

// R: Matrix:::.sortCsparse(x) :
SEXP Csparse_sort (SEXP x) {
   int ok = Csparse_sort_2(x, TRUE); // modifying x directly
   if(!ok) warning(_("Csparse_sort(x): x is not a valid (apart from sorting) CsparseMatrix"));
   return x;
}

SEXP Rsparse_validate(SEXP x)
{
    /* NB: we do *NOT* check a potential 'x' slot here, at all */
    SEXP pslot = GET_SLOT(x, Matrix_pSym),
	jslot = GET_SLOT(x, Matrix_jSym);
    Rboolean sorted, strictly;
    int i, k,
	*dims = INTEGER(GET_SLOT(x, Matrix_DimSym)),
	nrow = dims[0],
	ncol = dims[1],
	*xp = INTEGER(pslot),
	*xj = INTEGER(jslot);

    if (length(pslot) != dims[0] + 1)
	return mkString(_("slot p must have length = nrow(.) + 1"));
    if (xp[0] != 0)
	return mkString(_("first element of slot p must be zero"));
    if (length(jslot) < xp[nrow]) /* allow larger slots from over-allocation!*/
	return
	    mkString(_("last element of slot p must match length of slots j and x"));
    for (i = 0; i < length(jslot); i++) {
	if (xj[i] < 0 || xj[i] >= ncol)
	    return mkString(_("all column indices must be between 0 and ncol-1"));
    }
    sorted = TRUE; strictly = TRUE;
    for (i = 0; i < nrow; i++) {
	if (xp[i] > xp[i+1])
	    return mkString(_("slot p must be non-decreasing"));
	if(sorted)
	    for (k = xp[i] + 1; k < xp[i + 1]; k++) {
		if (xj[k] < xj[k - 1])
		    sorted = FALSE;
		else if (xj[k] == xj[k - 1])
		    strictly = FALSE;
	    }
    }
    if (!sorted)
	/* cannot easily use cholmod_sort(.) ... -> "error out" :*/
	return mkString(_("slot j is not increasing inside a column"));
    else if(!strictly) /* sorted, but not strictly */
	return mkString(_("slot j is not *strictly* increasing inside a column"));

    return ScalarLogical(1);
}


/* Called from ../R/Csparse.R : */
/* Can only return [dln]geMatrix (no symm/triang);
 * FIXME: replace by non-CHOLMOD code ! */
SEXP Csparse_to_dense(SEXP x)
{
    CHM_SP chxs = AS_CHM_SP__(x);
    /* This loses the symmetry property, since cholmod_dense has none,
     * BUT, much worse (FIXME!), it also transforms CHOLMOD_PATTERN ("n") matrices
     * to numeric (CHOLMOD_REAL) ones : */
    CHM_DN chxd = cholmod_sparse_to_dense(chxs, &c);
    int Rkind = (chxs->xtype == CHOLMOD_PATTERN)? -1 : Real_kind(x);
    R_CheckStack();

    return chm_dense_to_SEXP(chxd, 1, Rkind, GET_SLOT(x, Matrix_DimNamesSym));
}

// FIXME: do not go via CHM (should not be too hard, to just *drop* the x-slot, right?
SEXP Csparse_to_nz_pattern(SEXP x, SEXP tri)
{
    CHM_SP chxs = AS_CHM_SP__(x);
    CHM_SP chxcp = cholmod_copy(chxs, chxs->stype, CHOLMOD_PATTERN, &c);
    int tr = asLogical(tri);
    R_CheckStack();

    return chm_sparse_to_SEXP(chxcp, 1/*do_free*/,
			      tr ? ((*uplo_P(x) == 'U') ? 1 : -1) : 0,
			      0, tr ? diag_P(x) : "",
			      GET_SLOT(x, Matrix_DimNamesSym));
}

// n.CMatrix --> [dli].CMatrix  (not going through CHM!)
SEXP nz_pattern_to_Csparse(SEXP x, SEXP res_kind)
{
    return nz2Csparse(x, asInteger(res_kind));
}
// n.CMatrix --> [dli].CMatrix  (not going through CHM!)
SEXP nz2Csparse(SEXP x, enum x_slot_kind r_kind)
{
    const char *cl_x = class_P(x);
    if(cl_x[0] != 'n') error(_("not a 'n.CMatrix'"));
    if(cl_x[2] != 'C') error(_("not a CsparseMatrix"));
    int nnz = LENGTH(GET_SLOT(x, Matrix_iSym));
    SEXP ans;
    char *ncl = alloca(strlen(cl_x) + 1); /* not much memory required */
    strcpy(ncl, cl_x);
    double *dx_x; int *ix_x;
    ncl[0] = (r_kind == x_double ? 'd' :
	      (r_kind == x_logical ? 'l' :
	       /* else (for now):  r_kind == x_integer : */ 'i'));
    PROTECT(ans = NEW_OBJECT(MAKE_CLASS(ncl)));
    // create a correct 'x' slot:
    switch(r_kind) {
	int i;
    case x_double: // 'd'
	dx_x = REAL(ALLOC_SLOT(ans, Matrix_xSym, REALSXP, nnz));
	for (i=0; i < nnz; i++) dx_x[i] = 1.;
	break;
    case x_logical: // 'l'
	ix_x = LOGICAL(ALLOC_SLOT(ans, Matrix_xSym, LGLSXP, nnz));
	for (i=0; i < nnz; i++) ix_x[i] = TRUE;
	break;
    case x_integer: // 'i'
	ix_x = INTEGER(ALLOC_SLOT(ans, Matrix_xSym, INTSXP, nnz));
	for (i=0; i < nnz; i++) ix_x[i] = 1;
	break;

    default:
	error(_("nz2Csparse(): invalid/non-implemented r_kind = %d"),
	      r_kind);
    }

    // now copy all other slots :
    slot_dup(ans, x, Matrix_iSym);
    slot_dup(ans, x, Matrix_pSym);
    slot_dup(ans, x, Matrix_DimSym);
    slot_dup(ans, x, Matrix_DimNamesSym);
    if(ncl[1] != 'g') { // symmetric or triangular ...
	slot_dup_if_has(ans, x, Matrix_uploSym);
	slot_dup_if_has(ans, x, Matrix_diagSym);
    }
    UNPROTECT(1);
    return ans;
}

SEXP Csparse_to_matrix(SEXP x)
{
    return chm_dense_to_matrix(cholmod_sparse_to_dense(AS_CHM_SP__(x), &c),
			       1 /*do_free*/, GET_SLOT(x, Matrix_DimNamesSym));
}
SEXP Csparse_to_vector(SEXP x)
{
    return chm_dense_to_vector(cholmod_sparse_to_dense(AS_CHM_SP__(x), &c), 1);
}

SEXP Csparse_to_Tsparse(SEXP x, SEXP tri)
{
    CHM_SP chxs = AS_CHM_SP__(x);
    CHM_TR chxt = cholmod_sparse_to_triplet(chxs, &c);
    int tr = asLogical(tri);
    int Rkind = (chxs->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
    R_CheckStack();

    return chm_triplet_to_SEXP(chxt, 1,
			       tr ? ((*uplo_P(x) == 'U') ? 1 : -1) : 0,
			       Rkind, tr ? diag_P(x) : "",
			       GET_SLOT(x, Matrix_DimNamesSym));
}

/* this used to be called  sCMatrix_to_gCMatrix(..)   [in ./dsCMatrix.c ]: */
SEXP Csparse_symmetric_to_general(SEXP x)
{
    CHM_SP chx = AS_CHM_SP__(x), chgx;
    int Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
    R_CheckStack();

    if (!(chx->stype))
	error(_("Nonsymmetric matrix in Csparse_symmetric_to_general"));
    chgx = cholmod_copy(chx, /* stype: */ 0, chx->xtype, &c);
    /* xtype: pattern, "real", complex or .. */
    return chm_sparse_to_SEXP(chgx, 1, 0, Rkind, "",
			      GET_SLOT(x, Matrix_DimNamesSym));
}

SEXP Csparse_general_to_symmetric(SEXP x, SEXP uplo)
{
    int *adims = INTEGER(GET_SLOT(x, Matrix_DimSym)), n = adims[0];
    if(n != adims[1]) {
	error(_("Csparse_general_to_symmetric(): matrix is not square!"));
	return R_NilValue; /* -Wall */
    }
    CHM_SP chx = AS_CHM_SP__(x), chgx;
    int uploT = (*CHAR(STRING_ELT(uplo,0)) == 'U') ? 1 : -1;
    int Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
    R_CheckStack();
    chgx = cholmod_copy(chx, /* stype: */ uploT, chx->xtype, &c);
    /* xtype: pattern, "real", complex or .. */
    return chm_sparse_to_SEXP(chgx, 1, 0, Rkind, "",
			      GET_SLOT(x, Matrix_DimNamesSym));
}

SEXP Csparse_transpose(SEXP x, SEXP tri)
{
    /* TODO: lgCMatrix & igC* currently go via double prec. cholmod -
     *       since cholmod (& cs) lacks sparse 'int' matrices */
    CHM_SP chx = AS_CHM_SP__(x);
    int Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
    CHM_SP chxt = cholmod_transpose(chx, chx->xtype, &c);
    SEXP dn = PROTECT(duplicate(GET_SLOT(x, Matrix_DimNamesSym))), tmp;
    int tr = asLogical(tri);
    R_CheckStack();

    tmp = VECTOR_ELT(dn, 0);	/* swap the dimnames */
    SET_VECTOR_ELT(dn, 0, VECTOR_ELT(dn, 1));
    SET_VECTOR_ELT(dn, 1, tmp);
    UNPROTECT(1);
    return chm_sparse_to_SEXP(chxt, 1, /* SWAP 'uplo' for triangular */
			      tr ? ((*uplo_P(x) == 'U') ? -1 : 1) : 0,
			      Rkind, tr ? diag_P(x) : "", dn);
}

SEXP Csparse_Csparse_prod(SEXP a, SEXP b)
{
    CHM_SP
	cha = AS_CHM_SP(a),
	chb = AS_CHM_SP(b),
	chc = cholmod_ssmult(cha, chb, /*out_stype:*/ 0,
			       /* values:= is_numeric (T/F) */ cha->xtype > 0,
			       /*out sorted:*/ 1, &c);
    const char *cl_a = class_P(a), *cl_b = class_P(b);
    char diag[] = {'\0', '\0'};
    int uploT = 0;
    SEXP dn = PROTECT(allocVector(VECSXP, 2));
    R_CheckStack();

#ifdef DEBUG_Matrix_verbose
    Rprintf("DBG Csparse_C*_prod(%s, %s)\n", cl_a, cl_b);
#endif

    /* Preserve triangularity and even unit-triangularity if appropriate.
     * Note that in that case, the multiplication itself should happen
     * faster.  But there's no support for that in CHOLMOD */

    /* UGLY hack -- rather should have (fast!) C-level version of
     *       is(a, "triangularMatrix") etc */
    if (cl_a[1] == 't' && cl_b[1] == 't')
	/* FIXME: fails for "Cholesky","BunchKaufmann"..*/
	if(*uplo_P(a) == *uplo_P(b)) { /* both upper, or both lower tri. */
	    uploT = (*uplo_P(a) == 'U') ? 1 : -1;
	    if(*diag_P(a) == 'U' && *diag_P(b) == 'U') { /* return UNIT-triag. */
		/* "remove the diagonal entries": */
		chm_diagN2U(chc, uploT, /* do_realloc */ FALSE);
		diag[0]= 'U';
	    }
	    else diag[0]= 'N';
	}
    SET_VECTOR_ELT(dn, 0,	/* establish dimnames */
		   duplicate(VECTOR_ELT(GET_SLOT(a, Matrix_DimNamesSym), 0)));
    SET_VECTOR_ELT(dn, 1,
		   duplicate(VECTOR_ELT(GET_SLOT(b, Matrix_DimNamesSym), 1)));
    UNPROTECT(1);
    return chm_sparse_to_SEXP(chc, 1, uploT, /*Rkind*/0, diag, dn);
}

SEXP Csparse_Csparse_crossprod(SEXP a, SEXP b, SEXP trans)
{
    int tr = asLogical(trans);
    CHM_SP
	cha = AS_CHM_SP(a),
	chb = AS_CHM_SP(b),
	chTr, chc;
    const char *cl_a = class_P(a), *cl_b = class_P(b);
    char diag[] = {'\0', '\0'};
    int uploT = 0;
    SEXP dn = PROTECT(allocVector(VECSXP, 2));
    R_CheckStack();

    chTr = cholmod_transpose((tr) ? chb : cha, chb->xtype, &c);
    chc = cholmod_ssmult((tr) ? cha : chTr, (tr) ? chTr : chb,
			 /*out_stype:*/ 0, cha->xtype, /*out sorted:*/ 1, &c);
    cholmod_free_sparse(&chTr, &c);

    /* Preserve triangularity and unit-triangularity if appropriate;
     * see Csparse_Csparse_prod() for comments */
    if (cl_a[1] == 't' && cl_b[1] == 't')
	if(*uplo_P(a) != *uplo_P(b)) { /* one 'U', the other 'L' */
	    uploT = (*uplo_P(b) == 'U') ? 1 : -1;
	    if(*diag_P(a) == 'U' && *diag_P(b) == 'U') { /* return UNIT-triag. */
		chm_diagN2U(chc, uploT, /* do_realloc */ FALSE);
		diag[0]= 'U';
	    }
	    else diag[0]= 'N';
	}
    SET_VECTOR_ELT(dn, 0,	/* establish dimnames */
		   duplicate(VECTOR_ELT(GET_SLOT(a, Matrix_DimNamesSym), (tr) ? 0 : 1)));
    SET_VECTOR_ELT(dn, 1,
		   duplicate(VECTOR_ELT(GET_SLOT(b, Matrix_DimNamesSym), (tr) ? 0 : 1)));
    UNPROTECT(1);
    return chm_sparse_to_SEXP(chc, 1, uploT, /*Rkind*/0, diag, dn);
}

SEXP Csparse_dense_prod(SEXP a, SEXP b)
{
    CHM_SP cha = AS_CHM_SP(a);
    SEXP b_M = PROTECT(mMatrix_as_dgeMatrix(b));
    CHM_DN chb = AS_CHM_DN(b_M);
    CHM_DN chc = cholmod_allocate_dense(cha->nrow, chb->ncol, cha->nrow,
					chb->xtype, &c);
    SEXP dn = PROTECT(allocVector(VECSXP, 2));
    double one[] = {1,0}, zero[] = {0,0};
    int nprot = 2;
    R_CheckStack();
    /* Tim Davis, please FIXME:  currently (2010-11) *fails* when  a  is a pattern matrix:*/
    if(cha->xtype == CHOLMOD_PATTERN) {
	/* warning(_("Csparse_dense_prod(): cholmod_sdmult() not yet implemented for pattern./ ngCMatrix" */
	/* 	  " --> slightly inefficient coercion")); */

	// This *fails* to produce a CHOLMOD_REAL ..
	// CHM_SP chd = cholmod_l_copy(cha, cha->stype, CHOLMOD_REAL, &c);
	// --> use our Matrix-classes
	SEXP da = PROTECT(nz2Csparse(a, x_double)); nprot++;
	cha = AS_CHM_SP(da);
    }
    cholmod_sdmult(cha, 0, one, zero, chb, chc, &c);
    SET_VECTOR_ELT(dn, 0,	/* establish dimnames */
		   duplicate(VECTOR_ELT(GET_SLOT(a, Matrix_DimNamesSym), 0)));
    SET_VECTOR_ELT(dn, 1,
		   duplicate(VECTOR_ELT(GET_SLOT(b_M, Matrix_DimNamesSym), 1)));
    UNPROTECT(nprot);
    return chm_dense_to_SEXP(chc, 1, 0, dn);
}

SEXP Csparse_dense_crossprod(SEXP a, SEXP b)
{
    CHM_SP cha = AS_CHM_SP(a);
    SEXP b_M = PROTECT(mMatrix_as_dgeMatrix(b));
    CHM_DN chb = AS_CHM_DN(b_M);
    CHM_DN chc = cholmod_allocate_dense(cha->ncol, chb->ncol, cha->ncol,
					chb->xtype, &c);
    SEXP dn = PROTECT(allocVector(VECSXP, 2)); int nprot = 2;
    double one[] = {1,0}, zero[] = {0,0};
    R_CheckStack();
    // -- see Csparse_dense_prod() above :
    if(cha->xtype == CHOLMOD_PATTERN) {
	SEXP da = PROTECT(nz2Csparse(a, x_double)); nprot++;
	cha = AS_CHM_SP(da);
    }
    cholmod_sdmult(cha, 1, one, zero, chb, chc, &c);
    SET_VECTOR_ELT(dn, 0,	/* establish dimnames */
		   duplicate(VECTOR_ELT(GET_SLOT(a, Matrix_DimNamesSym), 1)));
    SET_VECTOR_ELT(dn, 1,
		   duplicate(VECTOR_ELT(GET_SLOT(b_M, Matrix_DimNamesSym), 1)));
    UNPROTECT(nprot);
    return chm_dense_to_SEXP(chc, 1, 0, dn);
}

/* Computes   x'x  or  x x' -- *also* for Tsparse (triplet = TRUE)
   see Csparse_Csparse_crossprod above for  x'y and x y' */
SEXP Csparse_crossprod(SEXP x, SEXP trans, SEXP triplet)
{
    int trip = asLogical(triplet),
	tr   = asLogical(trans); /* gets reversed because _aat is tcrossprod */
#ifdef AS_CHM_DIAGU2N_FIXED_FINALLY
    CHM_TR cht = trip ? AS_CHM_TR(x) : (CHM_TR) NULL;
#else /* workaround needed:*/
    SEXP xx = PROTECT(Tsparse_diagU2N(x));
    CHM_TR cht = trip ? AS_CHM_TR__(xx) : (CHM_TR) NULL;
#endif
    CHM_SP chcp, chxt,
	chx = (trip ?
	       cholmod_triplet_to_sparse(cht, cht->nnz, &c) :
	       AS_CHM_SP(x));
    SEXP dn = PROTECT(allocVector(VECSXP, 2));
    R_CheckStack();

    if (!tr) chxt = cholmod_transpose(chx, chx->xtype, &c);
    chcp = cholmod_aat((!tr) ? chxt : chx, (int *) NULL, 0, chx->xtype, &c);
    if(!chcp) {
	UNPROTECT(1);
	error(_("Csparse_crossprod(): error return from cholmod_aat()"));
    }
    cholmod_band_inplace(0, chcp->ncol, chcp->xtype, chcp, &c);
    chcp->stype = 1;
    if (trip) cholmod_free_sparse(&chx, &c);
    if (!tr) cholmod_free_sparse(&chxt, &c);
    SET_VECTOR_ELT(dn, 0,	/* establish dimnames */
		   duplicate(VECTOR_ELT(GET_SLOT(x, Matrix_DimNamesSym),
					(tr) ? 0 : 1)));
    SET_VECTOR_ELT(dn, 1, duplicate(VECTOR_ELT(dn, 0)));
#ifdef AS_CHM_DIAGU2N_FIXED_FINALLY
    UNPROTECT(1);
#else
    UNPROTECT(2);
#endif
    return chm_sparse_to_SEXP(chcp, 1, 0, 0, "", dn);
}

/* Csparse_drop(x, tol):  drop entries with absolute value < tol, i.e,
*  at least all "explicit" zeros */
SEXP Csparse_drop(SEXP x, SEXP tol)
{
    const char *cl = class_P(x);
    /* dtCMatrix, etc; [1] = the second character =?= 't' for triangular */
    int tr = (cl[1] == 't');
    CHM_SP chx = AS_CHM_SP__(x);
    CHM_SP ans = cholmod_copy(chx, chx->stype, chx->xtype, &c);
    double dtol = asReal(tol);
    int Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
    R_CheckStack();

    if(!cholmod_drop(dtol, ans, &c))
	error(_("cholmod_drop() failed"));
   return chm_sparse_to_SEXP(ans, 1,
			      tr ? ((*uplo_P(x) == 'U') ? 1 : -1) : 0,
			      Rkind, tr ? diag_P(x) : "",
			      GET_SLOT(x, Matrix_DimNamesSym));
}

SEXP Csparse_horzcat(SEXP x, SEXP y)
{
    CHM_SP chx = AS_CHM_SP__(x), chy = AS_CHM_SP__(y);
    int Rk_x = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0,
	Rk_y = (chy->xtype != CHOLMOD_PATTERN) ? Real_kind(y) : 0,
	Rkind = /* logical if both x and y are */ (Rk_x == 1 && Rk_y == 1) ? 1 : 0;
    R_CheckStack();

    /* TODO: currently drops dimnames - and we fix at R level */
    return chm_sparse_to_SEXP(cholmod_horzcat(chx, chy, 1, &c),
			      1, 0, Rkind, "", R_NilValue);
}

SEXP Csparse_vertcat(SEXP x, SEXP y)
{
    CHM_SP chx = AS_CHM_SP__(x), chy = AS_CHM_SP__(y);
    int Rk_x = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0,
	Rk_y = (chy->xtype != CHOLMOD_PATTERN) ? Real_kind(y) : 0,
	Rkind = /* logical if both x and y are */ (Rk_x == 1 && Rk_y == 1) ? 1 : 0;
    R_CheckStack();

    /* TODO: currently drops dimnames - and we fix at R level */
    return chm_sparse_to_SEXP(cholmod_vertcat(chx, chy, 1, &c),
			      1, 0, Rkind, "", R_NilValue);
}

SEXP Csparse_band(SEXP x, SEXP k1, SEXP k2)
{
    CHM_SP chx = AS_CHM_SP__(x);
    int Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
    CHM_SP ans = cholmod_band(chx, asInteger(k1), asInteger(k2), chx->xtype, &c);
    R_CheckStack();

    return chm_sparse_to_SEXP(ans, 1, 0, Rkind, "",
			      GET_SLOT(x, Matrix_DimNamesSym));
}

SEXP Csparse_diagU2N(SEXP x)
{
    const char *cl = class_P(x);
    /* dtCMatrix, etc; [1] = the second character =?= 't' for triangular */
    if (cl[1] != 't' || *diag_P(x) != 'U') {
	/* "trivially fast" when not triangular (<==> no 'diag' slot),
	   or not *unit* triangular */
	return (x);
    }
    else { /* unit triangular (diag='U'): "fill the diagonal" & diag:= "N" */
	CHM_SP chx = AS_CHM_SP__(x);
	CHM_SP eye = cholmod_speye(chx->nrow, chx->ncol, chx->xtype, &c);
	double one[] = {1, 0};
	CHM_SP ans = cholmod_add(chx, eye, one, one, TRUE, TRUE, &c);
	int uploT = (*uplo_P(x) == 'U') ? 1 : -1;
	int Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;

	R_CheckStack();
	cholmod_free_sparse(&eye, &c);
	return chm_sparse_to_SEXP(ans, 1, uploT, Rkind, "N",
				  GET_SLOT(x, Matrix_DimNamesSym));
    }
}

SEXP Csparse_diagN2U(SEXP x)
{
    const char *cl = class_P(x);
    /* dtCMatrix, etc; [1] = the second character =?= 't' for triangular */
    if (cl[1] != 't' || *diag_P(x) != 'N') {
	/* "trivially fast" when not triangular (<==> no 'diag' slot),
	   or already *unit* triangular */
	return (x);
    }
    else { /* triangular with diag='N'): now drop the diagonal */
	/* duplicate, since chx will be modified: */
	SEXP xx = PROTECT(duplicate(x));
	CHM_SP chx = AS_CHM_SP__(xx);
	int uploT = (*uplo_P(x) == 'U') ? 1 : -1,
	    Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
	R_CheckStack();

	chm_diagN2U(chx, uploT, /* do_realloc */ FALSE);

	SEXP ans = chm_sparse_to_SEXP(chx, /*dofree*/ 0/* or 1 ?? */,
				      uploT, Rkind, "U",
				      GET_SLOT(x, Matrix_DimNamesSym));
	UNPROTECT(1);// only now !
	return ans;
    }
}

/**
 * "Indexing" aka subsetting : Compute  x[i,j], also for vectors i and j
 * Working via CHOLMOD_submatrix, see ./CHOLMOD/MatrixOps/cholmod_submatrix.c
 * @param x CsparseMatrix
 * @param i row     indices (0-origin), or NULL (R's)
 * @param j columns indices (0-origin), or NULL
 *
 * @return x[i,j]  still CsparseMatrix --- currently, this loses dimnames
 */
SEXP Csparse_submatrix(SEXP x, SEXP i, SEXP j)
{
    CHM_SP chx = AS_CHM_SP(x); /* << does diagU2N() when needed */
    int rsize = (isNull(i)) ? -1 : LENGTH(i),
	csize = (isNull(j)) ? -1 : LENGTH(j);
    int Rkind = (chx->xtype != CHOLMOD_PATTERN) ? Real_kind(x) : 0;
    R_CheckStack();

    if (rsize >= 0 && !isInteger(i))
	error(_("Index i must be NULL or integer"));
    if (csize >= 0 && !isInteger(j))
	error(_("Index j must be NULL or integer"));

    if (!chx->stype) {/* non-symmetric Matrix */
	return chm_sparse_to_SEXP(cholmod_submatrix(chx,
						    (rsize < 0) ? NULL : INTEGER(i), rsize,
						    (csize < 0) ? NULL : INTEGER(j), csize,
						    TRUE, TRUE, &c),
				  1, 0, Rkind, "",
				  /* FIXME: drops dimnames */ R_NilValue);
    }
				/* for now, cholmod_submatrix() only accepts "generalMatrix" */
    CHM_SP tmp = cholmod_copy(chx, /* stype: */ 0, chx->xtype, &c);
    CHM_SP ans = cholmod_submatrix(tmp,
				   (rsize < 0) ? NULL : INTEGER(i), rsize,
				   (csize < 0) ? NULL : INTEGER(j), csize,
				   TRUE, TRUE, &c);
    cholmod_free_sparse(&tmp, &c);
    return chm_sparse_to_SEXP(ans, 1, 0, Rkind, "", R_NilValue);
}

#define _d_Csp_
#include "t_Csparse_subassign.c"

#define _l_Csp_
#include "t_Csparse_subassign.c"

#define _i_Csp_
#include "t_Csparse_subassign.c"

#define _n_Csp_
#include "t_Csparse_subassign.c"

#define _z_Csp_
#include "t_Csparse_subassign.c"



SEXP Csparse_MatrixMarket(SEXP x, SEXP fname)
{
    FILE *f = fopen(CHAR(asChar(fname)), "w");

    if (!f)
	error(_("failure to open file \"%s\" for writing"),
	      CHAR(asChar(fname)));
    if (!cholmod_write_sparse(f, AS_CHM_SP(x),
			      (CHM_SP)NULL, (char*) NULL, &c))
	error(_("cholmod_write_sparse returned error code"));
    fclose(f);
    return R_NilValue;
}


/**
 * Extract the diagonal entries from *triangular* Csparse matrix  __or__ a
 * cholmod_sparse factor (LDL = TRUE).
 *
 * @param n  dimension of the matrix.
 * @param x_p  'p' (column pointer) slot contents
 * @param x_x  'x' (non-zero entries) slot contents
 * @param perm 'perm' (= permutation vector) slot contents; only used for "diagBack"
 * @param resultKind a (SEXP) string indicating which kind of result is desired.
 *
 * @return  a SEXP, either a (double) number or a length n-vector of diagonal entries
 */
SEXP diag_tC_ptr(int n, int *x_p, double *x_x, int *perm, SEXP resultKind)
/*                                ^^^^^^ FIXME[Generalize] to int / ... */
{
    const char* res_ch = CHAR(STRING_ELT(resultKind,0));
    enum diag_kind { diag, diag_backpermuted, trace, prod, sum_log
    } res_kind = ((!strcmp(res_ch, "trace")) ? trace :
		  ((!strcmp(res_ch, "sumLog")) ? sum_log :
		   ((!strcmp(res_ch, "prod")) ? prod :
		    ((!strcmp(res_ch, "diag")) ? diag :
		     ((!strcmp(res_ch, "diagBack")) ? diag_backpermuted :
		      -1)))));
    int i, n_x, i_from = 0;
    SEXP ans = PROTECT(allocVector(REALSXP,
/*                                 ^^^^  FIXME[Generalize] */
				   (res_kind == diag ||
				    res_kind == diag_backpermuted) ? n : 1));
    double *v = REAL(ans);
/*  ^^^^^^      ^^^^  FIXME[Generalize] */

#define for_DIAG(v_ASSIGN)					\
    for(i = 0; i < n; i++, i_from += n_x) {			\
	/* looking at i-th column */				\
	n_x = x_p[i+1] - x_p[i];/* #{entries} in this column */	\
	v_ASSIGN;						\
    }

    /* NOTA BENE: we assume  -- uplo = "L" i.e. lower triangular matrix
     *            for uplo = "U" (makes sense with a "dtCMatrix" !),
     *            should use  x_x[i_from + (nx - 1)] instead of x_x[i_from],
     *            where nx = (x_p[i+1] - x_p[i])
     */

    switch(res_kind) {
    case trace:
	v[0] = 0.;
	for_DIAG(v[0] += x_x[i_from]);
	break;

    case sum_log:
	v[0] = 0.;
	for_DIAG(v[0] += log(x_x[i_from]));
	break;

    case prod:
	v[0] = 1.;
	for_DIAG(v[0] *= x_x[i_from]);
	break;

    case diag:
	for_DIAG(v[i] = x_x[i_from]);
	break;

    case diag_backpermuted:
	for_DIAG(v[i] = x_x[i_from]);

	warning(_("%s = '%s' (back-permuted) is experimental"),
		"resultKind", "diagBack");
	/* now back_permute : */
	for(i = 0; i < n; i++) {
	    double tmp = v[i]; v[i] = v[perm[i]]; v[perm[i]] = tmp;
	    /*^^^^ FIXME[Generalize] */
	}
	break;

    default: /* -1 from above */
	error(_("diag_tC(): invalid 'resultKind'"));
	/* Wall: */ ans = R_NilValue; v = REAL(ans);
    }

    UNPROTECT(1);
    return ans;
}

/**
 * Extract the diagonal entries from *triangular* Csparse matrix  __or__ a
 * cholmod_sparse factor (LDL = TRUE).
 *
 * @param pslot  'p' (column pointer)   slot of Csparse matrix/factor
 * @param xslot  'x' (non-zero entries) slot of Csparse matrix/factor
 * @param perm_slot  'perm' (= permutation vector) slot of corresponding CHMfactor;
 *		     only used for "diagBack"
 * @param resultKind a (SEXP) string indicating which kind of result is desired.
 *
 * @return  a SEXP, either a (double) number or a length n-vector of diagonal entries
 */
SEXP diag_tC(SEXP pslot, SEXP xslot, SEXP perm_slot, SEXP resultKind)
{
    int n = length(pslot) - 1, /* n = ncol(.) = nrow(.) */
	*x_p  = INTEGER(pslot),
	*perm = INTEGER(perm_slot);
    double *x_x = REAL(xslot);
/*  ^^^^^^        ^^^^ FIXME[Generalize] to INTEGER(.) / LOGICAL(.) / ... xslot !*/

    return diag_tC_ptr(n, x_p, x_x, perm, resultKind);
}

/**
 * Create a Csparse matrix object from indices and/or pointers.
 *
 * @param cls name of actual class of object to create
 * @param i optional integer vector of length nnz of row indices
 * @param j optional integer vector of length nnz of column indices
 * @param p optional integer vector of length np of row or column pointers
 * @param np length of integer vector p.  Must be zero if p == (int*)NULL
 * @param x optional vector of values
 * @param nnz length of vectors i, j and/or x, whichever is to be used
 * @param dims optional integer vector of length 2 to be used as
 *     dimensions.  If dims == (int*)NULL then the maximum row and column
 *     index are used as the dimensions.
 * @param dimnames optional list of length 2 to be used as dimnames
 * @param index1 indicator of 1-based indices
 *
 * @return an SEXP of class cls inheriting from CsparseMatrix.
 */
SEXP create_Csparse(char* cls, int* i, int* j, int* p, int np,
		    void* x, int nnz, int* dims, SEXP dimnames,
		    int index1)
{
    SEXP ans;
    int *ij = (int*)NULL, *tri, *trj,
	mi, mj, mp, nrow = -1, ncol = -1;
    int xtype = -1;		/* -Wall */
    CHM_TR T;
    CHM_SP A;

    if (np < 0 || nnz < 0)
	error(_("negative vector lengths not allowed: np = %d, nnz = %d"),
	      np, nnz);
    if (1 != ((mi = (i == (int*)NULL)) +
	      (mj = (j == (int*)NULL)) +
	      (mp = (p == (int*)NULL))))
	error(_("exactly 1 of 'i', 'j' or 'p' must be NULL"));
    if (mp) {
	if (np) error(_("np = %d, must be zero when p is NULL"), np);
    } else {
	if (np) {		/* Expand p to form i or j */
	    if (!(p[0])) error(_("p[0] = %d, should be zero"), p[0]);
	    for (int ii = 0; ii < np; ii++)
		if (p[ii] > p[ii + 1])
		    error(_("p must be non-decreasing"));
	    if (p[np] != nnz)
		error("p[np] = %d != nnz = %d", p[np], nnz);
	    ij = Calloc(nnz, int);
	    if (mi) {
		i = ij;
		nrow = np;
	    } else {
		j = ij;
		ncol = np;
	    }
	    /* Expand p to 0-based indices */
	    for (int ii = 0; ii < np; ii++)
		for (int jj = p[ii]; jj < p[ii + 1]; jj++) ij[jj] = ii;
	} else {
	    if (nnz)
		error(_("Inconsistent dimensions: np = 0 and nnz = %d"),
		      nnz);
	}
    }
    /* calculate nrow and ncol */
    if (nrow < 0) {
	for (int ii = 0; ii < nnz; ii++) {
	    int i1 = i[ii] + (index1 ? 0 : 1); /* 1-based index */
	    if (i1 < 1) error(_("invalid row index at position %d"), ii);
	    if (i1 > nrow) nrow = i1;
	}
    }
    if (ncol < 0) {
	for (int jj = 0; jj < nnz; jj++) {
	    int j1 = j[jj] + (index1 ? 0 : 1);
	    if (j1 < 1) error(_("invalid column index at position %d"), jj);
	    if (j1 > ncol) ncol = j1;
	}
    }
    if (dims != (int*)NULL) {
	if (dims[0] > nrow) nrow = dims[0];
	if (dims[1] > ncol) ncol = dims[1];
    }
    /* check the class name */
    if (strlen(cls) != 8)
	error(_("strlen of cls argument = %d, should be 8"), strlen(cls));
    if (!strcmp(cls + 2, "CMatrix"))
	error(_("cls = \"%s\" does not end in \"CMatrix\""), cls);
    switch(cls[0]) {
    case 'd':
    case 'l':
	xtype = CHOLMOD_REAL;
    break;
    case 'n':
	xtype = CHOLMOD_PATTERN;
	break;
    default:
	error(_("cls = \"%s\" must begin with 'd', 'l' or 'n'"), cls);
    }
    if (cls[1] != 'g')
	error(_("Only 'g'eneral sparse matrix types allowed"));
    /* allocate and populate the triplet */
    T = cholmod_allocate_triplet((size_t)nrow, (size_t)ncol, (size_t)nnz, 0,
				 xtype, &c);
    T->x = x;
    tri = (int*)T->i;
    trj = (int*)T->j;
    for (int ii = 0; ii < nnz; ii++) {
	tri[ii] = i[ii] - ((!mi && index1) ? 1 : 0);
	trj[ii] = j[ii] - ((!mj && index1) ? 1 : 0);
    }
    /* create the cholmod_sparse structure */
    A = cholmod_triplet_to_sparse(T, nnz, &c);
    cholmod_free_triplet(&T, &c);
    /* copy the information to the SEXP */
    ans = PROTECT(NEW_OBJECT(MAKE_CLASS(cls)));
/* FIXME: This has been copied from chm_sparse_to_SEXP in chm_common.c */
    /* allocate and copy common slots */
    nnz = cholmod_nnz(A, &c);
    dims = INTEGER(ALLOC_SLOT(ans, Matrix_DimSym, INTSXP, 2));
    dims[0] = A->nrow; dims[1] = A->ncol;
    Memcpy(INTEGER(ALLOC_SLOT(ans, Matrix_pSym, INTSXP, A->ncol + 1)), (int*)A->p, A->ncol + 1);
    Memcpy(INTEGER(ALLOC_SLOT(ans, Matrix_iSym, INTSXP, nnz)), (int*)A->i, nnz);
    switch(cls[1]) {
    case 'd':
	Memcpy(REAL(ALLOC_SLOT(ans, Matrix_xSym, REALSXP, nnz)), (double*)A->x, nnz);
	break;
    case 'l':
	error(_("code not yet written for cls = \"lgCMatrix\""));
    }
/* FIXME: dimnames are *NOT* put there yet (if non-NULL) */
    cholmod_free_sparse(&A, &c);
    UNPROTECT(1);
    return ans;
}
