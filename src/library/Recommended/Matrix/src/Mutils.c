#include <limits.h>

#include "Mutils.h"
#include <R_ext/Lapack.h>

#if 0 /* defined(R_VERSION) && R_VERSION >= R_Version(2, 7, 0) *
       * La_norm_type() & La_rcond_type() are now in R_ext/Lapack.h
       * but because of the 'module-mess' that's not sufficient */
#else
char La_norm_type(const char *typstr)
{
    char typup;

    if (strlen(typstr) != 1)
	error(
	    _("argument type[1]='%s' must be a one-letter character string"),
	    typstr);
    typup = toupper(*typstr);
    if (typup == '1')
	typup = 'O'; /* alias */
    else if (typup == 'E')
	typup = 'F';
    else if (typup != 'M' && typup != 'O' && typup != 'I' && typup != 'F')
	error(_("argument type[1]='%s' must be one of 'M','1','O','I','F' or 'E'"),
	      typstr);
    return typup;
}

char La_rcond_type(const char *typstr)
{
    char typup;

    if (strlen(typstr) != 1)
	error(
	    _("argument type[1]='%s' must be a one-letter character string"),
	    typstr);
    typup = toupper(*typstr);
    if (typup == '1')
	typup = 'O'; /* alias */
    else if (typup != 'O' && typup != 'I')
	error(_("argument type[1]='%s' must be one of '1','O', or 'I'"),
	      typstr);
    return typup;
}
#endif

double get_double_by_name(SEXP obj, char *nm)
{
    SEXP nms = getAttrib(obj, R_NamesSymbol);
    int i, len = length(obj);

    if ((!isReal(obj)) || (length(obj) > 0 && nms == R_NilValue))
	error(_("object must be a named, numeric vector"));
    for (i = 0; i < len; i++) {
	if (!strcmp(nm, CHAR(STRING_ELT(nms, i)))) {
	    return REAL(obj)[i];
	}
    }
    return R_NaReal;
}

SEXP
set_double_by_name(SEXP obj, double val, char *nm)
{
    SEXP nms = getAttrib(obj, R_NamesSymbol);
    int i, len = length(obj);

    if ((!isReal(obj)) || (length(obj) > 0 && nms == R_NilValue))
	error(_("object must be a named, numeric vector"));
    for (i = 0; i < len; i++) {
	if (!strcmp(nm, CHAR(STRING_ELT(nms, i)))) {
	    REAL(obj)[i] = val;
	    return obj;
	}
    }
    {
	SEXP nx = PROTECT(allocVector(REALSXP, len + 1)),
	    nnms = allocVector(STRSXP, len + 1);

	setAttrib(nx, R_NamesSymbol, nnms);
	for (i = 0; i < len; i++) {
	    REAL(nx)[i] = REAL(obj)[i];
	    SET_STRING_ELT(nnms, i, duplicate(STRING_ELT(nms, i)));
	}
	REAL(nx)[len] = val;
	SET_STRING_ELT(nnms, len, mkChar(nm));
	UNPROTECT(1);
	return nx;
    }
}

SEXP as_det_obj(double val, int log, int sign)
{
    SEXP det = PROTECT(allocVector(VECSXP, 2)),
	nms = PROTECT(allocVector(STRSXP, 2)),
	vv = PROTECT(ScalarReal(val));

    setAttrib(det, R_NamesSymbol, nms);
    SET_STRING_ELT(nms, 0, mkChar("modulus"));
    SET_STRING_ELT(nms, 1, mkChar("sign"));
    setAttrib(vv, install("logarithm"), ScalarLogical(log));
    SET_VECTOR_ELT(det, 0, vv);
    SET_VECTOR_ELT(det, 1, ScalarInteger(sign));
    setAttrib(det, R_ClassSymbol, mkString("det"));
    UNPROTECT(3);
    return det;
}

SEXP get_factors(SEXP obj, char *nm)
{
    SEXP fac = GET_SLOT(obj, Matrix_factorSym),
	nms = getAttrib(fac, R_NamesSymbol);
    int i, len = length(fac);

    if ((!isNewList(fac)) || (length(fac) > 0 && nms == R_NilValue))
	error(_("'factors' slot must be a named list"));
    for (i = 0; i < len; i++) {
	if (!strcmp(nm, CHAR(STRING_ELT(nms, i)))) {
	    return VECTOR_ELT(fac, i);
	}
    }
    return R_NilValue;
}

/* In the past this function installed a duplicate of val in the
 * factors slot for obj then returned the (typically unprotected)
 * val.  This is now changed to return the duplicate, which will be
 * protected if obj is protected. */
SEXP set_factors(SEXP obj, SEXP val, char *nm)
{
    SEXP fac = GET_SLOT(obj, Matrix_factorSym),
	nms = getAttrib(fac, R_NamesSymbol), nfac, nnms;
    int i, len = length(fac);

    if ((!isNewList(fac)) || (length(fac) > 0 && nms == R_NilValue))
	error(_("'factors' slot must be a named list"));
    PROTECT(val); /* set_factors(..) may be called as "finalizer" after UNPROTECT()*/
    for (i = 0; i < len; i++) {
	if (!strcmp(nm, CHAR(STRING_ELT(nms, i)))) {
	    SET_VECTOR_ELT(fac, i, duplicate(val));
	    return val;
	}
    }
    nfac = PROTECT(allocVector(VECSXP, len + 1));
    nnms = PROTECT(allocVector(STRSXP, len + 1));
    setAttrib(nfac, R_NamesSymbol, nnms);
    for (i = 0; i < len; i++) {
	SET_VECTOR_ELT(nfac, i, VECTOR_ELT(fac, i));
	SET_STRING_ELT(nnms, i, duplicate(STRING_ELT(nms, i)));
    }
    SET_VECTOR_ELT(nfac, len, duplicate(val));
    SET_STRING_ELT(nnms, len, mkChar(nm));
    SET_SLOT(obj, Matrix_factorSym, nfac);
    UNPROTECT(3);
    return VECTOR_ELT(nfac, len);
}

#if 0 				/* unused */
/* useful for all the ..CMatrix classes (and ..R by [0] <-> [1]); but unused */
SEXP CMatrix_set_Dim(SEXP x, int nrow)
{
    int *dims = INTEGER(GET_SLOT(x, Matrix_DimSym));

    dims[0] = nrow;
    dims[1] = length(GET_SLOT(x, Matrix_pSym)) - 1;
    return x;
}
#endif	/* unused */

/* Fill in the "trivial remainder" in  n*m  array ;
 *  typically the 'x' slot of a "dtrMatrix" :
 * But should be usable for double/logical/int/complex : */

#define MAKE_TRIANGULAR_BODY(_TO_, _FROM_, _ZERO_, _ONE_)	\
{								\
    int i, j, *dims = INTEGER(GET_SLOT(_FROM_, Matrix_DimSym));	\
    int n = dims[0], m = dims[1];				\
								\
    if (*uplo_P(_FROM_) == 'U') {				\
	for (j = 0; j < n; j++)					\
	    for (i = j+1; i < m; i++)				\
		_TO_[i + j*m] = _ZERO_;				\
    } else {							\
	for (j = 1; j < n; j++)					\
	    for (i = 0; i < j && i < m; i++)			\
		_TO_[i + j*m] = _ZERO_;				\
    }								\
    if (*diag_P(_FROM_) == 'U') {				\
	j = (n < m) ? n : m;					\
	for (i = 0; i < j; i++)					\
	    _TO_[i * (m + 1)] = _ONE_;				\
    }								\
}

void make_d_matrix_triangular(double *to, SEXP from)
    MAKE_TRIANGULAR_BODY(to, from, 0., 1.)
void make_i_matrix_triangular(int *to, SEXP from)
    MAKE_TRIANGULAR_BODY(to, from, 0, 1)


/* Should work for double/logical/int/complex : */
#define MAKE_SYMMETRIC_BODY(_TO_, _FROM_)			\
{								\
    int i, j, n = INTEGER(GET_SLOT(_FROM_, Matrix_DimSym))[0];	\
								\
    if (*uplo_P(_FROM_) == 'U') {				\
	for (j = 0; j < n; j++)					\
	    for (i = j+1; i < n; i++)				\
		_TO_[i + j*n] = _TO_[j + i*n];			\
    } else {							\
	for (j = 1; j < n; j++)					\
	    for (i = 0; i < j && i < n; i++)			\
		_TO_[i + j*n] = _TO_[j + i*n];			\
    }								\
}

void make_d_matrix_symmetric(double *to, SEXP from)
    MAKE_SYMMETRIC_BODY(to, from)

void make_i_matrix_symmetric(int *to, SEXP from)
    MAKE_SYMMETRIC_BODY(to, from)


#define Matrix_Error_Bufsiz    4096

/**
 * Check validity of 1-letter string from a set of possible values
 * (typically used in  S4 validity method)
 *
 * @param sP
 * @param vals a string containing the possible valid letters
 * @param nm   the name of the slot being checked
 *
 * @return a SEXP, either NULL (= success) or an error message
 */
SEXP check_scalar_string(SEXP sP, char *vals, char *nm)
{
    SEXP val = ScalarLogical(1);
    char *buf;
    /* only allocate when needed: in good case, none is needed */
#define SPRINTF buf = Alloca(Matrix_Error_Bufsiz, char); R_CheckStack(); sprintf

    if (length(sP) != 1) {
	SPRINTF(buf, _("'%s' slot must have length 1"), nm);
    } else {
	const char *str = CHAR(STRING_ELT(sP, 0));
	if (strlen(str) != 1) {
	    SPRINTF(buf, _("'%s' must have string length 1"), nm);
	} else {
	    int i, len;
	    for (i = 0, len = strlen(vals); i < len; i++) {
		if (str[0] == vals[i])
		    return R_NilValue;
	    }
	    SPRINTF(buf, _("'%s' must be in '%s'"), nm, vals);
	}
    }
    /* 'error' returns : */
    val = mkString(buf);
    return val;
#undef SPRINTF
}

/* FIXME? Something like this should be part of the R API ?
 *        But then, R has the more general  compute_identical()
 * in src/main/identical.c: Rboolean compute_identical(SEXP x, SEXP y);
*/
Rboolean equal_string_vectors(SEXP s1, SEXP s2)
{
    Rboolean n1 = isNull(s1), n2 = isNull(s2);
    if (n1 || n2)
	return (n1 == n2) ? TRUE : FALSE;
    else if (TYPEOF(s1) != STRSXP || TYPEOF(s2) != STRSXP) {
	error(_("'s1' and 's2' must be \"character\" vectors"));
	return FALSE; /* -Wall */
    } else {
	int n = LENGTH(s1), i;
	if (n != LENGTH(s2))
	    return FALSE;
	for(i = 0; i < n; i++) {
	    /* note that compute_identical() code for STRSXP
	       is careful about NA's which we don't need */
	    if(strcmp(CHAR(STRING_ELT(s1, i)),
		      CHAR(STRING_ELT(s2, i))))
		return FALSE;
	}
	return TRUE; /* they *are* equal */
    }
}


SEXP dense_nonpacked_validate(SEXP obj)
{
    int *dims = INTEGER(GET_SLOT(obj, Matrix_DimSym));
    if ((dims[0] * dims[1]) != length(GET_SLOT(obj, Matrix_xSym)))
	return mkString(_("length of x slot != prod(Dim)"));
    return ScalarLogical(1);
}


#define PACKED_TO_FULL(TYPE)						\
TYPE *packed_to_full_ ## TYPE(TYPE *dest, const TYPE *src,		\
		        int n, enum CBLAS_UPLO uplo)			\
{									\
    int i, j, pos = 0;							\
									\
    AZERO(dest, n*n);							\
    for (j = 0; j < n; j++) {						\
	switch(uplo) {							\
	case UPP:							\
	    for (i = 0; i <= j; i++) dest[i + j * n] = src[pos++];	\
	    break;							\
	case LOW:							\
	    for (i = j; i < n; i++) dest[i + j * n] = src[pos++];	\
	    break;							\
	default:							\
	    error(_("'uplo' must be UPP or LOW"));			\
	}								\
    }									\
    return dest;							\
}

PACKED_TO_FULL(double)
PACKED_TO_FULL(int)

#define FULL_TO_PACKED(TYPE)						\
TYPE *full_to_packed_ ## TYPE(TYPE *dest, const TYPE *src, int n,	\
		      enum CBLAS_UPLO uplo, enum CBLAS_DIAG diag)	\
{									\
    int i, j, pos = 0;							\
									\
    for (j = 0; j < n; j++) {						\
	switch(uplo) {							\
	case UPP:							\
	    for (i = 0; i <= j; i++)					\
		dest[pos++] = (i == j && diag== UNT) ? 1 : src[i + j*n]; \
	    break;							\
	case LOW:							\
	    for (i = j; i < n; i++)					\
		dest[pos++] = (i == j && diag== UNT) ? 1 : src[i + j*n]; \
	    break;							\
	default:							\
	    error(_("'uplo' must be UPP or LOW"));			\
	}								\
    }									\
    return dest;							\
}

FULL_TO_PACKED(double)
FULL_TO_PACKED(int)



/**
 * Copy the diagonal elements of the packed denseMatrix x to dest
 *
 * @param dest vector of length ncol(x)
 * @param x pointer to an object representing a packed array
 * @param n number of columns in the matrix represented by x
 *
 * @return dest
 */
void d_packed_getDiag(double *dest, SEXP x, int n)
{
    double *xx = REAL(GET_SLOT(x, Matrix_xSym));

#define END_packed_getDiag						\
    int j, pos = 0;							\
									\
    if (*uplo_P(x) == 'U') {						\
	for(pos= 0, j=0; j < n; pos += 1+(++j))	 dest[j] = xx[pos];	\
    } else {								\
	for(pos= 0, j=0; j < n; pos += (n - j), j++) dest[j] = xx[pos]; \
    }									\
    return

    END_packed_getDiag;
}

void l_packed_getDiag(int *dest, SEXP x, int n)
{
    int *xx = LOGICAL(GET_SLOT(x, Matrix_xSym));

    END_packed_getDiag;
}

#undef END_packed_getDiag

void tr_d_packed_getDiag(double *dest, SEXP x)
{
    int n = INTEGER(GET_SLOT(x, Matrix_DimSym))[0];
    SEXP val = PROTECT(allocVector(REALSXP, n));
    double *v = REAL(val);

    if (*diag_P(x) == 'U') {
	int j;
	for (j = 0; j < n; j++) v[j] = 1.;
    } else {
	d_packed_getDiag(v, x, n);
    }
    UNPROTECT(1);
    return;
}

void tr_l_packed_getDiag(   int *dest, SEXP x)
{
    int n = INTEGER(GET_SLOT(x, Matrix_DimSym))[0];
    SEXP val = PROTECT(allocVector(LGLSXP, n));
    int *v = LOGICAL(val);

    if (*diag_P(x) == 'U') {
	int j;
	for (j = 0; j < n; j++) v[j] = 1;
    } else {
	l_packed_getDiag(v, x, n);
    }
    UNPROTECT(1);
    return;
}

SEXP Matrix_expand_pointers(SEXP pP)
{
    int n = length(pP) - 1;
    int *p = INTEGER(pP);
    SEXP ans = PROTECT(allocVector(INTSXP, p[n]));

    expand_cmprPt(n, p, INTEGER(ans));
    UNPROTECT(1);
    return ans;
}


/**
 * Return the element of a given name from a named list
 *
 * @param list
 * @param nm name of desired element
 *
 * @return element of list with name nm
 */
SEXP
Matrix_getElement(SEXP list, char *nm) {
    SEXP names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < LENGTH(names); i++)
	if (!strcmp(CHAR(STRING_ELT(names, i)), nm))
	    return(VECTOR_ELT(list, i));
    return R_NilValue;
}

/**
 * Zero a square matrix of size nc then copy a vector to the diagonal
 *
 * @param dest destination array of length nc * nc
 * @param A pointer to a square Matrix object
 *
 * @return dest
 */
static double *
install_diagonal(double *dest, SEXP A)
{
    int nc = INTEGER(GET_SLOT(A, Matrix_DimSym))[0];
    int i, ncp1 = nc + 1, unit = *diag_P(A) == 'U';
    double *ax = REAL(GET_SLOT(A, Matrix_xSym));

    AZERO(dest, nc * nc);
    for (i = 0; i < nc; i++)
	dest[i * ncp1] = (unit) ? 1. : ax[i];
    return dest;
}

static int *
install_diagonal_int(int *dest, SEXP A)
{
    int nc = INTEGER(GET_SLOT(A, Matrix_DimSym))[0];
    int i, ncp1 = nc + 1, unit = *diag_P(A) == 'U';
    int *ax = INTEGER(GET_SLOT(A, Matrix_xSym));

    AZERO(dest, nc * nc);
    for (i = 0; i < nc; i++)
	dest[i * ncp1] = (unit) ? 1 : ax[i];
    return dest;
}


/**  Duplicate a [dln]denseMatrix or a numeric matrix or vector
 *  as a [dln]geMatrix.
 *  This is for the many *_matrix_{prod,crossprod,tcrossprod,etc.}
 *  functions that work with both classed and unclassed matrices.
 *
 * @param A	  either a denseMatrix object or a matrix object
 */
/* NOTA BENE: If you enlarge this list, do change '14' and '6' below !
 * ---------  ddiMatrix & ldiMatrix  are no longer ddense or ldense on the R level,
 *            ---         ---        but are still dealt with here.
 */

#define ddense_CLASSES							\
		    "dgeMatrix", "dtrMatrix",				\
		    "dsyMatrix", "dpoMatrix", "ddiMatrix",		\
		    "dtpMatrix", "dspMatrix", "dppMatrix",		\
		    /* sub classes of those above:*/			\
		    /* dtr */ "Cholesky", "LDL", "BunchKaufman",	\
		    /* dtp */ "pCholesky", "pBunchKaufman",		\
		    /* dpo */ "corMatrix"

#define ldense_CLASSES					\
		    "lgeMatrix", "ltrMatrix",		\
		    "lsyMatrix", "ldiMatrix",		\
		    "ltpMatrix", "lspMatrix"

#define ndense_CLASSES					\
		    "ngeMatrix", "ntrMatrix",		\
		    "nsyMatrix",			\
		    "ntpMatrix", "nspMatrix"

/* Generalized -- "geMatrix" -- dispatch where needed : */
SEXP dup_mMatrix_as_geMatrix(SEXP A)
{
    SEXP ans, ad = R_NilValue, an = R_NilValue;	/* -Wall */
    static const char *valid[] = {
	"_NOT_A_CLASS_",/* *_CLASSES defined in ./Mutils.h */
	ddense_CLASSES, /* 14 */
	ldense_CLASSES, /* 6  */
	ndense_CLASSES, /* 5  */
	""};
    int sz, ctype = Matrix_check_class_etc(A, valid),
	nprot = 1;
    enum dense_enum { ddense, ldense, ndense } M_type = ddense /* -Wall */;

    if (ctype > 0) { /* a [nld]denseMatrix object */
	ad = GET_SLOT(A, Matrix_DimSym);
	an = GET_SLOT(A, Matrix_DimNamesSym);
	M_type = (ctype <= 14) ? ddense :
	    ((ctype <= 14+6) ? ldense : ndense);
    }
    else if (ctype < 0) {	/* not a (recognized) classed matrix */

	if (isReal(A))
	    M_type = ddense;
	else if (isInteger(A)) {
	    A = PROTECT(coerceVector(A, REALSXP));
	    nprot++;
	    M_type = ddense;
	}
	else if (isLogical(A))
	    M_type = ldense;
	else
	    error(_("invalid class '%s' to dup_mMatrix_as_geMatrix"),
		  class_P(A));

#define	DUP_MMATRIX_NON_CLASS						\
	if (isMatrix(A)) {	/* "matrix" */				\
	    ad = getAttrib(A, R_DimSymbol);				\
	    an = getAttrib(A, R_DimNamesSymbol);			\
	} else {/* maybe "numeric" (incl integer,logical) --> (n x 1) */\
	    int* dd = INTEGER(ad = PROTECT(allocVector(INTSXP, 2)));	\
	    nprot++;							\
	    dd[0] = LENGTH(A);						\
	    dd[1] = 1;							\
	    an = R_NilValue;						\
	}								\
	ctype = 0

	DUP_MMATRIX_NON_CLASS;
    }

    ans = PROTECT(NEW_OBJECT(MAKE_CLASS(M_type == ddense ? "dgeMatrix" :
					(M_type == ldense ? "lgeMatrix" :
					 "ngeMatrix"))));
#define DUP_MMATRIX_SET_1					\
    SET_SLOT(ans, Matrix_DimSym, duplicate(ad));		\
    SET_SLOT(ans, Matrix_DimNamesSym, (LENGTH(an) == 2) ? 	\
	     duplicate(an): allocVector(VECSXP, 2));		\
    sz = INTEGER(ad)[0] * INTEGER(ad)[1]

    DUP_MMATRIX_SET_1;

    if(M_type == ddense) { /* ddense -> dge */

	double *ansx;

#define DUP_MMATRIX_ddense_CASES						\
	ansx = REAL(ALLOC_SLOT(ans, Matrix_xSym, REALSXP, sz));			\
	switch(ctype) {								\
	case 0:			/* unclassed real matrix */			\
	    Memcpy(ansx, REAL(A), sz);						\
	    break;								\
	case 1:			/* dgeMatrix */					\
	    Memcpy(ansx, REAL(GET_SLOT(A, Matrix_xSym)), sz);			\
	    break;								\
	case 2:			/* dtrMatrix   and subclasses */		\
	case 9: case 10: case 11:   /* ---	Cholesky, LDL, BunchKaufman */	\
	    Memcpy(ansx, REAL(GET_SLOT(A, Matrix_xSym)), sz);			\
	    make_d_matrix_triangular(ansx, A);					\
	    break;								\
	case 3:			/* dsyMatrix */					\
	case 4:			/* dpoMatrix  + subclass */			\
	case 14:	 		/* ---	corMatrix */			\
	    Memcpy(ansx, REAL(GET_SLOT(A, Matrix_xSym)), sz);			\
	    make_d_matrix_symmetric(ansx, A);					\
	    break;								\
	case 5:			/* ddiMatrix */					\
	    install_diagonal(ansx, A);						\
	    break;								\
	case 6:			/* dtpMatrix  + subclasses */			\
	case 12: case 13: 		/* ---	pCholesky, pBunchKaufman */	\
	    packed_to_full_double(ansx, REAL(GET_SLOT(A, Matrix_xSym)),		\
				  INTEGER(ad)[0],				\
				  *uplo_P(A) == 'U' ? UPP : LOW);		\
	    make_d_matrix_triangular(ansx, A);					\
	    break;								\
	case 7:			/* dspMatrix */					\
	case 8:			/* dppMatrix */					\
	    packed_to_full_double(ansx, REAL(GET_SLOT(A, Matrix_xSym)),		\
				  INTEGER(ad)[0],				\
				  *uplo_P(A) == 'U' ? UPP : LOW);		\
	    make_d_matrix_symmetric(ansx, A);					\
	    break;								\
	}  /* switch(ctype) */

	DUP_MMATRIX_ddense_CASES

    }
    else { /* M_type == ldense || M_type = ndense  */
	/* ldense -> lge */
	/* ndense -> nge */
	int *ansx = LOGICAL(ALLOC_SLOT(ans, Matrix_xSym, LGLSXP, sz));

	switch(ctype) {
	case 0:			/* unclassed logical matrix */
	    Memcpy(ansx, LOGICAL(A), sz);
	    break;

	case 1+14:			/* lgeMatrix */
	case 1+14+6:			/* ngeMatrix */
	    Memcpy(ansx, LOGICAL(GET_SLOT(A, Matrix_xSym)), sz);
	    break;
	case 2+14:			/* ltrMatrix */
	case 2+14+6:			/* ntrMatrix */
	    Memcpy(ansx, LOGICAL(GET_SLOT(A, Matrix_xSym)), sz);
	    make_i_matrix_triangular(ansx, A);
	    break;
	case 3+14:			/* lsyMatrix */
	case 3+14+6:			/* nsyMatrix */
	    Memcpy(ansx, LOGICAL(GET_SLOT(A, Matrix_xSym)), sz);
	    make_i_matrix_symmetric(ansx, A);
	    break;
	case 4+14:			/* ldiMatrix */
	case 4+14+6:			/* ndiMatrix */
	    install_diagonal_int(ansx, A);
	    break;
	case 5+14:			/* ltpMatrix */
	case 5+14+6:			/* ntpMatrix */
	    packed_to_full_int(ansx, LOGICAL(GET_SLOT(A, Matrix_xSym)),
			       INTEGER(ad)[0],
			       *uplo_P(A) == 'U' ? UPP : LOW);
	    make_i_matrix_triangular(ansx, A);
	    break;
	case 6+14:			/* lspMatrix */
	case 6+14+6:			/* nspMatrix */
	    packed_to_full_int(ansx, LOGICAL(GET_SLOT(A, Matrix_xSym)),
			       INTEGER(ad)[0],
			       *uplo_P(A) == 'U' ? UPP : LOW);
	    make_i_matrix_symmetric(ansx, A);
	    break;

	default:
	    error(_("unexpected ctype = %d in dup_mMatrix_as_geMatrix"), ctype);
	}  /* switch(ctype) */

    }  /* if(M_type == .) */

    UNPROTECT(nprot);
    return ans;
}

SEXP dup_mMatrix_as_dgeMatrix(SEXP A)
{
    SEXP ans = PROTECT(NEW_OBJECT(MAKE_CLASS("dgeMatrix"))),
	ad = R_NilValue , an = R_NilValue;	/* -Wall */
    static const char *valid[] = {"_NOT_A_CLASS_", ddense_CLASSES, ""};
    int ctype = Matrix_check_class_etc(A, valid),
	nprot = 1, sz;
    double *ansx;

    if (ctype > 0) {		/* a ddenseMatrix object */
	ad = GET_SLOT(A, Matrix_DimSym);
	an = GET_SLOT(A, Matrix_DimNamesSym);
    }
    else if (ctype < 0) {	/* not a (recognized) classed matrix */

	DUP_MMATRIX_NON_CLASS;

	if (isInteger(A) || isLogical(A)) {
	    A = PROTECT(coerceVector(A, REALSXP));
	    nprot++;
	}
	if (!isReal(A))
	    error(_("invalid class '%s' to dup_mMatrix_as_dgeMatrix"),
		  class_P(A));
    }

    DUP_MMATRIX_SET_1;

    DUP_MMATRIX_ddense_CASES

    UNPROTECT(nprot);
    return ans;
}


SEXP new_dgeMatrix(int nrow, int ncol)
{
    SEXP ans = PROTECT(NEW_OBJECT(MAKE_CLASS("dgeMatrix"))),
	 ad = PROTECT(allocVector(INTSXP, 2));

    INTEGER(ad)[0] = nrow;
    INTEGER(ad)[1] = ncol;
    SET_SLOT(ans, Matrix_DimSym, ad);
    SET_SLOT(ans, Matrix_DimNamesSym, allocVector(VECSXP, 2));
    ALLOC_SLOT(ans, Matrix_xSym, REALSXP, nrow * ncol);

    UNPROTECT(2);
    return ans;
}

/**
 * Encode Matrix index (i,j)  |-->  i + j * nrow   {i,j : 0-origin}
 *
 * @param ij: 2-column integer matrix
 * @param di: dim(.), i.e. length 2 integer vector
 * @param chk_bnds: logical indicating  0 <= ij[,k] < di[k]  need to be checked.
 *
 * @return encoded index; integer if prod(dim) is small; double otherwise
 */
SEXP m_encodeInd(SEXP ij, SEXP di, SEXP chk_bnds)
{
    SEXP ans;
    int *ij_di = NULL, n, *Di = INTEGER(di), *IJ, *j_;
    Rboolean check_bounds = asLogical(chk_bnds);

    ij = PROTECT(coerceVector(ij, INTSXP));
    if(!isMatrix(ij) ||
       (ij_di = INTEGER(getAttrib(ij, R_DimSymbol)))[1] != 2)
	error(_("Argument ij must be 2-column integer matrix"));
    n = ij_di[0];
    IJ = INTEGER(ij);
    j_ = IJ+n;/* pointer offset! */

    if((Di[0] * (double) Di[1]) >= 1 + (double)INT_MAX) { /* need double */
	ans = PROTECT(allocVector(REALSXP, n));
	double *ii = REAL(ans), nr = (double) Di[0];
#define do_ii_FILL(_i_, _j_)					\
	int i;								\
	if(check_bounds) {						\
	    for(i=0; i < n; i++) {					\
		if(_i_[i] == NA_INTEGER || _j_[i] == NA_INTEGER)	\
		    ii[i] = NA_INTEGER;					\
		else {							\
		    if(_i_[i] < 0 || _i_[i] >= Di[0])			\
			error(_("subscript 'i' out of bounds in M[ij]")); \
		    if(_j_[i] < 0 || _j_[i] >= Di[1])			\
			error(_("subscript 'j' out of bounds in M[ij]")); \
		    ii[i] = _i_[i] + _j_[i] * nr;			\
		}							\
	    }								\
	} else {							\
	    for(i=0; i < n; i++)					\
		ii[i] = (_i_[i] == NA_INTEGER || _j_[i] == NA_INTEGER)	\
		    ? NA_INTEGER : _i_[i] + _j_[i] * nr;		\
	}

	do_ii_FILL(IJ, j_);
    } else {
	ans = PROTECT(allocVector(INTSXP, n));
	int *ii = INTEGER(ans), nr = Di[0];

	do_ii_FILL(IJ, j_);
    }
    UNPROTECT(2);
    return ans;
}

/**
 * Encode Matrix index (i,j)  |-->  i + j * nrow   {i,j : 0-origin}
 *
 * @param i: integer vector
 * @param j: integer vector of same length as 'i'
 * @param di: dim(.), i.e. length 2 integer vector
 * @param chk_bnds: logical indicating  0 <= ij[,k] < di[k]  need to be checked.
 *
 * @return encoded index; integer if prod(dim) is small; double otherwise
 */
SEXP m_encodeInd2(SEXP i, SEXP j, SEXP di, SEXP chk_bnds)
{
    SEXP ans;
    int n = LENGTH(i);
    int *Di = INTEGER(di), *i_, *j_;
    Rboolean check_bounds = asLogical(chk_bnds);

    if(LENGTH(j) != n || !isInteger(i) || !isInteger(j))
	error(_("i and j must be integer vectors of the same length"));
    i_ = INTEGER(i);
    j_ = INTEGER(j);

    if((Di[0] * (double) Di[1]) >= 1 + (double)INT_MAX) { /* need double */
	ans = PROTECT(allocVector(REALSXP, n));
	double *ii = REAL(ans), nr = (double) Di[0];

	do_ii_FILL(i_, j_);
    } else {
	ans = PROTECT(allocVector(INTSXP, n));
	int *ii = INTEGER(ans), nr = Di[0];

	do_ii_FILL(i_, j_);
    }
    UNPROTECT(1);
    return ans;
}
#undef do_ii_FILL


#if R_VERSION < R_Version(2, 13, 0)
/**
 * Return the 0-based index of an is() match in a vector of class-name
 * strings terminated by an empty string.  Returns -1 for no match.
 *
 * @param x  an R object, about which we want is(x, .) information.
 * @param valid vector of possible matches terminated by an empty string.
 * @param rho  the environment in which the class definitions exist.
 *
 * @return index of match or -1 for no match
 */
int Matrix_check_class_and_super(SEXP x, const char **valid, SEXP rho)
{
    int ans;
    SEXP cl = getAttrib(x, R_ClassSymbol);
    const char *class = CHAR(asChar(cl));
    for (ans = 0; ; ans++) {
	if (!strlen(valid[ans])) // empty string
	    break;
	if (!strcmp(class, valid[ans])) return ans;
    }
    /* if not found directly, now search the non-virtual super classes :*/
    if(IS_S4_OBJECT(x)) {
	/* now try the superclasses, i.e.,  try   is(x, "....");  superCl :=
	   .selectSuperClasses(getClass("...")@contains, dropVirtual=TRUE)  */
	SEXP classExts, superCl, _call;
	int i;
	PROTECT(_call = lang2(install("getClassDef"), cl));
	classExts = GET_SLOT(eval(_call, rho),
			     install("contains"));
	UNPROTECT(1);
	PROTECT(classExts);
	PROTECT(_call = lang3(install(".selectSuperClasses"), classExts,
			      /* dropVirtual = */ ScalarLogical(1)));
	superCl = eval(_call, rho);
	UNPROTECT(2);
	PROTECT(superCl);
	for(i=0; i < length(superCl); i++) {
	    const char *s_class = CHAR(STRING_ELT(superCl, i));
	    for (ans = 0; ; ans++) {
		if (!strlen(valid[ans]))
		    break;
		if (!strcmp(s_class, valid[ans])) {
		    UNPROTECT(1);
		    return ans;
		}
	    }
	}
	UNPROTECT(1);
    }
    return -1;
}
#endif

/**
 * Return the 0-based index of an is() match in a vector of class-name
 * strings terminated by an empty string.  Returns -1 for no match.
 * Strives to find the correct environment() for is().
 *
 * @param x  an R object, about which we want is(x, .) information.
 * @param valid vector of possible matches terminated by an empty string.
 *
 * @return index of match or -1 for no match
 */
int Matrix_check_class_etc(SEXP x, const char **valid)
{
    static SEXP s_M_classEnv = NULL;
    SEXP cl = getAttrib(x, R_ClassSymbol), rho = R_GlobalEnv, pkg;
/*     PROTECT(cl); */
    if(!s_M_classEnv)
	s_M_classEnv = install(".M.classEnv");

    pkg = getAttrib(cl, R_PackageSymbol); /* ==R== packageSlot(class(x)) */
    if(!isNull(pkg)) { /* find  rho := correct class Environment */
	SEXP clEnvCall;
	/* need to make sure we find ".M.classEnv" even if Matrix is not
	   attached, but just namespace-loaded: */

	/* Matrix::: does not work here either ... :
	 *	    rho = eval(lang2(install("Matrix:::.M.classEnv"), cl), */

	/* Now make this work via .onLoad() hack in ../R/zzz.R  : */
	PROTECT(clEnvCall = lang2(s_M_classEnv, cl));
	rho = eval(clEnvCall, R_GlobalEnv);
	UNPROTECT(1);

	if(!isEnvironment(rho))
	    error(_("could not find correct environment; please report!"));
    }
/*     UNPROTECT(1); */
    return Matrix_check_class_and_super(x, valid, rho);
}
