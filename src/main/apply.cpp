/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2015  The R Core Team
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include "rho/ExpressionVector.hpp"

using namespace rho;

/* .Internal(lapply(X, FUN)) */

/* This is a special .Internal, so has unevaluated arguments.  It is
   called from a closure wrapper, so X and FUN are promises.

   FUN must be unevaluated for use in e.g. bquote .
*/
SEXP attribute_hidden do_lapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    PROTECT_INDEX px;

    SEXP X, XX, FUN;
    PROTECT_WITH_INDEX(X =CAR(args), &px);
    XX = PROTECT(eval(CAR(args), rho));
    R_xlen_t n = xlength(XX);  // a vector, so will be valid.
    FUN = CADR(args);
    Rboolean realIndx = Rboolean(n > INT_MAX);

    SEXP ans = PROTECT(allocVector(VECSXP, n));
    SEXP names = getAttrib(XX, R_NamesSymbol);
    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);

    /* Build call: FUN(XX[[<ind>]], ...) */

    SEXP ind = PROTECT(allocVector(realIndx ? REALSXP : INTSXP, 1));
    static Symbol* isym = Symbol::obtain("i");
    defineVar(isym, ind, rho);
    SET_NAMED(ind, 1);

    Expression* item = new Expression(R_Bracket2Symbol,
				     { X, isym });
    Expression* R_fcall = new CachingExpression(FUN, { item, R_DotsSymbol });

    for(R_xlen_t i = 0; i < n; i++) {
	if (realIndx) REAL(ind)[0] = (double)(i + 1);
	else INTEGER(ind)[0] = (int)(i + 1);
	SEXP tmp = R_forceAndCall(R_fcall, 1, rho);
	if (MAYBE_REFERENCED(tmp)) tmp = lazy_duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
    }

    UNPROTECT(4);
    return ans;
}

/* .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES)) */

/* This is a special .Internal */
SEXP attribute_hidden do_vapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, names = R_NilValue, rowNames = R_NilValue,
	X, XX, FUN, value, dim_v;
    R_xlen_t i, n;
    int commonLen;
    int useNames, rnk_v = -1; // = array_rank(value) := length(dim(value))
    Rboolean array_value;
    SEXPTYPE commonType;
    PROTECT_INDEX index = 0; /* initialize to avoid a warning */

    PROTECT(X = CAR(args));
    PROTECT(XX = eval(CAR(args), rho));
    FUN = CADR(args);  /* must be unevaluated for use in e.g. bquote */
    PROTECT(value = eval(CADDR(args), rho));
    if (!isVector(value)) error(_("'FUN.VALUE' must be a vector"));
    useNames = asLogical(eval(CADDDR(args), rho));
    if (useNames == NA_LOGICAL) error(_("invalid '%s' value"), "USE.NAMES");

    n = xlength(XX);
    if (n == NA_INTEGER) error(_("invalid length"));
    Rboolean realIndx = RHOCONSTRUCT(Rboolean, n > INT_MAX);

    commonLen = Rf_length(value);
    if (commonLen > 1 && n > INT_MAX)
	error(_("long vectors are not supported for matrix/array results"));
    commonType = TYPEOF(value);
    // check once here
    if (commonType != CPLXSXP && commonType != REALSXP &&
	commonType != INTSXP  && commonType != LGLSXP &&
	commonType != RAWSXP  && commonType != STRSXP &&
	commonType != VECSXP)
	error(_("type '%s' is not supported"), type2char(commonType));
    dim_v = getAttrib(value, R_DimSymbol);
    array_value = RHOCONSTRUCT(Rboolean, (TYPEOF(dim_v) == INTSXP && LENGTH(dim_v) >= 1));
    PROTECT(ans = allocVector(commonType, n*commonLen));
    if (useNames) {
	PROTECT(names = getAttrib(XX, R_NamesSymbol));
	if (isNull(names) && TYPEOF(XX) == STRSXP) {
	    UNPROTECT(1);
	    PROTECT(names = XX);
	}
	PROTECT_WITH_INDEX(rowNames = getAttrib(value,
						array_value ? R_DimNamesSymbol
						: R_NamesSymbol),
			   &index);
    }
    /* The R level code has ensured that XX is a vector.
       If it is atomic we can speed things up slightly by
       using the evaluated version.
    */
    {
	SEXP ind;
	/* Build call: FUN(XX[[<ind>]], ...) */

	static Symbol* isym = Symbol::obtain("i");
	PROTECT(ind = allocVector(INTSXP, 1));
	defineVar(isym, ind, rho);
	SET_NAMED(ind, 1);

	Expression* item = new Expression(R_Bracket2Symbol, { X, isym });
	Expression* R_fcall = new Expression(FUN, { item, R_DotsSymbol });

	int common_len_offset = 0;
	for(i = 0; i < n; i++) {
	    SEXP val; SEXPTYPE valType;
	    PROTECT_INDEX indx;
	    if (realIndx) REAL(ind)[0] = (double)(i + 1);
	    else INTEGER(ind)[0] = (int)(i + 1);
	    val = R_forceAndCall(R_fcall, 1, rho);
	    if (MAYBE_REFERENCED(val))
		val = lazy_duplicate(val); // Need to duplicate? Copying again anyway
	    PROTECT_WITH_INDEX(val, &indx);
	    if (Rf_length(val) != commonLen)
		error(_("values must be length %d,\n but FUN(X[[%d]]) result is length %d"),
	               commonLen, i+1, Rf_length(val));
	    valType = TYPEOF(val);
	    if (valType != commonType) {
	    	bool okay = FALSE;
		switch (commonType) {
		case CPLXSXP: okay = (valType == REALSXP) || (valType == INTSXP)
				    || (valType == LGLSXP); break;
		case REALSXP: okay = (valType == INTSXP) || (valType == LGLSXP); break;
		case INTSXP:  okay = (valType == LGLSXP); break;
		default:
		    Rf_error(_("Internal error: unexpected SEXPTYPE"));
		}
		if (!okay)
		    error(_("values must be type '%s',\n but FUN(X[[%d]]) result is type '%s'"),
			  type2char(commonType), i+1, type2char(valType));
		REPROTECT(val = coerceVector(val, commonType), indx);
	    }
	    /* Take row names from the first result only */
	    if (i == 0 && useNames && isNull(rowNames))
		REPROTECT(rowNames = getAttrib(val,
					       array_value ? R_DimNamesSymbol : R_NamesSymbol),
			  index);
	    // two cases - only for efficiency
	    if(commonLen == 1) { // common case
		switch (commonType) {
		case CPLXSXP: COMPLEX(ans)[i] = COMPLEX(val)[0]; break;
		case REALSXP: REAL(ans)   [i] = REAL   (val)[0]; break;
		case INTSXP:  INTEGER(ans)[i] = INTEGER(val)[0]; break;
		case LGLSXP:  LOGICAL(ans)[i] = LOGICAL(val)[0]; break;
		case RAWSXP:  RAW(ans)    [i] = RAW    (val)[0]; break;
		case STRSXP:  SET_STRING_ELT(ans, i, STRING_ELT(val, 0)); break;
		case VECSXP:  SET_VECTOR_ELT(ans, i, VECTOR_ELT(val, 0)); break;
		}
	    } else { // commonLen > 1 (typically, or == 0) :
		switch (commonType) {
		case REALSXP:
		    memcpy(REAL(ans) + common_len_offset,
			   REAL(val), commonLen * sizeof(double)); break;
		case INTSXP:
		    memcpy(INTEGER(ans) + common_len_offset,
			   INTEGER(val), commonLen * sizeof(int)); break;
		case LGLSXP:
		    memcpy(LOGICAL(ans) + common_len_offset,
			   LOGICAL(val), commonLen * sizeof(int)); break;
		case RAWSXP:
		    memcpy(RAW(ans) + common_len_offset,
			   RAW(val), commonLen * sizeof(Rbyte)); break;
		case CPLXSXP:
		    memcpy(COMPLEX(ans) + common_len_offset,
			   COMPLEX(val), commonLen * sizeof(Rcomplex)); break;
		case STRSXP:
		    for (int j = 0; j < commonLen; j++)
			SET_STRING_ELT(ans, common_len_offset + j, STRING_ELT(val, j));
		    break;
		case VECSXP:
		    for (int j = 0; j < commonLen; j++)
			SET_VECTOR_ELT(ans, common_len_offset + j, VECTOR_ELT(val, j));
		    break;
		}
		common_len_offset += commonLen;
	    }
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }

    if (commonLen != 1) {
	SEXP dim;
	rnk_v = array_value ? LENGTH(dim_v) : 1;
	PROTECT(dim = allocVector(INTSXP, rnk_v+1));
	if(array_value)
	    for(int j = 0; j < rnk_v; j++)
		INTEGER(dim)[j] = INTEGER(dim_v)[j];
	else
	    INTEGER(dim)[0] = commonLen;
	INTEGER(dim)[rnk_v] = int( n);  // checked above
	setAttrib(ans, R_DimSymbol, dim);
	UNPROTECT(1);
    }

    if (useNames) {
	if (commonLen == 1) {
	    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
	} else {
	    if (!isNull(names) || !isNull(rowNames)) {
		SEXP dimnames;
		PROTECT(dimnames = allocVector(VECSXP, rnk_v+1));
		if(array_value && !isNull(rowNames)) {
		    if(TYPEOF(rowNames) != VECSXP || LENGTH(rowNames) != rnk_v)
			// should never happen ..
			error(_("dimnames(<value>) is neither NULL nor list of length %d"),
			      rnk_v);
		    for(int j = 0; j < rnk_v; j++)
			SET_VECTOR_ELT(dimnames, j, VECTOR_ELT(rowNames, j));
		} else
		    SET_VECTOR_ELT(dimnames, 0, rowNames);

		SET_VECTOR_ELT(dimnames, rnk_v, names);
		setAttrib(ans, R_DimNamesSymbol, dimnames);
		UNPROTECT(1);
	    }
	}
    }
    UNPROTECT(useNames ? 6 : 4); /* X, XX, value, ans, and maybe names and rowNames */
    return ans;
}

static SEXP do_one(SEXP X, SEXP FUN, SEXP classes, SEXP deflt,
		   Rboolean replace, SEXP rho)
{
    SEXP ans, names, klass;
    int i, j, n;
    Rboolean matched = FALSE;

    /* if X is a list, recurse.  Otherwise if it matches classes call f */
    if(isNewList(X)) {
	n = Rf_length(X);
  if (replace) {
    PROTECT(ans = shallow_duplicate(X));
  } else {
    PROTECT(ans = allocVector(VECSXP, n));
    names = getAttrib(X, R_NamesSymbol);
    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
  }
	for(i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, do_one(VECTOR_ELT(X, i), FUN, classes,
					  deflt, replace, rho));
	UNPROTECT(1);
	return ans;
    }
    if(strcmp(CHAR(STRING_ELT(classes, 0)), "ANY") == 0) /* ASCII */
	matched = TRUE;
    else {
	PROTECT(klass = R_data_class(X, FALSE));
	for(i = 0; i < LENGTH(klass); i++)
	    for(j = 0; j < Rf_length(classes); j++)
		if(Seql(STRING_ELT(klass, i), STRING_ELT(classes, j)))
		    matched = TRUE;
	UNPROTECT(1);
    }
    if(matched) {
	/* This stores value to which the function is to be applied in
	   a variable X in the environment of the rapply closure call
	   that calls into the rapply .Internal. */
	SEXP R_fcall; /* could allocate once and preserve for re-use */
	SEXP Xsym = install("X");
	defineVar(Xsym, X, rho);
	INCREMENT_NAMED(X);
	/* PROTECT(R_fcall = lang2(FUN, Xsym)); */
	PROTECT(R_fcall = lang3(FUN, Xsym, R_DotsSymbol));
	ans = R_forceAndCall(R_fcall, 1, rho);
	if (MAYBE_REFERENCED(ans))
	    ans = lazy_duplicate(ans);
	UNPROTECT(1);
	return(ans);
    } else if(replace) return lazy_duplicate(X);
    else return lazy_duplicate(deflt);
}

SEXP attribute_hidden do_rapply(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::Environment* rho, rho::RObject* const* args, int num_args, const rho::PairList* tags)
{
    SEXP X, FUN, classes, deflt, how, ans, names;
    int i, n;
    Rboolean replace;

    X = args[0]; args = (args + 1);
    FUN = args[0]; args = (args + 1);
    if(!isFunction(FUN)) error(_("invalid '%s' argument"), "f");
    classes = args[0]; args = (args + 1);
    if(!isString(classes)) error(_("invalid '%s' argument"), "classes");
    deflt = args[0]; args = (args + 1);
    how = args[0];
    if(!isString(how)) error(_("invalid '%s' argument"), "how");
    replace = RHOCONSTRUCT(Rboolean, strcmp(CHAR(STRING_ELT(how, 0)), "replace") == 0); /* ASCII */
    n = Rf_length(X);
    if (replace) {
      PROTECT(ans = shallow_duplicate(X));
    } else {
      PROTECT(ans = allocVector(VECSXP, n));
      names = getAttrib(X, R_NamesSymbol);
      if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
    }
    for(i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, do_one(VECTOR_ELT(X, i), FUN, classes, deflt,
				      replace, rho));
    UNPROTECT(1);
    return ans;
}

static Rboolean islistfactor(SEXP X)
{
    int i, n = Rf_length(X);

    if(n == 0) return FALSE;
    switch(TYPEOF(X)) {
    case VECSXP:
	for(i = 0; i < LENGTH(X); i++)
	    if(!islistfactor(VECTOR_ELT(X, i))) return FALSE;
	return TRUE;
	break;
    case EXPRSXP:
	for(i = 0; i < LENGTH(X); i++)
	    if(!islistfactor(XVECTOR_ELT(X, i))) return FALSE;
	return TRUE;
	break;
    default:  // -Wswitch
	break;
    }
    return isFactor(X);
}


/* is this a tree with only factor leaves? */

SEXP attribute_hidden do_islistfactor(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* recursive_)
{
    SEXP X;
    Rboolean lans = TRUE, recursive;
    int i, n;

    X = x_;
    recursive = RHOCONSTRUCT(Rboolean, asLogical(recursive_));
    n = Rf_length(X);
    if(n == 0 || !isVectorList(X)) {
	lans = FALSE;
	goto do_ans;
    }
    if(!recursive) {
    for(i = 0; i < LENGTH(X); i++)
	if(!isFactor(VECTOR_ELT(X, i))) {
	    lans = FALSE;
	    break;
	}
    } else {
	switch(TYPEOF(X)) {
	case VECSXP:
	    for(i = 0; i < LENGTH(X); i++)
		if(!islistfactor(VECTOR_ELT(X, i))) {
		    lans = FALSE;
		    break;
		}
	    break;
	case EXPRSXP:
	    for(i = 0; i < LENGTH(X); i++)
		if(!islistfactor(XVECTOR_ELT(X, i))) {
		    lans = FALSE;
		    break;
		}
	    break;
	default:
	    break;
	}
    }
do_ans:
    return ScalarLogical(lans);
}
