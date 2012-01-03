/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-12 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2010  The R Development Core Team
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
 *  http://www.r-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>
/* -> Rinternals.h which exports R_compute_identical() */

/* Implementation of identical(x, y) */

/* How are  R "double"s compared : */
typedef enum {
    bit_NA__num_bit = 0,/* S's default - look at bit pattern, also for NA/NaN's */
    bit_NA__num_eq  = 1,/* bitwise comparison for NA / NaN; '==' for other numbers */
    single_NA__num_bit = 2,/*         one   "  "  NA          "  " 'bit'comparison */
    single_NA__num_eq  = 3,/* R's default: one kind of NA or NaN; for num, use '==' */
} ne_strictness_type;

/* NOTE:  ne_strict = num_eq + (single_NA * 2)  = num_eq | (single_NA << 1)   */

static Rboolean neWithNaN(double x, double y, ne_strictness_type str);


/* .Internal(identical(..)) */
SEXP attribute_hidden do_identical(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int num_eq, single_NA, attr_as_set, nargs = length(args);
    /* avoid problems with earlier version captured in S4 methods */
    /* checkArity(op, args); */
    if (nargs != 2 && nargs != 5)
	error("%d arguments passed to .Internal(%s) which requires %d",
	      length(args), PRIMNAME(op), PRIMARITY(op));

    if (nargs == 5) {
	num_eq      = asLogical(CADDR(args));
	single_NA   = asLogical(CADDDR(args));
	attr_as_set = asLogical(CAD4R(args));

	if(num_eq      == NA_LOGICAL) error(_("invalid '%s' value"), "num.eq");
	if(single_NA   == NA_LOGICAL) error(_("invalid '%s' value"), "single.NA");
	if(attr_as_set == NA_LOGICAL) error(_("invalid '%s' value"), "attrib.as.set");
    } else {
	num_eq      = 1;
	single_NA   = 1;
	attr_as_set = 1;
    }

    return ScalarLogical(R_compute_identical(CAR(args), CADR(args),
					     Rboolean( num_eq),
					     Rboolean( single_NA),
					     Rboolean( attr_as_set)));
}

/* do the two objects compute as identical?
   used in unique.c */
Rboolean
R_compute_identical(SEXP x, SEXP y, Rboolean num_eq,
		    Rboolean single_NA, Rboolean attr_as_set)
{
    SEXP ax, ay, atrx, atry;
    if(x == y) /* same pointer */
	return TRUE;
    if(TYPEOF(x) != TYPEOF(y))
	return FALSE;
    if(OBJECT(x) != OBJECT(y))
	return FALSE;

    /* Skip attribute checks for CHARSXP -- such attributes can be used for internal purposes */
    if(TYPEOF(x) == CHARSXP)
    {
	/* This matches NAs */
	return CXXRCONSTRUCT(Rboolean, Seql(x, y));
    }

    ax = ATTRIB(x); ay = ATTRIB(y);
    if (!attr_as_set) {
	if(!R_compute_identical(ax, ay, num_eq, single_NA, FALSE)) return FALSE;
    }
    /* Attributes are special: they should be tagged pairlists.  We
       don't test them if they are not, and we do not test the order
       if they are.

       This code is not very efficient, but then neither is using
       pairlists for attributes.  If long attribute lists become more
       common (and they are used for S4 slots) we should store them in a hash
       table.
    */
    else if(ax != R_NilValue || ay != R_NilValue) {
	if(ax == R_NilValue || ay == R_NilValue)
	    return FALSE;
	if(TYPEOF(ax) != LISTSXP || TYPEOF(ay) != LISTSXP) {
	    warning(_("ignoring non-pairlist attributes"));
	} else {
	    SEXP elx, ely;
	    if(length(ax) != length(ay)) return FALSE;
	    /* They are the same length and should have
	       unique non-empty non-NA tags */
	    for(elx = ax; elx != R_NilValue; elx = CDR(elx)) {
		const char *tx = CHAR(PRINTNAME(TAG(elx)));
		for(ely = ay; ely != R_NilValue; ely = CDR(ely))
		    if(streql(tx, CHAR(PRINTNAME(TAG(ely))))) {
			/* We need to treat row.names specially here */
			if(streql(tx, "row.names")) {
			    PROTECT(atrx = getAttrib(x, R_RowNamesSymbol));
			    PROTECT(atry = getAttrib(y, R_RowNamesSymbol));
			    if(!R_compute_identical(atrx, atry,
						    num_eq, single_NA, TRUE)) {
				UNPROTECT(2);
				return FALSE;
			    } else
				UNPROTECT(2);
			} else
			    if(!R_compute_identical(CAR(elx), CAR(ely),
						    num_eq, single_NA, TRUE))
				return FALSE;
			break;
		    }
		if(ely == R_NilValue) return FALSE;
	    }
	}
    }
    switch (TYPEOF(x)) {
    case NILSXP:
	return TRUE;
    case LGLSXP:
	if (length(x) != length(y)) return FALSE;
	/* Use memcmp (which is ISO C) to speed up the comparison */
	return memcmp(CXXRNOCAST(void *)LOGICAL(x), CXXRNOCAST(void *)LOGICAL(y),
		      length(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case INTSXP:
	if (length(x) != length(y)) return FALSE;
	/* Use memcmp (which is ISO C) to speed up the comparison */
	return memcmp(CXXRNOCAST(void *)INTEGER(x), CXXRNOCAST(void *)INTEGER(y),
		      length(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case REALSXP:
    {
	int n = length(x);
	if(n != length(y)) return FALSE;
	else {
	    double *xp = REAL(x), *yp = REAL(y);
	    int i, ne_strict = num_eq | (single_NA << 1);
	    for(i = 0; i < n; i++)
		if(neWithNaN(xp[i], yp[i], CXXRCONSTRUCT(ne_strictness_type, ne_strict))) return FALSE;
	}
	return TRUE;
    }
    case CPLXSXP:
    {
	int n = length(x);
	if(n != length(y)) return FALSE;
	else {
	    Rcomplex *xp = COMPLEX(x), *yp = COMPLEX(y);
	    int i, ne_strict = num_eq | (single_NA << 1);
	    for(i = 0; i < n; i++)
		if(neWithNaN(xp[i].r, yp[i].r, CXXRCONSTRUCT(ne_strictness_type, ne_strict)) ||
		   neWithNaN(xp[i].i, yp[i].i, CXXRCONSTRUCT(ne_strictness_type, ne_strict)))
		    return FALSE;
	}
	return TRUE;
    }
    case STRSXP:
    {
	int i, n = length(x);
	if(n != length(y)) return FALSE;
	for(i = 0; i < n; i++) {
	    /* This special-casing for NAs is not needed */
	    Rboolean na1 = (CXXRCONSTRUCT(Rboolean, STRING_ELT(x, i) == NA_STRING)),
		na2 = (CXXRCONSTRUCT(Rboolean, STRING_ELT(y, i) == NA_STRING));
	    if(na1 ^ na2) return FALSE;
	    if(na1 && na2) continue;
	    if (! Seql(STRING_ELT(x, i), STRING_ELT(y, i))) return FALSE;
	}
	return TRUE;
    }
    case CHARSXP: /* Probably unreachable, but better safe than sorry... */
    {
	/* This matches NAs */
	return CXXRCONSTRUCT(Rboolean, Seql(x, y));
    }
    case VECSXP:
    case EXPRSXP:
    {
	int i, n = length(x);
	if(n != length(y)) return FALSE;
	for(i = 0; i < n; i++)
	    if(!R_compute_identical(VECTOR_ELT(x, i),VECTOR_ELT(y, i),
				    num_eq, single_NA, attr_as_set))
		return FALSE;
	return TRUE;
    }
    case LANGSXP:
    case LISTSXP:
    {
	while (x != R_NilValue) {
	    if(y == R_NilValue)
		return FALSE;
	    if(!R_compute_identical(CAR(x), CAR(y),
				    num_eq, single_NA, attr_as_set))
		return FALSE;
	    {
		SEXP tx = TAG(x);
		SEXP ty = TAG(y);
		if ((tx == 0) != (ty == 0))
		    return FALSE;
		if(tx && ty
		   && !R_compute_identical(PRINTNAME(tx), PRINTNAME(ty),
					   num_eq, single_NA, attr_as_set))
		    return FALSE;
	    }
	    x = CDR(x);
	    y = CDR(y);
	}
	return(CXXRCONSTRUCT(Rboolean, y == R_NilValue));
    }
    case CLOSXP:
	return(R_compute_identical(FORMALS(x), FORMALS(y),
				   num_eq, single_NA, attr_as_set) &&
	       R_compute_identical(BODY_EXPR(x), BODY_EXPR(y),
				   num_eq, single_NA, attr_as_set) &&
	       CLOENV(x) == CLOENV(y) ? TRUE : FALSE);
    case SPECIALSXP:
    case BUILTINSXP:
	return(PRIMOFFSET(x) == PRIMOFFSET(y) ? TRUE : FALSE);
    case ENVSXP:
    case SYMSXP:
    case WEAKREFSXP:
#ifdef BYTECODE
    case BCODESXP: /**** is this the best approach? */
#endif
	return(x == y ? TRUE : FALSE);
    case EXTPTRSXP:
	return (EXTPTR_PTR(x) == EXTPTR_PTR(y) ? TRUE : FALSE);
    case RAWSXP:
	if (length(x) != length(y)) return FALSE;
	/* Use memcmp (which is ISO C) to speed up the comparison */
	return memcmp(CXXRNOCAST(void *)RAW(x), CXXRNOCAST(void *)RAW(y),
		      length(x) * sizeof(Rbyte)) == 0 ? TRUE : FALSE;

	/*  case PROMSXP: args are evaluated, so will not be seen */
	/* test for equality of the substituted expression -- or should
	   we require both expression and environment to be identical? */
	/*#define PREXPR(x)	((x)->u.promsxp.expr)
	  #define PRENV(x)	((x)->u.promsxp.env)
	  return(R_compute_identical(subsititute(PREXPR(x), PRENV(x),
	                             num_eq, single_NA, attr_as_set),
	  subsititute(PREXPR(y), PRENV(y))));*/
    case S4SXP:
	/* attributes already tested, so all slots identical */
	return TRUE;
    default:
	/* these are all supposed to be types that represent constant
	   entities, so no further testing required ?? */
	printf("Unknown Type: %s (%x)\n", type2char(TYPEOF(x)), TYPEOF(x));
	return TRUE;
    }
}


/**
 * [N]ot [E]qual  (x, y)   <==>   x  "!="  y
 *  where the NA/NaN and "-0." / "+0." cases treatment depend on 'str'.
 *
 * @param x
 * @param y  the two "number"s to be compared
 * @param str a "strictness" indicator, one of 2*2 (one|bit)_NA__num_(eq|bit)
 *  "single_NA" means: x and y differ in the case
 *    that one, but not both are NaN.  Two NaN values are judged
 *    identical for this purpose, but NA != NaN
 *
 *  "num_eq" means: (x != y) is used when both are not NA or NaN
 *  whereas "bit_NA" and "num_bit" use the bitwise memory comparison  memcmp();
 *  notably "*_num_bit" will differentiate '+0.' and '-0.'.
 *
 * @return FALSE or TRUE indicating if x or y differ
 */
static Rboolean neWithNaN(double x, double y, ne_strictness_type str)
{
    switch (str) {
    case single_NA__num_eq:
    case single_NA__num_bit:
	if(R_IsNA(x))
	    return(R_IsNA(y) ? FALSE : TRUE);
	if(R_IsNA(y))
	    return(R_IsNA(x) ? FALSE : TRUE);
	if(ISNAN(x))
	    return(ISNAN(y) ? FALSE : TRUE);

    case bit_NA__num_eq:
    case bit_NA__num_bit:
	; /* do nothing */
    }

    switch (str) {
    case single_NA__num_eq:
	return(CXXRCONSTRUCT(Rboolean, x != y));
    case bit_NA__num_eq:
	if(!ISNAN(x) && !ISNAN(y))
	    return(CXXRCONSTRUCT(Rboolean, x != y));
	else /* bitwise check for NA/NaN's */
	    return memcmp(CXXRNOCAST(const void *) &x,
			  CXXRNOCAST(const void *) &y, sizeof(double)) ? TRUE : FALSE;
    case bit_NA__num_bit:
    case single_NA__num_bit:
	return memcmp(CXXRNOCAST(const void *) &x,
		      CXXRNOCAST(const void *) &y, sizeof(double)) ? TRUE : FALSE;
    default: /* Wall */
	return FALSE;
    }
}
