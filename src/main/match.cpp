/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2014   The R Core Team.
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


 *  Matching and Partial Matching for Strings
 *
 *  In theory all string matching code should be placed in this file
 *  At present there are still a couple of rogue matchers about.
 *
 *
 *  psmatch(char *, char *, int);
 *
 *  This code will perform partial matching for list tags.  When
 *  exact is 1, and exact match is required (typically after ...)
 *  otherwise partial matching is performed.
 *
 *  Examples:
 *
 *	psmatch("aaa", "aaa", 0) -> 1
 *	psmatch("aaa", "aa", 0) -> 1
 *	psmatch("aa", "aaa", 0) -> 0
 *
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "rho/ArgMatcher.hpp"
#include "rho/DottedArgs.hpp"
#include "rho/FunctionContext.hpp"
#include "rho/StringVector.hpp"

using namespace rho;

/* used in subscript.c and subassign.c */
Rboolean Rf_NonNullStringMatch(SEXP s, SEXP t)
{
    /* "" or NA string matches nothing */
    if (s == NA_STRING || t == NA_STRING) return FALSE;
    if (CHAR(s)[0] && CHAR(t)[0] && Rf_Seql(s, t))
	return TRUE;
    else
	return FALSE;
}

/* currently unused outside this file */
Rboolean Rf_psmatch(const char *f, const char *t, Rboolean exact)
{
    if (exact)
	return Rboolean(!strcmp(f, t));
    /* else */
    while (*t) {
	if (*t != *f)   return FALSE;
	t++;
	f++;
    }
    return TRUE;
}


/* Matching formals and arguments */

/* Are these are always native charset? */
Rboolean Rf_pmatch(SEXP formal, SEXP tag, Rboolean exact)
{
    const char *f, *t;
    const void *vmax = vmaxget();
    Rboolean res;  // In rho this must be declared outside the span of the gotos
    switch (TYPEOF(formal)) {
    case SYMSXP:
	f = CHAR(PRINTNAME(formal));
	break;
    case CHARSXP:
	f = CHAR(formal);
	break;
    case STRSXP:
	f = Rf_translateChar(STRING_ELT(formal, 0));
	break;
    default:
	goto fail;
    }
    switch(TYPEOF(tag)) {
    case SYMSXP:
	t = CHAR(PRINTNAME(tag));
	break;
    case CHARSXP:
	t = CHAR(tag);
	break;
    case STRSXP:
	t = Rf_translateChar(STRING_ELT(tag, 0));
	break;
    default:
	goto fail;
    }
    res = Rf_psmatch(f, t, exact);
    vmaxset(vmax);
    return res;
 fail:
    Rf_error(_("invalid partial string match"));
    return FALSE;/* for -Wall */
}

void ArgMatcher::unusedArgsError(const SuppliedList& supplied_list)
{
    GCStackRoot<PairList> unused_list;
    // Produce a PairList of the unused args:
    for (SuppliedList::const_reverse_iterator rit = supplied_list.rbegin();
	 rit != supplied_list.rend(); ++rit) {
	const SuppliedData& supplied_data = *rit;
	RObject* value = supplied_data.value;
	if (value->sexptype() == PROMSXP)
	    value = const_cast<RObject*>(PREXPR(value));
	unused_list = PairList::cons(value, unused_list, supplied_data.tag);
    }
    unusedArgsError(unused_list);
}

void ArgMatcher::unusedArgsError(const ConsCell* unused_list)
{
    // Prepare error message:
    GCStackRoot<StringVector>
	argstrv(static_cast<StringVector*>(
		    Rf_deparse1line(
			const_cast<ConsCell*>(unused_list), FALSE)));
    // '+ 4' is to remove 'list' from 'list(badTag1, ...' :
    const char* errdetails = (*argstrv)[0]->c_str() + 4;
    Rf_error(_("unused argument(s) %s"), errdetails);
}

/* Destructively Extract A Named List Element. */
/* Returns the first exactly matching tag found. */
/* Pattern is a symbol. */
// Used for matching na.rm in logic.cpp and match.cpp.
SEXP attribute_hidden Rf_matchArgExact(SEXP tag, SEXP *list)
{
    if (*list == R_NilValue)
	return R_MissingArg;
    else if (TAG(*list) == tag) {
	SEXP s = *list;
	*list = CDR(*list);
	return CAR(s);
    }
    else {
	SEXP last = *list;
	SEXP next = CDR(*list);
	while (next != R_NilValue) {
	    if (TAG(next) == tag) {
		SETCDR(last, CDR(next));
		return CAR(next);
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	return R_MissingArg;
    }
}
