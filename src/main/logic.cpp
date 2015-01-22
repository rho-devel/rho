/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999--2012  The R Core Team.
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
#include <Internal.h>

#include "boost/lambda/lambda.hpp"
#include "CXXR/BinaryFunction.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/RawVector.h"
#include "CXXR/UnaryFunction.hpp"

using namespace CXXR;
using namespace VectorOps;

// Functionality to support do_logic() :
namespace {
    // Special handling is needed for '&' to ensure
    // that FALSE & NA -> FALSE
    struct AndOp {
	int operator()(int l, int r) const
	{
	    if (l == 0 || r == 0)
		return 0;
	    if (isNA(l) || isNA(r))
		return NA<int>();
	    return 1;
	}
    };

    // Special handling is needed for '|' to ensure
    // that TRUE | NA -> TRUE
    struct OrOp {
	int operator()(int l, int r) const
	{
	    if ((!isNA(l) && l != 0) || (!isNA(r) && r != 0))
		return 1;
	    if (isNA(l) || isNA(r))
		return NA<int>();
	    return 0;
	}
    };

    LogicalVector* binaryLogic(int opcode, const LogicalVector* l,
			       const LogicalVector* r)
    {
	switch (opcode) {
	case 1:
	    {
		BinaryFunction<AndOp, GeneralBinaryAttributeCopier,
		               NullBinaryFunctorWrapper> bf;
		return bf.apply<LogicalVector>(l, r);
	    }
	case 2:
	    {
		BinaryFunction<OrOp, GeneralBinaryAttributeCopier,
		               NullBinaryFunctorWrapper> bf;
		return bf.apply<LogicalVector>(l, r);
	    }
	}
	return nullptr;  // -Wall
    }

    struct BitwiseAnd {
	unsigned char operator()(unsigned char l, unsigned char r) const
	{
	    return l & r;
	}
    };

    struct BitwiseOr {
	unsigned char operator()(unsigned char l, unsigned char r) const
	{
	    return l | r;
	}
    };

    RawVector* bitwiseBinary(int opcode, const RawVector* l, const RawVector* r)
    {
	using namespace boost::lambda;
	switch (opcode) {
	case 1:
	    {
		BinaryFunction<BitwiseAnd, GeneralBinaryAttributeCopier,
			       NullBinaryFunctorWrapper> bf;
		return bf.apply<RawVector>(l, r);
	    }
	case 2:
	    {
		BinaryFunction<BitwiseOr, GeneralBinaryAttributeCopier,
			       NullBinaryFunctorWrapper> bf;
		return bf.apply<RawVector>(l, r);
	    }
	}
	return nullptr;  // -Wall
    }

    RObject* lbinary(RObject* op, RObject* args)
    {
	/* logical binary : "&" or "|" */
	SEXP x = CAR(args);
	SEXP y = CADR(args);
	if (x && x->sexptype() == RAWSXP
	    && y && y->sexptype() == RAWSXP) {
	    // Bitwise operations:
	    RawVector* vl = static_cast<RawVector*>(x);
	    RawVector* vr = static_cast<RawVector*>(y);
	    return bitwiseBinary(PRIMVAL(op), vl, vr);
	}
	if (!isNumber(x) || !isNumber(y))
	    Rf_error(_("operations are possible only for"
		       " numeric, logical or complex types"));
	GCStackRoot<LogicalVector>
	    vl(static_cast<LogicalVector*>(coerceVector(x, LGLSXP)));
	GCStackRoot<LogicalVector>
	    vr(static_cast<LogicalVector*>(coerceVector(y, LGLSXP)));
	return binaryLogic(PRIMVAL(op), vl, vr);
    }

    RObject* lnot(RObject* arg)
    {
	using namespace boost::lambda;
	if (arg && arg->sexptype() == RAWSXP) {
	    // Bit inversion:
	    RawVector* rv = static_cast<RawVector*>(arg);
	    return
		makeUnaryFunction<CopyLayoutAttributes>(_1^0xff)
		.apply<RawVector>(rv);
	} else if (!isLogical(arg) && !isNumber(arg)) {
	    if (Rf_length(arg) == 0U)  // For back-compatibility
		return LogicalVector::create(0);
	    Rf_error(_("invalid argument type"));
	}
	// Logical negation:
	GCStackRoot<LogicalVector>
	    lv(static_cast<LogicalVector*>(coerceVector(arg, LGLSXP)));
	return
	    makeUnaryFunction<CopyLayoutAttributes>(!_1)
	    .apply<LogicalVector>(lv.get());
    }
}

/* & | ! */
SEXP attribute_hidden do_logic(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    // It would be logical to test the arity before calling
    // DispatchGroup, but tests/primitives.R assumes otherwise.
    if (DispatchGroup("Ops",call, op, args, env, &ans))
	return ans;
    checkArity(op, args);
    switch (PRIMVAL(op)) {
    case 1:
    case 2:
	return lbinary(op, args);
    case 3:
	return lnot(CAR(args));
    default:
	error(_("internal error in do_logic"));
    }
    return nullptr;  // -Wall
}

/* && || */
SEXP attribute_hidden do_logic2(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*  &&	and  ||	 */
    SEXP s1, s2;
    int x1, x2;
    SEXP ans;

    if (length(args) != 2)
	error(_("'%s' operator requires 2 arguments"),
	      PRIMVAL(op) == 1 ? "&&" : "||");

    s1 = CAR(args);
    s2 = CADR(args);
    PROTECT(ans = allocVector(LGLSXP, 1));
    s1 = eval(s1, env);
    if (!isNumber(s1))
	errorcall(call, _("invalid 'x' type in 'x %s y'"),
		  PRIMVAL(op) == 1 ? "&&" : "||");
    x1 = asLogical(s1);

#define get_2nd							\
	s2 = eval(s2, env);					\
	if (!isNumber(s2))					\
	    errorcall(call, _("invalid 'y' type in 'x %s y'"),	\
		      PRIMVAL(op) == 1 ? "&&" : "||");		\
	x2 = asLogical(s2);

    switch (PRIMVAL(op)) {
    case 1: /* && */
	if (x1 == FALSE)
	    LOGICAL(ans)[0] = FALSE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL)
		LOGICAL(ans)[0] = (x2 == NA_LOGICAL || x2) ? NA_LOGICAL : x2;
	    else /* x1 == TRUE */
		LOGICAL(ans)[0] = x2;
	}
	break;
    case 2: /* || */
	if (x1 == TRUE)
	    LOGICAL(ans)[0] = TRUE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL)
		LOGICAL(ans)[0] = (x2 == NA_LOGICAL || !x2) ? NA_LOGICAL : x2;
	    else /* x1 == FALSE */
		LOGICAL(ans)[0] = x2;
	}
    }
    UNPROTECT(1);
    return ans;
}


#define _OP_ALL 1
#define _OP_ANY 2

static int checkValues(int op, int na_rm, int *x, R_xlen_t n)
{
    R_xlen_t i;
    int has_na = 0;
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        if (!na_rm && x[i] == NA_LOGICAL) has_na = 1;
        else {
            if (x[i] == TRUE && op == _OP_ANY) return TRUE;
            if (x[i] == FALSE && op == _OP_ALL) return FALSE;
	}
    }
    switch (op) {
    case _OP_ANY:
        return has_na ? NA_LOGICAL : FALSE;
    case _OP_ALL:
        return has_na ? NA_LOGICAL : TRUE;
    default:
        error("bad op value for do_logic3");
    }
    return NA_LOGICAL; /* -Wall */
}

extern SEXP fixup_NaRm(SEXP args); /* summary.c */

/* all, any */
SEXP attribute_hidden do_logic3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, s, t, call2;
    int narm, has_na = 0;
    /* initialize for behavior on empty vector
       all(logical(0)) -> TRUE
       any(logical(0)) -> FALSE
     */
    Rboolean val = PRIMVAL(op) == _OP_ALL ? TRUE : FALSE;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = duplicate(call));
    SETCDR(call2, args);

    if (DispatchGroup("Summary", call2, op, args, env, &ans)) {
	UNPROTECT(2);
	return(ans);
    }

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical(ans);

    for (s = args; s != R_NilValue; s = CDR(s)) {
	t = CAR(s);
	/* Avoid memory waste from coercing empty inputs, and also
	   avoid warnings with empty lists coming from sapply */
	if(xlength(t) == 0) continue;
	/* coerceVector protects its argument so this actually works
	   just fine */
	if (TYPEOF(t) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if(TYPEOF(t) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    type2char(TYPEOF(t)));
	    t = coerceVector(t, LGLSXP);
	}
	val = CXXRCONSTRUCT(Rboolean, checkValues(PRIMVAL(op), narm, LOGICAL(t), XLENGTH(t)));
        if (val != NA_LOGICAL) {
            if ((PRIMVAL(op) == _OP_ANY && val)
                || (PRIMVAL(op) == _OP_ALL && !val)) {
                has_na = 0;
                break;
            }
        } else has_na = 1;
    }
    UNPROTECT(2);
    return has_na ? ScalarLogical(NA_LOGICAL) : ScalarLogical(val);
}
#undef _OP_ALL
#undef _OP_ANY

namespace CXXR {
    namespace VectorOps {
	void checkOperandsConformable(const VectorBase* vl, const VectorBase* vr)
	{
	    // Temporary kludge:
	    VectorBase* vlnc = const_cast<VectorBase*>(vl);
	    VectorBase* vrnc = const_cast<VectorBase*>(vr);
	    if (Rf_isArray(vlnc) && Rf_isArray(vrnc)
		&& !Rf_conformable(vlnc, vrnc))
		Rf_error(_("non-conformable arrays"));
	    if (isTs(vlnc)) {
		if (isTs(vrnc) && !Rf_tsConform(vlnc, vrnc))
		    Rf_error(_("non-conformable time-series"));
		if (vr->size() > vl->size())
		    Rf_error(_("time-series/vector length mismatch"));
	    } else if (isTs(vrnc) && vl->size() > vr->size())
		Rf_error(_("time-series/vector length mismatch"));
	}
    } // namespace VectorOps
} // namespace CXXR  
