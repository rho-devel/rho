/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
#include <Rmath.h>
#include <errno.h>
#include <utility>

#include "basedecl.h"

#include "CXXR/BinaryFunction.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/LogicalVector.h"

using namespace CXXR;
using namespace VectorOps;

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000

static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2);

namespace {
    bool isNaOrNaN(Logical value) { return isNA(value); }
    bool isNaOrNaN(int value)     { return isNA(value); }
    bool isNaOrNaN(double value)  { return std::isnan(value); }
    bool isNaOrNaN(Complex value) { return (isNaOrNaN(value.r)
					    || isNaOrNaN(value.i)); }

    template<typename T, typename Op>
    Logical withNaHandling(T lhs, T rhs, Op op) {
	bool eitherIsNaOrNan = isNaOrNaN(lhs) || isNaOrNaN(rhs);
	return eitherIsNaOrNan ? Logical::NA() : op(lhs, rhs);
    }

    template<typename T, typename Op>
    LogicalVector* relop_aux(const T* lhs, const T* rhs, Op op) {
	typedef typename T::value_type Value;
	return applyBinaryOperator(
	    [=](Value l, Value r) {
		return withNaHandling(l, r, op);
	    },
	    GeneralBinaryAttributeCopier(),
	    lhs, rhs);
    }

    template <class V>
    LogicalVector* relop(const V* vl, const V* vr, RELOP_TYPE code)
    {
	typedef typename V::value_type Value;

	switch (code) {
	case EQOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs == rhs; });
	case NEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs != rhs; });
	case LTOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs < rhs; });
	case GTOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs > rhs; });
	case LEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs <= rhs; });
	case GEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs >= rhs; });
	}
	return nullptr;  // -Wall
    }

    template <class V>
    LogicalVector* relop_no_order(const V* vl, const V* vr, RELOP_TYPE code)
    {
	typedef typename V::value_type Value;

	switch (code) {
	case EQOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs == rhs; });
	case NEOP:
	    return relop_aux(vl, vr,
			     [](Value lhs, Value rhs) { return lhs != rhs; });
	default:
	    Rf_error(_("comparison of these types is not implemented"));
	}
	return nullptr;  // -Wall
    }
}  // anonymous namespace

RObject* attribute_hidden CXXR::do_relop(const Expression* call,
					 const BuiltInFunction* op,
					 Environment* env,
					 int num_args,
					 RObject** args,
					 const PairList* tags)
{
    // If any of the args has a class, then we might need to dispatch.
    auto result = op->InternalGroupDispatch("Ops", call, env, num_args, args,
					    tags);
    if (result.first)
	return result.second;

    op->checkNumArgs(num_args, call);
    return do_relop_dflt(const_cast<Expression*>(call),
			 const_cast<BuiltInFunction*>(op),
			 args[0], args[1]);
}

SEXP attribute_hidden do_relop_dflt(SEXP call, SEXP op, SEXP xarg, SEXP yarg)
{
    GCStackRoot<> x(xarg), y(yarg);

    /* That symbols and calls were allowed was undocumented prior to
       R 2.5.0.  We deparse them as deparse() would, minus attributes */
    bool iS;
    if ((iS = isSymbol(x)) || TYPEOF(x) == LANGSXP) {
	GCStackRoot<> tmp(allocVector(STRSXP, 1));
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(x) :
		       STRING_ELT(deparse1(x, CXXRFALSE, DEFAULTDEPARSE), 0));
	x = tmp;
    }
    if ((iS = isSymbol(y)) || TYPEOF(y) == LANGSXP) {
	GCStackRoot<> tmp(allocVector(STRSXP, 1));
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(y) :
		       STRING_ELT(deparse1(y, CXXRFALSE, DEFAULTDEPARSE), 0));
	y = tmp;
    }

    if (!isVector(x) || !isVector(y)) {
	if (isNull(x) || isNull(y))
	    return allocVector(LGLSXP,0);
	errorcall(call,
		  _("comparison (%d) is possible only for atomic and list types"),
		  PRIMVAL(op));
    }

    if (TYPEOF(x) == EXPRSXP || TYPEOF(y) == EXPRSXP)
	errorcall(call, _("comparison is not allowed for expressions"));

    /* ELSE :  x and y are both atomic or list */

    if (XLENGTH(x) <= 0 || XLENGTH(y) <= 0) {
	return allocVector(LGLSXP, 0);
    }

    RELOP_TYPE opcode = RELOP_TYPE(PRIMVAL(op));
    if (isString(x) || isString(y)) {
	// This case has not yet been brought into line with the
	// general CXXR pattern.
	VectorBase* xv = static_cast<VectorBase*>(coerceVector(x, STRSXP));
	VectorBase* yv = static_cast<VectorBase*>(coerceVector(y, STRSXP));
	checkOperandsConformable(xv, yv);
	int nx = length(xv);
	int ny = length(yv);
	bool mismatch = false;
	if (nx > 0 && ny > 0)
	    mismatch = (((nx > ny) ? nx % ny : ny % nx) != 0);
	if (mismatch)
	    warningcall(call, _("longer object length is not"
				" a multiple of shorter object length"));
	GCStackRoot<VectorBase>
	    ans(static_cast<VectorBase*>(string_relop(opcode, xv, yv)));
	GeneralBinaryAttributeCopier::copyAttributes(ans, xv, yv);
	return ans;
    }
    else if (isComplex(x) || isComplex(y)) {
	GCStackRoot<ComplexVector>
	    vl(static_cast<ComplexVector*>(coerceVector(x, CPLXSXP)));
	GCStackRoot<ComplexVector>
	    vr(static_cast<ComplexVector*>(coerceVector(y, CPLXSXP)));
	return relop_no_order(vl.get(), vr.get(), opcode);
    }
    else if (isReal(x) || isReal(y)) {
	GCStackRoot<RealVector>
	    vl(static_cast<RealVector*>(coerceVector(x, REALSXP)));
	GCStackRoot<RealVector>
	    vr(static_cast<RealVector*>(coerceVector(y, REALSXP)));
	return relop(vl.get(), vr.get(), opcode);
    }
    else if (isInteger(x) || isInteger(y)) {
	GCStackRoot<IntVector>
	    vl(static_cast<IntVector*>(coerceVector(x, INTSXP)));
	GCStackRoot<IntVector>
	    vr(static_cast<IntVector*>(coerceVector(y, INTSXP)));
	return relop(vl.get(), vr.get(), opcode);
    }
    else if (isLogical(x) || isLogical(y)) {
	// TODO(kmillar): do this without promoting to integer.
	GCStackRoot<IntVector>
	    vl(static_cast<IntVector*>(coerceVector(x, INTSXP)));
	GCStackRoot<IntVector>
	    vr(static_cast<IntVector*>(coerceVector(y, INTSXP)));
	return relop(vl.get(), vr.get(), opcode);
    }
    else if (TYPEOF(x) == RAWSXP || TYPEOF(y) == RAWSXP) {
	GCStackRoot<RawVector>
	    vl(static_cast<RawVector*>(coerceVector(x, RAWSXP)));
	GCStackRoot<RawVector>
	    vr(static_cast<RawVector*>(coerceVector(y, RAWSXP)));
	return relop(vl.get(), vr.get(), opcode);
    } else errorcall(call, _("comparison of these types is not implemented"));
    return nullptr;  // -Wall
}

/* POSIX allows EINVAL when one of the strings contains characters
   outside the collation domain. */
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, res;
    SEXP ans, c1, c2;
    const void *vmax = vmaxget(); // for Scollate

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    PROTECT(ans = allocVector(LGLSXP, n));

    switch (code) {
    case EQOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = Seql(c1, c2) ? 1 : 0;
	}
	break;
    case NEOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = Seql(c1, c2) ? 0 : 1;
	}
	break;
    case LTOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else if (c1 == c2)
		LOGICAL(ans)[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res < 0) ? 1 : 0;
	    }
	}
	break;
    case GTOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else if (c1 == c2)
		LOGICAL(ans)[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res > 0) ? 1 : 0;
	    }
	}
	break;
    case LEOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else if (c1 == c2)
		LOGICAL(ans)[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res <= 0) ? 1 : 0;
	    }
	}
	break;
    case GEOP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i % n1);
	    c2 = STRING_ELT(s2, i % n2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else if (c1 == c2)
		LOGICAL(ans)[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res >= 0) ? 1 : 0;
	    }
	}
	break;
    }
    UNPROTECT(3);
    vmaxset(vmax);
    return ans;
}


static SEXP bitwiseNot(SEXP a)
{
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    R_xlen_t i, m = XLENGTH(a);
    SEXP ans = allocVector(TYPEOF(a), m);
    switch(TYPEOF(a)) {
    case INTSXP:
	for(i = 0; i < m; i++) {
	    int aa = INTEGER(a)[i];
	    INTEGER(ans)[i] = (aa == NA_INTEGER) ? aa : ~aa;
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitNot", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

#define mymax(x, y) ((x >= y) ? x : y)

#define BIT(op, name) \
    int np = 0; \
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;} \
    if(isReal(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;} \
    if (TYPEOF(a) != TYPEOF(b)) error(_("'a' and 'b' must have the same type"));  \
    R_xlen_t i, m = XLENGTH(a), n = XLENGTH(b), mn = (m && n) ? mymax(m, n) : 0;  \
    SEXP ans = allocVector(TYPEOF(a), mn); \
    switch(TYPEOF(a)) { \
    case INTSXP: \
	for(i = 0; i < mn; i++) { \
	    int aa = INTEGER(a)[i%m], bb =  INTEGER(b)[i%n]; \
	    INTEGER(ans)[i] = (aa == NA_INTEGER || bb == NA_INTEGER) ? NA_INTEGER : aa op bb; \
	} \
	break; \
    default: \
	UNIMPLEMENTED_TYPE(name, a); \
    } \
    if(np) UNPROTECT(np); \
    return ans

static SEXP bitwiseAnd(SEXP a, SEXP b)
{
    BIT(&, "bitwAnd");
}

static SEXP bitwiseOr(SEXP a, SEXP b)
{
    BIT(|, "bitwOr");
}

static SEXP bitwiseXor(SEXP a, SEXP b)
{
    BIT(^, "bitwXor");
}

static SEXP bitwiseShiftL(SEXP a, SEXP b)
{
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if(!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    R_xlen_t i, m = XLENGTH(a), n = XLENGTH(b), 
	mn = (m && n) ? mymax(m, n) : 0;
    SEXP ans = allocVector(TYPEOF(a), mn);
    switch(TYPEOF(a)) {
    case INTSXP:
	for(i = 0; i < mn; i++) {
	    int aa = INTEGER(a)[i%m], bb = INTEGER(b)[i%n];
	    INTEGER(ans)[i] = 
		(aa == NA_INTEGER || bb == NA_INTEGER || bb < 0 || bb > 31) ? NA_INTEGER : (static_cast<unsigned int>(aa) << bb);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftL", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

static SEXP bitwiseShiftR(SEXP a, SEXP b)
{
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if(!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    R_xlen_t i, m = XLENGTH(a), n = XLENGTH(b), 
	mn = (m && n) ? mymax(m, n) : 0;
    SEXP ans = allocVector(TYPEOF(a), mn);
    switch(TYPEOF(a)) {
    case INTSXP:
	for(i = 0; i < mn; i++) {
	    int aa = INTEGER(a)[i%m], bb = INTEGER(b)[i%n];
	    INTEGER(ans)[i] = 
		(aa == NA_INTEGER || bb == NA_INTEGER || bb < 0 || bb > 31) ? NA_INTEGER : (static_cast<unsigned int>(aa) >> bb);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftR", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

SEXP attribute_hidden do_bitwise(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP ans = R_NilValue; /* -Wall */
    switch(PRIMVAL(op)) {
    case 1: ans = bitwiseAnd(CAR(args), CADR(args)); break;
    case 2: ans = bitwiseNot(CAR(args)); break;
    case 3: ans = bitwiseOr(CAR(args), CADR(args)); break;
    case 4: ans = bitwiseXor(CAR(args), CADR(args)); break;
    case 5: ans = bitwiseShiftL(CAR(args), CADR(args)); break;
    case 6: ans = bitwiseShiftR(CAR(args), CADR(args)); break;
    }
    return ans;
}
