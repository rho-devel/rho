/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2015	    The R Core Team.
 *  Copyright (C) 2003--2015	    The R Foundation
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

/** @arithmetic.cpp
 *
 * All sorts of arithmetical and mathematical functions, from addition
 * through to Bessel functions.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <limits>

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif
#include <Defn.h>		/*-> Arith.h -> math.h */
#ifdef __OpenBSD__
# undef __LIBM_PRIVATE
#endif

#include <Internal.h>

#define R_MSG_NA	_("NaNs produced")
#define R_MSG_NONNUM_MATH _("non-numeric argument to mathematical function")

#include <Rmath.h>

#include "arithmetic.h"

#include <errno.h>
#include <math.h>

#include "R_ext/Itermacros.h"
#include "rho/ArgMatcher.hpp"
#include "rho/BinaryFunction.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/LogicalVector.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/IntVector.hpp"
#include "rho/RAllocStack.hpp"
#include "rho/RealVector.hpp"
#include "rho/UnaryFunction.hpp"

using namespace rho;
using namespace VectorOps;

#ifdef HAVE_MATHERR

/* Override the SVID matherr function:
   the main difference here is not to print warnings.
 */
#ifndef __cplusplus
int matherr(struct exception *exc)
{
    switch (exc->type) {
    case DOMAIN:
    case SING:
	errno = EDOM;
	break;
    case OVERFLOW:
	errno = ERANGE;
	break;
    case UNDERFLOW:
	exc->retval = 0.0;
	break;
	/*
	   There are cases TLOSS and PLOSS which are ignored here.
	   According to the Solaris man page, there are for
	   trigonometric algorithms and not needed for good ones.
	 */
    }
    return 1;
}
#endif
#endif

typedef union
{
    double value;
    unsigned int word[2];
} ieee_double;

/* gcc had problems with static const on AIX and Solaris
   Solaris was for gcc 3.1 and 3.2 under -O2 32-bit on 64-bit kernel */
#ifdef _AIX
#define CONST
#elif defined(sparc) && defined (__GNUC__) && __GNUC__ == 3
#define CONST
#else
#define CONST const
#endif

#ifdef WORDS_BIGENDIAN
static CONST int hw = 0;
static CONST int lw = 1;
#else  /* !WORDS_BIGENDIAN */
static CONST int hw = 1;
static CONST int lw = 0;
#endif /* WORDS_BIGENDIAN */


static double R_ValueOfNA(void)
{
    /* The gcc shipping with Fedora 9 gets this wrong without
     * the volatile declaration. Thanks to Marc Schwartz. */
    volatile ieee_double x;
    x.word[hw] = 0x7ff00000;
    x.word[lw] = 1954;
    return x.value;
}

int R_IsNA(double x)
{
    if (ISNAN(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] == 1954);
    }
    return 0;
}

int R_IsNaN(double x)
{
    if (ISNAN(x)) {
	ieee_double y;
	y.value = x;
	return RHOCONSTRUCT(Rboolean, (y.word[lw] != 1954));
    }
    return FALSE;
}

/* Mainly for use in packages */
int R_isnancpp(double x) {
    return std::isnan(x);
}
int R_finite(double x) {
    return std::isfinite(x);
}

/* Arithmetic Initialization */

void attribute_hidden InitArithmetic()
{
    R_NaInt = INT_MIN;
    R_NaReal = R_ValueOfNA();
    R_NaN = std::numeric_limits<double>::quiet_NaN();
    R_PosInf = std::numeric_limits<double>::infinity();
    R_NegInf = -R_PosInf;  // is this portable?
}

/* Keep these two in step */
/* FIXME: consider using
    tmp = (LDOUBLE)x1 - floor(q) * (LDOUBLE)x2;
 */
static double myfmod(double x1, double x2)
{
    if (x2 == 0.0) return R_NaN;
    double q = x1 / x2, tmp = x1 - floor(q) * x2;
    if(R_FINITE(q) && (fabs(q) > 1/R_AccuracyInfo.eps))
	warning(_("probable complete loss of accuracy in modulus"));
    q = floor(tmp/x2);
    return tmp - q * x2;
}

static double myfloor(double x1, double x2)
{
    double q = x1 / x2, tmp;

    if (x2 == 0.0) return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp/x2);
}

double R_pow(double x, double y) /* = x ^ y */
{
    /* squaring is the most common of the specially handled cases so
       check for it first. */
    if(y == 2.0)
	return x * x;
    if(x == 1. || y == 0.)
	return(1.);
    if(x == 0.) {
	if(y > 0.) return(0.);
	else if(y < 0) return(R_PosInf);
	else return(y); /* NA or NaN, we assert */
    }
    if (R_FINITE(x) && R_FINITE(y)) {
	/* There was a special case for y == 0.5 here, but
	   gcc 4.3.0 -g -O2 mis-compiled it.  Showed up with
	   100^0.5 as 3.162278, example(pbirthday) failed. */
#ifdef USE_POWL_IN_R_POW
    return powl(x, y);
#else
    return pow(x, y);
#endif
    }
    if (ISNAN(x) || ISNAN(y))
	return(x + y);
    if(!R_FINITE(x)) {
	if(x > 0)		/* Inf ^ y */
	    return (y < 0.)? 0. : R_PosInf;
	else {			/* (-Inf) ^ y */
	    if(R_FINITE(y) && y == floor(y)) /* (-Inf) ^ n */
		return (y < 0.) ? 0. : (myfmod(y, 2.) != 0 ? x  : -x);
	}
    }
    if(!R_FINITE(y)) {
	if(x >= 0) {
	    if(y > 0)		/* y == +Inf */
		return (x >= 1) ? R_PosInf : 0.;
	    else		/* y == -Inf */
		return (x < 1) ? R_PosInf : 0.;
	}
    }
    return R_NaN; // all other cases: (-Inf)^{+-Inf, non-int}; (neg)^{+-Inf}
}

double R_pow_di(double x, int n)
{
    double xn = 1.0;

    if (ISNAN(x)) return x;
    if (n == NA_INTEGER) return NA_REAL;

    if (n != 0) {
	if (!R_FINITE(x)) return R_POW(x, double(n));

	bool is_neg = (n < 0);
	if(is_neg) n = -n;
	for(;;) {
	    if(n & 01) xn *= x;
	    if(n >>= 1) x *= x; else break;
	}
        if(is_neg) xn = 1. / xn;
    }
    return xn;
}


static SEXP real_binary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP integer_binary(ARITHOP_TYPE, SEXP, SEXP, SEXP);

#if 0
static R_INLINE SEXP ScalarValue1(SEXP x)
{
    if (NO_REFERENCES(x))
	return x;
    else
	return allocVector(TYPEOF(x), 1);
}

static R_INLINE SEXP ScalarValue2(SEXP x, SEXP y)
{
    if (NO_REFERENCES(x))
	return x;
    else if (NO_REFERENCES(y))
	return y;
    else
	return allocVector(TYPEOF(x), 1);
}
#endif

/* Unary and Binary Operators */

RObject* attribute_hidden do_arith(/*const*/ Expression* call_,
				   const BuiltInFunction* op_,
				   Environment* env,
				   RObject* const* args,
				   int num_args,
				   const PairList* tags)
{
    Expression* call = const_cast<Expression*>(call_);
    BuiltInFunction* op = const_cast<BuiltInFunction*>(op_);

    switch(num_args) {
    case 1:
	return R_unary(call, op, args[0]);
    case 2:
	return R_binary(call, op, args[0], args[1]);
    default:
	errorcall(call, _("operator needs one or two arguments"));
    }
    return nullptr;			/* never used; to keep -Wall happy */
}

namespace {
    VectorBase* COERCE_IF_NEEDED(SEXP v, SEXPTYPE tp) {
	if (v->sexptype() != tp)
	    v = coerceVector(v, tp);
	return static_cast<VectorBase*>(v);
    }

    VectorBase* FIXUP_NULL_AND_CHECK_TYPES(SEXP v)
    {
	if (!v)
	    return RealVector::create(0);
	switch (v->sexptype()) {
	case CPLXSXP:
	case REALSXP:
	case INTSXP:
	case LGLSXP:
	    break;
	default:
	    Rf_error(_("non-numeric argument to binary operator"));
	}
	return static_cast<VectorBase*>(v);
    }
}

SEXP attribute_hidden R_binary(SEXP call, SEXP op, SEXP xarg, SEXP yarg)
{
    SEXP lcall = call;
    ARITHOP_TYPE oper = ARITHOP_TYPE( PRIMVAL(op));

    GCStackRoot<VectorBase> x(FIXUP_NULL_AND_CHECK_TYPES(xarg));
    GCStackRoot<VectorBase> y(FIXUP_NULL_AND_CHECK_TYPES(yarg));
    checkOperandsConformable(x, y);

    R_xlen_t nx = XLENGTH(x);
    R_xlen_t ny = XLENGTH(y);

    GCStackRoot<> val;
    /* need to preserve object here, as *_binary copies class attributes */
    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
	bool mismatch = false;  // -Wall
	if (nx == ny || nx == 1 || ny == 1) mismatch = false;
	else if (nx > 0 && ny > 0) {
	    if (nx > ny) mismatch = (nx % ny != 0);
	    else mismatch = (ny % nx != 0);
	}

	if (mismatch)
	    warningcall(lcall,
			_("longer object length is not a multiple of shorter object length"));

	x = COERCE_IF_NEEDED(x, CPLXSXP);
	y = COERCE_IF_NEEDED(y, CPLXSXP);
	val = complex_binary(oper, x, y);
	BinaryArithmeticAttributeCopier::copyAttributes(
	    SEXP_downcast<VectorBase*>(val.get()), x, y);
    }
    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
	/* real_binary can handle REALSXP or INTSXP operand, but not LGLSXP. */
	/* Can get a LGLSXP. In base-Ex.R on 24 Oct '06, got 8 of these. */
	if (TYPEOF(x) != INTSXP) x = COERCE_IF_NEEDED(x, REALSXP);
	if (TYPEOF(y) != INTSXP) y = COERCE_IF_NEEDED(y, REALSXP);
	val = real_binary(oper, x, y);
    }
    else {
	val = integer_binary(oper, x, y, lcall);
    }

    return val;
}

namespace {

struct Negate {
    template<class InType, class OutType = InType>
    OutType operator()(InType value) { return -value; }
};

template<>
int Negate::operator()(int value) {
    return value == NA_INTEGER ? NA_INTEGER : -value;
}

template<typename InputType>
static SEXP typed_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    switch (code) {
    case PLUSOP:
	return s1;
    case MINUSOP:
	{
	    using namespace VectorOps;
	    return applyUnaryOperator(Negate(),
				      CopyAllAttributes(),
				      SEXP_downcast<InputType*>(s1));
	}
    default:
	errorcall(call, _("invalid unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

// In R, unary operators behave differently on logicals than other types.
// Specifically, only the layout attributes are copied and the result is
// an IntVector, not a LogicalVector.
SEXP logical_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    using namespace VectorOps;
    switch (code) {
    case PLUSOP:
	return applyUnaryOperator([](Logical value) {
		return static_cast<int>(value); },
	    CopyLayoutAttributes(),
	    SEXP_downcast<LogicalVector*>(s1));
    case MINUSOP:
	return applyUnaryOperator([](Logical value) {
		return static_cast<int>(!value);
	    },
	    CopyLayoutAttributes(),
	    SEXP_downcast<LogicalVector*>(s1));
	break;
    default:
	errorcall(call, _("invalid unary operator"));
    }
}

}  // anonymous namespace

SEXP attribute_hidden R_unary(SEXP call, SEXP op, SEXP s1)
{
    ARITHOP_TYPE operation = ARITHOP_TYPE( PRIMVAL(op));
    switch (TYPEOF(s1)) {
    case LGLSXP:
	// arithmetic on logicals makes no sense but R defines it anyway.
	return logical_unary(operation, s1, call);
    case INTSXP:
	return typed_unary<IntVector>(operation, s1, call);
    case REALSXP:
	return typed_unary<RealVector>(operation, s1, call);
    case CPLXSXP:
	return complex_unary(operation, s1, call);
    default:
	errorcall(call, _("invalid argument to unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

namespace {
    int integer_plus(int lhs, int rhs, Rboolean* naflag) {
	if (lhs == NA_INTEGER || rhs == NA_INTEGER) {
	    return NA_INTEGER;
	}
	if ((lhs > 0 && INT_MAX - lhs < rhs)
	    || (lhs < 0 && INT_MAX + lhs < -rhs)) {
	    // Integer overflow.
	    *naflag = TRUE;
	    return NA_INTEGER;
	}
	return lhs + rhs;
    }

    int integer_minus(int lhs, int rhs, Rboolean* naflag) {
	if (rhs == NA_INTEGER)
	    return NA_INTEGER;
	return integer_plus(lhs, -rhs, naflag);
    }

    int integer_times(int lhs, int rhs, Rboolean* naflag) {
	if (lhs == NA_INTEGER || rhs == NA_INTEGER) {
	    return NA_INTEGER;
	}
	// This relies on the assumption that a double can represent all the
	// possible values of an integer.  This isn't true for 64-bit integers.
	static_assert(sizeof(int) <= 4,
		      "integer_times assumes 32 bit integers which isn't true on this platform");
	double result = static_cast<double>(lhs) * static_cast<double>(rhs);
	if (std::abs(result) > INT_MAX) {
	    // Integer overflow.
	    *naflag = TRUE;
	    return NA_INTEGER;
	}
	return static_cast<int>(result);
    }

    double integer_divide(int lhs, int rhs) {
	if (lhs == NA_INTEGER || rhs == NA_INTEGER) {
	    return NA_REAL;
	}
	return static_cast<double>(lhs) / static_cast<double>(rhs);
    }

    double integer_pow(int lhs, int rhs) {
	if(lhs == 1 || rhs == 0)
	    return 1;
	if (lhs == NA_INTEGER || rhs == NA_INTEGER) {
	    return NA_REAL;
	}
	return R_POW(static_cast<double>(lhs), static_cast<double>(rhs));
    }

    int integer_mod(int lhs, int rhs) {
	if (lhs == NA_INTEGER || rhs == NA_INTEGER || rhs == 0)
	    return NA_INTEGER;
	return (lhs >= 0 && rhs > 0) ? lhs % rhs :
	    static_cast<int>(myfmod(lhs, rhs));
    }

    int integer_idiv(int lhs, int rhs) {
	/* This had x %/% 0 == 0 prior to 2.14.1, but
	   it seems conventionally to be undefined */
	if (lhs == NA_INTEGER || rhs == NA_INTEGER || rhs == 0)
	    return NA_INTEGER;
	return static_cast<int>(floor(double(lhs) / static_cast<double>(rhs)));
    }

    template<class Op>
    VectorBase* apply_integer_binary(Op op, SEXP lhs, SEXP rhs) {
	if (TYPEOF(lhs) != INTSXP) {
	    // Probably a logical.
	    // TODO(kmillar): eliminate the need to coerce here.
	    lhs = coerceVector(lhs, INTSXP);
	}
	if (TYPEOF(rhs) != INTSXP) {
	    rhs = coerceVector(rhs, INTSXP);
	}

	return applyBinaryOperator(op,
				   BinaryArithmeticAttributeCopier(),
				   SEXP_downcast<IntVector*>(lhs),
				   SEXP_downcast<IntVector*>(rhs));
    }
}  // anonymous namespace

#define INTEGER_OVERFLOW_WARNING _("NAs produced by integer overflow")

static SEXP integer_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2, SEXP lcall)
{
    Rboolean naflag = FALSE;
    VectorBase* ans = nullptr;

    switch (code) {
    case PLUSOP:
	ans = apply_integer_binary(
	    [&](int lhs, int rhs) { return integer_plus(lhs, rhs, &naflag); },
	    s1, s2);
	break;
    case MINUSOP:
	ans = apply_integer_binary(
	    [&](int lhs, int rhs) { return integer_minus(lhs, rhs, &naflag); },
	    s1, s2);
	break;
    case TIMESOP:
	ans = apply_integer_binary(
	    [&](int lhs, int rhs) { return integer_times(lhs, rhs, &naflag); },
	    s1, s2);
	break;
    case DIVOP:
	ans = apply_integer_binary(
	    [](int lhs, int rhs) { return integer_divide(lhs, rhs); },
	    s1, s2);
	break;
    case POWOP:
	ans = apply_integer_binary(
	    [](int lhs, int rhs) { return integer_pow(lhs, rhs); },
	    s1, s2);
	break;
    case MODOP:
	ans = apply_integer_binary(
	    [](int lhs, int rhs) { return integer_mod(lhs, rhs); },
	    s1, s2);
	break;
    case IDIVOP:
	ans = apply_integer_binary(
	    [](int lhs, int rhs) { return integer_idiv(lhs, rhs); },
	    s1, s2);
	break;
    }
    if (naflag)
	warningcall(lcall, INTEGER_OVERFLOW_WARNING);

    return ans;
}

namespace {
    inline double intToReal(int value) {
	return isNA(value) ? NA_REAL : value;
    }
}

template<class Op>
static RealVector* apply_real_binary(Op op, SEXP lhs, SEXP rhs)
{
    using namespace VectorOps;

    SEXPTYPE lhs_type = TYPEOF(lhs);
    SEXPTYPE rhs_type = TYPEOF(rhs);

    if(lhs_type == REALSXP && rhs_type == REALSXP) {
	return applyBinaryOperator(
	    op,
	    BinaryArithmeticAttributeCopier(),
	    SEXP_downcast<RealVector*>(lhs),
	    SEXP_downcast<RealVector*>(rhs));
    } else if(lhs_type == INTSXP) {
	return applyBinaryOperator(
	    [=](int lhs, double rhs) { return op(intToReal(lhs), rhs); },
	    BinaryArithmeticAttributeCopier(),
	    SEXP_downcast<IntVector*>(lhs),
	    SEXP_downcast<RealVector*>(rhs));
    } else {
	assert(rhs_type == INTSXP);
	return applyBinaryOperator(
	    [=](double lhs, int rhs) { return op(lhs, intToReal(rhs)); },
	    BinaryArithmeticAttributeCopier(),
	    SEXP_downcast<RealVector*>(lhs),
	    SEXP_downcast<IntVector*>(rhs));
    }
}

static SEXP real_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
{
    switch (code) {
    case PLUSOP:
	return apply_real_binary(
	    [](double lhs, double rhs) { return lhs + rhs; },
	    s1, s2);
    case MINUSOP:
	return apply_real_binary(
	    [](double lhs, double rhs) { return lhs - rhs; },
	    s1, s2);
    case TIMESOP:
	return apply_real_binary(
	    [](double lhs, double rhs) { return lhs * rhs; },
	    s1, s2);
    case DIVOP:
	return apply_real_binary(
	    [](double lhs, double rhs) { return lhs / rhs; },
	    s1, s2);
    case POWOP:
	return apply_real_binary(
	    [](double lhs, double rhs) { return R_POW(lhs, rhs); },
	    s1, s2);
    case MODOP:
	return apply_real_binary(
	    [](double lhs, double rhs) { return myfmod(lhs, rhs); },
	    s1, s2);
    case IDIVOP:
	return apply_real_binary(
	    [](double lhs, double rhs) { return myfloor(lhs, rhs); },
	    s1, s2);
    }
    return R_NilValue;  // Unreachable; -Wall.
}


/* Mathematical Functions of One Argument */

// FunctorWrapper for VectorOps::UnaryFunction.  Warns if function
// application gives rise to any new NaNs.
class NaNWarner {
public:
    NaNWarner(double (*f)(double))
	: m_f(f), m_any_NaN(false)
    {}

    double operator()(double in)
    {
	/* This code assumes that isnan(in) implies isnan(m_f(in)), so we
	   only need to check isnan(in) if isnan(m_f(in)) is true. */
	double ans = m_f(in);
	if (ISNAN(ans)) {
	    if (ISNAN(in))
		ans = in; // ensure the incoming NaN is preserved.
	    else
		m_any_NaN = true;
	}
	return ans;
    }

    void warnings()
    {
	if (m_any_NaN)
	    Rf_warning(R_MSG_NA);
    }
private:
    double (*m_f)(double);
    bool m_any_NaN;
};

static SEXP math1(SEXP sa, double (*f)(double), SEXP lcall)
{
    using namespace VectorOps;
    if (!isNumeric(sa))
	errorcall(lcall, R_MSG_NONNUM_MATH);
    /* coercion can lose the object bit */
    GCStackRoot<RealVector>
	rv(static_cast<RealVector*>(coerceVector(sa, REALSXP)));
    NaNWarner op(f);
    RealVector* result = applyUnaryOperator(std::ref(op),
					    CopyAllAttributes(),
					    rv.get());
    op.warnings();
    return result;
}

SEXP attribute_hidden do_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;

    BuiltInFunction* builtin = SEXP_downcast<BuiltInFunction*>(op);

    if (isComplex(CAR(args)))
	return complex_math1(call, op, args, env);

#define MATH1(x) math1(CAR(args), x, call);
    switch (builtin->variant()) {
    case 1: return MATH1(floor);
    case 2: return MATH1(ceil);
    case 3: return MATH1(sqrt);
    case 4: return MATH1(sign);
	/* case 5: return MATH1(trunc); separate from 2.6.0 */

    case 10: return MATH1(exp);
    case 11: return MATH1(expm1);
    case 12: return MATH1(log1p);
    case 20: return MATH1(cos);
    case 21: return MATH1(sin);
    case 22: return MATH1(tan);
    case 23: return MATH1(acos);
    case 24: return MATH1(asin);
    case 25: return MATH1(atan);

    case 30: return MATH1(cosh);
    case 31: return MATH1(sinh);
    case 32: return MATH1(tanh);
    case 33: return MATH1(acosh);
    case 34: return MATH1(asinh);
    case 35: return MATH1(atanh);

    case 40: return MATH1(lgammafn);
    case 41: return MATH1(gammafn);

    case 42: return MATH1(digamma);
    case 43: return MATH1(trigamma);
	/* case 44: return MATH1(tetragamma);
	   case 45: return MATH1(pentagamma);
	   removed in 2.0.0

	   case 46: return MATH1(Rf_gamma_cody); removed in 2.8.0
	*/
    case 47: return MATH1(cospi);
    case 48: return MATH1(sinpi);
#if defined(HAVE_TANPI) || defined(HAVE___TANPI)
    case 49: return MATH1(Rtanpi);
#else
    case 49: return MATH1(tanpi);
#endif

    default:
	errorcall(call, _("unimplemented real function of 1 argument"));
    }
    return s; /* never used; to keep -Wall happy */
}

/* methods are allowed to have more than one arg */
SEXP attribute_hidden do_trunc(/*const*/ Expression* call, const BuiltInFunction* op, Environment* env, RObject* const* args, int num_args, const PairList* tags)
{
    call->check1arg("x"); // Checked _after_ internal dispatch.
    SEXP arg = num_args > 0 ? args[0] : R_NilValue;
    if (isComplex(arg))
	errorcall(call, _("unimplemented complex function"));
    return math1(arg, trunc, call);
}

/*
   Note that this is slightly different from the do_math1 set,
   both for integer/logical inputs and what it dispatches to for complex ones.
*/

SEXP attribute_hidden do_abs(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, s = R_NilValue /* -Wall */;

    x = CAR(args);
    if (isInteger(x) || isLogical(x)) {
	/* integer or logical ==> return integer,
	   factor was covered by Math.factor. */
	R_xlen_t i, n = XLENGTH(x);
	s = (NO_REFERENCES(x) && TYPEOF(x) == INTSXP) ?
	    x : allocVector(INTSXP, n);
	PROTECT(s);
	/* Note: relying on INTEGER(.) === LOGICAL(.) : */
	for(i = 0 ; i < n ; i++) {
            int xi = INTEGER(x)[i];
	    INTEGER(s)[i] = (xi == NA_INTEGER) ? xi : abs(xi);
        }
    } else if (TYPEOF(x) == REALSXP) {
	R_xlen_t i, n = XLENGTH(x);
	PROTECT(s = NO_REFERENCES(x) ? x : allocVector(REALSXP, n));
	for(i = 0 ; i < n ; i++)
	    REAL(s)[i] = fabs(REAL(x)[i]);
    } else if (isComplex(x)) {
        SET_TAG(args, R_NilValue); /* cmathfuns want "z"; we might have "x" PR#16047 */
	return do_cmathfuns(call, op, args, env);
    } else
	errorcall(call, R_MSG_NONNUM_MATH);

    if (x != s && ATTRIB(x) != R_NilValue)
	SHALLOW_DUPLICATE_ATTRIB(s, x);
    UNPROTECT(1);
    return s;
}

/* Mathematical Functions of Two Numeric Arguments (plus 1 int) */

#define if_NA_Math2_set(y,a,b)				\
	if      (ISNA (a) || ISNA (b)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)) y = R_NaN;

static SEXP math2(SEXP sa, SEXP sb, double (*f)(double, double),
		  SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int naflag;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    /* for 0-length a we want the attributes of a, not those of b
       as no recycling will occur */
#define SETUP_Math2				\
    na = XLENGTH(sa);				\
    nb = XLENGTH(sb);				\
    if ((na == 0) || (nb == 0))	{		\
	PROTECT(sy = allocVector(REALSXP, 0));	\
	if (na == 0) SHALLOW_DUPLICATE_ATTRIB(sy, sa);  \
	UNPROTECT(1);				\
	return(sy);				\
    }						\
    n = (na < nb) ? nb : na;			\
    PROTECT(sa = coerceVector(sa, REALSXP));	\
    PROTECT(sb = coerceVector(sb, REALSXP));	\
    PROTECT(sy = allocVector(REALSXP, n));	\
    a = REAL(sa);				\
    b = REAL(sb);				\
    y = REAL(sy);				\
    naflag = 0

    SETUP_Math2;

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

#define FINISH_Math2					\
    if(naflag) warning(R_MSG_NA);			\
    if (n == na)  SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    UNPROTECT(3)

    FINISH_Math2;

    return sy;
} /* math2() */

/* This is only used directly by .Internal for Bessel functions,
   so managing R_alloc stack is only prudence */
static SEXP math2B(SEXP sa, SEXP sb, double (*f)(double, double, double *),
		   SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int naflag;
    double amax, *work;
    size_t nw;

#define besselJY_max_nu 1e7

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    /* for 0-length a we want the attributes of a, not those of b
       as no recycling will occur */
    SETUP_Math2;

    /* allocate work array for BesselJ, BesselY large enough for all
       arguments */
    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (amax < av)
	    amax = av;
    }
    if (amax > besselJY_max_nu)
	amax = besselJY_max_nu; // and warning will happen in ../nmath/bessel_[jy].c
    const void *vmax = vmaxget();
    nw = 1 + (size_t)floor(amax);
    work = (double *) R_alloc(nw, sizeof(double));

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    vmaxset(vmax);
    FINISH_Math2;

    return sy;
} /* math2B() */

#define Math2(A, FUN)	  math2(x, y, FUN, call);
#define Math2B(A, FUN)	  math2B(x, y, FUN, call);

SEXP attribute_hidden do_math2(Expression* call,
			       const BuiltInFunction* op,
			       RObject* x, RObject* y)
{
    if (isComplex(x) ||
	(op->variant() == 0 && isComplex(y))) {
	return complex_math2(call, const_cast<BuiltInFunction*>(op),
			     Rf_list2(x, y), nullptr);
    }
    switch (op->variant()) {

    case  0: return Math2(args, atan2);
    case 10001: return Math2(args, fround);// round(),  ../nmath/fround.c
    case 10004: return Math2(args, fprec); // signif(), ../nmath/fprec.c

    case  2: return Math2(args, lbeta);
    case  3: return Math2(args, beta);
    case  4: return Math2(args, lchoose);
    case  5: return Math2(args, choose);

    case 24: return Math2B(args, bessel_j_ex);
    case 25: return Math2B(args, bessel_y_ex);
    case 26: return Math2(args, psigamma);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 2);
    }
    return nullptr;			/* never used; to keep -Wall happy */
}


/* The S4 Math2 group, round and signif */
/* This is a primitive SPECIALSXP with internal argument matching */
SEXP attribute_hidden do_Math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if (Rf_length(args) >= 2 &&
	isSymbol(CADR(args)) && R_isMissing(CADR(args), env)) {
	double digits = 0;
	if(PRIMVAL(op) == 10004) digits = 6.0; // for signif()
	args = list2(CAR(args), ScalarReal(digits));
    }

    args = evalListKeepMissing(args, env);
    Expression* call2 = new Expression(CAR(call),
				       SEXP_downcast<PairList*>(args));
    int n = Rf_length(args);
    if (n != 1 && n != 2)
        error(ngettext("%d argument passed to '%s' which requires 1 or 2 arguments",
                       "%d arguments passed to '%s'which requires 1 or 2 arguments", n),
              n, PRIMNAME(op));

    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    auto dispatched = SEXP_downcast<BuiltInFunction*>(op)
	->InternalDispatch(SEXP_downcast<Expression*>(call2),
			   SEXP_downcast<Environment*>(env),
			   &arglist);
    if (dispatched.first) {
	return dispatched.second;
    }

    if(n == 1) {
	double digits = PRIMVAL(op) == 10001 ? 0 : 6;
	return do_math2(SEXP_downcast<Expression*>(call),
			SEXP_downcast<const BuiltInFunction*>(op),
			CAR(args), ScalarReal(digits));
    }

    static GCRoot<ArgMatcher> matcher = new ArgMatcher({ "x", "digits" });
    SEXP x, digits;
    matcher->match(&arglist, { &x, &digits} );
    if (Rf_length(digits) == 0)
	errorcall(call, _("invalid second argument of length 0"));
    return do_math2(SEXP_downcast<Expression*>(call),
		    SEXP_downcast<const BuiltInFunction*>(op), x, digits);
}

/* log{2,10} are builtins */
SEXP attribute_hidden do_log1arg(/*const*/ Expression* call, const BuiltInFunction* op, Environment* env, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP tmp = R_NilValue /* -Wall */;

    if(op->variant() == 10) tmp = ScalarReal(10.0);
    if(op->variant() == 2)  tmp = ScalarReal(2.0);

    static RObject* log_symbol = Symbol::obtain("log");
    ArgList arglist2({ args[0], tmp }, ArgList::EVALUATED);
    Expression* call2 = new Expression(log_symbol,
				       const_cast<PairList*>(arglist2.list()));
    auto dispatch = op->InternalDispatch(call2, env, &arglist2);
    if (dispatch.first) {
	return dispatch.second;
    }

    if (isComplex(args[0]))
	return complex_math2(call2, const_cast<BuiltInFunction*>(op),
			     const_cast<PairList*>(arglist2.list()), env);
    else
	return math2(args[0], tmp, logbase, call);
}

#ifdef M_E
# define DFLT_LOG_BASE M_E
#else
# define DFLT_LOG_BASE exp(1.)
#endif

/* do_log is a primitive SPECIALSXP with internal argument
   matching. do_log_builtin is the BUILTIN version that expects
   evaluated arguments to be passed as 'args', expect that these may
   contain missing arguments.  */
SEXP attribute_hidden do_log(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = evalListKeepMissing(args, env);
    return  do_log_builtin(call, op, args, env);
}

SEXP attribute_hidden do_log_builtin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Expression* callx = SEXP_downcast<Expression*>(call);
    BuiltInFunction* builtin = SEXP_downcast<BuiltInFunction*>(op);
    Environment* env = SEXP_downcast<Environment*>(rho);

    PROTECT(args);
    int n = Rf_length(args);
    SEXP res;

    if (n == 1 && TAG(args) == R_NilValue) {
	/* log(x) is handled here */
	SEXP x = CAR(args);
	if (x != R_MissingArg && ! OBJECT(x)) {
	    if (isComplex(x))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(x, R_log, call);
	    UNPROTECT(1);
	    return res;
	}
    }
    else if (n == 2 &&
	     TAG(args) == R_NilValue &&
	     (TAG(CDR(args)) == R_NilValue || TAG(CDR(args)) == R_BaseSymbol)) {
	/* log(x, y) or log(x, base = y) are handled here */
	SEXP x = CAR(args);
	SEXP y = CADR(args);
	if (x != R_MissingArg && y != R_MissingArg &&
	    ! OBJECT(x) && ! OBJECT(y)) {
	    if (isComplex(x) || isComplex(y))
		res = complex_math2(call, op, args, env);
	    else
		res = math2(x, y, logbase, call);
	    UNPROTECT(1);
	    return res;
	}
    }

    static SEXP R_x_Symbol = Symbol::obtain("x");
    static GCRoot<ArgMatcher> matcher = new ArgMatcher({ "x", "base" });

    if (n == 1) {
	if (CAR(args) == R_MissingArg ||
	    (TAG(args) != R_NilValue && TAG(args) != R_x_Symbol))
	    error(_("argument \"%s\" is missing, with no default"), "x");

	ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
	auto dispatched = builtin->InternalDispatch(callx, env, &arglist);
	if (dispatched.first)
	    res = dispatched.second;
	else {
	    if (isComplex(CAR(args)))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(CAR(args), R_log, call);
	}
	UNPROTECT(1);
	return res;
    }
    else {
	/* match argument names if supplied */
	/* will signal an error unless there are one or two arguments */
	/* after the match, Rf_length(args) will be 2 */
	ArgList arglist1(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
	SEXP x, base;
	matcher->match(&arglist1, { &x, &base });

	if(x == R_MissingArg)
	    error(_("argument \"%s\" is missing, with no default"), "x");
	if (base == R_MissingArg)
	    base = ScalarReal(DFLT_LOG_BASE);

	args = Rf_list2(x, base);
	ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
	auto dispatched = builtin->InternalDispatch(callx, env, &arglist);
	if (dispatched.first) {
	    res = dispatched.second;
	} else {
	    if (Rf_length(base) == 0)
		errorcall(call, _("invalid argument 'base' of length 0"));
	    if (isComplex(x) || isComplex(base))
		res = complex_math2(call, op, args, env);
	    else
		res = math2(x, base, logbase, call);
	}
	UNPROTECT(2);
	return res;
    }
}


/* Mathematical Functions of Three (Real) Arguments */

#define if_NA_Math3_set(y,a,b,c)			        \
	if      (ISNA (a) || ISNA (b)|| ISNA (c)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)|| ISNAN(c)) y = R_NaN;

#define SETUP_Math3						\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))	\
	errorcall(lcall, R_MSG_NONNUM_MATH);			\
								\
    na = XLENGTH(sa);						\
    nb = XLENGTH(sb);						\
    nc = XLENGTH(sc);						\
    if ((na == 0) || (nb == 0) || (nc == 0))			\
	return(allocVector(REALSXP, 0));			\
    n = na;							\
    if (n < nb) n = nb;						\
    if (n < nc) n = nc;						\
    PROTECT(sa = coerceVector(sa, REALSXP));			\
    PROTECT(sb = coerceVector(sb, REALSXP));			\
    PROTECT(sc = coerceVector(sc, REALSXP));			\
    PROTECT(sy = allocVector(REALSXP, n));			\
    a = REAL(sa);						\
    b = REAL(sb);						\
    c = REAL(sc);						\
    y = REAL(sy);						\
    naflag = 0

#define FINISH_Math3					\
    if(naflag) warning(R_MSG_NA);			\
							\
    if (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    UNPROTECT(4)

/* This is only used directly by .Internal for Bessel functions,
   so managing R_alloc stack is only prudence */
static SEXP math3B(SEXP sa, SEXP sb, SEXP sc,
		   double (*f)(double, double, double, double *), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int naflag;
    double amax, *work;
    size_t nw;

    SETUP_Math3;

    /* allocate work array for BesselI, BesselK large enough for all
       arguments */
    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (av > amax) amax = av;
    }
    const void *vmax = vmaxget();
    nw = 1 + long(floor(amax));
    work = static_cast<double *>( RHO_alloc(size_t( nw), sizeof(double)));

    MOD_ITERATE3 (n, na, nb, nc, i, ia, ib, ic, {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math3;
    vmaxset(vmax);

    return sy;
} /* math3B */

#define Math3B(A, FUN)  math3B (x, nu, expon_scaled, FUN, call);

SEXP attribute_hidden do_math3(Expression* call,
			       const BuiltInFunction* op,
			       RObject* x, RObject* nu,
			       RObject* expon_scaled)
{
    switch (op->variant()) {

    case 43:  return Math3B(args, bessel_i_ex);
    case 44:  return Math3B(args, bessel_k_ex);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 3);
    }
    return nullptr;			/* never used; to keep -Wall happy */
} /* do_math3() */
