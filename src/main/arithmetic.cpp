/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2015	    The R Core Team.
 *  Copyright (C) 2003--2015	    The R Foundation
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

/** @arithmetic.cpp
 *
 * All sorts of arithmetical and mathematical functions, from addition
 * through to Bessel functions.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <limits>

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000


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

#ifndef isnan
// Needed for example with MacOS 10.5.7 + Xcode 3.1.3:
#define isnan std::isnan
#endif

#include "arithmetic.h"

#include <errno.h>
#include <math.h>

#include "CXXR/BinaryFunction.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/UnaryFunction.hpp"

using namespace CXXR;
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
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] == 1954);
    }
    return 0;
}

Rboolean R_IsNaN(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return CXXRCONSTRUCT(Rboolean, (y.word[lw] != 1954));
    }
    return FALSE;
}


/* Mainly for use in packages */

// Force a non-inline embodiment of R_finite():
namespace CXXR {
    namespace ForceNonInline{
	Rboolean (*R_finitep)(double) = R_finite;
    }
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
	return pow(x, y);
    }
    if (ISNAN(x) || ISNAN(y))
	return(x + y);
    if(!R_FINITE(x)) {
	if(x > 0)		/* Inf ^ y */
	    return (y < 0.)? 0. : R_PosInf;
	else {			/* (-Inf) ^ y */
	    if(R_FINITE(y) && y == floor(y)) /* (-Inf) ^ n */
		return (y < 0.) ? 0. : (myfmod(y, 2.) ? x  : -x);
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

    // If any of the args has a class, then we might need to dispatch.
    auto result = op_->InternalOpsGroupDispatch("Ops", call, env, num_args,
						args, tags);
    if (result.first)
	return result.second;

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

template<>
int Negate::operator()(Logical value) {
    return Negate()(static_cast<int>(value));
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

SEXP logical_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    using namespace VectorOps;
    switch (code) {
    case PLUSOP:
	return applyUnaryOperator([](Logical value) {
		return int(value); },
	    CopyLayoutAttributes(),
	    SEXP_downcast<LogicalVector*>(s1));
    case MINUSOP:
	return applyUnaryOperator([](Logical value) {
		if (value.isTrue()) return -1;
		if (value.isFalse()) return 0;
		return NA_INTEGER;
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
	// Unlike unary arithmetic on other types, the result only contains
	// the layout attributes.
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


/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

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
	if (isNA(in))
	    return NA<double>();
	double ans = m_f(in);
	if (isnan(ans) && !isnan(in))
	    m_any_NaN = true;
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

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    if (isComplex(CAR(args)))
	return complex_math1(call, op, args, env);

#define MATH1(x) math1(CAR(args), x, call);
    switch (PRIMVAL(op)) {
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
#ifndef HAVE_TANPI
    case 49: return MATH1(tanpi);
#else
    case 49: return MATH1(Rtanpi);
#endif

    default:
	errorcall(call, _("unimplemented real function of 1 argument"));
    }
    return s; /* never used; to keep -Wall happy */
}

/* methods are allowed to have more than one arg */
SEXP attribute_hidden do_trunc(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    // If any of the args has a class, then we might need to dispatch.
    auto result = op->InternalGroupDispatch("Math", call, env, num_args, args,
					    tags);
    if (result.first)
	return result.second;
    op->checkNumArgs(num_args, call); /* but is -1 in names.c */
    check1arg(tags, call, "x");
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

    checkArity(op, args);
    check1arg(args, call, "x");
    x = CAR(args);

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

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
	DUPLICATE_ATTRIB(s, x);
    UNPROTECT(1);
    return s;
}

/* Mathematical Functions of Two Numeric Arguments (plus 1 int) */

/* math2_1 and math2_2 and related can be removed  once the byte
  compiler knows how to optimize to .External rather than
  .Internal */

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
	if (na == 0) DUPLICATE_ATTRIB(sy, sa);  \
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

    mod_iterate(na, nb, ia, ib) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math2				\
    if(naflag) warning(R_MSG_NA);		\
    if (n == na)  DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
    UNPROTECT(3)

    FINISH_Math2;

    return sy;
} /* math2() */

static SEXP math2_1(SEXP sa, SEXP sb, SEXP sI,
		    double (*f)(double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int m_opt;
    int naflag;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    m_opt = asInteger(sI);

    mod_iterate(na, nb, ia, ib) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, m_opt);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_1() */

static SEXP math2_2(SEXP sa, SEXP sb, SEXP sI1, SEXP sI2,
		    double (*f)(double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int i_1, i_2;
    int naflag;
    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    i_1 = asInteger(sI1);
    i_2 = asInteger(sI2);

    mod_iterate(na, nb, ia, ib) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_2() */

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

    mod_iterate(na, nb, ia, ib) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    vmaxset(vmax);
    FINISH_Math2;

    return sy;
} /* math2B() */

#define Math2(A, FUN)	  math2(CAR(A), CADR(A), FUN, call);
#define Math2_1(A, FUN)	math2_1(CAR(A), CADR(A), CADDR(A), FUN, call);
#define Math2_2(A, FUN) math2_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN, call)
#define Math2B(A, FUN)	  math2B(CAR(A), CADR(A), FUN, call);

SEXP attribute_hidden do_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    if (isComplex(CAR(args)) ||
	(PRIMVAL(op) == 0 && isComplex(CADR(args))))
	return complex_math2(call, op, args, env);


    switch (PRIMVAL(op)) {

    case  0: return Math2(args, atan2);
    case 10001: return Math2(args, fround);// round(),  ../nmath/fround.c
    case 10004: return Math2(args, fprec); // signif(), ../nmath/fprec.c

    case  2: return Math2(args, lbeta);
    case  3: return Math2(args, beta);
    case  4: return Math2(args, lchoose);
    case  5: return Math2(args, choose);

    case  6: return Math2_1(args, dchisq);
    case  7: return Math2_2(args, pchisq);
    case  8: return Math2_2(args, qchisq);

    case  9: return Math2_1(args, dexp);
    case 10: return Math2_2(args, pexp);
    case 11: return Math2_2(args, qexp);

    case 12: return Math2_1(args, dgeom);
    case 13: return Math2_2(args, pgeom);
    case 14: return Math2_2(args, qgeom);

    case 15: return Math2_1(args, dpois);
    case 16: return Math2_2(args, ppois);
    case 17: return Math2_2(args, qpois);

    case 18: return Math2_1(args, dt);
    case 19: return Math2_2(args, pt);
    case 20: return Math2_2(args, qt);

    case 21: return Math2_1(args, dsignrank);
    case 22: return Math2_2(args, psignrank);
    case 23: return Math2_2(args, qsignrank);

    case 24: return Math2B(args, bessel_j_ex);
    case 25: return Math2B(args, bessel_y_ex);
    case 26: return Math2(args, psigamma);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 2);
    }
    return op;			/* never used; to keep -Wall happy */
}


/* The S4 Math2 group, round and signif */
/* This is a primitive SPECIALSXP with internal argument matching */
SEXP attribute_hidden do_Math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res, call2;
    int n, nprotect = 2;
    static SEXP do_Math2_formals = NULL;

    if (Rf_length(args) >= 2 &&
	isSymbol(CADR(args)) && R_isMissing(CADR(args), env)) {
	double digits = 0;
	if(PRIMVAL(op) == 10004) digits = 6.0; // for signif()
	PROTECT(args = list2(CAR(args), ScalarReal(digits))); nprotect++;
    }

    PROTECT(args = evalListKeepMissing(args, env));
    PROTECT(call2 = lang2(CAR(call), nullptr));
    SETCDR(call2, args);

    n = Rf_length(args);
    if (n != 1 && n != 2)
        error(ngettext("%d argument passed to '%s' which requires 1 or 2 arguments",
                       "%d arguments passed to '%s'which requires 1 or 2 arguments", n),
              n, PRIMNAME(op));

    if (! DispatchGroup("Math", call2, op, args, env, &res)) {
	if(n == 1) {
	    double digits = 0.0;
	    if(PRIMVAL(op) == 10004) digits = 6.0;
	    SETCDR(args, CONS(ScalarReal(digits), R_NilValue));
	} else {
	    /* If named, do argument matching by name */
	    if (TAG(args) != R_NilValue || TAG(CDR(args)) != R_NilValue) {
	        if (do_Math2_formals == NULL)
                    do_Math2_formals = allocFormalsList2(install("x"),
							 install("digits"));
		PROTECT(args = matchArgs(do_Math2_formals, args, call));
		nprotect++;
	    }
	    if (Rf_length(CADR(args)) == 0)
		errorcall(call, _("invalid second argument of length 0"));
	}
	res = do_math2(call, op, args, env);
    }
    UNPROTECT(nprotect);
    return res;
}

/* log{2,10} are builtins */
SEXP attribute_hidden do_log1arg(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP res, call2, args2, tmp = R_NilValue /* -Wall */;

    op->checkNumArgs(num_args, call);
    check1arg(tags, call, "x");

    auto dispatch = op->InternalGroupDispatch("Math", call, env, num_args, args,
					      tags);
    if (dispatch.first)
	return dispatch.second;

    if(op->variant() == 10) tmp = ScalarReal(10.0);
    if(op->variant() == 2)  tmp = ScalarReal(2.0);

    PROTECT(call2 = lang3(install("log"), args[0], tmp));
    PROTECT(args2 = list2(args[0], tmp));
    if (! DispatchGroup("Math", call2, const_cast<BuiltInFunction*>(op),
			args2, env, &res)) {
	if (isComplex(args[0]))
	    res = complex_math2(call2, const_cast<BuiltInFunction*>(op),
				args2, env);
	else
	    res = math2(args[0], tmp, logbase, call);
    }
    UNPROTECT(2);
    return res;
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

SEXP attribute_hidden do_log_builtin(SEXP call, SEXP op, SEXP args, SEXP env)
{
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

    static SEXP do_log_formals = NULL;
    static SEXP R_x_Symbol = NULL;
    if (do_log_formals == NULL) {
	R_x_Symbol = install("x");
	do_log_formals = allocFormalsList2(R_x_Symbol, R_BaseSymbol);
    }

    if (n == 1) {
	if (CAR(args) == R_MissingArg ||
	    (TAG(args) != R_NilValue && TAG(args) != R_x_Symbol))
	    error(_("argument \"%s\" is missing, with no default"), "x");

	if (! DispatchGroup("Math", call, op, args, env, &res)) {
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
	PROTECT(args = matchArgs(do_log_formals, args, call));

	if(CAR(args) == R_MissingArg)
	    error(_("argument \"%s\" is missing, with no default"), "x");
	if (CADR(args) == R_MissingArg)
	    SETCADR(args, ScalarReal(DFLT_LOG_BASE));

	if (! DispatchGroup("Math", call, op, args, env, &res)) {
	    if (Rf_length(CADR(args)) == 0)
		errorcall(call, _("invalid argument 'base' of length 0"));
	    if (isComplex(CAR(args)) || isComplex(CADR(args)))
		res = complex_math2(call, op, args, env);
	    else
		res = math2(CAR(args), CADR(args), logbase, call);
	}
	UNPROTECT(2);
	return res;
    }
}


/* Mathematical Functions of Three (Real) Arguments */

/* math3_1 and math3_2 and related can be removed once the byte
  compiler knows how to optimize to .External rather than
  .Internal */


#define if_NA_Math3_set(y,a,b,c)			        \
	if      (ISNA (a) || ISNA (b)|| ISNA (c)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)|| ISNAN(c)) y = R_NaN;

#define mod_iterate3(n1,n2,n3,i1,i2,i3) for (i=i1=i2=i3=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,				\
	i2 = (++i2==n2) ? 0 : i2,				\
	i3 = (++i3==n3) ? 0 : i3,				\
	++i)

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

#define FINISH_Math3				\
    if(naflag) warning(R_MSG_NA);		\
						\
    if (n == na) DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc);	\
    UNPROTECT(4)

static SEXP math3_1(SEXP sa, SEXP sb, SEXP sc, SEXP sI,
		    double (*f)(double, double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_1 */

static SEXP math3_2(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1,i_2;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_2 */

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
    work = static_cast<double *>( CXXR_alloc(size_t( nw), sizeof(double)));

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    vmaxset(vmax);

    return sy;
} /* math3B */

#define Math3_1(A, FUN)	math3_1(args[0], args[1], args[2], args[3], FUN, call);
#define Math3_2(A, FUN) math3_2(args[0], args[1], args[2], args[3], args[4], FUN, call)
#define Math3B(A, FUN)  math3B (args[0], args[1], args[2], FUN, call);

SEXP attribute_hidden do_math3(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    op->checkNumArgs(num_args, call);

    switch (op->variant()) {

    case  1:  return Math3_1(args, dbeta);
    case  2:  return Math3_2(args, pbeta);
    case  3:  return Math3_2(args, qbeta);

    case  4:  return Math3_1(args, dbinom);
    case  5:  return Math3_2(args, pbinom);
    case  6:  return Math3_2(args, qbinom);

    case  7:  return Math3_1(args, dcauchy);
    case  8:  return Math3_2(args, pcauchy);
    case  9:  return Math3_2(args, qcauchy);

    case 10:  return Math3_1(args, df);
    case 11:  return Math3_2(args, pf);
    case 12:  return Math3_2(args, qf);

    case 13:  return Math3_1(args, dgamma);
    case 14:  return Math3_2(args, pgamma);
    case 15:  return Math3_2(args, qgamma);

    case 16:  return Math3_1(args, dlnorm);
    case 17:  return Math3_2(args, plnorm);
    case 18:  return Math3_2(args, qlnorm);

    case 19:  return Math3_1(args, dlogis);
    case 20:  return Math3_2(args, plogis);
    case 21:  return Math3_2(args, qlogis);

    case 22:  return Math3_1(args, dnbinom);
    case 23:  return Math3_2(args, pnbinom);
    case 24:  return Math3_2(args, qnbinom);

    case 25:  return Math3_1(args, dnorm);
    case 26:  return Math3_2(args, pnorm);
    case 27:  return Math3_2(args, qnorm);

    case 28:  return Math3_1(args, dunif);
    case 29:  return Math3_2(args, punif);
    case 30:  return Math3_2(args, qunif);

    case 31:  return Math3_1(args, dweibull);
    case 32:  return Math3_2(args, pweibull);
    case 33:  return Math3_2(args, qweibull);

    case 34:  return Math3_1(args, dnchisq);
    case 35:  return Math3_2(args, pnchisq);
    case 36:  return Math3_2(args, qnchisq);

    case 37:  return Math3_1(args, dnt);
    case 38:  return Math3_2(args, pnt);
    case 39:  return Math3_2(args, qnt);

    case 40:  return Math3_1(args, dwilcox);
    case 41:  return Math3_2(args, pwilcox);
    case 42:  return Math3_2(args, qwilcox);

    case 43:  return Math3B(args, bessel_i_ex);
    case 44:  return Math3B(args, bessel_k_ex);

    case 45:  return Math3_1(args, dnbinom_mu);
    case 46:  return Math3_2(args, pnbinom_mu);
    case 47:  return Math3_2(args, qnbinom_mu);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 3);
    }
    return nullptr;			/* never used; to keep -Wall happy */
} /* do_math3() */

/* Mathematical Functions of Four (Real) Arguments */

/* This can be removed completely once the byte compiler knows how to
  optimize to .External rather than .Internal */

#define if_NA_Math4_set(y,a,b,c,d)				\
	if      (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)) y = NA_REAL;\
	else if (ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)) y = R_NaN;

#define mod_iterate4(n1,n2,n3,n4,i1,i2,i3,i4) for (i=i1=i2=i3=i4=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,					\
	i2 = (++i2==n2) ? 0 : i2,					\
	i3 = (++i3==n3) ? 0 : i3,					\
	i4 = (++i4==n4) ? 0 : i4,					\
	++i)

static SEXP math4(SEXP sa, SEXP sb, SEXP sc, SEXP sd,
		  double (*f)(double, double, double, double), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int naflag;

#define SETUP_Math4							\
    if(!isNumeric(sa)|| !isNumeric(sb)|| !isNumeric(sc)|| !isNumeric(sd))\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = XLENGTH(sa);							\
    nb = XLENGTH(sb);							\
    nc = XLENGTH(sc);							\
    nd = XLENGTH(sd);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0))		\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math4;

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math4				\
    if(naflag) warning(R_MSG_NA);		\
						\
    if (n == na) DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) DUPLICATE_ATTRIB(sy, sd);	\
    UNPROTECT(5)

    FINISH_Math4;

    return sy;
} /* math4() */

static SEXP math4_1(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, double (*f)(double, double, double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_1() */

static SEXP math4_2(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1, i_2;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_2() */


#define CAD3R	CADDDR
/* This is not (yet) in Rinternals.h : */
namespace {
    inline SEXP CAD5R(SEXP e) {return CAR(CDR(CDR(CDR(CDR(CDR(e))))));}
}

#define Math4(A, FUN)   math4  (CAR(A), CADR(A), CADDR(A), CAD3R(A), FUN, call)
#define Math4_1(A, FUN) math4_1(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				FUN, call)
#define Math4_2(A, FUN) math4_2(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				CAD5R(A), FUN, call)


SEXP attribute_hidden do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);


    switch (PRIMVAL(op)) {
    case  1: return Math4_1(args, dhyper);
    case  2: return Math4_2(args, phyper);
    case  3: return Math4_2(args, qhyper);

    case  4: return Math4_1(args, dnbeta);
    case  5: return Math4_2(args, pnbeta);
    case  6: return Math4_2(args, qnbeta);
    case  7: return Math4_1(args, dnf);
    case  8: return Math4_2(args, pnf);
    case  9: return Math4_2(args, qnf);
#ifdef UNIMP
    case 10: return Math4_1(args, dtukey);
#endif
    case 11: return Math4_2(args, ptukey);
    case 12: return Math4_2(args, qtukey);
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 4);
    }
    return op;			/* never used; to keep -Wall happy */
}


#ifdef WHEN_MATH5_IS_THERE/* as in ./arithmetic.h */

/* Mathematical Functions of Five (Real) Arguments */

#define if_NA_Math5_set(y,a,b,c,d,e)					\
	if     (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)|| ISNA (e))	\
		y = NA_REAL;						\
	else if(ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)|| ISNAN(e))	\
		y = R_NaN;

#define mod_iterate5(n1,n2,n3,n4,n5, i1,i2,i3,i4,i5)	\
 for (i=i1=i2=i3=i4=i5=0; i<n;				\
	i1 = (++i1==n1) ? 0 : i1,			\
	i2 = (++i2==n2) ? 0 : i2,			\
	i3 = (++i3==n3) ? 0 : i3,			\
	i4 = (++i4==n4) ? 0 : i4,			\
	i5 = (++i5==n5) ? 0 : i5,			\
	++i)

static SEXP math5(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP se, double (*f)())
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, ie, n, na, nb, nc, nd, ne;
    double ai, bi, ci, di, ei, *a, *b, *c, *d, *e, *y;

#define SETUP_Math5							\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) ||		\
	!isNumeric(sd) || !isNumeric(se))				\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = XLENGTH(sa);							\
    nb = XLENGTH(sb);							\
    nc = XLENGTH(sc);							\
    nd = XLENGTH(sd);							\
    ne = XLENGTH(se);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0) || (ne == 0))	\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    if (n < ne) n = ne;		/* n = max(na,nb,nc,nd,ne) */		\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(se = coerceVector(se, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    e = REAL(se);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math5;

    mod_iterate5 (na, nb, nc, nd, ne,
		  ia, ib, ic, id, ie) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	ei = e[ie];
	if_NA_Math5_set(y[i], ai,bi,ci,di,ei)
	else {
	    y[i] = f(ai, bi, ci, di, ei);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math5				\
    if(naflag) warning(R_MSG_NA);		\
						\
    if (n == na) DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) DUPLICATE_ATTRIB(sy, sd);	\
    else if (n == ne) DUPLICATE_ATTRIB(sy, se);	\
    UNPROTECT(6)

    FINISH_Math5;

    return sy;
} /* math5() */

#define Math5(A, FUN) \
	math5(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), FUN);

SEXP attribute_hidden do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- use math5() at all! : */
    case -99: return Math5(args, dhyper);
#ifdef UNIMP
    case  2: return Math5(args, p...);
    case  3: return Math5(args, q...);
#endif
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 5);
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math5() */

#endif /* Math5 is there */

#if 0
/* This is used for experimenting with parallelized nmath functions -- LT */
CCODE R_get_arith_function(int which)
{
    switch (which) {
    case 1: return do_math1;
    case 2: return do_math2;
    case 3: return do_math3;
    case 4: return do_math4;
    case 11: return complex_math1;
    case 12: return complex_math2;
    default: error("bad arith function index"); return nullptr;
    }
}
#endif
