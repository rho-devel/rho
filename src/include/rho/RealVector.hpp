/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file RealVector.h
 * @brief Class rho::RealVector and associated C interface.
 */

#ifndef REALVECTOR_H
#define REALVECTOR_H

#include "rho/VectorBase.hpp"
#include "R_ext/Arith.h"
#include "rho/FixedVector.hpp"
#include "rho/SEXP_downcast.hpp"

namespace rho {
    // Template specializations:
    namespace ElementTraits {
	template<>
	struct MustConstruct<double> : boost::mpl::false_ {};

	template<>
	struct MustDestruct<double> : boost::mpl::false_ {};

	template <>
        inline const double& NAFunc<double>::operator()() const
        {
	    static double na = NA_REAL;
	    return na;
	}

	template <>
        inline bool IsNA<double>::operator()(const double& t) const
        {
            return R_IsNA(t);
        }

	template <>
	inline bool IsNaOrNaN<double>::operator()(const double& t) const
	{
	    return std::isnan(t);
	}
    }

    /** @brief Vector of real numbers.
     */
    typedef rho::FixedVector<double, REALSXP> RealVector;

    template<>
    struct VectorTypeFor<double> {
      typedef RealVector type;
    };

}  // namespace rho

extern "C" {
    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is a real vector.
     */
    inline Rboolean Rf_isReal(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == REALSXP);
    }

/**
 * @param x Pointer to an \c RealVector (i.e. an R numeric vector).
 *          An error is generated if \a x is not pointer to an \c
 *          RealVector .
 * @return Pointer to element 0 of \a x .
 */
inline double *REAL(SEXP x)
{
    using namespace rho;
    return &(*SEXP_downcast<RealVector*>(x, false))[0];
}

}

#endif /* REALVECTOR_H */
