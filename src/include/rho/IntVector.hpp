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

/** @file IntVector.h
 * @brief Class rho::IntVector and associated C interface.
 */

#ifndef INTVECTOR_H
#define INTVECTOR_H

#include "R_ext/Arith.h"
#include "rho/ElementTraits.hpp"

namespace rho {
    // Template specializations:
    namespace ElementTraits {
	template <>
	inline const int& NAFunc<int>::operator()() const
	{
            static int na = NA_INTEGER;
            return na;
        }

	template<>
	struct MustConstruct<int> : boost::mpl::false_ {};

	template<>
	struct MustDestruct<int> : boost::mpl::false_ {};
    }
}

#include "rho/VectorBase.hpp"
#include "rho/FixedVector.hpp"
#include "rho/SEXP_downcast.hpp"

#ifndef USE_TYPE_CHECKING_STRICT
#include "rho/LogicalVector.hpp"
#endif

namespace rho {
    /** @brief Vector of integer values.
     */
    typedef FixedVector<int, INTSXP> IntVector;

    template<>
    struct VectorTypeFor<int> {
      typedef IntVector type;
    };

}  // namespace rho

extern "C" {

/**
 * @param x Pointer to an \c IntVector or a \c LogicalVector (i.e. an
 *          R integer or logical vector).  An error is generated if \a
 *          x is not a non-null pointer to an \c IntVector or a \c
 *          LogicalVector .
 *
 * @return Pointer to element 0 of \a x .
 */
inline int* INTEGER(SEXP x)
{
    using namespace rho;
#ifndef USE_TYPE_CHECKING_STRICT
    // Quicker than dynamic_cast:
    if (x && x->sexptype() == LGLSXP) {
	LogicalVector* lvec = static_cast<LogicalVector*>(x);
	return reinterpret_cast<int*>(&(*lvec)[0]);
    }
#endif
    return &(*SEXP_downcast<IntVector*>(x, false))[0];
}

}

#endif /* INTVECTOR_H */
