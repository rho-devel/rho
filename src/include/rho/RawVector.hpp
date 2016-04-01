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

/** @file RawVector.h
 * @brief Class rho::RawVector and associated C interface.
 */

#ifndef RAWVECTOR_H
#define RAWVECTOR_H

#include "rho/FixedVector.hpp"
#include "rho/SEXP_downcast.hpp"
#include "rho/VectorBase.hpp"

typedef unsigned char Rbyte;

namespace rho {
    // Template specializations:
    namespace ElementTraits {
	template<>
	struct MustConstruct<Rbyte> : boost::mpl::false_ {};

	template<>
	struct MustDestruct<Rbyte> : boost::mpl::false_ {};

	template <>
	inline const Rbyte& NAFunc<Rbyte>::operator()() const
	{
	    static Rbyte s_na = 0;
	    return s_na;
	}

	template <>
	inline bool IsNA<Rbyte>::operator()(const Rbyte&) const
	{
	    return false;
	}
    }

    /** @brief Vector of 'raw bytes'.
     */
    typedef rho::FixedVector<Rbyte, RAWSXP> RawVector;

    template<>
    struct VectorTypeFor<Rbyte> {
      typedef RawVector type;
    };
}  // namespace rho

extern "C" {
/**
 * @param x Pointer to a rho::RawVector (i.e. a RAWSXP).  An error is
 *          generated if \a x is not a non-null pointer to a
 *          rho::RawVector .
 *
 * @return Pointer to element 0 of \a x .
 */
inline Rbyte *RAW(SEXP x)
{
    using namespace rho;
    return &(*SEXP_downcast<RawVector*>(x, false))[0];
}

}  // extern "C"

#endif /* RAWVECTOR_H */
