/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
 * @brief Class CXXR::RawVector and associated C interface.
 */

#ifndef RAWVECTOR_H
#define RAWVECTOR_H

#include "CXXR/VectorBase.h"

typedef unsigned char Rbyte;

#ifdef __cplusplus

#include "CXXR/FixedVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
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

    template <>
    inline const char* FixedVector<Rbyte, RAWSXP>::staticTypeName()
    {
	return "raw";
    }

    /** @brief Vector of 'raw bytes'.
     */
    typedef CXXR::FixedVector<Rbyte, RAWSXP> RawVector;

    template<>
    struct VectorTypeFor<Rbyte> {
      typedef RawVector type;
    };
}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::RawVector)

extern "C" {
#endif /* __cplusplus */

/**
 * @param x Pointer to a CXXR::RawVector (i.e. a RAWSXP).  An error is
 *          generated if \a x is not a non-null pointer to a
 *          CXXR::RawVector .
 *
 * @return Pointer to element 0 of \a x .
 */
#ifndef __cplusplus
Rbyte *RAW(SEXP x);
#else
inline Rbyte *RAW(SEXP x)
{
    using namespace CXXR;
    return &(*SEXP_downcast<RawVector*>(x, false))[0];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* RAWVECTOR_H */
