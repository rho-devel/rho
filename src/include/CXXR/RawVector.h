/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2008
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
 * @brief Class RawVector and associated C interface.
 */

#ifndef RAWVECTOR_H
#define RAWVECTOR_H

#include "CXXR/VectorBase.h"

typedef unsigned char Rbyte;

#ifdef __cplusplus

#include "CXXR/DumbVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* DumbVector<Rbyte, RAWSXP>::staticTypeName()
    {
	return "raw";
    }

    /** @brief Vector of 'raw bytes'.
     */
    typedef CXXR::DumbVector<Rbyte, RAWSXP> RawVector;
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

/**
 * @param x Pointer to an \c RawVector (i.e. an R numeric vector).
 *          An error is generated if \a x is not pointer to an \c
 *          RawVector .
 * @return Pointer to element 0 of \a x .
 */
#ifndef __cplusplus
Rbyte *RAW(SEXP x);
#else
inline Rbyte *RAW(SEXP x)
{
    return &(*CXXR::SEXP_downcast<CXXR::RawVector*>(x))[0];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* RAWVECTOR_H */
