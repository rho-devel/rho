/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2008
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file RRealVector.h
 * @brief Class RRealVector and associated C interface.
 */

#ifndef RREALVECTOR_H
#define RREALVECTOR_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/RDumbVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* RDumbVector<double, REALSXP>::staticTypeName()
    {
	return "numeric";
    }

    /** @brief Vector of real numbers.
     */
    typedef CXXR::RDumbVector<double, REALSXP> RRealVector;
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

/**
 * @param x Pointer to an \c RRealVector (i.e. an R numeric vector).
 *          An error is generated if \a x is not pointer to an \c
 *          RRealVector .
 * @return Pointer to \a x 's element 0 of the vector.
 */
#ifndef __cplusplus
double *REAL(SEXP x);
#else
inline double *REAL(SEXP x)
{
    return &(*CXXR::SEXP_downcast<CXXR::RRealVector>(x))[0];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* RREALVECTOR_H */
