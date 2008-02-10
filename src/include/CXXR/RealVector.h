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

/** @file RealVector.h
 * @brief Class RealVector and associated C interface.
 */

#ifndef REALVECTOR_H
#define REALVECTOR_H

#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "CXXR/DumbVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* DumbVector<double, REALSXP>::staticTypeName()
    {
	return "numeric";
    }

    /** @brief Vector of real numbers.
     */
    typedef CXXR::DumbVector<double, REALSXP> RealVector;
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is a real vector.
     */
#ifndef __cplusplus
    Rboolean Rf_isReal(SEXP s);
#else
    inline Rboolean Rf_isReal(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == REALSXP);
    }
#endif

/**
 * @param x Pointer to an \c RealVector (i.e. an R numeric vector).
 *          An error is generated if \a x is not pointer to an \c
 *          RealVector .
 * @return Pointer to element 0 of \a x .
 */
#ifndef __cplusplus
double *REAL(SEXP x);
#else
inline double *REAL(SEXP x)
{
    return &(*CXXR::SEXP_downcast<CXXR::RealVector*>(x))[0];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* REALVECTOR_H */
