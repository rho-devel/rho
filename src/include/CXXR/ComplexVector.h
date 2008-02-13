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

/** @file ComplexVector.h
 * @brief Class ComplexVector and associated C interface.
 */

#ifndef COMPLEXVECTOR_H
#define COMPLEXVECTOR_H

#include "CXXR/VectorBase.h"
#include "R_ext/Complex.h"

#ifdef __cplusplus

#include "CXXR/DumbVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* DumbVector<Rcomplex, CPLXSXP>::staticTypeName()
    {
	return "complex";
    }

    /** @brief Vector of complex numbers.
     */
    typedef CXXR::DumbVector<Rcomplex, CPLXSXP> ComplexVector;
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is a complex vector.
     */
#ifndef __cplusplus
    Rboolean Rf_isComplex(SEXP s);
#else
    inline Rboolean Rf_isComplex(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == CPLXSXP);
    }
#endif

/**
 * @param x Pointer to a \c ComplexVector (i.e. an R complex vector).
 *          An error is generated if \a x is not pointer to a \c
 *          ComplexVector .
 * @return Pointer to element 0 of \a x .
 */
#ifndef __cplusplus
Rcomplex *COMPLEX(SEXP x);
#else
inline Rcomplex *COMPLEX(SEXP x)
{
    return &(*CXXR::SEXP_downcast<CXXR::ComplexVector*>(x))[0];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* COMPLEXVECTOR_H */
