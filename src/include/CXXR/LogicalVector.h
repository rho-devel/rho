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

/** @file LogicalVector.h
 * @brief Class CXXR::LogicalVector and associated C interface.
 */

#ifndef LOGICALVECTOR_H
#define LOGICALVECTOR_H

#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "CXXR/DumbVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* DumbVector<int, LGLSXP>::staticTypeName()
    {
	return "logical";
    }

    /** @brief Vector of truth values.
     */
    typedef CXXR::DumbVector<int, LGLSXP> LogicalVector;
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /**
     * @param s Pointer to a CXXR::RObject.
     * @return TRUE iff the CXXR::RObject pointed to by \a s is a
     *         logical vector.
     */
#ifndef __cplusplus
    Rboolean Rf_isLogical(SEXP s);
#else
    inline Rboolean Rf_isLogical(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == LGLSXP);
    }
#endif

/**
 * @param x Pointer to a CXXR::LogicalVector or a CXXR::IntVector (i.e. an
 *          R logical or integer vector).
 *          An error is generated if \a x is not pointer to a
 *          CXXR::LogicalVector or a CXXR::IntVector .
 * @return Pointer to element 0 of \a x .
 */
int *LOGICAL(SEXP x);

#ifdef __cplusplus
}
#endif

#endif /* LOGICALVECTOR_H */
