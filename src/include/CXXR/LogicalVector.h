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

/** @file LogicalVector.h
 * @brief Class CXXR::LogicalVector and associated C interface.
 */

#ifndef LOGICALVECTOR_H
#define LOGICALVECTOR_H

#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "R_ext/Arith.h"
#include "CXXR/FixedVector.hpp"
#include "CXXR/Logical.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* FixedVector<Logical, LGLSXP>::staticTypeName()
    {
	return "logical";
    }

    /** @brief Vector of truth values.
     */
    typedef CXXR::FixedVector<Logical, LGLSXP> LogicalVector;

}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::LogicalVector)

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
 * @param x Pointer to a CXXR::LogicalVector (checked).
 *
 * @return Pointer to element 0 of \a x .
 */
#ifndef __cplusplus
int* LOGICAL(SEXP x);
#else
inline int* LOGICAL(SEXP x)
{
    using namespace CXXR;
    return reinterpret_cast<int*>
      (&(*SEXP_downcast<LogicalVector*>(x, false))[0]);
}
#endif

    /** @brief Create a unit-length LogicalVector containing FALSE.
     *
     * @return a unit-length LogicalVector containing FALSE.
     */
    SEXP Rf_mkFalse();

    /** @brief Create a unit-length LogicalVector containing TRUE.
     *
     * @return a unit-length LogicalVector containing TRUE.
     */
    SEXP Rf_mkTrue();

#ifdef __cplusplus
}
#endif

#endif /* LOGICALVECTOR_H */
