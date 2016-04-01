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

/** @file LogicalVector.h
 * @brief Class rho::LogicalVector and associated C interface.
 */

#ifndef LOGICALVECTOR_H
#define LOGICALVECTOR_H

#include "R_ext/Arith.h"
#include "rho/VectorBase.hpp"
#include "rho/FixedVector.hpp"
#include "rho/Logical.hpp"
#include "rho/SEXP_downcast.hpp"

namespace rho {
    /** @brief Vector of truth values.
     */
    typedef rho::FixedVector<Logical, LGLSXP> LogicalVector;

    template<>
    struct VectorTypeFor<Logical> {
      typedef LogicalVector type;
    };
}  // namespace rho

extern "C" {
    /**
     * @param s Pointer to a rho::RObject.
     * @return TRUE iff the rho::RObject pointed to by \a s is a
     *         logical vector.
     */
    inline Rboolean Rf_isLogical(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == LGLSXP);
    }

/**
 * @param x Pointer to a rho::LogicalVector (checked).
 *
 * @return Pointer to element 0 of \a x .
 */
inline int* LOGICAL(SEXP x)
{
    using namespace rho;
    return reinterpret_cast<int*>
      (&(*SEXP_downcast<LogicalVector*>(x, false))[0]);
}

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
}

#endif /* LOGICALVECTOR_H */
