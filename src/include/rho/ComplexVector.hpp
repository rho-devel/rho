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

/** @file ComplexVector.h
 * @brief Class rho::ComplexVector and associated C interface.
 */

#ifndef COMPLEXVECTOR_H
#define COMPLEXVECTOR_H

#include "rho/VectorBase.hpp"
#include "R_ext/Complex.h"
#include "R_ext/Arith.h"
#include "rho/Complex.hpp"
#include "rho/FixedVector.hpp"
#include "rho/SEXP_downcast.hpp"

namespace rho {
    /** @brief Vector of complex numbers.
     */
    typedef rho::FixedVector<Complex, CPLXSXP> ComplexVector;
}  // namespace rho

extern "C" {
    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is a complex vector.
     */
    inline Rboolean Rf_isComplex(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == CPLXSXP);
    }

/**
 * @param x Pointer to a rho::ComplexVector (i.e. an R complex vector).
 *          An error is generated if \a x is not a non-null pointer to a
 *          rho::ComplexVector .
 *
 * @return Pointer to element 0 of \a x .
 */
inline Rcomplex *COMPLEX(SEXP x)
{
    using namespace rho;
    return &(*SEXP_downcast<ComplexVector*>(x, false))[0];
}

}

#endif /* COMPLEXVECTOR_H */
