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

/** @file ListVector.h
 * @brief Class rho::ListVector and associated C interface.
 *
 * (ListVector implements VECSXP.)
 */

#ifndef LISTVECTOR_H
#define LISTVECTOR_H

#include "rho/VectorBase.hpp"
#include "rho/FixedVector.hpp"
#include "rho/SEXP_downcast.hpp"

namespace rho {
    /** @brief General vector of RHandle<RObject>.
     */
    typedef FixedVector<RHandle<>, VECSXP> ListVector;
}  // namespace rho

extern "C" {

/** @brief Set element of rho::ListVector.
 *
 * @param x Pointer to a rho::ListVector.
 *
 * @param i Index of the required element.  There is no bounds checking.
 *
 * @param v Pointer, possibly null, to rho::RObject representing the
 *          new value.
 *
 * @return The new value \a v.
 */
SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);

extern SEXP XVECTOR_ELT(SEXP x, R_xlen_t i);

/** @brief Examine element of rho::ListVector.
 *
 * @param x Non-null pointer to a rho::ListVector .
 *
 * @param i Index of the required element.  There is no bounds checking.
 *
 * @return The value of the \a i 'th element.
 */
inline SEXP VECTOR_ELT(SEXP x, R_xlen_t i)
{
    using namespace rho;
    if (x && x->sexptype() == VECSXP) {
	ListVector* lv = SEXP_downcast<ListVector*>(x, false);
	return (*lv)[VectorBase::size_type(i)];
    } else if (x && x->sexptype() == EXPRSXP) {
      return XVECTOR_ELT(x, i);
    }  else {
	Rf_error("%s() can only be applied to a '%s', not a '%s'",
		 "VECTOR_ELT", "list", Rf_type2char(TYPEOF(x)));
    }
}

}

#endif /* LISTVECTOR_H */
