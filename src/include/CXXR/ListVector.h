/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

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

/** @file ListVector.h
 * @brief Class CXXR::ListVector and associated C interface.
 *
 * (ListVector implements VECSXP.)
 */

#ifndef LISTVECTOR_H
#define LISTVECTOR_H

#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "CXXR/EdgeVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* EdgeVector<RObject*, VECSXP>::staticTypeName()
    {
	return "list";
    }

    /** @brief Vector of GCEdge<RObject*>.
     */
    class ListVector : public EdgeVector<RObject*, VECSXP> {
    public:
	/** @brief Create a ListVector.
         *
         * Each element will initially encapsulate a null pointer.
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 */
	ListVector(size_t sz)
	    : EdgeVector<RObject*, VECSXP>(sz)
	{}

	/** @brief Construct from ExpressionVector.
	 *
	 * @param ev The ExpressionVector on which the constructed
	 *          ListVector is to be modelled.
	 */
	ListVector(const EdgeVector<RObject*, EXPRSXP>& ev);
    private:
	// Declared private to ensure that ListVectors are
	// allocated only using 'new'.
	~ListVector() {}
    };
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

/** @brief Set element of ListVector.
 * @param x Pointer to a \c ListVector .
 * @param i Index of the required element.  There is no bounds checking.
 * @param v Pointer to \c RObject representing the new value.
 * @return The new value \a v.
 */
#ifndef __cplusplus
SEXP SET_VECTOR_ELT(SEXP x, int i, SEXP v);
#else
inline SEXP SET_VECTOR_ELT(SEXP x, int i, SEXP v)
{
    CXXR::ListVector* lv = CXXR::SEXP_downcast<CXXR::ListVector*>(x);
    (*lv)[i] = v;
    return v;
}
#endif

/** @brief Examine element of ListVector.
 * @param x Pointer to a \c ListVector .
 * @param i Index of the required element.  There is no bounds checking.
 * @return The value of the \a i 'th element.
 */
#ifndef __cplusplus
SEXP VECTOR_ELT(SEXP x, int i);
#else
inline SEXP VECTOR_ELT(SEXP x, int i)
{
    return (*CXXR::SEXP_downcast<CXXR::ListVector*>(x))[i];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* LISTVECTOR_H */
