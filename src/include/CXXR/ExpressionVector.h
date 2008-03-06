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

/** @file ExpressionVector.h
 * @brief Class CXXR::ExpressionVector and associated C interface.
 *
 * (ExpressionVector implementins EXPRSXP.)
 */

#ifndef EXPRESSIONVECTOR_H
#define EXPRESSIONVECTOR_H

#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "CXXR/EdgeVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    class ListVector;

    // Template specialization:
    template <>
    inline const char* EdgeVector<RObject*, EXPRSXP>::staticTypeName()
    {
	return "expression";
    }

    /** @brief Vector of language objects, representing an expression.
     * @todo Replace the type parameter RObject* with something stricter.
     */
    class ExpressionVector : public EdgeVector<RObject*, EXPRSXP> {
    public:
	/** @brief Create an ExpressionVector.
	 *
	 * @param sz Number of elements required.  Zero is permissible.
	 */
	explicit ExpressionVector(size_t sz)
	    : EdgeVector<RObject*, EXPRSXP>(sz)
	{}

	/** @brief Create an ExpressionVector from a ListVector.
	 *
	 * @param lv The ListVector to be copied.  The
	 *           ExpressionVector created will comprise exactly
	 *           the same sequence of pointers to RObject as \a
	 *           lv.
	 *
	 * @note Q: Of all the possible coercions to ExpressionVector,
	 * why have a constructor to implement this one?  A: Because
	 * in all other cases, existing code in coerce.cpp needed at
	 * most trivial modification.
	 */
	explicit ExpressionVector(const ListVector& lv);
    private:
	// Declare private to ensure that ExpressionVector objects are
	// allocated only using 'new':
	~ExpressionVector() {}
    };
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /**
     * @param s Pointer to a CXXR::RObject.
     * @return TRUE iff the CXXR::RObject pointed to by \a s is an expression.
     */
#ifndef __cplusplus
    Rboolean Rf_isExpression(SEXP s);
#else
    inline Rboolean Rf_isExpression(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == EXPRSXP);
    }
#endif

/** @brief Set element of CXXR::ExpressionVector.
 * 
 * @param x Pointer to a CXXR::ExpressionVector .
 * @param i Index of the required element.  There is no bounds checking.
 * @param v Pointer to CXXR::RObject representing the new value.
 * @return The new value \a v.
 */
#ifndef __cplusplus
SEXP SET_XVECTOR_ELT(SEXP x, int i, SEXP v);
#else
inline SEXP SET_XVECTOR_ELT(SEXP x, int i, SEXP v)
{
    CXXR::ExpressionVector* ev
	= CXXR::SEXP_downcast<CXXR::ExpressionVector*>(x);
    (*ev)[i] = v;
    return v;
}
#endif

/**
 * @brief Examine element of a CXXR::ExpressionVector.
 * @param x Pointer to a CXXR::ExpressionVector .
 * @param i Index of the required element.  There is no bounds checking.
 * @return Pointer to extracted \a i 'th element.
 */
#ifndef __cplusplus
SEXP XVECTOR_ELT(SEXP x, int i);
#else
inline SEXP XVECTOR_ELT(SEXP x, int i)
{
    return (*CXXR::SEXP_downcast<CXXR::ExpressionVector*>(x))[i];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* EXPRESSIONVECTOR_H */
