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
 * @brief Class ExpressionVector and associated C interface.
 * Class ExpressionVector (implementing EXPRSXP) and associated C interface.
 */

#ifndef EXPRESSIONVECTOR_H
#define EXPRESSIONVECTOR_H

#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "CXXR/EdgeVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* EdgeVector<RObject*, EXPRSXP>::staticTypeName()
    {
	return "expression";
    }

    /** @brief Vector of language objects, representing an expression.
     * @todo Replace the type parameter RObject* with something stricter.
     */
    typedef CXXR::EdgeVector<RObject*, EXPRSXP> ExpressionVector;
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is an expression.
     */
#ifndef __cplusplus
    Rboolean Rf_isExpression(SEXP s);
#else
    inline Rboolean Rf_isExpression(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == EXPRSXP);
    }
#endif

/** @brief Set element of ExpressionVector.
 * 
 * @param x Pointer to an \c ExpressionVector .
 * @param i Index of the required element.  There is no bounds checking.
 * @param v Pointer to \c RObject representing the new value.
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
 * @brief Examine element of an ExpressionVector.
 * @param x Pointer to an \c ExpressionVector .
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
