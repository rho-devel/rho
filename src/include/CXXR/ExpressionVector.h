/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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
 * (CXXR::ExpressionVector implements EXPRSXP.)
 *
 * @todo Constrain the elements to be Expression objects?  However, as
 * currently used, the elements of an ExpressionVector may be Symbols
 * rather than Expressions.
 */

#ifndef EXPRESSIONVECTOR_H
#define EXPRESSIONVECTOR_H

#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "CXXR/FixedVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* FixedVector<RHandle<>, EXPRSXP>::staticTypeName()
    {
	return "expression";
    }

    /** @brief Expression vector.
     *
     * The vector contains smart pointers of type
     * RObject::Handle<RObject>, where the intention is that these
     * pointers should point to language objects.
     *
     * @todo Replace the encapsulated pointer type RObject* with something
     * stricter (but is needs to embrace Symbol as well as Expression).
     */
    typedef FixedVector<RHandle<>, EXPRSXP> ExpressionVector;
}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::ExpressionVector)

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
 *
 * @param i Index of the required element.  There is no bounds checking.
 *
 * @param v Pointer, possibly null, to CXXR::RObject representing the
 *          new value.
 *
 * @return The new value \a v.
 */
SEXP SET_XVECTOR_ELT(SEXP x, int i, SEXP v);

/**
 * @brief Examine element of a CXXR::ExpressionVector.
 *
 * @param x Non-null pointer to a CXXR::ExpressionVector .
 *
 * @param i Index of the required element.  There is no bounds checking.
 *
 * @return Pointer to extracted \a i 'th element.
 */
#ifndef __cplusplus
SEXP XVECTOR_ELT(SEXP x, int i);
#else
inline SEXP XVECTOR_ELT(SEXP x, int i)
{
    using namespace CXXR;
    ExpressionVector* ev = SEXP_downcast<ExpressionVector*>(x, false);
    return (*ev)[VectorBase::size_type(i)];
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* EXPRESSIONVECTOR_H */
