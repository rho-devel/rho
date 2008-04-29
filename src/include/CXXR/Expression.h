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

/** @file Expression.h
 * @brief Class CXXR::Expression and associated C interface.
 */

#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "CXXR/PairList.h"

#ifdef __cplusplus

namespace CXXR {
    /** @brief Singly linked list representing an R expression.
     *
     * R expression, represented as a LISP-like singly-linked list,
     * each element containing pointers to a 'car' object and to a
     * 'tag' object, as well as a pointer to the next element of the
     * list.  (Any of these pointers may be null.)  A Expression
     * object is considered to 'own' its car, its tag, and all its
     * successors.
     */
    class Expression : public ConsCell {
    public:
	/**
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit Expression(RObject* cr = 0, PairList* tl = 0, RObject* tg = 0)
	    : ConsCell(LANGSXP, cr, tl, tg)
	{}

	/** @brief Create an expression of a specified length.
	 *
	 * This constructor creates an Expression with a specified number
	 * of elements.  On creation, each element has null 'car' and
	 * 'tag'.
	 *
	 * @param sz Number of elements required in the list.  Must be
	 *           strictly positive; the constructor throws
	 *           std::out_of_range if \a sz is zero.
	 */
	explicit Expression(size_t sz)
	    throw (std::bad_alloc, std::out_of_range)
	    : ConsCell(LANGSXP, sz)
	{}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "language";
	}

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	// Declared private to ensure that Expression objects are
	// allocated only using 'new':
	~Expression() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	Expression(const Expression&);
	Expression& operator=(const Expression&);
    };
} // namespace CXXR

extern "C" {
#endif

    // Pointer to expression currently being evaluated:
    extern SEXP R_CurrentExpr;

    /** @brief Create a CXXR::Expression with a specified car and tail.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
#ifndef __cplusplus
    SEXP Rf_lcons(SEXP cr, SEXP tl);
#else
    inline SEXP Rf_lcons(SEXP cr, SEXP tl)
    {
	CXXR::GCRoot<> crr(cr);
	CXXR::GCRoot<CXXR::PairList>
	    tlr(CXXR::SEXP_downcast<CXXR::PairList*>(tl));
	return new CXXR::Expression(crr, tlr);
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* EXPRESSION_H */
