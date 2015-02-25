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

/** @file Expression.h
 * @brief Class CXXR::Expression and associated C interface.
 */

#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "CXXR/PairList.h"

#ifdef __cplusplus

#include <boost/serialization/access.hpp>
#include <boost/serialization/nvp.hpp>

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
	 *
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 *
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit Expression(RObject* cr = nullptr, PairList* tl = nullptr,
			    const RObject* tg = nullptr)
	    : ConsCell(LANGSXP, cr, tl, tg)
	{}

	/** @brief Copy constructor.
	 *
	 * @param pattern Expression to be copied.
	 */
	Expression(const Expression& pattern)
	    : ConsCell(pattern)
	{}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "language";
	}

	// Virtual functions of RObject:
	Expression* clone() const override;
	RObject* evaluate(Environment* env) override;
	const char* typeName() const override;
    private:
	friend class boost::serialization::access;
	// Declared private to ensure that Expression objects are
	// allocated only using 'new':
	~Expression() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	Expression& operator=(const Expression&);

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(ConsCell);
	}
    };
} // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::Expression)

/** @brief Pointer to expression currently being evaluated.
 */
    extern CXXR::GCRoot<> R_CurrentExpr;

extern "C" {
#endif

    /** @brief Expression currently being evaluated.
     *
     * @return Pointer to the Expression currently being evaluated.
     */
    SEXP Rf_currentExpression();

    /** @brief Create a CXXR::Expression with a specified car and tail.
     *
     * This function protects its arguments from the garbage collector.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
    SEXP Rf_lcons(SEXP cr, SEXP tl);

    /** @brief Designate the Expression currently being evaluated.
     *
     * @param e Pointer to the Expression now to be evaluated.  (Not
     *          currently checked in any way.)
     */
    void Rf_setCurrentExpression(SEXP e);
#ifdef __cplusplus
}
#endif

#endif /* EXPRESSION_H */
