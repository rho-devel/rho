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

/** @file PairList.h
 * @brief Class CXXR::PairList and associated C interface.
 */

#ifndef RPAIRLIST_H
#define RPAIRLIST_H

#include "CXXR/ConsCell.h"

#ifdef __cplusplus

#include "CXXR/GCRoot.h"

namespace CXXR {
    /** @brief Singly linked list of pairs.
     *
     * LISP-like singly-linked list, each element containing pointers to a
     * 'car' object (this is LISP terminology, and has nothing to do
     * with automobiles) and to a 'tag' object, as well as a pointer to
     * the next element of the list.  (Any of these pointers may be
     * null.)  A PairList object is considered to 'own' its car, its
     * tag, and all its successors.
     */
    class PairList : public ConsCell {
    public:
	/**
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit PairList(RObject* cr = 0, PairList* tl = 0, RObject* tg = 0)
	    : ConsCell(LISTSXP, cr, tl, tg)
	{}

	/** @brief Create a PairList element on the free store.
	 *
	 * Unlike the constructor (and contrary to CXXR conventions
	 * generally) this function protects its arguments from the
	 * garbage collector.
	 *
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 *
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 *
	 * @return Pointer to newly created PairList element.
	 */
	static PairList* cons(RObject* cr, PairList* tl=0)
	{
	    s_cons_car = cr;
	    s_cons_cdr = tl;
	    PairList* ans = new PairList(cr, tl);
	    s_cons_cdr = 0;
	    s_cons_car = 0;
	    ans->expose();
	    return ans;
	}

	/** @brief Create a PairList of a specified length.
	 *
	 * This constructor creates a chain of PairList nodes with a
	 * specified number of elements.  On creation, each element
	 * has null 'car' and 'tag'.
	 *
	 * @param sz Number of elements required in the list.  If
	 *           zero, the function returns a null pointer.
	 */
	static PairList* makeList(size_t sz) throw (std::bad_alloc);

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "pairlist";
	}

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	// Permanent GCRoots used to implement cons() without pushing
	// and popping:
	static GCRoot<> s_cons_car;
	static GCRoot<PairList> s_cons_cdr;

	// Declared private to ensure that PairList objects are
	// allocated only using 'new':
	~PairList() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	PairList(const PairList&);
	PairList& operator=(const PairList&);
    };

    inline void ConsCell::setTail(PairList* tl)
    {
	m_tail = tl;
	devolveAge(m_tail);
    }
} // namespace CXXR

extern "C" {
#endif

    /** @brief Get car of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     * @return Pointer to the value of the list car, or 0 if \a e is
     * a null pointer.
     */
#ifndef __cplusplus
    SEXP CAR(SEXP e);
#else
    inline SEXP CAR(SEXP e)
    {
	if (!e) return 0;
	CXXR::ConsCell& cc = *CXXR::SEXP_downcast<CXXR::ConsCell*>(e);
	return cc.car();
    }
#endif

    /**
     * @brief Equivalent to CAR(CAR(e)).
     */
#ifndef __cplusplus
    SEXP CAAR(SEXP e);
#else
    inline SEXP CAAR(SEXP e) {return CAR(CAR(e));}
#endif

    /** @brief Get tail of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     * @return Pointer to the tail of the list, or 0 if \a e is
     * a null pointer.
     */
#ifndef __cplusplus
    SEXP CDR(SEXP e);
#else
    inline SEXP CDR(SEXP e)
    {
	if (!e) return 0;
	CXXR::ConsCell& cc = *CXXR::SEXP_downcast<CXXR::ConsCell*>(e);
	return cc.tail();
    }
#endif

    /**
     * @brief Equivalent to CDR(CAR(e)).
     */
#ifndef __cplusplus
    SEXP CDAR(SEXP e);
#else
    inline SEXP CDAR(SEXP e) {return CDR(CAR(e));}
#endif

    /**
     * @brief Equivalent to CAR(CDR(e)).
     */
#ifndef __cplusplus
    SEXP CADR(SEXP e);
#else
    inline SEXP CADR(SEXP e) {return CAR(CDR(e));}
#endif

    /**
     * @brief Equivalent to CDR(CDR(e)).
     */
#ifndef __cplusplus
    SEXP CDDR(SEXP e);
#else
    inline SEXP CDDR(SEXP e) {return CDR(CDR(e));}
#endif

    /**
     * @brief Equivalent to CAR(CDR(CDR(e))).
     */
#ifndef __cplusplus
    SEXP CADDR(SEXP e);
#else
    inline SEXP CADDR(SEXP e) {return CAR(CDR(CDR(e)));}
#endif

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
#ifndef __cplusplus
    SEXP CADDDR(SEXP e);
#else
    inline SEXP CADDDR(SEXP e) {return CAR(CDR(CDR(CDR(e))));}
#endif

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(CDR(e))))).
     */
#ifndef __cplusplus
    SEXP CAD4R(SEXP e);
#else
    inline SEXP CAD4R(SEXP e) {return CAR(CDR(CDR(CDR(CDR(e)))));}
#endif

    /**
     * @brief Set the 'car' value of a CXXR::ConsCell.
     * @param x Pointer to a CXXR::ConsCell (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          list car.
     *
     * @returns \a y.
     */
    SEXP SETCAR(SEXP x, SEXP y);

    /**
     * @brief Replace the tail of a CXXR::ConsCell.
     * @param x Pointer to a CXXR::ConsCell (checked).
     * @param y Pointer a CXXR::RObject representing the new tail of the list.
     *
     * @returns \a y.
     */
    SEXP SETCDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the second element of list.
     * @param x Pointer to a CXXR::ConsCell with at least one successor
     *          (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          second element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the third element of list.
     * @param x Pointer to a CXXR::ConsCell with at least two
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          third element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fourth element of list.
     * @param x Pointer to a CXXR::ConsCell with at least three
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          fourth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fifth element of list.
     * @param x Pointer to a CXXR::ConsCell with at least four
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          fifth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCAD4R(SEXP x, SEXP y);

    /** @brief Create a CXXR::PairList of a specified length.
     *
     * This constructor creates a CXXR::PairList with a specified
     * number of elements.  On creation, each element has null 'car'
     * and 'tag'.
     *
     * @param n Number of elements required in the list.
     *
     * @return The constructed list, or a null pointer if \a n is zero.
     */
    SEXP Rf_allocList(unsigned int n);

    /** @brief Creates a CXXR::PairList with a specified car and tail.
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
    SEXP Rf_cons(SEXP cr, SEXP tl);

#ifdef __cplusplus
}
#endif

#endif /* RPAIRLIST_H */
