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

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include <stdexcept>
#include "CXXR/GCRoot.h"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    /** @brief Singly linked list of pairs.
     *
     * LISP-like singly-linked list, each element containing pointers to a
     * 'car' object (this is LISP terminology, and has nothing to do
     * with automobiles) and to a 'tag' object, as well as a pointer to
     * the next element of the list.  (Any of these pointers may be
     * null.)  A PairList object can be thought of either as
     * representing a single element (link) of such a list, or as
     * representing an entire list: that element and all its
     * successors.
     *
     * @note This class implements CR's LISTSXP, LANGSXP, DOTSXP and
     * (for the time being) BCODESXP.  Arguably these ought to be
     * completely distinct classes, but in that case it would have
     * been difficult efficiently to implement functions such as
     * CAR(), which are ubiquitous in the CR code.
     */
    class PairList : public RObject {
    public:
	/**
	 * @param st The required ::SEXPTYPE of the PairList.  Must
	 *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP (not
	 *           checked).
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit PairList(SEXPTYPE st,
			  RObject* cr = 0, PairList* tl = 0, RObject* tg = 0)
	    : RObject(st), m_car(cr), m_tail(tl), m_tag(tg)
	{
	    // checkST(st);
	}

	/** @brief Create a list of a specified length.
	 *
	 * This constructor creates a PairList with a specified number
	 * of elements.  On creation, each element has null 'car' and
	 * 'tag'.
	 *
	 * @param st The required ::SEXPTYPE of the PairList.  Must
	 *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP
	 *           (not checked).
	 * @param sz Number of elements required in the list.  Must be
	 *           strictly positive; the constructor throws
	 *           std::out_of_range if sz is zero.
	 */
	PairList(SEXPTYPE st, size_t sz)
	    throw (std::bad_alloc, std::out_of_range);

	/**
	 * @return a const pointer to the 'car' of this PairList
	 * element.
	 */
	const RObject* car() const
	{
	    return m_car;
	}

	/**
	 * @return a pointer to the 'car' of this PairList element. 
	 */
	RObject* car()
	{
	    return m_car;
	}

	/** @brief Set the 'car' value.
	 *
	 * @param cr Pointer to the new car object (or a null
	 *           pointer).
	 */
	void setCar(RObject* cr)
	{
	    m_car = cr;
	    devolveAge(m_car);
	}

	/** @brief Set the 'tag' value.
	 *
	 * @param tg Pointer to the new tag object (or a null
	 *           pointer).
	 */
	void setTag(RObject* tg)
	{
	    m_tag = tg;
	    devolveAge(m_tag);
	}

	/** @brief Set the 'tail' value.
	 *
	 * @param tl Pointer to the new tail list (or a null
	 *           pointer).
	 */
	void setTail(PairList* tl)
	{
	    m_tail = tl;
	    devolveAge(m_tail);
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "(pairlist type)";
	}

	/**
	 * @return a const pointer to the 'tag' of this PairList
	 * element.
	 */
	const RObject* tag() const
	{
	    return m_tag;
	}

	/**
	 * @return a pointer to the 'tag' of this PairList element.
	 */
	RObject* tag()
	{
	    return m_tag;
	}

	/**
	 * @return a const pointer to the 'tail' of this PairList
	 * element.
	 */
	const PairList* tail() const
	{
	    return m_tail;
	}

	/**
	 * @return a pointer to the 'tail' of this PairList element.
	 */
	PairList* tail()
	{
	    return m_tail;
	}

	// Virtual function of RObject:
	const char* typeName() const;

	// Virtual functions of GCNode:
	void visitChildren(const_visitor* v) const;
	void visitChildren(visitor* v);
    private:
	RObject* m_car;
	PairList* m_tail;
	RObject* m_tag;

	// Declared private to ensure that PairList objects are
	// allocated only using 'new':
	~PairList() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	PairList(const PairList&);
	PairList& operator=(const PairList&);

	// Check that st is a legal SEXPTYPE for a PairList:
	static void checkST(SEXPTYPE st);
    };

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void pldump(std::ostream& os, const PairList& pl, size_t margin = 0);
} // namespace CXXR

extern "C" {
#endif

    /* Accessor functions. */

    /** @brief Get car of CXXR::PairList element.
     *
     * @param e Pointer to a CXXR::PairList (checked), or a null pointer.
     * @return Pointer to the value of the list car, or 0 if \a e is
     * a null pointer.
     */
#ifndef __cplusplus
    SEXP CAR(SEXP e);
#else
    inline SEXP CAR(SEXP e)
    {
	if (!e) return 0;
	CXXR::PairList& pl = *CXXR::SEXP_downcast<CXXR::PairList*>(e);
	return pl.car();
    }
#endif

    /** @brief Get tail of CXXR::PairList element.
     *
     * @param e Pointer to a CXXR::PairList (checked), or a null pointer.
     * @return Pointer to the tail of the list, or 0 if \a e is
     * a null pointer.
     */
#ifndef __cplusplus
    SEXP CDR(SEXP e);
#else
    inline SEXP CDR(SEXP e)
    {
	if (!e) return 0;
	CXXR::PairList& pl = *CXXR::SEXP_downcast<CXXR::PairList*>(e);
	return pl.tail();
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

    /** @brief Get tag of CXXR::PairList element.
     *
     * @param e Pointer to a CXXR::PairList (checked), or a null pointer.
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
#ifndef __cplusplus
    SEXP TAG(SEXP e);
#else
    inline SEXP TAG(SEXP e)
    {
	if (!e) return 0;
	CXXR::PairList& pl = *CXXR::SEXP_downcast<CXXR::PairList*>(e);
	return pl.tag();
    }
#endif

    /**
     * @brief Set the tag of a CXXR::PairList element.
     *
     * @param x Pointer to a CXXR::PairList (checked).
     * @param y Pointer a CXXR::RObject representing the new tag of
     *          the CXXR::PairList element.
     */
    void SET_TAG(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of a CXXR::PairList element.
     * @param x Pointer to a CXXR::PairList (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          list car.
     *
     * @returns \a y.
     */
    SEXP SETCAR(SEXP x, SEXP y);

    /**
     * @brief Replace the tail of a CXXR::PairList element.
     * @param x Pointer to a CXXR::PairList (checked).
     * @param y Pointer a CXXR::RObject representing the new tail of the list.
     *
     * @returns \a y.
     */
    SEXP SETCDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the second element of list.
     * @param x Pointer to a CXXR::PairList element with at least one successor
     *          (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          second element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the third element of list.
     * @param x Pointer to a CXXR::PairList element with at least two
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          third element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fourth element of list.
     * @param x Pointer to a CXXR::PairList element with at least three
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          fourth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fifth element of list.
     * @param x Pointer to a CXXR::PairList element with at least four
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          fifth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCAD4R(SEXP x, SEXP y);

    /** @brief Create a LISTSXP CXXR::PairList of a specified length.
     *
     * This constructor creates a CXXR::PairList of ::SEXPTYPE LISTSXP
     * with a specified number of elements.  On creation, each element
     * has null 'car' and 'tag'.
     *
     * @param n Number of elements required in the list.
     *
     * @return The constructed list, or a null pointer if \a n is zero.
     */
#ifndef __cplusplus
    SEXP Rf_allocList(unsigned int n);
#else
    inline SEXP Rf_allocList(unsigned int n)
    {
	return n > 0 ? new CXXR::PairList(LISTSXP, n) : 0;
    }
#endif

    /** @brief Create a single CXXR::PairList element.
     *
     * Create a single CXXR::PairList element, with null car and tag
     * pointers.
     *
     * @param t The ::SEXPTYPE of the required object. Must be one of
     *          LISTSXP, LANGSXP, DOTSXP or BCODESXP (not checked).
     *
     * @return Pointer to the created object.
     */
#ifndef __cplusplus
    SEXP Rf_allocSExp(SEXPTYPE t);
#else
    inline SEXP Rf_allocSExp(SEXPTYPE t)
    {
	return new CXXR::PairList(t);
    }
#endif

    /** @brief Create a CXXR::PairList of ::SEXPTYPE LISTSXP.
     *
     * Creates a CXXR::PairList of ::SEXPTYPE LISTSXP with a specified
     * car and tail.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
#ifndef __cplusplus
    SEXP Rf_cons(SEXP cr, SEXP tl);
#else
    inline SEXP Rf_cons(SEXP cr, SEXP tl)
    {
	CXXR::GCRoot<> crr(cr);
	CXXR::GCRoot<CXXR::PairList*>
	    tlr(CXXR::SEXP_downcast<CXXR::PairList*>(tl));
	return new CXXR::PairList(LISTSXP, crr, tlr);
    }
#endif

    /** @brief Create a CXXR::PairList of ::SEXPTYPE LANGSXP.
     *
     * Creates a CXXR::PairList of ::SEXPTYPE LANGSXP with a specified
     * car and tail.
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
	CXXR::GCRoot<CXXR::PairList*>
	    tlr(CXXR::SEXP_downcast<CXXR::PairList*>(tl));
	return new CXXR::PairList(LANGSXP, crr, tlr);
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RPAIRLIST_H */
