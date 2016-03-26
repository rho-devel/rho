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

/** @file PairList.h
 * @brief C interface of class PairList.
 *
 * To facilitate inlining of various ConsCell member functions, the
 * definition of class rho::PairList itself is in ConsCell.h.
 *
 * This file includes C functions for examining and setting the CDR of
 * a rho::ConsCell, and other operations accessing the tail of the
 * list; functions for examining and setting the CAR and TAG of a
 * rho:ConsCell are to be found in ConsCell.h.
 */

#ifndef RPAIRLIST_H
#define RPAIRLIST_H

#include "rho/ConsCell.hpp"

extern "C" {
    // Used in matching formal and actual arguments (within match.cpp
    // and unique.cpp).
    inline unsigned char ARGUSED(SEXP x)
    {
	using namespace rho;
	return SEXP_downcast<PairList*>(x)->m_argused;
    }

    // Used in matching formal and actual arguments (within match.cpp
    // and unique.cpp).
    inline void SET_ARGUSED(SEXP x, unsigned char v)
    {
	using namespace rho;
	// The RHS is a kludge to avoid a -Wconversion warning:
	SEXP_downcast<PairList*>(x)->m_argused
	  = static_cast<unsigned int>(v & 3);
    }

    /** @brief Is a Binding locked?
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     *
     * @return true iff this Binding is locked.
     */
    inline Rboolean BINDING_IS_LOCKED(SEXP b)
    {
	using namespace rho;
	const PairList* pl = SEXP_downcast<PairList*>(b);
	return Rboolean(pl->m_binding_locked);
    }

    /** @brief Get tail of rho::ConsCell.
     *
     * @param e Pointer to a rho::ConsCell (checked), or a null pointer.
     * @return Pointer to the tail of the list, or 0 if \a e is
     * a null pointer.
     */
    inline SEXP CDR(SEXP e)
    {
	using namespace rho;
	return tail0(SEXP_downcast<ConsCell*>(e));
    }

    /**
     * @brief Equivalent to CDR(CAR(e)).
     */
    inline SEXP CDAR(SEXP e) {return CDR(CAR(e));}

    /**
     * @brief Equivalent to CAR(CDR(e)).
     */
    inline SEXP CADR(SEXP e)
    {
	using namespace rho;
	return car0(tail0(SEXP_downcast<ConsCell*>(e)));
    }

    /**
     * @brief Equivalent to CDR(CDR(e)).
     */
    inline SEXP CDDR(SEXP e)
    {
	using namespace rho;
	return tail0(tail0(SEXP_downcast<ConsCell*>(e)));
    }

    /**
     * @brief Equivalent to CAR(CDR(CDR(e))).
     */
    inline SEXP CADDR(SEXP e)
    {
	using namespace rho;
	return car0(tail0(tail0(SEXP_downcast<ConsCell*>(e))));
    }

    /**
     * @brief Equivalent to CDR(CDR(CDR(e))).
     */
    inline SEXP CDDDR(SEXP e)
    {
	using namespace rho;
	return tail0(tail0(tail0(SEXP_downcast<ConsCell*>(e))));
    }

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
    inline SEXP CADDDR(SEXP e)
    {
	using namespace rho;
	return car0(tail0(tail0(tail0(SEXP_downcast<ConsCell*>(e)))));
    }

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(CDR(e))))).
     */
    inline SEXP CAD4R(SEXP e)
    {
	using namespace rho;
	return car0(tail0(tail0(tail0(tail0(SEXP_downcast<ConsCell*>(e))))));
    }

    /** @brief Is a Binding active?
     *
     * @param b Pointer to a ConsCell object (checked). If \a b points
     *          to any type of ConsCell other than a PairList, the
     *          function returns FALSE.  Otherwise \a b should point
     *          to a PairList object representing a Frame::Binding
     *          (e.g. because it was produced using
     *          Frame::asPairList() ).
     *
     * @return true iff this is an active Binding.
     */
    Rboolean IS_ACTIVE_BINDING(SEXP b);

    /** @brief Lock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     */
    inline void LOCK_BINDING(SEXP b)
    {
	using namespace rho;
	PairList* pl = SEXP_downcast<PairList*>(b);
	pl->m_binding_locked = true;}

    /** @brief Designate as active the binding represented by a
     * PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     */
    inline void SET_ACTIVE_BINDING_BIT(SEXP b)
    {
	using namespace rho;
	PairList* pl = SEXP_downcast<PairList*>(b);
	pl->m_active_binding = true;
    }

    /**
     * @brief Replace the tail of a rho::ConsCell.
     * @param x Pointer to a rho::ConsCell (checked).
     * @param y Pointer a rho::RObject representing the new tail of the list.
     *
     * @returns \a y.
     */
    SEXP SETCDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the second element of list.
     * @param x Pointer to a rho::ConsCell with at least one successor
     *          (checked).
     * @param y Pointer a rho::RObject representing the new value of the
     *          second element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the third element of list.
     * @param x Pointer to a rho::ConsCell with at least two
     *          successors (checked).
     * @param y Pointer a rho::RObject representing the new value of the
     *          third element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fourth element of list.
     * @param x Pointer to a rho::ConsCell with at least three
     *          successors (checked).
     * @param y Pointer a rho::RObject representing the new value of the
     *          fourth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fifth element of list.
     * @param x Pointer to a rho::ConsCell with at least four
     *          successors (checked).
     * @param y Pointer a rho::RObject representing the new value of the
     *          fifth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCAD4R(SEXP x, SEXP y);

    /** @brief Unlock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     */
    inline void UNLOCK_BINDING(SEXP b)
    {
	using namespace rho;
	PairList* pl = SEXP_downcast<PairList*>(b);
	pl->m_binding_locked = false;
    }

    /** @brief Create a rho::PairList of a specified length.
     *
     * This constructor creates a rho::PairList with a specified
     * number of elements.  On creation, each element has null 'car'
     * and 'tag'.
     *
     * @param n Number of elements required in the list.
     *
     * @return The constructed list, or a null pointer if \a n is zero.
     */
    SEXP Rf_allocList(unsigned int n);

    /** @brief Creates a rho::PairList with a specified car and tail.
     *
     * This function protects its arguments from the garbage collector.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a rho::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
    SEXP Rf_cons(SEXP cr, SEXP tl);
}

#endif /* RPAIRLIST_H */
