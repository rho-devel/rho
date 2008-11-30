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

/** @file ConsCell.h
 * @brief Class CXXR::ConsCell and associated C interface.
 *
 * This file includes C functions for examining and setting the TAG of
 * a CXXR::ConsCell; functions for examining and setting the CAR and
 * CDR are to be found in PairList.h.
 */

#ifndef CONSCELL_H
#define CONSCELL_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include <stdexcept>
#include "CXXR/GCRoot.h"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    class PairList;

    /** @brief Element of a singly linked list.
     *
     * Element of a LISP-like singly-linked list, containing pointers
     * to a 'car' object (this is LISP terminology, and has nothing to
     * do with automobiles) and to a 'tag' object, as well as a
     * pointer to the next element of the list, which must be of the
     * derived type PairList.  (Any of these pointers may be null.)
     *
     * When the object is copied, the copy thus created will contain
     * copies of the 'car' and of subsequent elements of the list.
     * However, the tag is not copied: the copy object will simply
     * contain a pointer to the tag of the original object.  Despite
     * this, the tag is considered to be part of the object.
     *
     * @note This class is used as a base class to implement CR's
     * LISTSXP, LANGSXP, DOTSXP and (for the time being) BCODESXP.
     * Because what these ::SEXPTYPEs have in common is implementation
     * rather than meaning in the application domain, canons of
     * object-oriented design would argue against their publicly
     * inheriting from a common base class.  Without doing this,
     * however, it would have been difficult efficiently to implement
     * functions such as CAR(), which are ubiquitous in the CR code.
     *
     * @todo Constrain the tag to be a String?
     */
    class ConsCell : public RObject {
    public:
	/**
	 * @return a const pointer to the 'car' of this ConsCell
	 * element.
	 */
	const RObject* car() const
	{
	    return m_car;
	}

	/**
	 * @return a pointer to the 'car' of this ConsCell. 
	 */
	RObject* car()
	{
	    return m_car;
	}

	/** @brief Convert a ConsCell to a (possibly) different
	 * ConsCell type.
	 *
	 * @param T A (non-abstract) class derived from ConsCell.
	 *
	 * @param cc Pointer to a ConsCell (possibly null).  The
	 *          effect of the method on \a cc is undefined;
	 *          consequently \a cc should not be used subsequently to
	 *          the method call.
	 *
	 * @return Pointer to the converted object, or a null pointer
	 * if \a cc is null.  If \a cc is already of the desired type,
	 * the method simply returns \a cc.
	 */
	template <class T> static T* convert(ConsCell* cc)
	{
	    if (!cc) return 0;
	    if (T* ccc = dynamic_cast<T*>(cc)) return ccc;
	    T* ans = new T(cc->car(), cc->tail(), cc->tag());
	    ans->setAttributes(cc->attributes());
	    ans->expose();
	    return ans;
	}
			   
	/** @brief Set the 'car' value.
	 *
	 * @param cr Pointer to the new car object (or a null
	 *           pointer).
	 */
	void setCar(RObject* cr)
	{
	    m_car = cr;
	    propagateAge(m_car);
	}

	/** @brief Set the 'tag' value.
	 *
	 * @param tg Pointer to the new tag object (or a null
	 *           pointer).
	 */
	void setTag(RObject* tg)
	{
	    m_tag = tg;
	    propagateAge(m_tag);
	}

	/** @brief Set the 'tail' value.
	 *
	 * @param tl Pointer to the new tail list (or a null
	 *           pointer).
	 */
	void setTail(PairList* tl);
	// Implemented inline in CXXR/PairList.h

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "(pairlist type)";
	}

	/**
	 * @return a pointer to the 'tag' of this ConsCell.
	 */
	RObject* tag() const
	{
	    return m_tag;
	}

	/**
	 * @return a const pointer to the 'tail' of this ConsCell
	 * element.
	 */
	const PairList* tail() const
	{
	    return m_tail;
	}

	/**
	 * @return a pointer to the 'tail' of this ConsCell.
	 */
	PairList* tail()
	{
	    return m_tail;
	}

	// Virtual function of GCNode:
	void visitChildren(const_visitor* v) const;
    protected:
	/**
	 * @param st The required ::SEXPTYPE of the ConsCell.  Must
	 *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP (not
	 *           checked).
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit ConsCell(SEXPTYPE st,
			  RObject* cr = 0, PairList* tl = 0, RObject* tg = 0)
	    : RObject(st), m_car(cr), m_tail(tl), m_tag(tg), m_missing(0)
	{
	    // checkST(st);
	}

	/** @brief Copy constructor.
	 *
	 * @param pattern ConsCell to be copied.  Beware that if this
	 *          ConsCell or any of its successors have unclonable
	 *          'car' objects, they will be shared between \a
	 *          pattern and the created object.  This is
	 *          necessarily prejudicial to the constness of the \a
	 *          pattern parameter.
	 */
	ConsCell(const ConsCell& pattern);

	/** @brief Tailless copy constructor.
	 *
	 * Copies the node without copying its tail.  Used in
	 * implementing the PairList copy constructor proper.
	 *
	 * @param pattern ConsCell to be copied.  Beware that if this
	 *          ConsCell or any of its successors have unclonable
	 *          'car' objects, they will be shared between \a
	 *          pattern and the created object.  This is
	 *          necessarily prejudicial to the constness of the \a
	 *          pattern parameter.
	 *
	 * @param dummy This parameter is used simply to provide the
	 *          constructor with a distinct signature.  Its value
	 *          is ignored.
	 */
	ConsCell(const ConsCell& pattern, int dummy);

	/**
	 * Declared protected to ensure that ConsCell objects are
	 * allocated only using 'new'.
	 */
	~ConsCell() {}

	/** @brief Set the 'tail' value during construction.
	 *
	 * This method should only be used during the construction of
	 * an object of a class derived from ConsCell, because it
	 * skips write-barrier checks.
	 *
	 * @param tl Pointer to the new tail list (or a null
	 *           pointer).
	 */
	void constructTail(PairList* tl)
	{
	    m_tail = tl;
	}
    private:
	friend class PairList;

	RObject* m_car;
	PairList* m_tail;
	RObject* m_tag;

	// Not implemented yet.  Declared to prevent
	// compiler-generated version:
	ConsCell& operator=(const ConsCell&);

	// Check that st is a legal SEXPTYPE for a ConsCell:
	static void checkST(SEXPTYPE st);
    public:
	// 'Scratchpad' field used in handling argument lists,
	// formerly hosted in the 'gp' field of sxpinfo_struct.  It
	// would be good to remove this from the class altogether.
	unsigned char m_missing;
    };

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void ccdump(std::ostream& os, const ConsCell& cc, size_t margin = 0);
} // namespace CXXR

extern "C" {

    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    inline int MISSING(SEXP x)
    {
	using namespace CXXR;
	return SEXP_downcast<ConsCell*>(x)->m_missing;
    }

    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    inline void SET_MISSING(SEXP x, int v)
    {
	using namespace CXXR;
	SEXP_downcast<ConsCell*>(x)->m_missing = v;
    }
#endif  /* __cplusplus */

    /** @brief Get tag of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
#ifndef __cplusplus
    SEXP TAG(SEXP e);
#else
    inline SEXP TAG(SEXP e)
    {
	if (!e) return 0;
	CXXR::ConsCell& cc = *CXXR::SEXP_downcast<CXXR::ConsCell*>(e);
	return cc.tag();
    }
#endif

    /**
     * @brief Set the tag of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     * @param y Pointer a CXXR::RObject representing the new tag of
     *          the CXXR::ConsCell.
     */
    void SET_TAG(SEXP x, SEXP y);

    /** @brief Create an object of a type derived from CXXR::ConsCell.
     *
     * The object is created with null car, tag and tail pointers.
     *
     * @param t The ::SEXPTYPE of the required object. Must be one of
     *          LISTSXP, LANGSXP, DOTSXP or BCODESXP (not checked).
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocSExp(SEXPTYPE t);

#ifdef __cplusplus
}
#endif

#endif /* CONSCELL_H */
