/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file GCEdge.hpp
 *
 * @brief Templated class CXXR::GCEdge.
 */

#ifndef GCEDGE_HPP
#define GCEDGE_HPP 1

#include "CXXR/GCNode.hpp"

namespace CXXR {
    class RObject;

    /** @brief Untemplated base class for GCEdge.
     */
    class GCEdgeBase {
    public:
	/** @brief Redirect the GCEdgeBase to point at a different node.
	 *
	 * @param from This \e must point to the GCNode object that
	 *          contains this GCEdge object.
	 *
	 * @param to Pointer to the object to which reference is now
	 *           to be made.
	 */
	void retarget(GCNode* from, const GCNode* to);
    private:
	static void abortIfNotExposed(const GCNode* target);
    };

    /** @brief Directed edge in the graph whose nodes are GCNode objects.
     *
     * This class encapsulates a pointer from one GCNode to another,
     * and carries out housekeeping required by the garbage collection
     * scheme.  The class name reflects the fact that these objects
     * represent directed edges in the directed graph with the GCNode
     * objects as its nodes.
     *
     * Whenever an object of a type derived from GCNode needs to refer
     * to another such object, it should do so by containing a GCEdge
     * object, rather than by containing a pointer or reference
     * directly.
     *
     * @param T GCNode or a type publicly derived from GCNode.  This
     *          may be qualified by const, so for example a const
     *          String* may be encapsulated in a GCEdge using the type
     *          GCEdge<const String>.
     */
    template <class T = RObject>
    class GCEdge : public GCEdgeBase {
    public:
	GCEdge()
	    : m_target(0)
	{}

	/** @brief Primary constructor.
	 *
	 * @param target Pointer to the object to which this GCEdge is
	 *          to refer.  A null pointer is permissible.
	 *
	 * @note Unless \a target is a null pointer, this constructor
	 * should be called only as part of the construction of the
	 * object derived from GCNode of which this GCEdge forms a
	 * part.  In other cases, construct the GCEdge with a null
	 * target, and then retarget it.
	 */
	explicit GCEdge(T* target)
	    : m_target(target)
	{
	    // This rather inelegant conditional compilation is
	    // necessary to allow the primary constructor of ConsCell
	    // to be inlined when CHECK_EXPOSURE is not defined.  Were
	    // the call to maybeCheckExposed() included in that case,
	    // the instantiation would fail for ConsCell::m_tail
	    // because the compiler wouldn't know that PairList
	    // inherits from RObject.
#ifdef CHECK_EXPOSURE
	    GCNode::maybeCheckExposed(m_target);
#endif
	}

	/** @brief Copy constructor.
	 *
	 * @param source GCEdge to be copied.  The constructed GCEdge
	 * will point to the same object (if any) as \a source. 
	 *
	 * @note This constructor should be called only as part of the
	 * construction of the object derived from GCNode of which
	 * this GCEdge forms a part.
	 */
	GCEdge(const GCEdge<T>& source)
	    : m_target(source.m_target)
	{}

	T* operator->() const
	{
	    return m_target;
	}

	/** @brief Extract encapsulated pointer
	 *
	 * @return The encapsulated pointer.
	 */
	operator T*() const
	{
	    return m_target;
	}

	/** @brief Redirect the GCEdge to point at a (possibly) different node.
	 *
	 * @param from This \e must point to the GCNode object that
	 *          contains this GCEdge object.
	 *
	 * @param to Pointer to the object to which reference is now
	 *           to be made.
	 */
	void retarget(GCNode* from, T* to)
	{
	    m_target = to;
	    GCEdgeBase::retarget(from, to);
	}
    private:
	T* m_target;

	// Not implemented.  Declared to prevent compiler-generated
	// version:
	GCEdge& operator=(const GCEdge&);
    };
}

#endif  // GCEDGE_HPP
