/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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
	/** @brief Null the encapsulated pointer.
	 */
	void detach();
    protected:
	GCEdgeBase()
	    : m_target(nullptr)
	{}

	/** @brief Copy constructor.
	 *
	 * @param source Pattern for the copy.
	 */
	GCEdgeBase(const GCEdgeBase& source)
	    : m_target(source.m_target)
	{
	    GCNode::incRefCount(m_target);
	}
	    
	~GCEdgeBase()
	{
	    GCNode::decRefCount(m_target);
	}

	/** @brief Get target of this edge.
	 *
	 * @return Pointer to the target (if any) of this GCEdgeBase.
	 */
	const GCNode* target() const
	{
	    return m_target;
	}
    protected:
	/** @brief Redirect the GCEdge to point at a (possibly) different node.
         *
         * @param newtarget Pointer to the object to which reference is now
         *           to be made.
         */
	void retarget(const GCNode* newtarget)
	{
	    GCNode::incRefCount(newtarget);
	    GCNode::decRefCount(m_target);
	    m_target = newtarget;
	}
    private:
	const GCNode* m_target;
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
     * @tparam T GCNode or a type publicly derived from GCNode.  This
     *           may be qualified by const, so for example a const
     *           String* may be encapsulated in a GCEdge using the type
     *           GCEdge<const String>.
     */
    template <class T = RObject>
    class GCEdge : public GCEdgeBase {
    public:
	typedef T type;

	GCEdge()
	{}

	// explicit GCEdge(T* target) is intentionally not defined here.
	//
	// This prevents object initializers of the form
	//     Foo::Foo() : m_edge(expression_that_might_gc()) { ... }
	// In that case, the expression is executed while the edge is
	// uninitialized.  If it causes a garbage collection, the GC's mark
	// routine will attempt to follow the uninitialized edge, causing
	// errors.
	// Object initializers should be written as:
	//     Foo::Foo() { m_edge = expression_that_might_gc(); ... }
	// which properly initialized the edge prior to doing the allocation.

	/** @brief Copy constructor.
	 *
	 * @param source GCEdge to be copied.  The constructed GCEdge
	 * will point to the same object (if any) as \a source. 
	 */
	GCEdge(const GCEdge<T>& source)
	    : GCEdgeBase(source)
	{}

	GCEdge<T>& operator=(const GCEdge<T>& source)
	{
	    retarget(source);
	    return *this;
	}

	GCEdge<T>& operator=(T* newtarget)
	{
	    retarget(newtarget);
	    return *this;
	}

	T* operator->() const
	{
	    return get();
	}

	/** @brief Extract encapsulated pointer
	 *
	 * @return The encapsulated pointer.
	 */
	operator T*() const
	{
	    return get();
	}

	/** @brief Access the target pointer.
	 *
	 * @return pointer to the current target (if any) of the edge.
	 */
	T* get() const
	{
	    return static_cast<T*>(const_cast<GCNode*>(target()));
	}
    };
} // namespace CXXR

#endif  // GCEDGE_HPP
