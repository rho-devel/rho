/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 * @brief Templated class rho::GCEdge.
 */

#ifndef GCEDGE_HPP
#define GCEDGE_HPP 1

#include "rho/GCNode.hpp"
#include "rho/ElementTraits.hpp"

namespace rho {
    class RObject;

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
    class GCEdge {
    public:
	typedef T type;

	GCEdge() : m_target(nullptr)
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
	    : m_target(source.m_target)
	{
	    static_assert(sizeof(T) >= 0, "T must be a complete type");
	    GCNode::incRefCount(m_target);
	}

	~GCEdge() {
	    check_complete_type();
	    GCNode::decRefCount(m_target);
	}

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
	    return m_target;
	}

	void detach() {
	    check_complete_type();
	    GCNode::decRefCount(m_target);
	    m_target = nullptr;
	}

    private:
	T* m_target;

	/** @brief Redirect the GCEdge to point at a (possibly) different node.
         *
         * @param newtarget Pointer to the object to which reference is now
         *           to be made.
         */
	void retarget(T* newtarget)
	{
	    check_complete_type();
	    GCNode::incRefCount(newtarget);
	    T* oldtarget = m_target;
	    m_target = newtarget;
	    GCNode::decRefCount(oldtarget);
	}
	// A GCEdge is a pointer, not an array.
	T& operator[](size_t) const = delete;

	static void check_complete_type() {
	    static_assert(sizeof(T) >= 0, "T must be a complete type");
	}
    };

    // Partial specializations of ElementTraits:
    namespace ElementTraits {
	template <class T>
	struct MustConstruct<GCEdge<T> > : boost::mpl::true_
	{};

	template <class T>
	struct MustDestruct<GCEdge<T> >  : boost::mpl::true_
	{};

	template<typename T>
	struct Duplicate<GCEdge<T>> {
		T* operator()(const GCEdge<T>& value) const {
		    return value.get() ? value.get()->clone() : nullptr;
	    }
	};

	template <class T>
	struct NAFunc<GCEdge<T> > {
	    const GCEdge<T>& operator()() const
	    {
		static GCEdge<T> na;
		return na;
	    }
	};

	template <class T>
	struct IsNA<GCEdge<T> > {
	    bool operator()(const GCEdge<T>& t) const
	    {
		return false;
	    }
	};

        template<typename T>
	struct IsGCEdge : public std::false_type {};

        template<typename T>
	struct IsGCEdge<GCEdge<T>> : public std::true_type {};
    
    }  // namespace ElementTraits
} // namespace rho

#endif  // GCEDGE_HPP
