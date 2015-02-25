/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file GCStackRoot.hpp
 *
 * @brief Templated class CXXR::GCStackRoot and its untemplated utility class
 * CXXR::GCStackRootBase.
 */

#ifndef GCSTACKROOT_HPP
#define GCSTACKROOT_HPP 1

#include "CXXR/GCNode.hpp"

namespace CXXR {
    class RObject;

    /** @brief Untemplated utility class for GCStackRoot.
     *
     * This class provides static functions useful for working with the set
     * of active stack roots.
     */
    class GCStackRootBase {
    public:
	/** @brief Conduct a const visitor to protected objects.
	 *
	 * Conduct a GCNode::const_visitor object to each node pointed
	 * to by a pointer on the stack.  Note that because CXXR does
	 * conservative stack scanning, it is possible that this may also visit
	 * some unreferenced objects.
	 *
	 * @param v Pointer to the const_visitor object.
	 */
	static void visitRoots(GCNode::const_visitor* v);

	/** @brief Conduct a const visitor to protected objects in a specified
	 *    range.
	 *
	 * Conduct a GCNode::const_visitor object to each node pointed
	 * to by a pointer in the given range on the stack.
	 * Note that because CXXR does conservative stack scanning, it is
	 * possible that this may also visit some unreferenced objects.
	 *
	 * @param v Pointer to the const_visitor object.
	 * @param start Pointer to the location on the stack to start at.  If
	 *    set nullptr, then starts at the base of the stack.
	 * @param end Pointer to the location on the stack to end at.
	 *
	 * Note that as usual in C++, start and end form a semi-closed interval
	 * [start, end).  In particular, if there is a pointer at end, it
	 * will not be visited.
	 */
	static void visitRoots(GCNode::const_visitor* v,
			       const void* start,
			       const void* end);

	/** @brief Ensures that the reference counts of all roots on the stack
	 *   have been updated.
	 *
	 * Calls the specified function in a context where all the defered
	 * updates to the reference counts from stack roots have been done.
	 *
	 * @param function The function to call.
	 */
	static void withAllStackNodesProtected(std::function<void()> function);

	/** @brief Informs the memory manager that the object at this address
	 *    must not be deleted prior to this call.
	 */
	static void ensureReachable(void* p);

    private:
	friend class GCStackFrameBoundary;

	static void visitRootsImpl(char*, void*);
	static void withAllStackNodesProtectedImpl(char*, void*);
	static void* getStackBase();
    };

    /** @brief Smart pointer to protect a GCNode from garbage
     * collection.
     *
     * This class encapsulates a pointer to an object of a type
     * derived from GCNode.  For as long as the GCStackRoot object
     * exists, the GCNode that it points to will not be garbage
     * collected.
     *
     * GCStackRoot objects are required to be allocated on the
     * processor stack.
     *
     * Note that because CXXR does conservative stack scanning, use of
     * stack roots is not necessary in many places.  The exception to this
     * is objects that manage memory.  In that case, the GCStackRoot is
     * required to prevent deletion in situations where the object is no longer
     * referenced but there are still pointers to the managed memory.
     *
     * Note also that GCStackRoot is not usable by C code, which should
     * continue to use ::PROTECT(), ::UNPROTECT() etc. as in CR, which
     * are implemented in CXXR via class ProtectStack.
     *
     * @tparam T GCNode or a type publicly derived from GCNode.  This
     *           may be qualified by const, so for example a const
     *           String* may be encapsulated in a GCStackRoot using the
     *           type GCStackRoot<const String>.
     */
    template <class T = RObject>
    class GCStackRoot {
    public:
	typedef T type;

	/**
	 * @param node Pointer the node to be pointed to, and
	 *          protected from the garbage collector, or a null
	 *          pointer.
	 */
	explicit GCStackRoot(T* node = 0)
	    : m_target(node) {}

	~GCStackRoot()
	{
	    GCStackRootBase::ensureReachable((void*)m_target);
	}

	/**
	 * This will cause this GCStackRoot to protect the same GCNode as
	 * is protected by source.
	 */
        GCStackRoot& operator=(const GCStackRoot& source) = default;

	/**
	 * This will cause this GCStackRoot to point to and protect node,
	 * instead of the node (if any) it currently points to and
	 * protects.
	 *
	 * @param node Pointer to the GCNode that is now to be pointed
	 *          to and protected from the garbage collector.
	 */
	GCStackRoot& operator=(T* node)
	{
	    m_target = node;
	    return *this;
	}

	/** @brief Access member via encapsulated pointer.
	 *
	 * @return the pointer currently encapsulated by the node.
	 */
	T* operator->() const
	{
	    return get();
	}

	/** @brief Dereference the encapsulated pointer.
	 *
	 * @return a reference to the object pointed to by the
	 * encapsulated pointer.  The effect is undefined if this
	 * object encapsulates a null pointer.
	 */
	T& operator*() const
	{
	    return *get();
	}

	/** @brief Implicit conversion to encapsulated pointer type.
	 *
	 * @return the pointer currently encapsulated by the node.
	 * The pointer is of type \a T* const to prevent its use as
	 * an lvalue, the effect of which would probably not be what
	 * the programmer wanted.
	 */
	operator T*() const
	{
	    return get();
	}

	/** @brief Access the encapsulated pointer.
	 *
	 * @return the pointer currently encapsulated by the node.
	 */
	T* get() const
	{
	    return m_target;
	}

    private:
	T* m_target;
    };
}  // namespace CXXR

#ifdef HAVE_GC_HEADER
#include "gc.h"

inline void CXXR::GCStackRootBase::ensureReachable(void* p)
{
    // Force the compiler to keep m_target live as long as this object
    // exists.
    GC_reachable_here(p);
}
#endif

#endif  // GCSTACKROOT_HPP
