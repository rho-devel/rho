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
 * @brief Templated class CXXR::GCStackRoot and its untemplated base class
 * CXXR::GCStackRootBase.
 *
 * See the paragraph 'Caller Protects' in the description of class
 * CXXR::GCStackRoot for recommended coding policy.
 */

#ifndef GCSTACKROOT_HPP
#define GCSTACKROOT_HPP 1

#include "CXXR/GCNode.hpp"

namespace CXXR {
    class RObject;

    /** @brief Untemplated base class for GCStackRoot.
     *
     * The preferred method for C++ code to protect a GCNode
     * from the garbage collector is to use the templated class
     * GCStackRoot, of which this is the untemplated base class, or
     * class GCRoot.
     *
     * However, GCStackRoot is not usable by C code, which should
     * continue to use ::PROTECT(), ::UNPROTECT() etc. as in CR, which
     * are implemented in CXXR via class ProtectStack.
     */
    class GCStackRootBase {
    public:
	/** @brief Conduct a const visitor to protected objects.
	 *
	 * Conduct a GCNode::const_visitor object to each node pointed
	 * to by a GCStackRootBase.
	 *
	 * @param v Pointer to the const_visitor object.
	 */
	static void visitRoots(GCNode::const_visitor* v);
    protected:
	/** @brief Primary constructor.
	 *
	 * @param node Pointer to be encapsulated by the GCStackRootBase.
	 */
	GCStackRootBase(const GCNode* node)
	    : m_next(s_roots), m_target(node), m_protecting(false)
	{
	    s_roots = this;
	    GCNode::maybeCheckExposed(node);
	}

	/** @brief Copy constructor.
	 *
	 * @param source Pattern for the copy.
	 */
	GCStackRootBase(const GCStackRootBase& source)
	    : m_next(s_roots), m_target(source.m_target), m_protecting(false)
	{
	    s_roots = this;
	}

	~GCStackRootBase()
	{
#ifndef NDEBUG
	    if (this != s_roots)
		seq_error();
#endif
	    if (m_protecting)
		destruct_aux();
	    s_roots = m_next;
	}

	GCStackRootBase& operator=(const GCStackRootBase& source)
	{
	    retarget(source.m_target);
	    return *this;
	}

	/** @brief Change the node protected by this GCStackRootBase.
	 *
	 * @param node Pointer to the node now to be protected, or a
	 * null pointer.
	 */
	void retarget(const GCNode* node)
	{
	    GCNode::maybeCheckExposed(node);
	    if (m_protecting)
		retarget_aux(node);
	    m_target = node;
	}

	/** @brief Access the encapsulated pointer.
	 *
	 * @return the GCNode pointer encapsulated by this object.
	 */
	const GCNode* ptr() const
	{
	    return m_target;
	}
    private:
	friend class GCNode;

	static GCStackRootBase* s_roots;

	GCStackRootBase* m_next;
	const GCNode* m_target;
	bool m_protecting;  // If this is set, it signifies that this
	  // GCStackRootBase object will have incremented the
	  // reference count of its target (if any).  In the interests
	  // of efficiency, initially m_protecting is false; it is set
	  // by call to protectAll().
	  //
	  // m_protecting && m_next implies m_next->m_protecting .

	// Helper function for destructor:
        void destruct_aux() HOT_FUNCTION;

	// Put all GCStackRootBase objects into the protecting state:
	static void protectAll() HOT_FUNCTION;

	// Helper function for retarget():
	void retarget_aux(const GCNode* node) HOT_FUNCTION;

	// Report out-of-sequence destructor call and abort program.
	// (We can't use an exception here because it's called from a
	// destructor.)
#ifdef __GNUC__
	__attribute__((cold))
#endif
	static void seq_error();
    };

    /** @brief Smart pointer to protect a GCNode from garbage
     * collection.
     *
     * This class encapsulates a pointer to an object of a type
     * derived from GCNode.  For as long as the GCStackRoot object
     * exists, the GCNode that it points to will not be garbage
     * collected.
     *
     * GCStackRoot objects are intended to be allocated on the
     * processor stack: specifically, the class implementation
     * requires that GCStackRoot objects are destroyed in the reverse
     * order of creation, and the destructor checks this.
     *
     * @tparam T GCNode or a type publicly derived from GCNode.  This
     *           may be qualified by const, so for example a const
     *           String* may be encapsulated in a GCStackRoot using the
     *           type GCStackRoot<const String>.
     *
     * \par Caller protects:
     * Suppose some code calls a function (or class method) that takes
     * a pointer or reference to a class derived from GCNode as an
     * argument, and/or returns a pointer to a class derived from
     * GCNode as its return value.  In CXXR, the preferred coding
     * approach is that the \e calling \e code should take
     * responsibility for protecting the arguments from the garbage
     * collector before calling the function, and likewise take
     * responsibility for protecting the returned value.  This is
     * because the calling code is in a better position to decide
     * whether any additional steps are necessary to achieve this, and
     * what they should be.  (The calling code may also need to protect
     * other objects: objects that are neither arguments to or values
     * returned from the called function, but which would otherwise be
     * vulnerable if the called function gave rise to a garbage
     * collection.)
     */
    template <class T = RObject>
    class GCStackRoot : public GCStackRootBase {
    public:
	typedef T type;

	/**
	 * @param node Pointer the node to be pointed to, and
	 *          protected from the garbage collector, or a null
	 *          pointer.
	 */
	explicit GCStackRoot(T* node = 0)
	    : GCStackRootBase(node) {}

	/** @brief Copy constructor.
	 *
	 * The constructed GCStackRoot will protect the same GCNode as
	 * source.  (There is probably no reason to use this
	 * constructor.)
	 */
	GCStackRoot(const GCStackRoot& source) : GCStackRootBase(source) {}

	/**
	 * This will cause this GCStackRoot to protect the same GCNode as
	 * is protected by source.  (There is probably no reason to
	 * use this method.)
	 */
	GCStackRoot& operator=(const GCStackRoot& source)
	{
	    GCStackRootBase::operator=(source);
	    return *this;
	}

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
	    GCStackRootBase::retarget(node);
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
	    return static_cast<T*>(const_cast<GCNode*>(ptr()));
	}
    };
}  // namespace CXXR

#endif  // GCSTACKROOT_HPP
