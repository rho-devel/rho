/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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

/** @file NodeStack.hpp
 *
 * @brief Class CXXR::NodeStack.
 */

#ifndef NODESTACK_H
#define NODESTACK_H 1

#include <vector>
#include "CXXR/GCNode.hpp"
#include "CXXR/RObject.h"

namespace CXXR {
    class RObject;

    /** @brief Class implementing a stack of RObject*.
     *
     * This class is not intended for general use.  It is currently
     * used in class ProtectStack and in the bytecode interpreter.
     *
     * Note that it is necessary for GCNode::gclite() to call the
     * protectAll() method of every NodeStack in existence before it
     * starts to delete nodes with zero references counts.
     */
    class NodeStack {
    public:
	/** @brief Proxy object for an element of a NodeStack.
	 *
	 * Objects of this class are used to allow the elements of an
	 * NodeStack to be examined and modified using the same syntax
	 * as would be used for accessing an array of
	 * <tt>RObject*</tt>, whilst nevertheless enforcing the logic
	 * for protection against garbage collection.  See Item 30 of
	 * Scott Meyers's 'More Effective C++' for a general
	 * discussion of proxy objects, but see the <a
	 * href="http://www.aristeia.com/BookErrata/mec++-errata_frames.html">errata</a>.
	 * (It may look complicated, but an optimising compiler should
	 * be able to distil an invocation of NodeStack::operator[]
	 * into very few instructions.)
	 */
	class ElementProxy {
	public:
	    /** Copy the value of the proxied element from another
	     *  proxied element.
	     *
	     * @param rhs Proxied element whose value is to be copied.
	     *
	     * @return Reference to this ElementProxy.
	     */
	    ElementProxy& operator=(const ElementProxy& rhs)
	    {
		m_stack->retarget((*rhs.m_stack)[rhs.m_index], m_index);
		return *this;
	    }

	    /** @brief Redirect encapsulated pointer.
	     *
	     * @param node New pointer value.
	     *
	     * @return Reference to this ElementProxy.
	     */
	    ElementProxy& operator=(RObject* node)
	    {
		m_stack->retarget(node, m_index);
		return *this;
	    }

	    /**
	     * @return The pointer encapsulated by the proxied
	     *         element.
	     */
	    operator RObject* const() const
	    {
		return m_stack->m_vector[m_index];
	    }
	private:
	    friend class NodeStack;

	    NodeStack* m_stack;
	    unsigned int m_index;

	    ElementProxy(NodeStack* stack, unsigned int index)
		: m_stack(stack), m_index(index)
	    {}
	};

	/** @brief Object constraining lifetime of NodeStack entries.
	 *
	 * Scope objects must be declared on the processor stack
	 * (i.e. as C++ automatic variables).  Each Scope is
	 * associated with a particular NodeStack object.  Any entry
	 * pushed onto that NodeStack object during the lifetime of
	 * the Scope object will be automatically popped off when that
	 * lifetime comes to an end, i.e. when the Scope object itself
	 * goes out of scope.
	 */
	class Scope {
	public:
	    /** @brief Constructor
	     *
	     * @param stack Non-null pointer to the NodeStack object
	     *    with which this Scope is to be associated.
	     */
	    Scope(NodeStack* stack)
		: m_nodestack(stack),
#ifndef NDEBUG
		  m_next_scope(stack->m_innermost_scope),
#endif
		  m_saved_size(m_nodestack->size())
	    {
#ifndef NDEBUG
		stack->m_innermost_scope = this;
#endif
	    }

	    ~Scope()
	    {
#ifndef NDEBUG
		if (this != m_nodestack->m_innermost_scope)
		    nestingError();
#endif
		m_nodestack->resize(m_saved_size);
#ifndef NDEBUG
		m_nodestack->m_innermost_scope = m_next_scope;
#endif
	    }
	private:
	    friend class NodeStack;

	    NodeStack* m_nodestack;
#ifndef NDEBUG
	    Scope* m_next_scope;
#endif
	    size_t m_saved_size;

	    /** @brief NodeStack size at construction.
	     *
	     * @return The size of the NodeStack at the time this
	     * Scope object was constructed.  The NodeStack will be
	     * restored to this size by the Scope destructor.
	     */
	    size_t startSize() const
	    {
		return m_saved_size;
	    }

	    static void nestingError();
	};

	/** @brief Constructor.
	 *
	 * @param initial_capacity The initial capacity of the
	 *          NodeStack to be created.  The capacity will be
	 *          increased as necessary, so the value of this
	 *          parameter is not critical.
	 */
	NodeStack(size_t initial_capacity);

	~NodeStack()
	{
	    resize(0);
	}

	/** @brief Element access.
	 *
	 * @param index Index of required element (counting from
	 *          zero).  Bounds checking is applied unless the
	 *          class is compiled with NDEBUG.
	 *
	 * @return Proxy for the specified element, via which the
	 *         element can be examined or modified.
	 */
	ElementProxy operator[](unsigned int index)
	{
	    return ElementProxy(this, index);
	}

	/** @brief Read-only element access.
	 *
	 * @param index Index of required element (counting from
	 *          zero).  Bounds checking is applied unless the
	 *          class is compiled with NDEBUG.
	 *
	 * @return the specified element.
	 */
	RObject* const operator[](unsigned int index) const
	{
	    return m_vector[index];
	}

	/** @brief Remove topmost cell with given contents.
	 *
	 * Removes from the C pointer protection stack the uppermost
	 * stack cell containing a specified node address, and
	 * drops all the stack cells above it by one place.
	 *
	 * If this function is executed within a NodeStack::Scope
	 * pertaining to this NodeStack, its execution counts as a
	 * 'pop', and must balance a push() previously executed within
	 * the innermost such scope.  This is checked unless the class is
	 * compiled with NDEBUG.
	 *
	 * @param node Pointer value (possibly null) which must be
	 *          contained in at least one cell within the
	 *          NodeStack.  This cell will be removed from the
	 *          stack, and any cells above it dropped by one
	 *          place.
	 *
	 * @deprecated Used to implement the CR function
	 * UNPROTECT_PTR() (itself ghastly).  Its use for any other
	 * purpose is strongly deprecated.
	 */
	void eraseTopmost(RObject* node);

	/** @brief Element access with respect to stack top.
	 *
	 * @param count A value of 1 signifies the top value of the
	 *          stack, 2 the element immediately below that, and
	 *          so on.  Must be strictly positive and smaller than
	 *          the size of the stack (checked unless the class is
	 *          compiled with NDEBUG).
	 *
	 * @return the specified element.
	 */
	ElementProxy fromEnd(unsigned int count)
	{
	    return operator[](size() - count);
	}

	/** @brief Pop pointers from the NodeStack.
	 *
	 * If this function is executed within a NodeStack::Scope
	 * pertaining to this NodeStack, the number of elements popped
	 * must balance a corresponding number of push() operations
	 * previously executed within the innermost such scope.  This
	 * is checked unless the class is compiled with NDEBUG.
	 *
	 * @param count Number of cells to be popped.  Must not be
	 *          larger than the current size of the C pointer
	 *          protection stack (checked unless compiled with
	 *          NDEBUG).
	 */
#ifndef NDEBUG
	void pop(unsigned int count = 1);
#else
	void pop(unsigned int count = 1)
	{
	    resize(size() - count);
	}
#endif

	/** @brief Ensure GC protection of all nodes.
	 *
	 * This function ensures that all RObjects pointed to from the
	 * NodeStack are protected from garbage collection.
	 */
	void protectAll();

	/** @brief Push a node pointer onto the NodeStack.
	 *
	 * @param node Pointer, possibly null, to the node to be
	 *          pushed onto the NodeStack.
	 *
	 * @return Index of the stack cell thus created, counting from
	 * zero.
	 */
	unsigned int push(RObject* node)
	{
	    GCNode::maybeCheckExposed(node);
	    unsigned int index = m_vector.size();
	    m_vector.push_back(node);
	    return index;
	}

	/** @brief Change the target of a pointer on the PPS.
	 *
	 * Change the node that a particular cell in the C pointer
	 * protection stack protects.  As a consistency check, it is
	 * required that the retarget takes place within the same
	 * NodeStack::Scope as the corresponding protect.
	 *
	 * @param node Pointer to the node now to be protected from
	 *          the garbage collector by the designated stack
	 *          cell.  (Not necessarily a different node from the
	 *          one currently protected.)
	 *
	 * @param index Index (as returned by protect() ) of the stack
	 *          cell to be retargeted to node.  Must be less than
	 *          the current size of the C pointer protection
	 *          stack (checked).
	 */
#ifndef NDEBUG
	void retarget(RObject* node, unsigned int index);
#else
	void retarget(RObject* node, unsigned int index)
	{
	    GCNode::maybeCheckExposed(node);
	    if (index < m_protected_count)
		retarget_aux(m_vector[index], node);
	    m_vector[index] = node;
	}
#endif

	/** @brief Modify size of NodeStack.
	 *
	 * @param new_size The required size.  If larger than the
	 *          current size, the added cells will contain null
	 *          pointers.  If smaller than the current size, then
	 *          pointers are popped off the NodeStack to bring its
	 *          size down to \a new_size.
	 */
	void resize(size_t new_size)
	{
	    if (new_size >= m_protected_count)
		m_vector.resize(new_size, 0);
	    else resize_aux(new_size);
	}

	/** @brief Current size of NodeStack.
	 *
	 * @return the number of pointers currently on the NodeStack.
	 */
	size_t size() const
	{
	    return m_vector.size();
	}

	/** @brief pop and return the top element of the stack.
	 *
	 * The stack must not be empty; this is checked unless the
	 * class is compiled with NDEBUG.
	 *
	 * @return the pointer previously at the top of the stack.
	 */
	RObject* topnpop() {
	    RObject* ans = operator[](size() - 1);
	    pop();
	    return ans;
	}

	/** @brief Conduct a const visitor via the NodeStack.
	 *
	 * Conduct a GCNode::const_visitor object to each RObject
	 * pointed to by the NodeStack.
	 *
	 * @param v Pointer to the const_visitor object.
	 */
	void visitRoots(GCNode::const_visitor* v);
    private:
	std::vector<RObject*> m_vector;
	size_t m_protected_count;  // The nodes (if any) pointed to
	  // (*m_vector)[0] through (*m_vector)[m_protected_count - 1]
	  // will have had their reference counts increased by this
	  // class.  Stack entries beyond this (if any) will not yet
	  // have had this protection applied.
 
#ifndef NDEBUG
	Scope* m_innermost_scope;
#endif

	// Helper function for retarget(), handling the case where
	// 'index' is within the protected range:
#ifdef __GNUC__
	__attribute__((hot,fastcall))
#endif
	static void retarget_aux(RObject* oldnode, RObject* newnode);

	// Helper function for trim(), handling the case where the trim
	// cuts down into protected nodes:
#ifdef __GNUC__
	__attribute__((hot,fastcall))
#endif
	void resize_aux(size_t new_size);
    };
}  // namespace CXXR

#endif  // NODESTACK_H
