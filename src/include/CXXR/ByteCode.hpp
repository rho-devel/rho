/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-11 Andrew R. Runnalls, subject to such other
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

/** @file ByteCode.hpp
 * @brief Class CXXR::ByteCode.
 */

#ifndef BYTECODE_HPP
#define BYTECODE_HPP

#include "CXXR/ConsCell.h"
// Just to pick up define of BYTECODE:
#include "CXXR/Evaluator.h"

extern "C" {
#ifdef BYTECODE

// In CXXR for the time being:
//#define NO_THREADED_CODE

#if defined(__GNUC__) && ! defined(BC_PROFILING) && (! defined(NO_THREADED_CODE))
# define THREADED_CODE
#endif

#endif

#define TOKEN_THREADING
}

namespace CXXR {
    /** @brief Stub for future ByteCode class. 
     */
    class ByteCode : public RObject {
    public:
	/**
	 * @param code Non-null pointer to the 'bytecode' (actually a
	 *          set of integers).
	 *
	 * @param constants Non-null pointer to the associated
	 *          constants (FIXME: improve this documentation.)
	 */
	explicit ByteCode(IntVector* code, ListVector* constants)
	    : RObject(BCODESXP), m_code(code), m_constants(constants)
	{
#ifdef THREADED_CODE
#ifndef TOKEN_THREADING
	    thread();
#endif
#endif
	}

	// Interim accessor functions.  Try to get rid of these:

	/** @brief Not for general use.
	 */
	IntVector* code()
	{
	    return m_code;
	}

	/** @brief Not for general use.
	 */
	ListVector* constants()
	{
	    return m_constants;
	}

	/** @brief Initialize the class.
	 *
	 * This function should be called before any other use it made
	 * of the ByteCode class.
	 */
	static void initialize();

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "bytecode";
	}

	// Virtual functions of RObject:
	RObject* evaluate(Environment* env);
	const char* typeName() const;

	// Virtual functions of GCNode:
	void detachReferents();
	void visitReferents(const_visitor* v) const;

	// Make this private in due course:
#ifdef THREADED_CODE
#ifndef TOKEN_THREADING
#define ENCODED_BCODE
#endif
#endif

#ifdef ENCODED_BCODE
	typedef union { void *v; int i; } BCODE;
#else
	typedef int BCODE;
#endif
    private:
	/** @brief Virtual machine node stack.
	 *
	 * Stack of pointers to RObjects manipulated by the
	 * ByteCode virtual machine.
	 */
	class NodeStack : public GCNode {
	public:
	    NodeStack();

	    /** @brief Pointer to 'one beyond the end' of the NodeStack.
	     *
	     * @return A pointer to the GCEdge<> one beyond the end of
	     * the node stack.  This performs a function similar to
	     * R_BCNodeStackTop in CR.
	     */
	    GCEdge<>* end()
	    {
		return m_end;
	    }

	    /** @brief pop elements from the top of the stack.
	     *
	     * @param count The number of elements to be popped, which
	     *          must be no greater than the current size of
	     *          the stack (not checked).
	     *
	     * @return the pointer previously at the top of the stack.
	     */
	    void pop(size_t count = 1)
	    {
#ifndef NDEBUG
		if (size() == 0 && count > 0)
		    badpop();
#endif
		while (count--)
		    (--m_end)->~GCEdge<>();
	    }

	    /** @brief Push a pointer onto the stack.
	     *
	     * @param node Pointer (possibly null) to an RObject.
	     */
	    void push(RObject* node)
	    {
		if (reinterpret_cast<void**>(m_end) == &*m_edgevec.end())
		    enlarge();
		new (m_end++) GCEdge<>(node);
	    }

	    size_t size() const
	    {
		return (reinterpret_cast<void**>(m_end) - &*m_edgevec.begin());
	    }

	    /** @brief pop and return the top element of the stack.
	     *
	     * The stack must not be empty; this is not checked.
	     *
	     * @return the pointer previously at the top of the stack.
	     */
	    RObject* topnpop() {
#ifndef NDEBUG
		if (size() == 0)
		    badpop();
#endif
		--m_end;
		RObject* ans = *m_end;
		m_end->~GCEdge<>();
		return ans;
	    }

	    // Virtual functions of GCNode:
	    void detachReferents();
	    void visitReferents(const_visitor* v) const;
	private:
	    // This code assumes that sizeof(GCEdge<>) ==
	    // sizeof(void*) and that their alignments are the same.
	    // m_edgevec is conceptually a stack of GCEdge<>s, which
	    // are constructed and destructed by stack operations.
	    typedef std::vector<void*> EdgeVec;
	    EdgeVec m_edgevec;
	    GCEdge<>* m_end;

#ifndef NDEBUG
	    void badpop();
#endif
	    // Double the size of m_edgevec.  The implementation
	    // assumes that this is called only when m_edgevec is full
	    // to capacity.
	    void enlarge();
	};

	// Class to save and restore the state of the ByteCode
	// computation stacks.
	class Scope {
	public:
	    Scope()
	    {
		m_nodestack_size = s_nodestack->size();
	    }

	    ~Scope()
	    {
		s_nodestack->pop(s_nodestack->size() - m_nodestack_size);
	    }
	private:
	    size_t m_nodestack_size;
	};

	static GCRoot<NodeStack> s_nodestack;
#ifdef THREADED_CODE
	static void* s_op_address[];
#ifndef TOKEN_THREADING
	static int s_op_arity[];
#endif
#endif

	GCEdge<IntVector> m_code;
	GCEdge<ListVector> m_constants;
#ifdef THREADED_CODE
	std::vector<BCODE> m_threaded_code;
#endif

	// Declared private to ensure that ByteCode objects are
	// allocated only using 'new':
	~ByteCode() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	ByteCode(const ByteCode&);
	ByteCode& operator=(const ByteCode&);

	// Normally this implements evaluate() by evaluating bcode in
	// the environment env.  However, if called with a null
	// pointer for bcode, it initialises the opcode despatch
	// table(s).
	static RObject* interpret(ByteCode* bcode, Environment* env);

#ifdef ENCODED_BCODE
	// Initialize the m_threaded_code field by creating a threaded
	// form of the code.
	void thread();
#endif
    };
} // namespace CXXR

#endif /* BYTECODE_HPP */
