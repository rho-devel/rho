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
#ifdef BC_INT_STACK
#define R_BCINTSTACKSIZE 10000
    typedef union { void *p; int i; } IStackval;
    extern IStackval* R_BCIntStackBase;
    extern IStackval* R_BCIntStackTop;
    extern IStackval* R_BCIntStackEnd;
#endif
#endif
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
	{}

	// Interim accessor functions.  Try to get rid of these:

	IntVector* code()
	{
	    return m_code;
	}

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
    private:
	/** @brief Virtual machine node stack.
	 *
	 * Stack of pointers to RObjects manipulated by the
	 * ByteCode virtual machine.
	 */
	struct NodeStack : public GCNode, public std::vector<GCEdge<> > {
	    NodeStack();

	    /** @brief pop elements from the top of the stack.
	     *
	     * @param count The number of elements to be popped, which
	     *          must be no greater than the current size of
	     *          the stack (not checked).
	     *
	     * @return the pointer previously at the top of the stack.
	     */
	    void pop(size_t count = 1) {
		resize(size() - count);
	    }

	    /** @brief Push a pointer onto the stack.
	     *
	     * @param node Pointer (possibly null) to an RObject.
	     */
	    void push(RObject* node)
	    {
		resize(size() + 1);
		back() = node;
	    }

	    /** @brief pop and return the top element of the stack.
	     *
	     * The stack must not be empty; this is not checked.
	     *
	     * @return the pointer previously at the top of the stack.
	     */
	    RObject* topnpop() {
		RObject* ans = back();
		resize(size() - 1);
		return ans;
	    }

	    // Virtual functions of GCNode:
	    void detachReferents();
	    void visitReferents(const_visitor* v) const;
	};

	// Class to save and restore the state of the ByteCode
	// computation stacks.
	class Scope {
	public:
	    Scope()
	    {
		m_nodestack_size = s_nodestack->size();
#ifdef BC_INT_STACK
		m_intstack = R_BCIntStackTop;
#endif
	    }

	    ~Scope()
	    {
		s_nodestack->resize(m_nodestack_size);
#ifdef BC_INT_STACK
		R_BCIntStackTop = m_intstack;
#endif
	    }
	private:
	    size_t m_nodestack_size;
#ifdef BC_INT_STACK
	    IStackval *m_intstack;
#endif
	};

	static GCRoot<NodeStack> s_nodestack;

	GCEdge<IntVector> m_code;
	GCEdge<ListVector> m_constants;

	// Declared private to ensure that ByteCode objects are
	// allocated only using 'new':
	~ByteCode() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	ByteCode(const ByteCode&);
	ByteCode& operator=(const ByteCode&);

	static RObject* interpret(ByteCode* bcode, Environment* env);
    };
} // namespace CXXR

#endif /* BYTECODE_HPP */
