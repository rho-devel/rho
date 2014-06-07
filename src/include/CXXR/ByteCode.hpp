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
#include "CXXR/IntVector.h"
#include "CXXR/ListVector.h"
#include "CXXR/NodeStack.hpp"

extern "C" {
#ifdef BYTECODE

// In CXXR for the time being:
#define NO_THREADED_CODE

#if defined(__GNUC__) && ! defined(BC_PROFILING) && (! defined(NO_THREADED_CODE))
# define THREADED_CODE
#endif

#endif

#define TOKEN_THREADING
}

namespace CXXR {
    /** @brief ByteCode interpreter.
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
	 * This function should be called before any other use is made
	 * of the ByteCode class.
	 */
	static void initialize();

	/** @brief Ensure GC protection of all nodes.
	 *
	 * This function ensures that all RObjects pointed to from the
	 * NodeStack are protected from garbage collection.
	 */
	static void protectAll();

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "bytecode";
	}

	/** @brief Conduct a const visitor via the NodeStack.
	 *
	 * Conduct a GCNode::const_visitor object to each RObject
	 * pointed to by the NodeStack.
	 *
	 * @param v Pointer to the const_visitor object.
	 */
	static void visitRoots(GCNode::const_visitor* v);

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
	// Object whose constructor saves, and destructor restores,
	// the states of s_nodestack and s_loopvar_stack:
	class Scope {
	public:
	    Scope()
		: m_nodestack_scope(ByteCode::s_nodestack),
		  m_loopvar_stack_size(ByteCode::s_loopvar_stack->size())
	    {}

	    ~Scope()
	    {
		ByteCode::s_loopvar_stack->resize(m_loopvar_stack_size);
	    }
	private:
	    NodeStack::Scope m_nodestack_scope;
	    size_t m_loopvar_stack_size;
	};

	static NodeStack* s_nodestack;

	// Stack of pointers to the bindings of loop variables, which
	// CXXR manipulates alongside s_nodestack.  (In CR, these
	// bindings are put directly on s_nodestack, with type coercion.)
	static std::vector<Frame::Binding*>* s_loopvar_stack;
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

	// Helper functions from CR which need to be inside the
	// ByteCode class in CXXR:
	static void DO_MATSUBSET(SEXP rho);
	static void DO_SETVECSUBSET(SEXP rho);
	static void DO_SETMATSUBSET(SEXP rho);
    };
} // namespace CXXR

// Bytecode related stuff from Defn.h.  Try to get rid of these in due
// course:

typedef SEXP R_bcstack_t;

#endif /* BYTECODE_HPP */
