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
#define R_BCNODESTACKSIZE 10000
    extern SEXP* R_BCNodeStackBase;
    extern SEXP* R_BCNodeStackTop;
    extern SEXP* R_BCNodeStackEnd;
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
    class ByteCode : public ConsCell {
    public:
	/**
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit ByteCode(RObject* cr = 0, PairList* tl = 0, RObject* tg = 0)
	    : ConsCell(BCODESXP, cr, tl, tg)
	{}

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
    private:
	// Class to save and restore the state of the ByteCode
	// computation stacks.
	class Scope {
	public:
	    Scope()
	    {
		m_nodestack = R_BCNodeStackTop;
#ifdef BC_INT_STACK
		m_intstack = R_BCIntStackTop;
#endif
	    }

	    ~Scope()
	    {
		R_BCNodeStackTop = m_nodestack;
#ifdef BC_INT_STACK
		R_BCIntStackTop = m_intstack;
#endif
	    }
	private:
	    SEXP *m_nodestack;
#ifdef BC_INT_STACK
	    IStackval *m_intstack;
#endif
	};

	// Declared private to ensure that ByteCode objects are
	// allocated only using 'new':
	~ByteCode() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	ByteCode(const ByteCode&);
	ByteCode& operator=(const ByteCode&);
    };
} // namespace CXXR

#endif /* BYTECODE_HPP */
