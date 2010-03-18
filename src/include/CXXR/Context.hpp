/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
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

/** @file Context.hpp
 *
 * @brief Class CXXR::Context.
 */

#ifndef CONTEXT_HPP
#define CONTEXT_HPP 1

#include "CXXR/Evaluator.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/Promise.h"
#include "CXXR/ProtectStack.h"
#include "CXXR/RAllocStack.h"

extern "C" {
    // Parked here pending the creation of an ErrorHandling class:
    extern CXXR::GCRoot<> R_HandlerStack;  // Condition handler stack
    extern CXXR::GCRoot<> R_RestartStack;  // Stack of available restarts

    // Parked here temporarily:
    extern CXXR::RObject* R_Srcref;
}

namespace CXXR {
    class Context {
    public:
	/* The Various Context Types.
	 *
	* In general the type is a bitwise OR of the values below.
	* Only functions should have the third bit turned on;
	* this allows us to move up the context stack easily
	* with either RETURN's or GENERIC's.
	* If you add a new context type for functions make sure
	* NEWTYPE & FUNCTION > 0
	*/
	enum Type {
	    FUNCTION = 4,
	    RETURN   = 12,
	    BUILTIN  = 64  // used in profiling
	};

	Context();

	~Context();

	/** @brief Is this a generic function invocation?
	 *
	 * @return true iff this has been flagged as a generic
	 * function invocation.
	 */
	bool isGeneric() const
	{
	    return m_generic;
	}

	/** @brief The innermost Context.
	 *
	 * @return Pointer to the innermost Context belonging to the
	 * current Evaluator.
	 */
	static Context* innermost()
	{
	    return Evaluator::current()->innermostContext();
	}

	/** @brief Set status as generic function invocation.
	 *
	 * @param on true if this Context is to be designated as a
	 *           generic function invocation; false if this
	 *           designation is to be removed.  The generic status
	 *           is false by default.
	 */
	void setGeneric(bool on)
	{
	    m_generic = on;
	}

	Context *nextcontext;        // The next context up the chain
	Type callflag;		     // The context "type"
	int evaldepth;	             // evaluation depth at inception
	GCStackRoot<> promargs;      // Promises supplied to closure
	GCStackRoot<> callfun;       // The closure called
	GCStackRoot<> sysparent;     // environment the closure was called from
	GCStackRoot<> call;          // The call that effected this context
	GCStackRoot<Environment> cloenv;  // The environment
	GCStackRoot<> conexit;	     // Interpreted "on.exit" code
	Rboolean intsusp;            // interrupts are suspended
	GCStackRoot<> handlerstack;  // condition handler stack
	GCStackRoot<> restartstack;  // stack of available restarts
	GCStackRoot<> srcref;	     // The source line in effect
#ifdef BYTECODE
	SEXP *nodestack;
#ifdef BC_INT_STACK
	IStackval *intstack;
#endif
#endif
    private:
	ProtectStack::Scope m_protectstack_scope;
	RAllocStack::Scope m_rallocstack_scope;
	bool m_generic;
    };
}  // namespace CXXR

void Rf_begincontext(CXXR::Context*, CXXR::Context::Type, SEXP, SEXP, SEXP,
		     SEXP, SEXP);
SEXP Rf_dynamicfindVar(SEXP, CXXR::Context*);
int Rf_framedepth(CXXR::Context*);
void R_InsertRestartHandlers(CXXR::Context*, Rboolean);
SEXP R_syscall(int, CXXR::Context*);
int R_sysparent(int, CXXR::Context*);
SEXP R_sysframe(int, CXXR::Context*);
SEXP R_sysfunction(int, CXXR::Context*);

extern "C" {
    void Rf_jump_to_toplevel(void);
}

#endif  // CONTEXT_HPP
