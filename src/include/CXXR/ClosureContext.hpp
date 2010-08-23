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

/** @file ClosureContext.hpp
 *
 * @brief Class CXXR::ClosureContext.
 */

#ifndef CLOSURECONTEXT_HPP
#define CLOSURECONTEXT_HPP 1

#include "CXXR/FunctionBase.h"
#include "CXXR/FunctionContext.hpp"
#include "CXXR/ProtectStack.h"
#include "CXXR/RAllocStack.h"

extern "C" {
    // Parked here pending the creation of an ErrorHandling class:
    extern CXXR::GCRoot<> R_HandlerStack;  // Condition handler stack
    extern CXXR::GCRoot<> R_RestartStack;  // Stack of available restarts
}

namespace CXXR {
    /** @brief Context recording the invocation of a Closure.
     */
    class ClosureContext : public FunctionContext {
    public:
	/** @brief Constructor
	 *
	 * @param the_call Pointer to the call with which this Context
	 *          is associated.
	 *
	 * @param call_env Pointer to the Environment in which \a
	 *          the_call is to be evaluated.
	 *
	 * @param function Pointer to the Closure being applied.
	 *
	 * @param working_env Pointer to the working environment of
	 *          the Closure, i.e. the environment in which
	 *          assignments create bindings, and in which default
	 *          values of parameters are evaluated.
	 *
	 * @param promise_args Pointer, possibly null, to the list of
	 *          arguments to the call, each wrapped in a Promise.
	 */
	ClosureContext(Expression* the_call, Environment* call_env,
		       FunctionBase* function, Environment* working_env,
		       PairList* promise_args);

	~ClosureContext();

	/** @brief (Not for general use.)
	 *
	 * This function will be removed in future refactorization.
	 *
	 * @return Pointer to the handler stack associated with this
	 * Context.
	 */
	RObject* handlerStack() const
	{
	    return m_handlerstack;
	}

	/** @brief The innermost ClosureContext.
	 *
	 * @return Pointer to the innermost ClosureContext belonging to the
	 * current Evaluator, or a null pointer if there is no such
	 * context.
	 */
	static ClosureContext* innermost()
	{
	    return innermost(Evaluator::Context::innermost());
	}

	/** @brief Search outwards for a ClosureContext.
	 *
	 * This function works outwards from the Evaluator::Context \a
	 * start until it finds a ClosureContext (possibly \a start
	 * itself), and returns a pointer to that ClosureContext.
	 *
	 * @param start The Evaluator::Context from which the search
	 * is to start.
	 *
	 * @return Pointer to the innermost ClosureContext found, or
	 * a null pointer if no such context was found.
	 */
	static ClosureContext* innermost(Evaluator::Context* start);

	/** @brief on.exit code.
	 *
	 * @return Pointer, possibly null, to an RObject to be
	 * evaluated on exit from the Context, whether by normal exit
	 * or by propagation of a C++ exception.
	 */
	RObject* onExit() const
	{
	    return m_onexit;
	}

	/** @brief Call arguments wrapped in Promises.
	 *
	 * @return pointer, possibly null, to the list of arguments to
	 * the call, each wrapped in a Promise.
	 */
	PairList* promiseArgs() const
	{
	    return m_promise_args;
	}

	/** @brief Designate an on.exit object.
	 *
	 * @param onexit Pointer, possibly null, to an RObject to be
	 *          evaluated on exit from the Context, whether by
	 *          normal exit or by propagation of a C++ exception.
	 */
	void setOnExit(RObject* onexit)
	{
	    m_onexit = onexit;
	}

	/** @brief Working environment of the Context's Closure.
	 *
	 * @return Pointer to the working environment of this
	 * Context's Closure, i.e. the environment in which
	 * assignments create bindings, and in which default values of
	 * parameters are evaluated.
	 */
	Environment* workingEnvironment() const
	{
	    return m_working_env;
	}
    private:
	ProtectStack::Scope m_protectstack_scope;
	RAllocStack::Scope m_rallocstack_scope;
	Rboolean m_interrupts_suspended;
	GCStackRoot<> m_handlerstack;
	GCStackRoot<> m_restartstack;
#ifdef BYTECODE
	SEXP *m_nodestack;
#ifdef BC_INT_STACK
	IStackval *m_intstack;
#endif
#endif
	GCStackRoot<Environment> m_working_env;
	GCStackRoot<PairList> m_promise_args;
	GCStackRoot<> m_onexit;
    };
}  // namespace CXXR

SEXP Rf_dynamicfindVar(SEXP, CXXR::ClosureContext*);
int Rf_framedepth(CXXR::ClosureContext*);
void R_InsertRestartHandlers(CXXR::ClosureContext*, Rboolean);
SEXP R_syscall(int, CXXR::ClosureContext*);
int R_sysparent(int, CXXR::ClosureContext*);
SEXP R_sysframe(int, CXXR::ClosureContext*);
SEXP R_sysfunction(int, CXXR::ClosureContext*);

#endif  // CLOSURECONTEXT_HPP
