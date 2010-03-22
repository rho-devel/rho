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

#include "CXXR/Closure.h"
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

	/** @brief Constructor
	 *
	 * @param the_call Pointer to the call with which this Context
	 *          is associated.
	 *
	 * @param call_env Pointer to the Environment in which \a
	 *          the_call is to be evaluated.
	 *
	 * @param function Pointer, possibly null, to the function
	 *          (normally a Closure) being applied.
	 *
	 * @param working_env Pointer to the working environment of
	 *          the Closure, i.e. the environment in which
	 *          assignments create bindings, and in which default
	 *          values of parameters are evaluated.  If this
	 *          pointer is null, it signifies that this is a
	 *          BUILTIN Context, and in that case \a function and
	 *          \a promise_args should also be null.
	 *
	 * @param promise_args Pointer, possibly null, to the list of
	 *          arguments to the call, each wrapped in a Promise.
	 */
	Context(Expression* the_call, Environment* call_env,
		FunctionBase* function, Environment* working_env,
		PairList* promise_args);

	~Context();

	/** @brief The call of the Context.
	 *
	 * @return Pointer to the call with which the Context is
	 * associated.
	 */
	Expression* call() const
	{
	    return m_call;
	}

	/** @brief The call Environment.
	 *
	 * @return Pointer to the Environment in which the Context's
	 * call is to be evaluated.
	 */
	Environment* callEnvironment() const
	{
	    return m_call_env;
	}

	/** @brief Function being applied.
	 *
	 * @return Pointer, possibly null, to the function being
	 * applied in this Context.
	 */
	FunctionBase* function() const
	{
	    return m_function;
	}

	RObject* handlerStack() const
	{
	    return m_handlerstack;
	}

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

	/** @brief Next Context out.
	 *
	 * @return pointer to the Context object most narrowly
	 * enclosing this Context, or a null pointer if this is the
	 * outermost Context of the current Evaluator.
	 */
	Context* nextOut() const
	{
	    return m_next_out;
	}

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

	/** @brief Source location associated with this Context.
	 *
	 * @return Pointer, possibly null, to the source location
	 * associated with this Context.
	 */
	RObject* sourceLocation() const
	{
	    return m_srcref;
	}

	/** @brief Type of the Context.
	 *
	 * @return the Type of this Context.
	 */
	Type type() const
	{
	    return m_type;
	}

	/** @brief Working environment of the Context's Closure.
	 *
	 * @return Pointer to the working environment of this
	 * Context's Closure, i.e. the environment in which
	 * assignments create bindings, and in which default values of
	 * parameters are evaluated.  If this is a BUILTIN Context,
	 * this pointer is null.
	 */
	Environment* workingEnvironment() const
	{
	    return m_working_env;
	}
    private:
	ProtectStack::Scope m_protectstack_scope;
	RAllocStack::Scope m_rallocstack_scope;
	Context *m_next_out;
	unsigned int m_eval_depth;
	Type m_type;
	GCStackRoot<> m_srcref;
	GCStackRoot<Expression> m_call;
	GCStackRoot<Environment> m_call_env;
	Rboolean m_interrupts_suspended;
	GCStackRoot<> m_handlerstack;
	GCStackRoot<> m_restartstack;
#ifdef BYTECODE
	SEXP *m_nodestack;
#ifdef BC_INT_STACK
	IStackval *m_intstack;
#endif
#endif
	GCStackRoot<FunctionBase> m_function;
	GCStackRoot<Environment> m_working_env;
	GCStackRoot<PairList> m_promise_args;
	GCStackRoot<> m_onexit;
	bool m_generic;
    };
}  // namespace CXXR

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
