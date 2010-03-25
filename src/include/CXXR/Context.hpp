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
 * @brief Class CXXR::Evaluator::Context.
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
    /** @brief Housekeeping information for FunctionBase::apply().
     *
     * Evaluator::Context objects must be declared on the processor
     * stack (i.e. as C++ automatic variables).
     * 
     * This class performs two functions:
     * <ol>
     *
     * <li>The primary function is to maintain an 'Ariadne's thread',
     * recording information about the stack of R function calls
     * currently active.  This is to support error reporting,
     * traceback, and the 'sys' R functions.  Calls to built-in
     * functions, other than 'foreign' functions and various other
     * special cases, are not recorded unless R profiling is
     * enabled.</li>
     *
     * <li>A secondary function is to save and restore information
     * about the evaluation state.  Certain aspects of the state of
     * the current Evaluator are saved by the constructor of a Context
     * object; the state is then restored by the object's destructor.
     * Since Context objects are allocated on the processor stack,
     * this means that the evaluation state will automatically be
     * restored both under the normal flow of control and when the
     * stack is unwound during the propagation of a C++ exception.
     * (Beware however that some of the save/restore functionality
     * that CR's RCNTXT offers is handled separately in CXXR via
     * classes such as Browser; this trend of moving save/restore
     * functionality out of the Evaluator::Context class is likely to
     * continue.  Moreover, in cases where save/restore functions
     * continue to be effected by Evaluator::Context, this is in some
     * cases achieved by incorporating within a Context object an
     * object with more specific save/restore role, such as a
     * ProtectStack::Scope object.</li>
     *
     * <ol>
     *
     * Evaluator::Context objects are of two kinds according to
     * whether or not the \c working_env argument to the constructor
     * is a null pointer.  If \c working_env is null, then the Context
     * contains information relating to an application of a
     * BuiltInFunction, and in that case the \c function and \c
     * promise_args constructor arguments should also be null; this
     * corresponds to CR's BUILTIN context type.  If \c working_env is
     * non-null, then the Context typically contains information about
     * an application of a Closure; some other cases, such as
     * generically-despatched BuiltInFunction calls and calls to
     * do_eval(), are handled in the same way as Closure applications.
     * This corresponds roughly to CR's FUNCTION context type.
     *
     * @note It is likely that in future refactorisation this class
     * will be divided into two: (i) a base class containing information
     * applicable to any call of FunctionBase::apply(), and with
     * little or no save/restore functionality, and (ii) a derived
     * class containing extra information relating to Closure
     * applications and kindred, and with additional save/restore
     * functions.
     */
    class Evaluator::Context {
    public:
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
	 *          pointer is null, it signifies that this Context
	 *          relates to a built-in function, and in that case
	 *          \a function and \a promise_args should also be null.
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

	/** @brief The innermost Context.
	 *
	 * @return Pointer to the innermost Context belonging to the
	 * current Evaluator.
	 */
	static Context* innermost()
	{
	    return Evaluator::current()->innermostContext();
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

SEXP Rf_dynamicfindVar(SEXP, CXXR::Evaluator::Context*);
int Rf_framedepth(CXXR::Evaluator::Context*);
void R_InsertRestartHandlers(CXXR::Evaluator::Context*, Rboolean);
SEXP R_syscall(int, CXXR::Evaluator::Context*);
int R_sysparent(int, CXXR::Evaluator::Context*);
SEXP R_sysframe(int, CXXR::Evaluator::Context*);
SEXP R_sysfunction(int, CXXR::Evaluator::Context*);

extern "C" {
    void Rf_jump_to_toplevel(void);
}

#endif  // CONTEXT_HPP
