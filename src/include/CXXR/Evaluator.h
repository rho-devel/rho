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

/** @file Evaluator.h
 *
 * @brief Class CXXR::Evaluator
 */

#ifndef EVALUATOR_H
#define EVALUATOR_H

#include "R_ext/Boolean.h"
#include "CXXR/Environment.h"
#include "CXXR/PairList.h"

// Move this to ByteCode.hpp in due course:
#define BYTECODE

#ifdef __cplusplus
#include <utility>

extern "C" {
#endif

    /** @brief Print expression value?
     *
     * If R_Visible is TRUE when the evaluation of a top-level R
     * expression completes, the value of the expression is printed.
     *
     * @note In CXXR, R_Visible is evolving towards becoming a static
     * data member of class CXXR::Evaluator.
     */
    extern Rboolean R_Visible;

    /** @brief Are any user interrupts currently pending?
     *
     * If user interrupts are attempted while user interrupts are
     * suspended, this is set non-zero.  The interrupt is then
     * services when the period of suspension ends.
     */
    extern int R_interrupts_pending;

    /** @brief Are interrupts currently suspended?
     */
    extern Rboolean R_interrupts_suspended;

    /** @brief Is a Symbol missing within an Environment?
     *
     * @param symbol Pointer to the Symbol whose missing status is
     * required.
     *
     * @param rho Pointer to the Environment in whose Frame \a symbol
     *          is to be sought.
     *
     * @return A non-zero value iff \a symbol is missing in the Frame
     * of \a rho.
     *
     * @note For more information, refer to the code, which is
     * surprisingly complicated.
     */
    int R_isMissing(SEXP symbol, SEXP rho);

#ifdef __cplusplus
} // extern "C"

namespace CXXR {
    class RObject;
    class Expression;

    /** @brief Framework for R command evaluation.
     * 
     * An object of this class provides a framework within which
     * evaluation of R commands (i.e. top-level R expressions) takes
     * place, and provides various housekeeping services to control
     * and support that evaluation.  Evaluator objects conceptually
     * form a stack, so they must be destroyed in the reverse order of
     * creation: it is therefore recommended that they always be
     * declared on the processor stack, i.e. as C++ automatic
     * variables.  The Evaluator at the top of the stack is the \e
     * current Evaluator.
     *
     * If an error occurs during R expression evaluation, the
     * resulting exception is caught within the scope of the current
     * Evaluator; the relevant code will then typically request
     * another R command (if interactive), or terminate the scope of
     * the current Evaluator, thus popping it off the Evaluator stack,
     * and return control to code within the scope of the next
     * Evaluator down.
     * 
     * Each Evaluator object contains a nested sequence of zero or
     * more Contexts.  A newly created Context is added to the
     * sequence belonging to the current Evaluator.  It is therefore
     * an error to declare a Context object outside the scope of any
     * Evaluator object.
     *
     * @note Evaluator objects fulfil many of the roles of TOPLEVEL
     * contexts within CR (which do not exist within CXXR).  Note that
     * in CXXR, each Evaluator has its own singly-linked list of
     * Context objects (starting with the innermost); there is no
     * overall list of Context objects spanning different Evaluator
     * objects.
     */
    class Evaluator {
    public:
	class Context;

	Evaluator()
	    : m_next(s_current), m_innermost_context(0)
	{
	    s_current = this;
	}

	~Evaluator()
	{
	    s_current = m_next;
	}

	/** @brief The current Evaluator.
	 *
	 * @return Pointer to the current (innermost) Evaluator.
	 */
	static Evaluator* current()
	{
	    return s_current;
	}

	/** @brief (Not for general use.)
	 *
	 * Used in context.cpp to save the evaluation depth
	 * associated with an RCNTXT.  Also used in do_Cstack_info()
	 * in platform.cpp.
	 *
	 * @return The current evaluation depth.
	 */
	static unsigned int depth()
	{
	    return s_depth;
	}

	/** @brief Maximum depth of R expression nesting.
	 *
	 * @return The current maximum nesting depth (disregarding any
	 * extra depth that may have been introduced by extraDepth()
	 * ).
	 */
	static unsigned int depthLimit()
	{
	    return s_depth_limit;
	}

	/** @brief (Not for general use.)
	 *
	 * @param on If true, and extra depth is not already enabled,
	 *          an increase is applied to the permissible depth of
	 *          nested evaluations to allow error reporting to be
	 *          carried out.  If false, any such extra depth
	 *          currently in force is removed.
	 */
	static void enableExtraDepth(bool on)
	{
	    s_depth_threshold = s_depth_limit + (on ? 500 : 0);
	}

	/** @brief (Not for general use.)
	 *
	 * This function is for use by the profiling code in eval.cpp
	 * to record whether profiling is currently enabled.
	 *
	 * @param on true iff printing is required.
	 */
	static void enableProfiling(bool on)
	{
	    s_profiling = on;
	}

	/** @brief Specify whether the result of top-level expression
	 * be printed.
	 *
	 * @param on true iff printing is required.
	 */
	static void enableResultPrinting(bool on)
	{
	    R_Visible = Rboolean(on);
	}

	/** @brief Evaluate RObject in a specified Environment.
	 *
	 * For most types of RObject, this simply returns a pointer to
	 * the RObject itself.
	 *
	 * @param object Pointer to the RObject to be evaluated.  May
	 *          be null, in which case the function returns a null
	 *          pointer.
	 *
	 * @param env Pointer to the environment in which evaluation
	 *          is to take place.  May be null only if \a object
	 *          is null.
	 *
	 * @return Pointer to the result of evaluation.
	 */
	static RObject* evaluate(RObject* object, Environment* env)
          HOT_FUNCTION;

	/** @brief Innermost Context belonging to this Evaluator.
	 *
	 * @return Pointer to the innermost Context belonging to this
	 * Evaluator.
	 */
	Context* innermostContext() const
	{
	    return m_innermost_context;
	}

	/** @brief Is profiling currently enabled?
	 *
	 * @return true iff profiling is currently in progress.
	 */
	static bool profiling()
	{
	    return s_profiling;
	}

	/** @brief Is the result of top-level expression evaluation
	 * printed?
	 *
	 * @return true iff it is currently specified that the result
	 * of a top-level R expression evaluation should be printed.
	 */
	static bool resultPrinted()
	{
	    return R_Visible;
	}

	/** @brief (Not for general use.)
	 *
	 * Used in context.cpp to restore the evaluation depth
	 * associated with an RCNTXT.  Also used in main.cpp to reset
	 * the evaluation depth to zero.
	 *
	 * @param depth The required depth.
	 */
	static void setDepth(unsigned int depth)
	{
	    s_depth = depth;
	}

	/** @brief Set maximum depth of R expression nesting.
	 *
	 * @param depth The required maximum nesting depth.  If the
	 *          supplied value lies outside the permissible range,
	 *          an error is reported and the nesting depth is left
	 *          unchanged.
	 */
	static void setDepthLimit(unsigned int depth);
   private:
	friend class Context;  // Unnecessary in C++ 0x

	static unsigned int s_depth;  // Current depth of expression evaluation 
	static unsigned int s_depth_threshold;  // An error will be
			      // reported if s_depth exceeds this
			      // value.  s_depth_threshold is normally
			      // equal to s_depth_limit, but may be
			      // temporarily increased above s_depth_limit
			      // to allow error reporting.
	static unsigned int s_depth_limit;  // The value (controlled
			      // by the 'expressions' R option) to
			      // which s_depth_threshold is set except
			      // during error reporting.
	static unsigned int s_countdown;  // Number of calls of
			      // Evaluator::evaluate() to go before a
			      // check is made for user interrupts.
	static unsigned int s_countdown_start;  // Value from which
			      // s_countdown starts counting down
	static Evaluator* s_current;  // The current (innermost) Evaluator
	static bool s_profiling;  // True iff profiling enabled

	Evaluator* m_next;  // Next Evaluator down the stack
	Context* m_innermost_context;  // Innermost Context belonging
		   // to this Evaluator
    };

    /** @brief Shorthand for Evaluator::evaluate().
     *
     * @param object Pointer to the RObject to be evaluated.  May be
     *          null, in which case the function returns a null
     *          pointer. 
     *
     * @param env Pointer to the environment in which evaluation
     *          is to take place.  May be null only if \a object is
     *          null.
     *
     * @return Pointer to the result of evaluation.
     */
    inline RObject* evaluate(RObject* object, Environment* env)
    {
	return Evaluator::evaluate(object, env);
    }
}

extern "C" {
#endif /* __cplusplus */

    /** @brief Evaluate an object in a specified Environment.
     *
     * @param e Pointer (possibly null) to the object to be evaluated.
     *
     * @param rho Pointer to an Environment (checked unless \a e is null).
     *
     * @return Pointer to the result of evaluating \a e in \a rho, or
     * a null pointer if \a e is null.
     */
#ifndef __cplusplus
    SEXP Rf_eval(SEXP e, SEXP rho);
#else
    inline SEXP Rf_eval(SEXP e, SEXP rho)
    {
	using namespace CXXR;
	Environment* env = 0;
	if (e)
	    env = SEXP_downcast<Environment*>(rho);	
	return evaluate(e, env);
    }
#endif
 
#ifdef __cplusplus
}
#endif

#endif /* EVALUATOR_H */
