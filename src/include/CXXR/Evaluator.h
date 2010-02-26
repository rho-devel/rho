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

/** @file Evaluator.h
 *
 * @brief Class CXXR::Evaluator
 */

#ifndef EVALUATOR_H
#define EVALUATOR_H

#include "R_ext/Boolean.h"
#include "CXXR/PairList.h"

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

#define BYTECODE

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
    class Environment;

    /** @brief Housekeeping services for R expression evaluation.
     * 
     * All members of this class are static.
     */
    class Evaluator {
    public:
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
	static RObject* evaluate(RObject* object, Environment* env);

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

	/** @brief Map RObject::evaluate() over a PairList.
	 *
	 * Except as regards the ... handling described next, this
	 * function produces an output list of the same length as \a
	 * inlist, and whose tags are the same as the tags of \a
	 * inlist, but whose elements ('car' values) are the result of
	 * evaluating the corresponding elements of \a inlist within
	 * \a env.
	 *
	 * If an element of the list is R_DotsSymbol, its binding
	 * within \a env is examined.  If it is bound to NULL or to
	 * Symbol::missingArgument() (R_MissingArg), this element of
	 * \a inlist is skipped.  If it is bound to a DottedArgs list,
	 * then this element of \a inlist is replaced by the one or
	 * more elements resulting from evaluating the elements of the
	 * DottedArgs list, and carrying across the corresponding
	 * tags.
	 *
	 * If any element of \a inlist is missing (i.e. it is
	 * Symbol::missingArgument(), or is a Symbol missing within \a
	 * env), then the corresponding element of the output list is
	 * set to Symbol::missingArgument(), and the presence of a
	 * missing value is reported in the return value.
	 *
	 * @param inlist The PairList to be mapped through
	 *          RObject::evaluate().
	 *
	 * @param env The Environment in which evaluations are to take
	 *          place.
	 *
	 * @return The second element of the pair is the output list.
	 * The first element is the index, counting from one, of the
	 * first position in \a inlist in which a missing value was
	 * encountered, or zero if \a inlist contained no missing
	 * values.
	 *
	 * @note This function is intended within CXXR to supersede
	 * CR's evalList() and evalListKeepMissing().  However, note
	 * that these CR functions coerce the tags in the output list
	 * to be Symbol objects, whereas this function does not.
	 */
	static std::pair<unsigned int, PairList*>
	mapEvaluate(const PairList* inlist, Environment* env);

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
	static void setDepthLimit(int depth);
   private:
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
	static bool s_profiling;  // True iff profiling enabled
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
#endif /* __cplusplus */

#endif /* EVALUATOR_H */
