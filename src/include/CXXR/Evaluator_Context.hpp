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

/** @file Evaluator_Context.hpp
 *
 * @brief Class CXXR::Evaluator::Context.
 *
 * @note The long name Evaluator_Context.hpp is used rather than
 * Context.hpp because otherwise the corresponding file Context.cpp
 * would differ only in case from the filename of context.cpp derived
 * from CR's context.c.
 */

#ifndef CONTEXT_HPP
#define CONTEXT_HPP 1

#include "CXXR/Evaluator.h"

namespace CXXR {
    /** @brief Housekeeping information on R call stack.
     *
     * Evaluator::Context objects must be declared on the processor
     * stack (i.e. as C++ automatic variables).
     * 
     * This class (together with its derived classes) performs two
     * functions:
     * <ol>
     *
     * <li>The primary function is to maintain an 'Ariadne's thread',
     * recording information about the stack of R function calls
     * currently active.  This is to support error reporting,
     * traceback, and the 'sys' R functions.  As in CR, calls to
     * 'special' BuiltInFunction objects (SPECIALSXP) are not
     * recorded; however, unlike CR, calls to other BuiltInFunction
     * objects (BUILTINSXP) are always recorded.</li>
     *
     * <li>Derived classes may also carry out a secondary function,
     * namely to save and restore information about the evaluation
     * state.  Certain aspects of the state of the current Evaluator
     * are saved by the derived class's constructor; the state is then
     * restored by the class's destructor.  Since Context objects are
     * allocated on the processor stack, this means that the
     * evaluation state will automatically be restored both under the
     * normal flow of control and when the stack is unwound during the
     * propagation of a C++ exception.  (Beware however that some of
     * the save/restore functionality that CR's RCNTXT offers is
     * handled separately in CXXR via classes such as Browser; this
     * trend of moving save/restore functionality out of classes
     * derived from Evaluator::Context is likely to continue.
     * Moreover, in cases where save/restore functions continue to be
     * effected by classes inheriting from Evaluator::Context, this is
     * in some cases achieved by incorporating within a Context object
     * an object with more specific save/restore role, such as a
     * ProtectStack::Scope object.</li>
     *
     * </ol>
     */
    class Evaluator::Context {
    public:
	/** @brief Context types.
	 *
	 * Different Context types correspond to different derived
	 * classes.  This base class contains a type field to allow
	 * Context types to be distinguished without the overhead of a
	 * \c dynamic_cast .
	 */
	enum Type {
	    BAILOUT = 0, /**< Context understanding Bailout objects. */
	    PLAIN,       /**< Lightweight Context neutralising BailoutContext. */
	    FUNCTION,    /**< Context corresponding to a BuiltInFunction. */
	    CLOSURE      /**< Context corresponding to a Closure. */
	};

	~Context()
	{
	    Evaluator::setDepth(m_eval_depth);
	    Evaluator::current()->m_innermost_context = m_next_out;
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

	/** @brief Type of the Context.
	 *
	 * @return The Context's Type.
	 */
	Type type() const
	{
	    return m_type;
	}
    protected:
	Context()
	    : m_next_out(innermost()), m_eval_depth(Evaluator::depth())
	{
	    Evaluator::current()->m_innermost_context = this;
	}

	/** @brief Set the type of the Context.
	 *
	 * @param the_type The desired Type of the Context.
	 */
	void setType(Type the_type)
	{
	    m_type = the_type;
	}
    private:
	Context *m_next_out;
	unsigned int m_eval_depth;
	Type m_type;
    };
}  // namespace CXXR

extern "C" {
    void Rf_jump_to_toplevel(void);
}

#endif  // CONTEXT_HPP
