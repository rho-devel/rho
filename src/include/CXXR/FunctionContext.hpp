/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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

/** @file FunctionContext.hpp
 *
 * @brief Class CXXR::FunctionContext.
 */

#ifndef FUNCTIONCONTEXT_HPP
#define FUNCTIONCONTEXT_HPP 1

#include "CXXR/Evaluator_Context.hpp"
#include "CXXR/Expression.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/GCStackRoot.hpp"

extern "C" {
    // Parked here temporarily:
    extern CXXR::RObject* R_Srcref;
}

namespace CXXR {
    /** @brief Context recording the invocation of a FunctionBase.
     *
     * This class is the base class of ClosureContext.  A FunctionContext
     * object that is not also a ClosureContext records the invocation
     * of a BuiltInFunction.
     *
     * Note that as in CR, calls to 'special' BuiltInFunction objects
     * (SPECIALSXP) are not recorded; however, unlike CR, calls to
     * other BuiltInFunction objects (BUILTINSXP) are always recorded.
     */
    class FunctionContext : public Evaluator::Context {
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
	 *          being applied.
	 */
	FunctionContext(const Expression* the_call, Environment* call_env,
			const FunctionBase* function)
	    : m_srcref(R_Srcref), m_call(the_call), m_call_env(call_env),
	      m_function(function)
	{
	    setType(FUNCTION);
	}

	~FunctionContext() {
	    R_Srcref = m_srcref;
	}

	/** @brief The call of the Context.
	 *
	 * @return Pointer to the call with which the Context is
	 * associated.
	 */
	const Expression* call() const
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
	const FunctionBase* function() const
	{
	    return m_function;
	}

	/** @brief Search outwards for a FunctionContext.
	 *
	 * This function works outwards from the Evaluator::Context \a
	 * start until it finds a FunctionContext (possibly \a start
	 * itself), and returns a pointer to that FunctionContext.
	 *
	 * @param start The Evaluator::Context from which the search
	 * is to start.
	 *
	 * @return Pointer to the innermost FunctionContext found, or
	 * a null pointer if no such context was found.
	 */
	static FunctionContext* innermost(Evaluator::Context* start
					  = Evaluator::Context::innermost());

	/** @brief Source location associated with this Context.
	 *
	 * @return Pointer, possibly null, to the source location
	 * associated with this Context.
	 */
	RObject* sourceLocation() const
	{
	    return m_srcref;
	}

    private:
	GCStackRoot<> m_srcref;
	GCStackRoot<const Expression> m_call;
	GCStackRoot<Environment> m_call_env;
	GCStackRoot<const FunctionBase> m_function;
    };
}  // namespace CXXR

#endif  // FUNCTIONCONTEXT_HPP
