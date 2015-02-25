/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file ReturnBailout.hpp
 *
 * @brief Class CXXR::ReturnBailout.
 */

#ifndef RETURNBAILOUT_HPP
#define RETURNBAILOUT_HPP 1

#include "CXXR/Bailout.hpp"
#include "CXXR/Evaluator.h"

namespace CXXR {
    class Environment;

    /** @brief Bailout class to convey return value.
     *
     * A Bailout of this class conveys a value back to a computation
     * (typically a Closure application) operating within a specified
     * working Environment, and is used for example to implement the R
     * return command.
     */
    class ReturnBailout : public Bailout {
    public:
	/** @brief Constructor.
	 *
	 * @param the_environment Pointer to the working Environment
	 *          of the computational context to which a return is
	 *          to be made.
	 *
	 * @param the_value Pointer, possibly null, to the RObject to
	 *          be conveyed back to the return destination.
	 */
	ReturnBailout(Environment* the_environment, RObject* the_value)
	    : m_print_result(R_Visible)
	{
	    m_environment = the_environment;
	    m_value = the_value;
	}

	/** @brief Target Environment of this ReturnBailout.
	 *
	 * @return pointer to the Environment within which this
	 * ReturnBailout should be caught.
	 */
	Environment* environment() const
	{
	    return m_environment;
	}

	/** @brief Should result be printed?
	 *
	 * @return true iff the return value should be printed if it
	 * ends up as the result of a top-level command.
	 */
	bool printResult() const
	{
	    return m_print_result;
	}

	/** @brief Payload of this ReturnBailout.
	 *
	 * @return Pointer, possibly null, to the RObject conveyed to
	 * the target Environment by this ReturnBailout.
	 */
	RObject* value() const
	{
	    return m_value;
	}

	// Virtual function of Bailout:
	void throwException() override;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const override;
    protected:
	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	GCEdge<Environment> m_environment;
	GCEdge<> m_value;
	bool m_print_result;

	// Declared private to ensure that ReturnBailout objects are
	// allocated only using 'new':
	~ReturnBailout() {}

	// Not implemented.  Declared to prevent compiler-generated versions:
	ReturnBailout(const ReturnBailout&);
        ReturnBailout& operator=(const ReturnBailout&);
    };
}

#endif  // RETURNBAILOUT_HPP
