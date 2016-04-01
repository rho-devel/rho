/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file LoopBailout.hpp
 *
 * @brief Class rho::LoopBailout.
 */

#ifndef LOOPBAILOUT_HPP
#define LOOPBAILOUT_HPP 1

#include "rho/Bailout.hpp"

namespace rho {
    class Environment;

    /** @brief Bailout class for R commands 'break' and 'next'.
     *
     * A Bailout of this class conveys a value back to a computation
     * (typically a Closure application) operating within a specified
     * working Environment, and is used for example to implement the R
     * return command.
     */
    class LoopBailout : public Bailout {
    public:
	/** @brief Constructor.
	 *
	 * @param the_environment Pointer to the working Environment
	 *          of the computational context in which the relevant
	 *          loop is executing.
	 *
	 * @param next_iteration true for 'next'; false for 'break'.
	 */
	LoopBailout(Environment* the_environment, bool next_iteration)
	    : m_next(next_iteration)
	{
	    m_environment = the_environment;
	}

	/** @brief Target Environment of this LoopBailout.
	 *
	 * @return pointer to the Environment within which this
	 * LoopBailout should be caught.
	 */
	Environment* environment() const
	{
	    return m_environment;
	}

	/** @brief Continue with next iteration of the loop (if any)?
	 *
	 * @return true if this LoopException arose from the R 'next'
	 * command; false if it arose from 'break'.
	 */
	bool next() const
	{
	    return m_next;
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
	bool m_next;

	// Declared private to ensure that LoopBailout objects are
	// allocated only using 'new':
	~LoopBailout() {}

	// Not implemented.  Declared to prevent compiler-generated versions:
	LoopBailout(const LoopBailout&);
        LoopBailout& operator=(const LoopBailout&);
    };
}

#endif  // LOOPBAILOUT_HPP
