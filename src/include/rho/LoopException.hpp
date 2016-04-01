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

/** @file LoopException.hpp
 *
 * @brief Class rho::LoopException.
 */

#ifndef LOOPEXCEPTION_HPP
#define LOOPEXCEPTION_HPP 1

#include "rho/Environment.hpp"
#include "rho/GCRoot.hpp"

namespace rho {
    /** @brief Exception thrown by R commands 'break' and 'next'.
     */
    class LoopException {
    public:
	/** @brief Constructor
	 *
	 * @param env Evaluation environment in which 'break' or
	 *          'next' occurred.
	 *
	 * @param next_iteration true for 'next'; false for 'break'.
	 */
	LoopException(Environment* env, bool next_iteration)
	    : m_environment(env), m_next(next_iteration)
	{}

	/** @brief Evaluation environment.
	 *
	 * @return Pointer to the evaluation environment in which
	 *         'break' or 'next' occurred.
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
    private:
	GCRoot<Environment> m_environment;
	bool m_next;
    };
}

#endif  // LOOPEXCEPTION_HPP
