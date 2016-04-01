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

/** @file Browser.hpp
 *
 * @brief Class rho::Browser.
 */

#ifndef BROWSER_HPP
#define BROWSER_HPP 1

#include "rho/Evaluator_Context.hpp"
#include "rho/GCStackRoot.hpp"

namespace rho {
    class RObject;

    /** @brief Class recording the use of R browsers.
     *
     * Browser objects must be declared on the processor stack
     * (i.e. as C++ automatic variables).  The class maintains a
     * vector of pointers to the Browser objects currently in
     * existence.
     */
    class Browser {
    public:
	/** @brief Constructor.
	 *
	 * @param the_text 'text' argument supplied to R browser
	 *          command.
	 *
	 * @param the_condition 'condition' argument supplied to R browser
	 *          command.
	 */
	Browser(RObject* the_text, RObject* the_condition)
	    : m_text(the_text), m_condition(the_condition),
	      m_context(Evaluator::Context::innermost())
	{
	    s_browsers.push_back(this);
	}

	~Browser()
	{
	    s_browsers.pop_back();
	}

	/** @brief Number of browser levels currently active.
	 *
	 * @return the number of browser levels currently active.
	 */
	static size_t numberActive()
	{
	    return s_browsers.size();
	}

	/** @brief Condition argument associated with a Browser.
	 *
	 * @return The 'condition' argument associated with this Browser.
	 */
	RObject* condition() const
	{
	    return m_condition;
	}

	/** @brief Context within which Browser was declared.
	 *
	 * @return Pointer to the Context in which this Browser was
	 * declared.
	 *
	 * @note This function is used to reproduce the rather strange
	 * behaviour of the R function browserSetDebug in CR.
	 */
	Evaluator::Context* context() const
	{
	    return m_context;
	}

	/** @brief Browser at specified level of nesting.
	 *
	 * @param i Index of the Browser required.  0 signifies the
	 *          outermost (first invoked) browser level.  Must be
	 *          less than numberActive().
	 *
	 * @return Pointer to the Browser at level \a i.
	 */
	static Browser* fromOutermost(size_t i)
	{
	    return s_browsers.at(i);
	}

	/** @brief Text argument associated with Browser.
	 *
	 * @return The 'text' argument associated with this Browser.
	 */
	RObject* text() const
	{
	    return m_text;
	}
    private:
	static std::vector<Browser*> s_browsers;
	GCStackRoot<> m_text;
	GCStackRoot<> m_condition;
	Evaluator::Context* m_context;
    };
}

#endif  // BROWSER_HPP
