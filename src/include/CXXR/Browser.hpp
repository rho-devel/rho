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

/** @file Browser.hpp
 *
 * @brief Class CXXR::Browser.
 */

#ifndef BROWSER_HPP
#define BROWSER_HPP 1

#include "CXXR/Context.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/RObject.h"

namespace CXXR {
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
	      m_context(Context::innermost())
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
	static unsigned int numberActive()
	{
	    return s_browsers.size();
	}

	/** @brief Condition argument associated with a Browser.
	 *
	 * @param i Index of the Browser whose condition is required.
	 *          0 signifies the innermost (deepest) browser level:
	 *          note that this numbering convention is different
	 *          from that used by the R browserCondition function,
	 *          which starts counting from 1.
	 *
	 * @return The 'condition' argument associated with Browser \a i.
	 */
	static RObject* condition(unsigned int i)
	{
	    return s_browsers.at(i)->m_condition;
	}

	/** @brief Context within which a Browser was declared.
	 *
	 * @param i Index of the Browser whose Context is required.
	 *          0 signifies the innermost (deepest) browser level.
	 *
	 * @return Pointer to the Context in which Browser \a i was
	 * declared.
	 *
	 * @note This function is used to reproduce the rather strange
	 * behaviour of the R function browserSetDebug in CR.
	 */
	static Context* context(unsigned int i)
	{
	    return s_browsers.at(i)->m_context;
	}

	/** @brief Text argument associated with a Browser.
	 *
	 * @param i Index of the Browser whose text is required.  0
	 *          signifies the innermost (deepest) browser level:
	 *          note that this numbering convention is different
	 *          from that used by the R browserText function,
	 *          which starts counting from 1.
	 *
	 * @return The 'text' argument associated with Browser \a i.
	 */
	static RObject* text(unsigned int i)
	{
	    return s_browsers.at(i)->m_text;
	}
    private:
	static std::vector<Browser*> s_browsers;
	GCStackRoot<> m_text;
	GCStackRoot<> m_condition;
	Context* m_context;
    };
}

#endif  // BROWSER_HPP
