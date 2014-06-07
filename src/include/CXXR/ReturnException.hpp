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

/** @file ReturnException.hpp
 *
 * @brief Class CXXR::ReturnException.
 */

#ifndef RETURNEXCEPTION_HPP
#define RETURNEXCEPTION_HPP 1

#include "CXXR/GCRoot.h"

namespace CXXR {
    class Environment;

    /** @brief Exception class to convey return value.
     *
     * An exception of this class conveys a value back to a
     * computation (typically a Closure application) operating within
     * a specified working Environment, and is used for example to
     * implement the R return command.
     */
    class ReturnException {
    public:
	/** @brief Constructor.
	 *
	 * @param the_environment Pointer to the working Environment
	 *          of the computational context within which the
	 *          exception is to be caught.  (catch blocks within
	 *          other contexts should rethrow the exception.)
	 *
	 * @param the_value Pointer, possibly null, to the RObject to
	 *          be conveyed back to the target Environment.
	 */
	ReturnException(Environment* the_environment, RObject* the_value)
	    : m_environment(the_environment), m_value(the_value)
	{}

	/** @brief Target Environment of this ReturnException.
	 *
	 * @return pointer to the Environment within which this
	 * ReturnException should be caught.
	 */
	Environment* environment() const
	{
	    return m_environment;
	}

	/** @brief Payload of this ReturnException.
	 *
	 * @return Pointer, possibly null, to the RObject conveyed to
	 * the target Environment by this ReturnException.
	 */
	RObject* value() const
	{
	    return m_value;
	}
    private:
	Environment* m_environment;
	GCRoot<> m_value;
    };
}

#endif  // RETURNEXCEPTION_HPP
