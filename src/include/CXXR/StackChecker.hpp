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

/** @file StackChecker.hpp
 *
 * @brief Class CXXR::StackChecker.
 * @brief Class CXXR::IncrementStackDepthScope;
 * @brief Class CXXR::DisableStackCheckingScope;
 */

#ifndef CXXR_STACKDEPTH_HPP
#define CXXR_STACKDEPTH_HPP

#include "Defn.h"

namespace CXXR {

    class IncrementStackDepthScope;
    class DisableStackCheckingScope;

    class StackChecker
    {
    public:
	// Stack depth is in 'evaluator frames' (which is fairly loosely
	// defined).  In CXXR, it includes non-inlined function calls (including
	// builtins) and promise evaluations.

	/** @brief (Not for general use.)
	 *
	 * Used in do_Cstack_info() in platform.cpp.
	 *
	 * @return The current evaluation depth.
	 */
	static unsigned int depth() {
	    return s_depth;
	}

	/** @brief Maximum depth of R expression nesting.
	 *
	 * @return The current maximum nesting depth.
	 */
	static unsigned int depthLimit() {
	    return s_depth_limit;
	}

	/** @brief Set maximum depth of R expression nesting.
	 *
	 * @param depth The required maximum nesting depth.  If the
	 *          supplied value lies outside the permissible range,
	 *          an error is reported and the nesting depth is left
	 *          unchanged.
	 */
	static void setDepthLimit(unsigned int depth);

	// Check that there is sufficient stack space for <required_bytes>
	// plus some additional allowance.
	// Throws an exception otherwise.
	static void checkAvailableStackSpace(size_t required_bytes = 0);

    private:
	friend class IncrementStackDepthScope;
	friend class DisableStackCheckingScope;

	static void handleStackDepthExceeded() __attribute__((noreturn));
	static void handleStackSpaceExceeded() __attribute__((noreturn));

	// Current depth of expression evaluation.
	static unsigned int s_depth;

	// The limit (controlled by the 'expressions' R option) at which we
	// trigger an error.
	static unsigned int s_depth_limit;
    };

    class IncrementStackDepthScope {
    public:
	IncrementStackDepthScope() {
	    StackChecker::s_depth++;
	    if (StackChecker::s_depth > StackChecker::s_depth_limit) {
		StackChecker::handleStackDepthExceeded();
	    }
	    StackChecker::checkAvailableStackSpace();
	}
	~IncrementStackDepthScope() {
	    StackChecker::s_depth--;
	}
    private:
	IncrementStackDepthScope(const IncrementStackDepthScope&) = delete;
	IncrementStackDepthScope& operator=(
	    const IncrementStackDepthScope&) = delete;
    };

    /*
     * This class disables the normal stack checking to allow error reporting
     * and stack unwinding to proceed.
     */
    class DisableStackCheckingScope {
    public:
	DisableStackCheckingScope();
	~DisableStackCheckingScope();

    private:
	unsigned int m_previous_limit;
	uintptr_t m_previous_stack_limit;

	DisableStackCheckingScope(const DisableStackCheckingScope&) = delete;
	DisableStackCheckingScope& operator=(
	    const DisableStackCheckingScope&) = delete;
    };

}  // namespace CXXR

#endif  // CXXR_STACKDEPTH_HPP
