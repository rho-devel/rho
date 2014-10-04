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

#include "CXXR/StackChecker.hpp"

namespace CXXR {

namespace {
    const unsigned int R_MIN_EXPRESSIONS_OPT = 25;
    const unsigned int R_MAX_EXPRESSIONS_OPT = 500000;
}

unsigned int StackChecker::s_depth = 0;
unsigned int StackChecker::s_depth_limit = 5000;

void StackChecker::checkAvailableStackSpace(size_t required_bytes)
{
    if (R_CStackLimit == -1)
	return;

    char dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);
    if (usage + required_bytes > R_CStackLimit - R_CStackLimit / 16)
    {
	handleStackSpaceExceeded();
    }
}

void StackChecker::setDepthLimit(unsigned int depth)
{
    if (depth < R_MIN_EXPRESSIONS_OPT || depth > R_MAX_EXPRESSIONS_OPT)
    {
	Rf_error(_("'expressions' parameter invalid, allowed %d...%d"),
		 R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
    }
    s_depth_limit = depth;
}

void StackChecker::handleStackDepthExceeded()
{
    DisableStackCheckingScope no_stack_checking;
    Rf_errorcall(nullptr, _("evaluation nested too deeply: "
		      "infinite recursion / options(expressions=)?"));
}	

void StackChecker::handleStackSpaceExceeded()
{
    // We'll need to use the remaining stack space for error recovery,
    // so temporarily disable stack checking.
    DisableStackCheckingScope no_stack_checking;
    
    // Do not translate this, to save stack space.
    Rf_errorcall(R_NilValue, "C stack usage is too close to the limit");
}

DisableStackCheckingScope::DisableStackCheckingScope()
{
    m_previous_limit = StackChecker::depthLimit();
    StackChecker::setDepthLimit(R_MAX_EXPRESSIONS_OPT);
    
    m_previous_stack_limit = R_CStackLimit;
    R_CStackLimit = -1;
}

DisableStackCheckingScope::~DisableStackCheckingScope()
{
    StackChecker::setDepthLimit(m_previous_limit);
    R_CStackLimit = m_previous_stack_limit;
}

}  // namespace CXXR
