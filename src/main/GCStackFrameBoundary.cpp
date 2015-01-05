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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file GCStackFrameBoundary
 *
 * Class GCStackFrameBoundary.
 */

#include "CXXR/GCStackFrameBoundary.hpp"
#include "CXXR/GCNode.hpp"

namespace CXXR {

GCStackFrameBoundary *GCStackFrameBoundary::s_top = nullptr;
GCStackFrameBoundary *GCStackFrameBoundary::s_barrier = nullptr;

RObject* GCStackFrameBoundary::withStackFrameBoundary(
    std::function<RObject*()> function)
{
    // This function should not be inlined, nor inline the call to function()
    // in order to ensure that the compiler doesn't reorder objects on the
    // stack.  Otherwise, the compiler can generate code where the roots occur
    // on the wrong side of the stack frame boundary.
    GCStackFrameBoundary boundary;
    return function();
}

void GCStackFrameBoundary::advanceBarrier()
{
    if (!s_top) {  // There are no nodes to protect, so no barrier is needed.
	return;
    }

    // Protect nodes
    GCStackRootBase* current_location = getLocation(s_barrier);
    GCStackRootBase* new_location = getLocation(s_top);
    GCStackRootBase::incrementReferenceCounts(new_location, current_location);

    // Move the barrier.
    s_barrier = s_top;
}

void GCStackFrameBoundary::applyBarrier()
{
    assert(s_barrier == this);
    GCStackFrameBoundary* new_barrier = m_next;

    // Unprotect nodes.
    GCStackRootBase* current_location = getLocation(this);
    GCStackRootBase* new_location = getLocation(new_barrier);
    GCStackRootBase::decrementReferenceCounts(current_location, new_location);

    // Move the barrier back.
    s_barrier = new_barrier;
}

}  // namespace CXXR
