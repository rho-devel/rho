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

/** @file GCStackFrameBoundary.hpp
 *
 * @brief Class CXXR::GCStackFrameBoundary.
 */

#ifndef CXXR_GC_STACK_FRAME_BOUNDARY_HPP
#define CXXR_GC_STACK_FRAME_BOUNDARY_HPP

#include "CXXR/GCStackRoot.hpp"

namespace CXXR {

// The reference counting scheme uses an incremental stack scanning
// implementation for updating the reference counts of objects that have
// stack roots.
//
// The GC maintains a location on the stack known as the GC stack barrier, which
// has the property that all stack roots below the barrier have incremented their
// reference count, and none of the ones above the barrier have.
// At garbage collection time, only the locations on the stack between the
// barrier and the top of the stack need to be scanned, and the barrier can be
// advanced to reduce the amount of scanning that needs to occur in later
// collections.
// 
// A GCStackFrameBoundary is a location on the stack where the barrier can be
// placed.  To ensure correctness of the reference counting, it must be
// inserted only in locations where all the roots further down the stack are
// known to be immutable during its lifetime.
// The GCStackFrameBoundary's destructor automatically tests if the stack is
// being popped past the barrier, and if so, moves the barrier and updates the
// reference counts of the stack roots.
class GCStackFrameBoundary {
public:
    GCStackFrameBoundary() : m_location(nullptr)
    {
	m_next = s_top;
	s_top = this;
    }
    
    ~GCStackFrameBoundary()
    {
	assert(this == s_top);
	if (this == s_barrier) {
	    applyBarrier();
	}
	s_top = m_next;
    }

    static void advanceBarrier();
private:
    static GCStackRootBase* getLocation(GCStackFrameBoundary* boundary) {
	return boundary ? &boundary->m_location : nullptr;
    }
    void applyBarrier();
    
    GCStackRoot<GCNode> m_location;
    GCStackFrameBoundary* m_next;
    static GCStackFrameBoundary* s_top;
    static GCStackFrameBoundary* s_barrier;
};

}  // namespace CXXR

#endif  // CXXR_GC_STACK_BOUNDARY_HPP