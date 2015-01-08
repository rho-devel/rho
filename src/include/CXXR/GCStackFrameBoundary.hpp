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
#include <functional>
#include <stack>
#include <boost/intrusive/list.hpp>

namespace CXXR {

// The reference counting scheme uses an incremental stack scanning
// implementation for updating the reference counts of objects that have
// stack roots.
//
// The GC maintains a location on the stack known as the GC stack barrier, which
// has the property that all stack roots below the barrier have incremented
// their reference count, and none of the ones above the barrier have.
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
class GCStackFrameBoundary :
	public boost::intrusive::list_base_hook<>
{
public:
    /** @brief Insert a stack frame boundary between function frames.
     *
     * Calls the specified function, with a stack frame boundary inserted
     * between the calling and called functions.
     *
     * @param function The function to call.
     */
    static RObject* withStackFrameBoundary(std::function<RObject*()> function);

    /** @brief Advance the GC stack barrier to the topmost boundary.
     *
     * The location of the GC stack barrier is moved to the location of the
     * top boundary on the stack, ensuring that all roots below that boundary
     * have their reference counts incremented.
     *
     * In particular, if there are no important roots after the topmost
     * boundary, this ensures that all stack roots have been processed.
     */
    static void advanceBarrier();
private:

    GCStackFrameBoundary() : m_num_protected_pointers(0)
    {
	s_boundaries.push_back(*this);
    }
    
    ~GCStackFrameBoundary()
    {
	assert(this == &s_boundaries.back());
	if (this == s_barrier) {
	    applyBarrier();
	}
	s_boundaries.pop_back();
    }

    void* getStackPointer();
    static void advanceBarrierOneFrame(GCStackFrameBoundary* frame_start,
				       GCStackFrameBoundary* frame_end);

    void applyBarrier();

    int m_num_protected_pointers;

    static GCStackFrameBoundary* s_barrier;

    // We can't use the std::stack wrapper here as it gives compilation errors
    // when using the intrusive list as the underlying container.
    typedef boost::intrusive::list<
	GCStackFrameBoundary,
	boost::intrusive::constant_time_size<false>> BoundaryStack;
    static BoundaryStack s_boundaries;
 
    static GCStackFrameBoundary* s_bottom_of_stack;

    static std::stack<const GCNode*> s_protected_nodes;

    struct ProtectPointerVisitor;

    GCStackFrameBoundary(const GCStackFrameBoundary&) = delete;
    GCStackFrameBoundary& operator=(const GCStackFrameBoundary&) = delete;
};

}  // namespace CXXR

#endif  // CXXR_GC_STACK_BOUNDARY_HPP
