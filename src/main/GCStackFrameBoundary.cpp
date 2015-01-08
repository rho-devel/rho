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

GCStackFrameBoundary::BoundaryStack GCStackFrameBoundary::s_boundaries;

// To simplify the code, we add a dummy boundary representing the bottom of
// the stack and initially set the barrier to it.
GCStackFrameBoundary *GCStackFrameBoundary::s_bottom_of_stack
    = new GCStackFrameBoundary();
GCStackFrameBoundary *GCStackFrameBoundary::s_barrier = s_bottom_of_stack;

// While the contents of the stack below the boundary are assumed to not change,
// the set of pointers found by the conservative stack scanner in that region
// can.  To ensure that the same nodes are protected and unprotected, we keep
// a separate stack containing just the protected nodes here.
std::stack<const GCNode*> GCStackFrameBoundary::s_protected_nodes;

struct GCStackFrameBoundary::ProtectPointerVisitor
    : public GCNode::const_visitor
{
    ProtectPointerVisitor(int* count) {
	m_count = count;
    }

    void operator()(const GCNode* node) override
    {
	s_protected_nodes.push(node);
	GCNode::incRefCount(node);
	++(*m_count);
    } 

    int* m_count;
};

void* GCStackFrameBoundary::getStackPointer()
{
    return (this == s_bottom_of_stack)
	? GCStackRootBase::getStackBase() :  reinterpret_cast<void*>(this);
}

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
    BoundaryStack::iterator frame_start = s_boundaries.iterator_to(*s_barrier);
    BoundaryStack::iterator frame_end = std::next(frame_start);
    while (frame_end != s_boundaries.end()) {
	advanceBarrierOneFrame(&*frame_start, &*frame_end);
        frame_start = frame_end;
        ++frame_end;
    }   

    // Move the barrier.
    s_barrier = &s_boundaries.back();
}

void GCStackFrameBoundary::advanceBarrierOneFrame(
    GCStackFrameBoundary* frame_start, GCStackFrameBoundary* frame_end)
{
    ProtectPointerVisitor visitor(&frame_end->m_num_protected_pointers);
    
    GCStackRootBase::visitRoots(&visitor,
				frame_start->getStackPointer(),
				frame_end->getStackPointer());
}

void GCStackFrameBoundary::applyBarrier()
{
    assert(s_barrier == this);

    // Pop off the protected nodes.
    for (int i = 0; i < m_num_protected_pointers; i++) {
	const GCNode* pointer = s_protected_nodes.top();
	s_protected_nodes.pop();
	GCNode::decRefCount(pointer);
    }

    // Move the barrier back.
    BoundaryStack::iterator current_it = s_boundaries.iterator_to(*this);
    assert(std::next(current_it) == s_boundaries.end());
    s_barrier = &*std::prev(current_it);
}

}  // namespace CXXR
