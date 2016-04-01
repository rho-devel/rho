/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
 */

/** @file GCStackFrameBoundary
 *
 * Class GCStackFrameBoundary.
 */

#include "rho/GCStackFrameBoundary.hpp"
#include "rho/GCNode.hpp"

namespace rho {

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
// Also note that this only stores the first instance of each node.
std::stack<const GCNode*> GCStackFrameBoundary::s_protected_nodes;

struct GCStackFrameBoundary::ProtectPointerVisitor
    : public GCNode::const_visitor
{
    ProtectPointerVisitor(int* count) {
	m_count = count;
    }

    void operator()(const GCNode* node) override
    {
	if (!node->isOnStackBitSet()) {
	    s_protected_nodes.push(node);
	    node->setOnStackBit();
	    ++(*m_count);
	}
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
	pointer->clearOnStackBit();
    }

    // Move the barrier back.
    BoundaryStack::iterator current_it = s_boundaries.iterator_to(*this);
    assert(std::next(current_it) == s_boundaries.end());
    s_barrier = &*std::prev(current_it);
}

}  // namespace rho
