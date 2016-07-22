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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include "rho/GCNode.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <map>
#include <set>
#include <utility>

#include "rho/GCManager.hpp"
#include "rho/GCRoot.hpp"
#include "rho/GCStackFrameBoundary.hpp"
#include "rho/ProtectStack.hpp"
#include "rho/RAllocStack.hpp"
#include "rho/WeakRef.hpp"

#include "rho/GCNodeAllocator.hpp"

using namespace std;
using namespace rho;

vector<const GCNode*>* GCNode::s_moribund = 0;
unsigned int GCNode::s_num_nodes = 0;
bool GCNode::s_on_stack_bits_correct = false;

// Used to update reference count bits of a GCNode. The array element at index
// n+1 is XORed with the current rcmms bits to compute the updated reference
// count n+1.  This does not overflow the reference count bits and preserves
// the other bits in m_rcmms.
const unsigned char GCNode::s_decinc_refcount[]
= {0,    2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x1e,
   0x1e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x3e,
   0x3e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x1e,
   0x1e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 0,    0};

unsigned char GCNode::s_mark = 0;

HOT_FUNCTION void* GCNode::operator new(size_t bytes) {
    GCManager::maybeGC();
    MemoryBank::notifyAllocation(bytes);
    void *result;

    result = GCNodeAllocator::allocate(bytes);

    // Because garbage collection may occur between this point and the GCNode's
    // constructor running, we need to ensure that this space is at least
    // minimally initialized.
    // We construct a GCNode with a ficticious reference count of 1, to prevent
    // it from getting marked as moribund or garbage collected (the mark-sweep
    // GC uses the reference counts, so this is always effective).
    // It will be overwritten by the real constructor.
    new (result)GCNode(static_cast<CreateAMinimallyInitializedGCNode*>(nullptr));
    return result;
}

GCNode::GCNode(CreateAMinimallyInitializedGCNode*) : m_rcmms(s_decinc_refcount[1]) {
}

void GCNode::operator delete(void* pointer, size_t bytes) {
    MemoryBank::notifyDeallocation(bytes);

    GCNodeAllocator::free(pointer);
}

bool GCNode::check() {
    // Check moribund list:
    for (const GCNode* node : *s_moribund) {
        if (!(node->m_rcmms & s_moribund_mask)) {
            cerr << "GCNode::check() : "
                "Node on moribund list without moribund bit set.\n";
            abort();
        }
    }

    return true;
}

void GCNode::destruct_aux() {
    // Erase this node from the moribund list:
    typedef std::vector<const GCNode*>::iterator Iter;
    Iter it = std::find(s_moribund->begin(), s_moribund->end(), this);
    if (it == s_moribund->end()) {
        abort();  // Should never happen!
    }
    s_moribund->erase(it);
}
    
extern RObject* R_Srcref;

void GCNode::gc(bool markSweep) {
    if (GCManager::GCInhibitor::active()) {
        return;
    }
    GCManager::GCInhibitor inhibitor;

    ProtectStack::protectAll();
    incRefCount(R_Srcref);

    if (markSweep) {
        GCStackRootBase::withAllStackNodesProtected(markSweepGC);
    } else {
        GCStackRootBase::withAllStackNodesProtected(gclite);
    }

    decRefCount(R_Srcref);
}

void GCNode::markSweepGC() {
    // NB: setting this flag implies that the garbage collection will ignore
    // any new stack roots.  To ensure correctness, this function must not call
    // any code that depends on normal operation of the garbage collector.
    s_on_stack_bits_correct = true;

    mark();
    sweep();

    s_on_stack_bits_correct = false;
}

void GCNode::gclite() {
    s_on_stack_bits_correct = true;

    while (!s_moribund->empty()) {
        // Last in, first out, for cache efficiency:
        const GCNode* node = s_moribund->back();
        s_moribund->pop_back();
        // Clear moribund bit.  Beware ~ promotes to unsigned int.
        node->m_rcmms &= static_cast<unsigned char>(~s_moribund_mask);

        if (node->maybeGarbage()) {
            delete node;
        }
    }

    s_on_stack_bits_correct = false;
}

void GCNode::initialize() {
    GCNodeAllocator::initialize();
    s_moribund = new vector<const GCNode*>();
}

void GCNode::makeMoribund() const {
    if (s_on_stack_bits_correct) {
        // In this case, the node can be deleted immediately.
        delete this;
    } else {
        addToMoribundList();
    }
}

void GCNode::addToMoribundList() const {
    m_rcmms |= s_moribund_mask;
    s_moribund->push_back(this);
}

void GCNode::mark() {
    // In the first mark-sweep collection, the marking of a node is
    // indicated by the mark bit being set; in the second mark sweep,
    // marking is indicated by the bit being clear, and so on in
    // alternation.  This avoids the need for the sweep phase to
    // iterate through the surviving nodes simply to remove marks.
    s_mark ^= s_mark_mask;
    GCNode::Marker marker;
    GCRootBase::visitRoots(&marker);
    GCStackRootBase::visitRoots(&marker);
    ProtectStack::visitRoots(&marker);
    WeakRef::markThru();
    if (R_Srcref) {
        marker(R_Srcref);
    }
}

void GCNode::sweep() {
    // Detach the referents of nodes that haven't been marked.
    // Once this is done, all of the nodes in the cycle will be unreferenced
    // and they will have been deleted unless their reference count is
    // saturated.
    vector<GCNode*> to_delete;
    GCNodeAllocator::applyToAllAllocations([&](void* pointer) {
        // The pointer is still allocated, so detach referents.
        GCNode* node = static_cast<GCNode*>(pointer);
        if (!node->isMarked()) {
            int ref_count = node->getRefCount();
            incRefCount(node);
            if (node->getRefCount() == ref_count) {
                // The reference count has saturated.
                node->detachReferents();
                to_delete.push_back(node);
            } else {
                node->detachReferents();
                decRefCount(node);
            }
        }
    });
    // At this point, the only unmarked objects are GCNodes with saturated
    // reference counts.  Delete them.
    for (GCNode* node : to_delete) {
        delete node;
    }
}

void GCNode::Marker::operator()(const GCNode* node) {
    if (node->isMarked()) {
        return;
    }
    // Update mark  Beware ~ promotes to unsigned int.
    node->m_rcmms &= static_cast<unsigned char>(~s_mark_mask);
    node->m_rcmms |= s_mark;
    node->visitReferents(this);
}

void GCNode::CountingMarker::operator()(const GCNode* node) {
    if (node->isMarked()) {
        return;
    }
    Marker::operator()(node);
    m_marks_applied += 1;
}

void rho::initializeMemorySubsystem() {
    static bool initialized = false;
    if (!initialized) {
        MemoryBank::initialize();
        GCNode::initialize();
        ProtectStack::initialize();
        RAllocStack::initialize();

        initialized = true;
    }
}

// Test if the argument is a GCNode pointer.
// Returns nullptr if the candidate pointer is not inside a GCNode,
// otherwise returns the pointer to the enclosign GCNode.
GCNode* GCNode::asGCNode(void* candidate_pointer) {
    return GCNodeAllocator::lookupPointer(candidate_pointer);
}

GCNode::InternalData GCNode::storeInternalData() const {
    return m_rcmms;
}

void GCNode::restoreInternalData(InternalData data) {
    m_rcmms = data;
}

