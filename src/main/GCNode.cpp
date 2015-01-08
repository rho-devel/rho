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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include "CXXR/GCNode.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <limits>
#include "CXXR/ByteCode.hpp"
#include "CXXR/GCManager.hpp"
#include "CXXR/GCRoot.h"
#include "CXXR/GCStackFrameBoundary.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/ProtectStack.h"
#include "CXXR/RAllocStack.h"
#include "CXXR/WeakRef.h"
#include "gc.h"
#include "gc_mark.h"

extern "C" {
    // Declared in src/extra/gc/include/private/gc_priv.h.
    void GC_with_callee_saves_pushed(void (*fn)(char*, void *),
				     char* arg);
}

using namespace std;
using namespace CXXR;

GCNode::List* GCNode::s_live = 0;
vector<const GCNode*>* GCNode::s_moribund = 0;
GCNode::List* GCNode::s_reachable = 0;
unsigned int GCNode::s_num_nodes = 0;
unsigned int GCNode::s_inhibitor_count = 0;
const unsigned char GCNode::s_decinc_refcount[]
= {0,    2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x1e,
   0x1e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x3e,
   0x3e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x1e,
   0x1e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 0,    0};
const size_t GCNode::s_gclite_margin = 10000;
size_t GCNode::s_gclite_threshold = s_gclite_margin;
unsigned char GCNode::s_mark = 0;

void* GCNode::operator new(size_t bytes) HOT_FUNCTION
{
    if (s_inhibitor_count == 0)
    {
#ifdef AGGRESSIVE_GC
	if (true)
#elif defined(RARE_GC)
	if (MemoryBank::bytesAllocated() > GCManager::triggerLevel())
#else
	if (MemoryBank::bytesAllocated() > s_gclite_threshold)
#endif
	{
	    gclite();
	}

	if (MemoryBank::bytesAllocated() > GCManager::triggerLevel())
	{
	    GCManager::gc();
	}
    }
    MemoryBank::notifyAllocation(bytes);
    void* result = GC_malloc(bytes);
    // We repurpose the BDW collection's mark bit as an allocated flag.  This
    // works since the mark-sweep functionality of the BDW GC isn't used at all.
    GC_set_mark_bit(result);

    return result;
}

void GCNode::operator delete(void* p, size_t bytes)
{
    MemoryBank::notifyDeallocation(bytes);
    GC_clear_mark_bit(p);
    GC_free(p);
}

bool GCNode::check()
{
    if (s_live == nullptr) {
	cerr << "GCNode::check() : class not initialised.\n";
	abort();
    }
    unsigned int numnodes = 0;
    unsigned int virgins = 0;
    // Check live list:
    for (const GCNode* node: *s_live) {
	++numnodes;
	if (node->getRefCount() == 0)
	    ++virgins;
    }

    // Check moribund list:
    for (const GCNode* node: *s_moribund) {
	if (!(node->m_rcmmu & s_moribund_mask)) {
	    cerr << "GCNode::check() : "
		"Node on moribund list without moribund bit set.\n";
	    abort();
	}
    }

    // Check total number of nodes:
    if (numnodes != s_num_nodes) {
	cerr << "GCNode::check() :"
	    "recorded number of nodes inconsistent with nodes found.\n";
	abort();
    }
    // Report number of 'virgins', if any:
    if (virgins > 0)
	cerr << "GCNode::check() : " << virgins
	     << " nodes whose refcount has always been zero.\n";
    return true;
}

void GCNode::destruct_aux()
{
    // Erase this node from the moribund list:
    typedef std::vector<const GCNode*>::iterator Iter;
    Iter it = std::find(s_moribund->begin(), s_moribund->end(), this);
    if (it == s_moribund->end())
	abort();  // Should never happen!
    s_moribund->erase(it);
}
    
void GCNode::gc()
{
    // Note that recursion prevention is applied in GCManager::gc(),
    // not here.

    // cout << "GCNode::gc()\n";
    // GCNode::check();
    // cout << "Precheck completed OK: " << s_num_nodes << " nodes\n";

    if (s_inhibitor_count != 0) {
	cerr << "GCNode::gc() : mark-sweep GC must not be used"
	    " while a GCNode is under construction, or while garbage"
	    " collection is inhibited.\n";
	abort();
    }
    mark();
    sweep();
    // MemoryBank::defragment();

    // cout << "Finishing garbage collection\n";
    // GCNode::check();
    // cout << "Postcheck completed OK: " << s_num_nodes << " nodes\n";
}

void GCNode::gclite()
{
    if (s_inhibitor_count != 0)
	return;
    GCInhibitor inhibitor;
    ProtectStack::protectAll();
    ByteCode::protectAll();

    GCStackRootBase::withAllStackNodesProtected(gcliteImpl);
}

void GCNode::gcliteImpl() {
    while (!s_moribund->empty()) {
	// Last in, first out, for cache efficiency:
	const GCNode* node = s_moribund->back();
	s_moribund->pop_back();
	// Clear moribund bit.  Beware ~ promotes to unsigned int.
	node->m_rcmmu &= static_cast<unsigned char>(~s_moribund_mask);

	if (node->getRefCount() == 0)
	    delete node;
    }
    s_gclite_threshold = MemoryBank::bytesAllocated() + s_gclite_margin;
}

void GCNode::initialize()
{
    s_live = new List();
    s_reachable = new List();
    s_moribund = new vector<const GCNode*>();
    s_gclite_threshold = s_gclite_margin;

    // Initialize the Boehm GC.
    GC_set_all_interior_pointers(1);
    GC_set_dont_precollect(1);
    GC_set_no_dls(1);
    GC_set_pages_executable(0);

    GC_INIT();

    // We do our own garbage collection, so disable the Boehm GC's collector.
    // The collector library is used to do allocations, deallocations, find
    // stack roots and walk the heap in the mark/sweep cleanup, but no actual
    // garbage collection.
    GC_disable();
}

void GCNode::makeMoribund() const
{
    m_rcmmu |= s_moribund_mask;
    s_moribund->push_back(this);
}
    
void GCNode::mark()
{
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
    ByteCode::visitRoots(&marker);
    WeakRef::markThru();
}

void GCNode::sweep()
{
    // Detach the referents of nodes that haven't been moved to a
    // reachable list (i.e. are unreachable).
    // Once all of these objects have been processed, they will have no
    // incoming references and will have been placed on the moribund list for
    // deletion in the following call to gclite().
    while (!s_live->empty()) {
	GCNode* node = s_live->front();
	node->detachReferents();
	node->freeLink();
    }
    // Transfer the s_reachable list to the live list:
    s_live->splice_back(s_reachable);
    // Cleanup all the nodes that got placed on the moribund list.
    gclite();
}

void GCNode::Marker::operator()(const GCNode* node)
{
    if (node->isMarked()) {
	return;
    }
    // Update mark  Beware ~ promotes to unsigned int.
    node->m_rcmmu &= static_cast<unsigned char>(~s_mark_mask);
    node->m_rcmmu |= s_mark;
    ++m_marks_applied;
    s_reachable->splice_back(node);
    node->visitReferents(this);
}

void CXXR::initializeMemorySubsystem()
{
    static bool initialized = false;
    if (!initialized) {
	MemoryBank::initialize();
	GCNode::initialize();
	ProtectStack::initialize();
	RAllocStack::initialize();

	initialized = true;
    }
}


GCNode* GCNode::asGCNode(void* candidate_pointer)
{
    if (candidate_pointer < GC_least_plausible_heap_addr
	|| candidate_pointer > GC_greatest_plausible_heap_addr)
	return nullptr;

    void* base_pointer = GC_base(candidate_pointer);
    if (base_pointer && GC_is_marked(base_pointer)) {
	return reinterpret_cast<GCNode*>(base_pointer);
    }
    return nullptr;
}
