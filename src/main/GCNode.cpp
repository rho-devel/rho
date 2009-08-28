/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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

#include <cstdlib>
#include <iostream>
#include <limits>
#include "CXXR/GCManager.hpp"
#include "CXXR/GCRoot.h"
#include "CXXR/GCStackRoot.h"
#include "CXXR/WeakRef.h"

using namespace std;
using namespace CXXR;

GCNode::List* GCNode::s_live;
GCNode::List* GCNode::s_moribund;
GCNode::List* GCNode::s_reachable;
unsigned char GCNode::s_mark = 0;
unsigned int GCNode::s_num_nodes = 0;
unsigned int GCNode::s_under_construction = 0;
unsigned int GCNode::s_inhibitor_count = 0;
#ifdef GCID
unsigned int GCNode::s_last_id = 0;
const GCNode* GCNode::s_watch_addr = 0;
unsigned int GCNode::s_watch_id = 0;
#endif

void* GCNode::operator new(size_t bytes)
{
#ifndef RARE_GC
    if (!s_moribund->empty() && s_inhibitor_count == 0)
	gclite();
#endif
    if (MemoryBank::bytesAllocated() > GCManager::triggerLevel()
	&& s_under_construction + s_inhibitor_count == 0) {
#ifdef RARE_GC
	gclite();
#endif
	GCManager::gc();
    }
    return MemoryBank::allocate(bytes);
}

void GCNode::abortIfNotExposed(const GCNode* node)
{
    if (node && (node->m_bits & UNDER_CONSTRUCTION)) {
	cerr << "Internal error: GCNode not exposed to GC.\n";
	abort();
    }
}

bool GCNode::check()
{
    if (s_live == 0) {
	cerr << "GCNode::check() : class not initialised.\n";
	abort();
    }
    unsigned int numnodes = 0;
    unsigned int virgins = 0;
    // Check live list:
    {
	List::const_iterator end = s_live->end();
	for (List::const_iterator it = s_live->begin();
	     it != end; ++it) {
	    const GCNode* node = *it;
	    ++numnodes;
	    if (node->m_bits & MORIBUND) {
		cerr << "GCNode::check() : "
		    " moribund node on live list.\n";
		abort();
	    }
	    if (node->m_refcount == 0)
		++virgins;
	}
    }
    // Check moribund list:
    {
	List::const_iterator end = s_moribund->end();
	for (List::const_iterator it = s_moribund->begin();
	     it != end; ++it) {
	    const GCNode* node = *it;
	    ++numnodes;
	    if (!(node->m_bits & MORIBUND)) {
		cerr << "GCNode::check() : "
		    "Node on moribund list without moribund bit set.\n";
		abort();
	    }
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

void GCNode::cleanup()
{
    GCManager::cleanup();
    GCStackRootBase::cleanup();
    GCRootBase::cleanup();
}

void GCNode::gc()
{
    // cout << "GCNode::gc()\n";
    // GCNode::check();
    // cout << "Precheck completed OK: " << s_num_nodes << " nodes\n";

    if (s_under_construction != 0) {
	cerr << "GCNode::gc() : mark-sweep GC must not be used"
	    " while a GCNode is under construction.\n";
	abort();
    }
    mark();
    sweep();

    // cout << "Finishing garbage collection\n";
    // GCNode::check();
    // cout << "Postcheck completed OK: " << s_num_nodes << " nodes\n";
}

void GCNode::gclite()
{
    /*
      static long k = 0;
      if (++k%1000 == 0)
      check();
    */
    unsigned int protect_count = protectCstructs();
    while (!s_moribund->empty()) {
	// Last in, first out, for cache efficiency:
	GCNode* node = s_moribund->back();
	if (node->m_refcount == 0)
	    delete node;
	else {
	    // Node had been 'resurrected': restore it to the
	    // exposed list:
	    node->m_bits &= ~MORIBUND;
	    s_live->splice_back(node);
	}
    }
    GCStackRootBase::unprotect(protect_count);
}

void GCNode::initialize()
{
    s_live = new List;
    s_moribund = new List;
    s_reachable = new List;
#ifdef GCID
    s_last_id = 0;
    s_watch_addr = 0;
    s_watch_id = 0;
    // To monitor operations on a node with a particular id (or nodes at a
    // particular address), put a breakpoint on the following line, and
    // on arrival at that bp, use the debugger to set s_watch_id (or
    // s_watch_addr) to the required value.  Also set a breakpoint as
    // indicated within the watch() function below.
#endif
    GCRootBase::initialize();
    GCStackRootBase::initialize();
    GCManager::initialize();
}

void GCNode::makeMoribund() const
{
    if (!(m_bits&MORIBUND)) {
#ifdef GCID
	watch();
#endif
	m_bits |= MORIBUND;
	s_moribund->splice_back(this);
    }
}

void GCNode::mark()
{
    // In the first mark-sweep collection, the marking of a node is
    // indicated by the mark bit being set; in the second mark sweep,
    // marking is indicated by the bit being clear, and so on in
    // alternation.  This avoids the need for the sweep phase to
    // iterate through the surviving nodes simply to remove marks.
    s_mark ^= MARK;
    GCNode::Marker marker;
    GCRootBase::visitRoots(&marker);
    unsigned int protect_count = protectCstructs();
    GCStackRootBase::visitRoots(&marker);
    GCStackRootBase::unprotect(protect_count);
    WeakRef::markThru();
}

void GCNode::nodeCheck(const GCNode* node)
{
    if (node && (node->m_bits & ~(UNDER_CONSTRUCTION|MORIBUND|MARK))) abort();
}

// GCNode::protectCstructs() is in memory.cpp

void GCNode::sweep()
{
    List zombies;
    // Detach the referents of nodes that haven't been moved to a
    // reachable list (i.e. are unreachable), and relist these nodes
    // as zombies:
    while (!s_live->empty()) {
	GCNode* node = s_live->front();
	node->detachReferents();
	zombies.splice_back(node);
    }
    // Transfer the s_reachable list to the exposed list:
    s_live->splice_back(s_reachable);
    // The preceding will have resulted in some nodes within
    // unreachable subgraphs getting transferred to the moribund list,
    // rather than being deleted immediately.  Now we clear up this detritus:
    gclite();
    // At this point we can be confident that there will be no further
    // invocation of defRefCount() on the 'zombie' nodes, so we can
    // get rid of them.  The destructor of 'zombies' will do this
    // automatically.
}

#ifdef GCID
void GCNode::watch() const
{
    if ((s_watch_id && m_id == s_watch_id)
	|| (s_watch_addr && this == s_watch_addr))
	// This is just somewhere to put a breakpoint:
	m_bits = m_bits;
}
#endif

bool GCNode::Marker::operator()(const GCNode* node)
{
    if (node->isMarked())
	return false;
    // Update mark:
    node->m_bits = s_mark;
    s_reachable->splice_back(node);
    return true;
}
