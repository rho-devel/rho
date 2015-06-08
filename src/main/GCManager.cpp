/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file GCManager.cpp
 *
 * Class GCManager and associated C-callable functions.
 */

#include "CXXR/GCManager.hpp"

#include <cmath>
#include <cstdarg>
#include <iomanip>
#include <iostream>
#include <limits>
#include "R_ext/Print.h"
#include "CXXR/GCNode.hpp"
#include "CXXR/WeakRef.h"

using namespace CXXR;

unsigned int GCManager::s_inhibitor_count = 0;

size_t GCManager::s_threshold = std::numeric_limits<size_t>::max();
size_t GCManager::s_min_threshold = s_threshold;
const size_t GCManager::s_gclite_margin = 10000;
size_t GCManager::s_gclite_threshold = s_gclite_margin;
bool GCManager::s_gc_is_running = false;
size_t GCManager::s_max_bytes = 0;
size_t GCManager::s_max_nodes = 0;

std::ostream* GCManager::s_os = nullptr;

void (*GCManager::s_pre_gc)() = nullptr;
void (*GCManager::s_post_gc)() = nullptr;

namespace {
    unsigned int gc_count = 0;


#ifdef DEBUG_GC
    // This ought to go in GCNode.
    void DEBUG_GC_SUMMARY(int full_gc)
    {
	int gen, OldCount;
	REprintf("\n%s, VSize = %lu", full_gc ? "Full" : "Minor",
		 MemoryBank::bytesAllocated()/sizeof(VECREC));
	for (gen = 0, OldCount = 0; gen < num_old_generations; gen++)
	    OldCount += GCNode::s_oldcount[gen];
	REprintf(", %d", OldCount);
    }
#else
#define DEBUG_GC_SUMMARY(x)
#endif /* DEBUG_GC */
}

void GCManager::gc(bool force_full_collection)
{
    // Prevent recursion:
    if (s_gc_is_running)
	return;
    s_gc_is_running = true;
    ++gc_count;

    s_max_bytes = std::max(s_max_bytes, MemoryBank::bytesAllocated());
    s_max_nodes = std::max(s_max_nodes, GCNode::numNodes());

    if (s_pre_gc) (*s_pre_gc)();

    GCNode::gc(false);

    if (force_full_collection || MemoryBank::bytesAllocated() > s_threshold) {
	GCNode::gc(true);
	s_threshold = std::max(size_t(0.9*double(s_threshold)),
			       std::max(s_min_threshold,
					2*MemoryBank::bytesAllocated()));
    }

    s_gclite_threshold = MemoryBank::bytesAllocated() + s_gclite_margin;

    if (s_post_gc) (*s_post_gc)();

    s_gc_is_running = false;
}

void GCManager::resetMaxTallies()
{
    s_max_bytes = MemoryBank::bytesAllocated();
    s_max_nodes = GCNode::numNodes();
}

void GCManager::setGCThreshold(size_t initial_threshold)
{
    s_min_threshold = s_threshold = initial_threshold;
}

std::ostream* GCManager::setReporting(std::ostream* os)
{
    std::ostream* ans = s_os;
    s_os = os;
    return ans;
}
