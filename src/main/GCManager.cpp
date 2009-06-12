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

/** @file GCManager.cpp
 *
 * Class GCManager and associated C-callable functions.
 */

#include "CXXR/GCManager.hpp"

#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include "R_ext/Print.h"
#include "CXXR/GCNode.hpp"
#include "CXXR/WeakRef.h"

using namespace std;
using namespace CXXR;

size_t GCManager::s_threshold;
size_t GCManager::s_min_threshold;
size_t GCManager::s_max_bytes = 0;
size_t GCManager::s_max_nodes = 0;
bool GCManager::s_tortured = false;
ostream* GCManager::s_os = 0;

void (*GCManager::s_pre_gc)() = 0;
void (*GCManager::s_post_gc)() = 0;

namespace {
    unsigned int gc_count;


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

void GCManager::adjustThreshold(size_t bytes_needed)
{
    s_threshold = max(s_min_threshold,
		      2*(MemoryBank::bytesAllocated() + bytes_needed));
}

void GCManager::cue(size_t bytes_wanted)
{
    GCNode::gclite();
    if (MemoryBank::bytesAllocated() + bytes_wanted >= s_threshold)
	gc(bytes_wanted);
}

void GCManager::enableGC(size_t initial_threshold)
{
    s_min_threshold = s_threshold = initial_threshold;
    gc_count = 0;
    MemoryBank::setGCCuer(cue);
}

void GCManager::gc(size_t bytes_wanted)
{
    static bool running_finalizers = false;
    // Prevent recursion:
    if (running_finalizers) return;
    gcController(bytes_wanted);
    /* Run any eligible finalizers.  The return result of
       RunFinalizers is TRUE if any finalizers are actually run.
       There is a small chance that running finalizers here may
       chew up enough memory to make another immediate collection
       necessary.  If so, we do another collection. */
    running_finalizers = true;
    WeakRef::runFinalizers();
    running_finalizers = false;
}

void GCManager::gcController(size_t bytes_wanted)
{
    gc_count++;

    s_max_bytes = max(s_max_bytes, MemoryBank::bytesAllocated());
    s_max_nodes = max(s_max_nodes, GCNode::numNodes());

    /*BEGIN_SUSPEND_INTERRUPTS { */
    if (s_pre_gc) (*s_pre_gc)();
    GCNode::gc();
    adjustThreshold(bytes_wanted);
    if (s_post_gc) (*s_post_gc)();
    /* } END_SUSPEND_INTERRUPTS;*/

    /*
    if (s_os) {
	*s_os << "Garbage collection " << gc_count
	      << " = " << s_gen_gc_counts[0];
	for (unsigned int i = 0; i < s_num_old_generations; ++i)
	    *s_os << "+" << s_gen_gc_counts[i + 1];
	*s_os << " (level " << gens_collected << ") ... ";
	DEBUG_GC_SUMMARY(gens_collected == num_old_generations);
	double bytes = MemoryBank::bytesAllocated();
	double bfrac = (100.0 * bytes) / s_threshold;
	double mbytes = 0.1*ceil(10.0*bytes/1048576.0);  // 2^20
	*s_os << '\n' << fixed << setprecision(1)
	      << mbytes << " Mbytes used ("
	      << int(bfrac + 0.5) << "%)\n";
    }
    */
}

void GCManager::resetMaxTallies()
{
    s_max_bytes = MemoryBank::bytesAllocated();
    s_max_nodes = GCNode::numNodes();
}

ostream* GCManager::setReporting(ostream* os)
{
    ostream* ans = s_os;
    s_os = os;
    return ans;
}
