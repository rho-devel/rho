/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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

const unsigned int GCManager::s_collect_counts_max[s_num_old_generations]
= {20, 5};
unsigned int GCManager::s_gen_gc_counts[s_num_old_generations + 1];

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

    /* Tuning Constants. Most of these could be made settable from R,
       within some reasonable constraints at least.  Since there are
       quite a lot of constants it would probably make sense to put
       together several "packages" representing different space/speed
       tradeoffs (e.g. very aggressive freeing and small increments to
       conserve memory; much less frequent releasing and larger
       increments to increase speed). */

    /* When a level N collection fails to produce at least MinFreeFrac
       * s_threshold free vector space, the next collection will be a
       level N + 1 collection.

       This constant is also used in heap size adjustment as a minimal
       fraction of the minimal heap size levels that should be
       available for allocation. */
    const double MinFreeFrac = 0.2;

    /* The heap size constant s_threshold is used for triggering
       collections.  The initial value set by default or command line
       argument is used as a minimum value.  After full collections
       this threshold is adjusted up or down, though not below the
       minimal value or above the maximum value, towards maintaining
       heap occupancy within a specified range.  When the number of
       bytes in use reaches BGrowFrac * s_threshold, the value of
       s_threshold is incremented by BGrowIncrMin + BGrowIncrFrac *
       s_threshold.  When the number of bytes in use falls below
       BShrinkFrac, s_threshold is decremented by BShrinkIncrMin *
       BShrinkFrac * s_threshold.

       This mechanism for adjusting the heap size constants is very
       primitive but hopefully adequate for now.  Some modeling and
       experimentation would be useful.  We want the heap sizes to get
       set at levels adequate for the current computations.  The
       present mechanism uses only the size of the current live heap
       to provide information about the current needs; since the
       current live heap size can be very volatile, the adjustment
       mechanism only makes gradual adjustments.  A more sophisticated
       strategy would use more of the live heap history. */
    const double BGrowFrac = 0.70;
    const double BShrinkFrac = 0.30;

#ifdef SMALL_MEMORY
    /* On machines with only 32M of memory (or on a classic Mac OS
       port) it might be a good idea to use settings like these that
       are more aggressive at keeping memory usage down. */
    const double BGrowIncrFrac = 0.0, BShrinkIncrFrac = 0.2;
    const int BGrowIncrMin = 800000, BShrinkIncrMin = 0;
#else
    const double BGrowIncrFrac = 0.05, BShrinkIncrFrac = 0.2;
    const int BGrowIncrMin = 640000, BShrinkIncrMin = 0;
#endif

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
    size_t MinBFree = size_t(s_min_threshold * MinFreeFrac);
    size_t BNeeded = MemoryBank::bytesAllocated() + bytes_needed + MinBFree;
    double occup = double(BNeeded) / s_threshold;
    if (occup > 1.0) s_threshold = BNeeded;
    // This follows memory.c in 2.5.1, but should the following
    // actually read 'else if'?
    if (occup > BGrowFrac)
	s_threshold += size_t(BGrowIncrMin + BGrowIncrFrac*s_threshold);
    else if (occup < BShrinkFrac) {
	s_threshold = size_t(s_threshold - BShrinkIncrMin
			     - BShrinkIncrFrac * s_threshold);
	s_threshold = max(BNeeded, s_threshold);
	s_threshold = max(s_threshold, s_min_threshold);
    }
#ifdef DEBUG_ADJUST_HEAP
    if (s_os) {
	*s_os << "Bytes needed: " << BNeeded
	      << ", Occupancy: " << fixed << setprecision(0) << 100.0*occup
	      << "%, New threshold: " << s_threshold
	      << endl;
    }
#endif
}

bool GCManager::cue(size_t bytes_wanted, bool force)
{
    if (!force && MemoryBank::bytesAllocated() + bytes_wanted < s_threshold)
	return false;
    gc(bytes_wanted);
    return true;
}

void GCManager::enableGC(size_t initial_threshold)
{
    s_min_threshold = s_threshold = initial_threshold;
    gc_count = 0;
    for (unsigned int i = 0; i <= s_num_old_generations; ++i)
	s_gen_gc_counts[i] = 0;
    MemoryBank::setGCCuer(cue);
}

void GCManager::gc(size_t bytes_wanted, bool full)
{
    static bool running_finalizers = false;
    // Prevent recursion:
    if (running_finalizers) return;
    gcGenController(bytes_wanted, full);
    /* Run any eligible finalizers.  The return result of
       RunFinalizers is TRUE if any finalizers are actually run.
       There is a small chance that running finalizers here may
       chew up enough memory to make another immediate collection
       necessary.  If so, we do another collection. */
    running_finalizers = true;
    bool any_finalizers_run = WeakRef::runFinalizers();
    running_finalizers = false;
    if (any_finalizers_run &&
	MemoryBank::bytesAllocated() + bytes_wanted >= s_threshold)
	gcGenController(bytes_wanted, full);
}

void GCManager::gcGenController(size_t bytes_wanted, bool full)
{
    static unsigned int level = 0;
    if (full) level = s_num_old_generations;
    level = genRota(level);

    unsigned int gens_collected;

    gc_count++;

    s_max_bytes = max(s_max_bytes, MemoryBank::bytesAllocated());
    s_max_nodes = max(s_max_nodes, GCNode::numNodes());

    /*BEGIN_SUSPEND_INTERRUPTS { */
    if (s_pre_gc) (*s_pre_gc)();

    bool ok = false;
    while (!ok) {
	ok = true;
	GCNode::gc(level);
	gens_collected = level;

	/* update heap statistics */
	if (level < s_num_old_generations) {
	    if (MemoryBank::bytesAllocated() + bytes_wanted
		> (1.0 - MinFreeFrac)*s_threshold) {
		level++;
		if (MemoryBank::bytesAllocated() + bytes_wanted
		    >= s_threshold)
		    ok = false;
	    }
	    else level = 0;
	}
	else level = 0;
    }

    s_gen_gc_counts[gens_collected]++;

    if (gens_collected == s_num_old_generations) {
	/**** do some adjustment for intermediate collections? */
	adjustThreshold(bytes_wanted);
    }
    if (s_post_gc) (*s_post_gc)();
    /* } END_SUSPEND_INTERRUPTS;*/

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
}

unsigned int GCManager::genRota(unsigned int minlevel)
{
    static unsigned int collect_counts[s_num_old_generations];
    unsigned int level = minlevel;
    for (unsigned int i = 0; i < level; ++i)
	collect_counts[i] = 0;
    while (level < s_num_old_generations
	   && ++collect_counts[level] > s_collect_counts_max[level]) {
	collect_counts[level] = 0;
	++level;
    }
    return level;
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
