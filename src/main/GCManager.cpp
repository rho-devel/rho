/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2007 Andrew Runnalls.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file GCManager.cpp
 *
 * Class GCManager and associated C-callable functions.
 */

#include "CXXR/GCManager.hpp"

#include <cmath>
#include <limits>
#include "R_ext/Print.h"
#include "CXXR/GCNode.hpp"

using namespace std;
using namespace CXXR;

size_t GCManager::s_threshold;
size_t GCManager::s_max_threshold = numeric_limits<size_t>::max();
size_t GCManager::s_min_threshold;

namespace {
    int gc_reporting = 0;
    int gc_count = 0;

#define GC_TORTURE

#ifdef GC_TORTURE
# define FORCE_GC !gc_inhibit_torture
#else
# define FORCE_GC 0
#endif

#define GC_PROT(X) {int __t = gc_inhibit_torture;		\
	gc_inhibit_torture = 1 ; X ; gc_inhibit_torture = __t;}

    void R_gc_internal(size_t size_needed);

    /* Tuning Constants. Most of these could be made settable from R,
       within some reasonable constraints at least.  Since there are
       quite a lot of constants it would probably make sense to put
       together several "packages" representing different space/speed
       tradeoffs (e.g. very aggressive freeing and small increments to
       conserve memory; much less frequent releasing and larger
       increments to increase speed). */

    /* There are three levels of collections.  Level 0 collects only
       the youngest generation, level 1 collects the two youngest
       generations, and level 2 collects all generations.  Higher
       level collections occur at least after specified numbers of
       lower level ones.  After LEVEL_0_FREQ level zero collections a
       level 1 collection is done; after every LEVEL_1_FREQ level 1
       collections a level 2 collection occurs.  Thus, roughly, every
       LEVEL_0_FREQ-th collection is a level 1 collection and every
       (LEVEL_0_FREQ * LEVEL_1_FREQ)-th collection is a level 2
       collection.  */
#define LEVEL_0_FREQ 20
#define LEVEL_1_FREQ 5
    const int collect_counts_max[] = { LEVEL_0_FREQ, LEVEL_1_FREQ };

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
    void DEBUG_GC_SUMMARY(int full_gc)
    {
	int gen, OldCount;
	REprintf("\n%s, VSize = %lu", full_gc ? "Full" : "Minor",
		 Heap::bytesAllocated()/sizeof(VECREC));
	for (gen = 0, OldCount = 0; gen < GCNode::s_num_old_generations; gen++)
	    OldCount += GCNode::s_oldcount[gen];
	REprintf(", %d", OldCount);
    }
#else
#define DEBUG_GC_SUMMARY(x)
#endif /* DEBUG_GC */

#ifdef DEBUG_ADJUST_HEAP
    void DEBUG_ADJUST_HEAP_PRINT(double occup)
    {
	REprintf("Occupancy: %.0f%%\n", 100.0 * occup);
	REprintf("Total allocation: %lu\n", Heap::bytesAllocated());
	REprintf("Threshold %lu\n", GCManager::s_threshold);
    }
#else
#define DEBUG_ADJUST_HEAP_PRINT(occup)
#endif /* DEBUG_ADJUST_HEAP */
}

static void RunGenCollect(size_t size_needed)
{
    static unsigned int num_old_gens_to_collect = 0;
    static int gen_gc_counts[GCNode::s_num_old_generations + 1];
    static int collect_counts[GCNode::s_num_old_generations];
    unsigned int gens_collected;

    /* determine number of generations to collect */
    while (num_old_gens_to_collect < GCNode::s_num_old_generations) {
	if (collect_counts[num_old_gens_to_collect]-- <= 0) {
	    collect_counts[num_old_gens_to_collect] =
		collect_counts_max[num_old_gens_to_collect];
	    num_old_gens_to_collect++;
	}
	else break;
    }

    bool ok = false;
    while (!ok) {
	ok = true;
	GCNode::gc(num_old_gens_to_collect);
	gens_collected = num_old_gens_to_collect;

	/* update heap statistics */
	if (num_old_gens_to_collect < GCNode::s_num_old_generations) {
	    if (Heap::bytesAllocated() + size_needed
		> (1.0 - MinFreeFrac)*GCManager::s_threshold) {
		num_old_gens_to_collect++;
		if (Heap::bytesAllocated() + size_needed
		    >= GCManager::s_threshold)
		    ok = false;
	    }
	    else num_old_gens_to_collect = 0;
	}
	else num_old_gens_to_collect = 0;
    }

    gen_gc_counts[gens_collected]++;

    if (gens_collected == GCNode::s_num_old_generations) {
	/**** do some adjustment for intermediate collections? */
	GCManager::adjustThreshold(size_needed);

	if (gc_reporting) {
	    REprintf("Garbage collection %d = %d", gc_count, gen_gc_counts[0]);
	    for (unsigned int i = 0; i < GCNode::s_num_old_generations; i++)
		REprintf("+%d", gen_gc_counts[i + 1]);
	    REprintf(" (level %d) ... ", gens_collected);
	    DEBUG_GC_SUMMARY(gens_collected == GCNode::s_num_old_generations);
	}
    }
}

void GCManager::adjustThreshold(size_t bytes_needed)
{
    size_t MinBFree = size_t(s_min_threshold * MinFreeFrac);
    size_t BNeeded = Heap::bytesAllocated() + bytes_needed + MinBFree;
    double byte_occup = double(BNeeded) / s_threshold;
    if (byte_occup > 1.0 && BNeeded < s_max_threshold)
	s_threshold = BNeeded;
    if (byte_occup > BGrowFrac) {
	size_t change
	    = size_t(BGrowIncrMin + BGrowIncrFrac*s_threshold);
	if (s_max_threshold - s_threshold >= change)
	    s_threshold += change;
    }
    else if (byte_occup < BShrinkFrac) {
	s_threshold = size_t(s_threshold - BShrinkIncrMin
			     - BShrinkIncrFrac * s_threshold);
	if (s_threshold < BNeeded)
	    s_threshold = min(BNeeded, s_max_threshold);
	s_threshold = max(s_threshold, s_min_threshold);
    }
    DEBUG_ADJUST_HEAP_PRINT(byte_occup);
}

bool GCManager::cue(size_t bytes_wanted, bool force)
{
    if (!force && Heap::bytesAllocated() + bytes_wanted < s_threshold)
	return false;
    gc(bytes_wanted);
    return true;
}

// Put this prototype here temporarily!
bool RunFinalizers();

void GCManager::gc(size_t bytes_wanted, bool /*ignored for the mo*/)
{
    bool first = true;
    bool ok = false;
    while (!ok) {
	ok = true;
	gc_count++;

	s_max_bytes = max(s_max_bytes, Heap::bytesAllocated());

	/*BEGIN_SUSPEND_INTERRUPTS*/ {
	    /* gc_start_timing();*/
	    RunGenCollect(bytes_wanted);
	    /*gc_end_timing();*/
	} /*END_SUSPEND_INTERRUPTS*/;

	if (gc_reporting) {
	    double bytes = Heap::bytesAllocated();
	    double bfrac = (100.0 * bytes) / s_threshold;
	    bytes = 0.1*ceil(10.0*bytes/1048576.0);  // 2^20
	    REprintf("%.1f Mbytes used (%d%%)\n",
		     bytes, int(bfrac + 0.5));
	}

	if (first) {
	    first = false;
	    /* Run any eligible finalizers.  The return result of
	       RunFinalizers is TRUE if any finalizers are actually run.
	       There is a small chance that running finalizers here may
	       chew up enough memory to make another immediate collection
	       necessary.  If so, we jump back to the beginning and run
	       the collection, but on this second pass we do not run
	       finalizers. */
	    if (RunFinalizers() &&
		Heap::bytesAllocated() + bytes_wanted >= s_threshold)
		ok = false;
	}
    }
}

void GCManager::initialize(size_t initial_threshold)
{
    GCNode::initialize();
    s_min_threshold = s_threshold = initial_threshold;
    Heap::setGCCuer(cue);
}

void GCManager::setMaxThreshold(size_t newmax)
{
    s_max_threshold = max(s_max_threshold, newmax);
}
