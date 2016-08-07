/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file MemoryBank.cpp
 *
 * Implementation of class MemoryBank
 */

#include "rho/MemoryBank.hpp"

#include <iostream>
#include <limits>
#include <iterator>

using namespace std;
using namespace rho;

// If NO_CELLPOOLS is defined, all memory blocks are allocated
// directly via ::operator new.  Combined with address sanitizer
// (or valgrind's memcheck tool) and AGGRESSIVE_GC, this makes
// finding memory protection errors very simple.
#ifdef NO_CELLPOOLS
const size_t MemoryBank::s_new_threshold = 1;
#else
const size_t MemoryBank::s_new_threshold = 193;
#endif

size_t MemoryBank::s_blocks_allocated = 0;
size_t MemoryBank::s_bytes_allocated = 0;
#ifdef R_MEMORY_PROFILING
void (*MemoryBank::s_monitor)(size_t) = 0;
size_t MemoryBank::s_monitor_threshold = numeric_limits<size_t>::max();
#endif

MemoryBank::Pool* MemoryBank::s_pools = nullptr;

// Note that the C++ standard requires that an operator new returns a
// valid pointer even when 0 bytes are requested.  The entry at
// s_pooltab[0] ensures this.  This table assumes sizeof(double) == 8.
const unsigned char MemoryBank::s_pooltab[]
= {0, 0,                    // 8
   1,                       // 16
   2,                       // 24
   3,                       // 32
   4,                       // 40
   5,                       // 48
   6, 6,                    // 64
   7, 7, 7, 7,              // 96
   8, 8, 8, 8,              // 128
   9, 9, 9, 9, 9, 9, 9, 9}; // 192

#ifdef ALLOC_STATS
// Allocation and free frequency tables for profiling the allocator.
// Allocation stats are collected into 8-byte bins, allocations > 256 bytes are
// counted in the 256 byte bin.
size_t alloc_counts[32];
size_t free_counts[32];
#endif

// Computes the bin to update in an allocation frequency table.
static int get_bin_number(size_t bytes) {
    if (bytes >= 32 * 8) {
        // Allocations of size >= 256 are mapped to the 256-byte bin.
        return 31;
    } else if (bytes >= 8) {
        // Allocations of size n*8 to n*8-1 are mapped to the n-byte bin.
        return (31 & (bytes / 8)) - 1;
    } else {
        // Allocations of size < 8 are mapped to the 8-byte bin.
        return 0;
    }
}

void MemoryBank::adjustFreedSize(size_t original, size_t actual)
{
    adjustBytesAllocated(original - actual);
#ifdef ALLOC_STATS
    free_counts[get_bin_number(original)] -= 1;
    free_counts[get_bin_number(actual)] += 1;
#endif
}

void* MemoryBank::allocate(size_t bytes) throw (std::bad_alloc)
{
    notifyAllocation(bytes);
    void* p;
    if (bytes >= s_new_threshold)
	p = ::operator new(bytes);
    else {
	Pool& pool = s_pools[s_pooltab[(bytes + 7) >> 3]];
	p = pool.allocate();
    }
    return p;
}

void MemoryBank::check()
{
#ifndef NO_CELLPOOLS
    for (unsigned int i = 0; i < s_num_pools; ++i)
	s_pools[i].check();
#endif
}

void MemoryBank::defragment()
{
#ifndef NO_CELLPOOLS
    for (unsigned int i = 0; i < s_num_pools; ++i)
	s_pools[i].defragment();
#endif
}    

void MemoryBank::initialize()
{
    // The following leave some space at the end of each 4096-byte
    // page, in case posix_memalign needs to put some housekeeping
    // information for the next page there.
    s_pools = new Pool[s_num_pools];
    s_pools[0].initialize(1, 511);
    s_pools[1].initialize(2, 255);
    s_pools[2].initialize(3, 170);
    s_pools[3].initialize(4, 127);
    s_pools[4].initialize(5, 102);
    s_pools[5].initialize(6, 85);
    s_pools[6].initialize(8, 63);
    s_pools[7].initialize(12, 42);
    s_pools[8].initialize(16, 31);
    s_pools[9].initialize(24, 21);
}

void MemoryBank::notifyAllocation(size_t bytes)
{
#ifdef R_MEMORY_PROFILING
    if (s_monitor && bytes >= s_monitor_threshold) s_monitor(bytes);
#endif
    ++s_blocks_allocated;
    s_bytes_allocated += bytes;
#ifdef ALLOC_STATS
    alloc_counts[get_bin_number(bytes)] += 1;
#endif
}

void MemoryBank::notifyDeallocation(size_t bytes)
{
    s_bytes_allocated -= bytes;
    --s_blocks_allocated;
#ifdef ALLOC_STATS
    free_counts[get_bin_number(bytes)] += 1;
#endif
}

#ifdef R_MEMORY_PROFILING
void MemoryBank::setMonitor(void (*monitor)(size_t), size_t threshold)
{
    s_monitor = monitor;
    s_monitor_threshold
	= (monitor ? threshold : numeric_limits<size_t>::max());
}
#endif
