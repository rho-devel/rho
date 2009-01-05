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

/** @file MemoryBank.cpp
 *
 * Implementation of class MemoryBank
 */

#include "CXXR/MemoryBank.hpp"

#include <iostream>

#ifdef R_MEMORY_PROFILING
#include <limits>
#endif

using namespace std;
using namespace CXXR;

unsigned int MemoryBank::SchwarzCtr::s_count = 0;
unsigned int MemoryBank::s_blocks_allocated = 0;
unsigned int MemoryBank::s_bytes_allocated = 0;
bool (*MemoryBank::s_cue_gc)(size_t, bool) = 0;
#ifdef R_MEMORY_PROFILING
void (*MemoryBank::s_monitor)(size_t) = 0;
size_t MemoryBank::s_threshold = numeric_limits<size_t>::max();
#endif

void MemoryBank::pool_out_of_memory(CellHeap* pool)
{
    if (s_cue_gc) s_cue_gc(pool->superblockSize(), false);
}

CellHeap* MemoryBank::s_pools[s_num_pools];

// Note that the C++ standard requires that an operator new returns a
// valid pointer even when 0 bytes are requested.  The entry at
// s_pooltab[0] ensures this.  This table assumes sizeof(double) == 8.
const unsigned int MemoryBank::s_pooltab[]
= {0, 0, 0, 0, 0, 0, 0, 0, 0, // 8
   1, 1, 1, 1, 1, 1, 1, 1, // 16
   2, 2, 2, 2, 2, 2, 2, 2, // 24
   3, 3, 3, 3, 3, 3, 3, 3, // 32
   4, 4, 4, 4, 4, 4, 4, 4, // 40
   5, 5, 5, 5, 5, 5, 5, 5, // 48
   6, 6, 6, 6, 6, 6, 6, 6,
   6, 6, 6, 6, 6, 6, 6, 6, // 64
   7, 7, 7, 7, 7, 7, 7, 7,
   7, 7, 7, 7, 7, 7, 7, 7, // 80
   8, 8, 8, 8, 8, 8, 8, 8,
   8, 8, 8, 8, 8, 8, 8, 8, // 96
   9, 9, 9, 9, 9, 9, 9, 9,
   9, 9, 9, 9, 9, 9, 9, 9,
   9, 9, 9, 9, 9, 9, 9, 9,
   9, 9, 9, 9, 9, 9, 9, 9}; // 128

MemoryBank::SchwarzCtr::SchwarzCtr()
{
    if (!s_count++) MemoryBank::initialize();
}

MemoryBank::SchwarzCtr::~SchwarzCtr()
{
    if (!--s_count) MemoryBank::cleanup();
}
    
void* MemoryBank::allocate(size_t bytes) throw (std::bad_alloc)
{
#ifdef R_MEMORY_PROFILING
    if (s_monitor && bytes >= s_threshold) s_monitor(bytes);
#endif
#if VALGRIND_LEVEL >= 3
    size_t blockbytes = bytes + 1;  // trailing redzone
#else
    size_t blockbytes = bytes;
#endif
    // Assumes sizeof(double) == 8:
    void* p;
    p = (blockbytes > s_max_cell_size
	 || !(p = alloc1(blockbytes))) ? alloc2(blockbytes) : p;
#if VALGRIND_LEVEL >= 3
    char* c = static_cast<char*>(p);
    VALGRIND_MAKE_MEM_NOACCESS(c + bytes, 1);
    s_bytes_allocated -= 1;
#endif
    return p;
}

void* MemoryBank::alloc2(size_t bytes) throw (std::bad_alloc)
{
    CellHeap* pool = 0;
    void* p = 0;
    bool joy = false;  // true if GC succeeds after bad_alloc
    try {
	if (bytes > s_max_cell_size) {
	    if (s_cue_gc) s_cue_gc(bytes, false);
	    p = ::operator new(bytes);
	} else {
	    pool = s_pools[s_pooltab[bytes]];
	    p = pool->allocate();
	}
    }
    catch (bad_alloc) {
	if (s_cue_gc) {
	    // Try to force garbage collection if available:
	    size_t sought_bytes = (pool ? pool->superblockSize() : bytes);
	    joy = s_cue_gc(sought_bytes, true);
	}
	else throw;
    }
    if (!p && joy) {
	// Try once more:
	p = (pool ? pool->allocate() : ::operator new(bytes));
    }
    ++s_blocks_allocated;
    s_bytes_allocated += bytes;
#if VALGRIND_LEVEL >= 2
    if (pool) {
	// Fence off supernumerary bytes:
	size_t surplus = pool->cellSize() - bytes;
	if (surplus > 0) {
	    char* tail = static_cast<char*>(p) + bytes;
	    VALGRIND_MAKE_MEM_NOACCESS(tail, surplus);
	}
    }
#endif
    return p;
}
				
void MemoryBank::check()
{
    for (unsigned int i = 0; i < s_num_pools; ++i)
	s_pools[i]->check();
}

// Deleting heap objects on program exit is not strictly necessary,
// but doing so makes bugs more conspicuous when using valgrind.
void MemoryBank::cleanup()
{
    for (unsigned int i = 0; i < s_num_pools; ++i)
	delete s_pools[i];
}

// The following leave some space at the end of each 4096-byte page,
// in case posix_memalign needs to put some housekeeping information
// for the next page there.
void MemoryBank::initialize()
{
    s_pools[0] = new CellHeap(1, 496, pool_out_of_memory);
    s_pools[1] = new CellHeap(2, 248, pool_out_of_memory);
    s_pools[2] = new CellHeap(3, 165, pool_out_of_memory);
    s_pools[3] = new CellHeap(4, 124, pool_out_of_memory);
    s_pools[4] = new CellHeap(5, 99, pool_out_of_memory);
    s_pools[5] = new CellHeap(6, 83, pool_out_of_memory);
    s_pools[6] = new CellHeap(8, 62, pool_out_of_memory);
    s_pools[7] = new CellHeap(10, 49, pool_out_of_memory);
    s_pools[8] = new CellHeap(12, 41, pool_out_of_memory);
    s_pools[9] = new CellHeap(16, 31, pool_out_of_memory);
}

#ifdef R_MEMORY_PROFILING
void MemoryBank::setMonitor(void (*monitor)(size_t), size_t threshold)
{
    s_monitor = monitor;
    s_threshold = (monitor ? threshold : numeric_limits<size_t>::max());
}
#endif
