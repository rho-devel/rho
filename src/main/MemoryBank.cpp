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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
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

void MemoryBank::pool_out_of_memory(CellPool* pool)
{
    if (s_cue_gc) s_cue_gc(pool->superblockSize(), false);
}

CellPool* MemoryBank::s_pools[5];

// Note that the C++ standard requires that an operator new returns a
// valid pointer even when 0 bytes are requested.  The entry at
// s_pooltab[0] ensures this.  This table assumes sizeof(double) == 8.
unsigned int MemoryBank::s_pooltab[]
= {0, 0, 0, 0, 0, 0, 0, 0, 0,
   1, 1, 1, 1, 1, 1, 1, 1,
   2, 2, 2, 2, 2, 2, 2, 2,
   2, 2, 2, 2, 2, 2, 2, 2,
   3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4};
    
void* MemoryBank::alloc2(size_t bytes) throw (std::bad_alloc)
{
    CellPool* pool = 0;
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
	    char* tail = reinterpret_cast<char*>(p) + bytes;
	    VALGRIND_MAKE_MEM_NOACCESS(tail, surplus);
	}
    }
#endif
#ifdef R_MEMORY_PROFILING
    if (bytes >= s_threshold && s_monitor) s_monitor(bytes);
#endif
    return p;
}
				
void MemoryBank::check()
{
    for (unsigned int i = 0; i < 5; ++i)
	s_pools[i]->check();
}

// Deleting heap objects on program exit is not strictly necessary,
// but doing so makes bugs more conspicuous when using valgrind.
void MemoryBank::cleanup()
{
    for (unsigned int i = 0; i < 5; ++i)
	delete s_pools[i];
}

void MemoryBank::initialize()
{
    s_pools[0] = new CellPool(1, 512, pool_out_of_memory);
    s_pools[1] = new CellPool(2, 256, pool_out_of_memory);
    s_pools[2] = new CellPool(4, 128, pool_out_of_memory);
    s_pools[3] = new CellPool(8, 64, pool_out_of_memory);
    s_pools[4] = new CellPool(16, 32, pool_out_of_memory);
}

#ifdef R_MEMORY_PROFILING
void MemoryBank::setMonitor(void (*monitor)(size_t), size_t threshold)
{
    s_monitor = monitor;
    s_threshold = (monitor ? threshold : numeric_limits<size_t>::max());
}
#endif
