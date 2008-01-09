/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
 */

/** @file Heap.cpp
 *
 * Implementation of class Heap
 */

#include "CXXR/Heap.hpp"

#include <iostream>

#ifdef R_MEMORY_PROFILING
#include <limits>
#endif

using namespace std;
using namespace CXXR;

unsigned int Heap::SchwarzCtr::s_count = 0;
unsigned int Heap::s_blocks_allocated = 0;
unsigned int Heap::s_bytes_allocated = 0;
bool (*Heap::s_cue_gc)(size_t, bool) = 0;
#ifdef R_MEMORY_PROFILING
void (*Heap::s_monitor)(size_t) = 0;
size_t Heap::s_threshold = numeric_limits<size_t>::max();
#endif

void Heap::pool_out_of_memory(CellPool* pool)
{
    if (s_cue_gc) s_cue_gc(pool->superblockSize(), false);
}

CellPool* Heap::s_pools[5];

// Note that the C++ standard requires that an operator new returns a
// valid pointer even when 0 bytes are requested.  The entry at
// s_pooltab[0] ensures this.  This table assumes sizeof(double) == 8.
unsigned int Heap::s_pooltab[]
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
    
void* Heap::alloc2(size_t bytes) throw (std::bad_alloc)
{
    void* p = 0;
    bool joy = false;  // true if GC succeeds after bad_alloc
    try {
	if (bytes > s_max_cell_size) {
	    if (s_cue_gc) s_cue_gc(bytes, false);
	    p = ::operator new(bytes);
	}
	else p = s_pools[s_pooltab[bytes]]->allocate();
    }
    catch (bad_alloc) {
	if (s_cue_gc) {
	    // Try to force garbage collection if available:
	    size_t sought_bytes = bytes;
	    if (bytes < s_max_cell_size)
		sought_bytes = s_pools[s_pooltab[bytes]]->superblockSize();
	    joy = s_cue_gc(sought_bytes, true);
	}
	else throw;
    }
    if (!p && joy) {
	// Try once more:
	try {
	    if (bytes > s_max_cell_size) p = ::operator new(bytes);
	    else p = s_pools[s_pooltab[bytes]]->allocate();
	}
	catch (bad_alloc) {
	    throw;
	}
    }
    ++s_blocks_allocated;
    s_bytes_allocated += bytes;
#if VALGRIND_LEVEL >= 2
    if (bytes <= s_max_cell_size) VALGRIND_MAKE_MEM_UNDEFINED(p, bytes);
#endif
#ifdef R_MEMORY_PROFILING
    if (bytes >= s_threshold && s_monitor) s_monitor(bytes);
#endif
    return p;
}
				
void Heap::check()
{
    for (unsigned int i = 0; i < 5; ++i)
	s_pools[i]->check();
}

// Deleting heap objects on program exit is not strictly necessary,
// but doing so makes bugs more conspicuous when using valgrind.
void Heap::cleanup()
{
    for (unsigned int i = 0; i < 5; ++i)
	delete s_pools[i];
}

void Heap::initialize()
{
    s_pools[0] = new CellPool(1, 512, pool_out_of_memory);
    s_pools[1] = new CellPool(2, 256, pool_out_of_memory);
    s_pools[2] = new CellPool(4, 128, pool_out_of_memory);
    s_pools[3] = new CellPool(8, 64, pool_out_of_memory);
    s_pools[4] = new CellPool(16, 32, pool_out_of_memory);
}

#ifdef R_MEMORY_PROFILING
void Heap::setMonitor(void (*monitor)(size_t), size_t threshold)
{
    s_monitor = monitor;
    s_threshold = (monitor ? threshold : numeric_limits<size_t>::max());
}
#endif
