/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
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
#include <limits>

using namespace std;
using namespace CXXR;

// If NO_CELLPOOLS is defined, all memory blocks are allocated
// directly via ::operator new.  This enables valgrind's memcheck tool
// to do a more thorough job.  (Previously we tried instrumenting
// class CellPool with Valgrind client requests, but the result was
// intolerably slow running.)
#ifdef NO_CELLPOOLS
const size_t MemoryBank::s_max_cell_size = 0;
#else
const size_t MemoryBank::s_max_cell_size = 128;
#endif

size_t MemoryBank::s_blocks_allocated = 0;
size_t MemoryBank::s_bytes_allocated = 0;
#ifdef R_MEMORY_PROFILING
void (*MemoryBank::s_monitor)(size_t) = 0;
size_t MemoryBank::s_monitor_threshold = numeric_limits<size_t>::max();
#endif

MemoryBank::Pool* MemoryBank::s_pools[s_num_pools];

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

void* MemoryBank::allocate(size_t bytes) throw (std::bad_alloc)
{
#ifdef R_MEMORY_PROFILING
    if (s_monitor && bytes >= s_monitor_threshold) s_monitor(bytes);
#endif
    void* p;
    if (bytes > s_max_cell_size)
	p = ::operator new(bytes);
    else {
	Pool* pool = s_pools[s_pooltab[bytes]];
	p = pool->allocate();
    }
    ++s_blocks_allocated;
    s_bytes_allocated += bytes;
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

void MemoryBank::defragment()
{
    for (unsigned int i = 0; i < s_num_pools; ++i)
	s_pools[i]->defragment();
}    

// The following leave some space at the end of each 4096-byte page,
// in case posix_memalign needs to put some housekeeping information
// for the next page there.
void MemoryBank::initialize()
{
    s_pools[0] = new Pool(1, 496);
    s_pools[1] = new Pool(2, 248);
    s_pools[2] = new Pool(3, 165);
    s_pools[3] = new Pool(4, 124);
    s_pools[4] = new Pool(5, 99);
    s_pools[5] = new Pool(6, 83);
    s_pools[6] = new Pool(8, 62);
    s_pools[7] = new Pool(10, 49);
    s_pools[8] = new Pool(12, 41);
    s_pools[9] = new Pool(16, 31);
}

#ifdef R_MEMORY_PROFILING
void MemoryBank::setMonitor(void (*monitor)(size_t), size_t threshold)
{
    s_monitor = monitor;
    s_monitor_threshold
	= (monitor ? threshold : numeric_limits<size_t>::max());
}
#endif
