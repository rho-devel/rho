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

using namespace std;
using namespace CXXR;

unsigned int Heap::s_blocks_allocated = 0;
unsigned int Heap::s_bytes_allocated = 0;

CellPool Heap::s_pools[] = {CellPool(1, 512),
			    CellPool(2, 256),
			    CellPool(4, 128),
			    CellPool(8, 64),
			    CellPool(16, 32)};

// Note that the C++ standard requires that an operator new returns a
// valid pointer even when 0 bytes are requested.  The entry at
// s_pooltab[0] ensures this.
unsigned int Heap::s_pooltab[]
= {0, 0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4};
    
void* Heap::alloc2(size_t bytes) throw (std::bad_alloc)
{
    // Assumes sizeof(double) == 8:
    size_t dbls = (bytes + 7) >> 3;
    ++s_blocks_allocated;
    s_bytes_allocated += bytes;
    try {
	if (dbls > 16) return ::operator new(bytes);
	return s_pools[s_pooltab[dbls]].allocate();
    }
    catch (bad_alloc) {
	--s_blocks_allocated;
	s_bytes_allocated -= bytes;
	throw;
    }
}
				
void Heap::check()
{
    for (unsigned int i = 0; i < 5; ++i)
	s_pools[i].check();
}
