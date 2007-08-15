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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file Heap.cpp
 *
 * Implementation of class Heap
 */

#include "CXXR/Heap.hpp"

#include <iostream>
#include "CXXR/CellPool.hpp"

using namespace std;
using namespace CXXR;

unsigned int Heap::s_blocks_allocated = 0;

namespace {
    vector<CellPool*> pools(5);

    unsigned int pooltab[]
    = {0, 0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4};
}
    
void* Heap::allocate(size_t bytes) throw (std::bad_alloc)
{
    if (!pools[0]) {
	// Create pools:
	pools[0] = new CellPool(1, 512);
	pools[1] = new CellPool(2, 256);
	pools[2] = new CellPool(4, 128);
	pools[3] = new CellPool(8, 64);
	pools[4] = new CellPool(16, 32);
    }
    size_t dbls = (bytes + sizeof(double) - 1)/sizeof(double);
    if (dbls == 0) ++dbls;
    ++s_blocks_allocated;
    try {
	if (dbls > 16) return ::operator new(bytes);
	return pools[pooltab[dbls]]->allocate();
    }
    catch (bad_alloc) {
	--s_blocks_allocated;
	throw;
    }
}
				
void Heap::check()
{
    if (pools[0]) for (vector<CellPool*>::const_iterator it = pools.begin();
		       it != pools.end(); ++it)
	(*it)->check();
}

void  Heap::deallocate(void* p, size_t bytes)
{
    if (!p) return;
    size_t dbls = (bytes + sizeof(double) - 1)/sizeof(double);
    if (dbls > 16) ::operator delete(p);
    else pools[pooltab[dbls]]->deallocate(p);
    --s_blocks_allocated;
}

