/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  Andrew Runnalls
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file Heap.hpp
 *
 * Class Heap
 */

#ifndef CXXR_HEAP_HPP
#define CXXR_HEAP_HPP

#include "CXXR/CellPool.hpp"

namespace CXXR {
    /** Class to manage memory allocation and deallocation for CXXR.
     * 
     * Small objects are quickly allocated from CellPools of various cell
     * sizes; large objects are obtained directly from the main heap.
     */
    class Heap {
    public:
	/** Allocate a block of memory.
	 *
	 * @param bytes Required size in bytes of the block.
	 *
	 * @return a pointer to the allocated cell.
	 *
	 * @throws bad_alloc if a cell cannot be allocated.
	 */
	static void* allocate(size_t bytes) throw (std::bad_alloc)
	{
	    void* p;
	    // Assumes sizeof(double) == 8:
	    return (bytes > 128 || !(p = alloc1(bytes))) ? alloc2(bytes) : p;
	}

	/**
	 * @return the number of blocks of memory currently allocated.
	 */
	static unsigned int blocksAllocated() {return s_blocks_allocated;}

	/**
	 * @return the number of bytes of memory currently allocated.
	 *
	 * @note This refers to the total number of bytes \e requested
	 * in blocks that have been allocated but not subsequently
	 * deallocated.  Actual utilisation of memory in the main heap
	 * may be greater than this, possibly by as much as a factor
	 * of 2.
	 */
	static unsigned int bytesAllocated() {return s_bytes_allocated;}

	/** Integrity check.
	 *
	 * Aborts the program with an error message if the class is
	 * found to be internally inconsistent.
	 */
	static void check();

	/** Deallocate a block
	 *
	 * @param p Pointer to a block of memory previously allocated
	 *          by Heap::allocate(), or a null pointer (in which
	 *          case method does nothing).
	 *
	 * @param bytes Size in bytes of the block being deallocated.
	 *          Ignored if p is a null pointer.
	 */
	static void deallocate(void* p, size_t bytes)
	{
	    if (!p) return;
	    // Assumes sizeof(double) == 8:
	    size_t dbls = (bytes + 7) >> 3;
	    if (dbls > 16) ::operator delete(p);
	    else s_pools[s_pooltab[dbls]].deallocate(p);
	    --s_blocks_allocated;
	    s_bytes_allocated -= bytes;
	}
    private:
	static unsigned int s_blocks_allocated;
	static unsigned int s_bytes_allocated;
	static CellPool s_pools[];
	static unsigned int s_pooltab[];

	// First-line allocation attempt for small objects:
	static void* alloc1(size_t bytes) throw()
	{
	    // Assumes sizeof(double) == 8:
	    void* p = s_pools[s_pooltab[(bytes + 7) >> 3]].easyAllocate();
	    if (p) {
		++s_blocks_allocated;
		s_bytes_allocated += bytes;
	    }
	    return p;
	}

	// Allocation of large objects, and second-line allocation
	// attempt for small objects:
	static void* alloc2(size_t bytes) throw (std::bad_alloc);
    };
}

#endif /* CXXR_HEAP_HPP */
