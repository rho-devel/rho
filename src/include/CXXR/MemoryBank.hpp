/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file MemoryBank.hpp
 *
 * @brief Class CXXR::MemoryBank
 */

#ifndef MEMORYBANK_HPP
#define MEMORYBANK_HPP

#include <cstring>
#include "config.h"
#include "CXXR/CellPool.hpp"
#include "CXXR/SchwarzCounter.hpp"

namespace CXXR {
    /** @brief Class to manage memory allocation and deallocation for CXXR.
     * 
     * Small objects are quickly allocated from pools of various cell
     * sizes; large objects are obtained directly from the main heap.
     *
     * If the class is compiled with the FILL55 preprocessor variable
     * defined, released memory blocks are filled with 0x55 bytes
     * (though some of these may be overwritten by data used for free
     * list management).  This can be useful to show up premature
     * deallocation of memory blocks, especially if used in conjunction
     * with the CELLFIFO preprocessor variable documented in
     * config.hpp .
     */
    class MemoryBank {
    public:
	/** @brief Allocate a block of memory.
	 *
	 * @param bytes Required size in bytes of the block.
	 *
	 * @return a pointer to the allocated cell.
	 *
	 * @throws bad_alloc if a cell cannot be allocated.
	 */
#ifdef __GNUC__
	__attribute__((hot,fastcall))
#endif
	static void* allocate(size_t bytes) throw (std::bad_alloc);

	/** @brief Number of blocks currently allocated.
	 *
	 * @return the number of blocks of memory currently allocated.
	 */
	static size_t blocksAllocated() {return s_blocks_allocated;}

	/** @brief Number of bytes currently allocated.
	 *
	 * @return the number of bytes of memory currently allocated.
	 *
	 * @note This refers to the total number of bytes \e requested
	 * in blocks that have been allocated but not subsequently
	 * deallocated.  Actual utilisation of memory in the main heap
	 * may be greater than this, possibly by as much as a factor
	 * of 2.
	 */
	static size_t bytesAllocated() {return s_bytes_allocated;}

	/** @brief Integrity check.
	 *
	 * Aborts the program with an error message if the class is
	 * found to be internally inconsistent.
	 */
	static void check();

	/** @brief Deallocate a block
	 *
	 * @param p Pointer to a block of memory previously allocated
	 *          by MemoryBank::allocate(), or a null pointer (in which
	 *          case method does nothing).
	 *
	 * @param bytes The number of bytes in the memory block,
	 *          i.e. the number of bytes requested in the
	 *          corresponding call to allocate().
	 */
	static void deallocate(void* p, size_t bytes)
	{
	    if (!p) return;
#ifdef FILL55
	    // This helps to diagnose premature GC:
	    memset(p, 0x55, bytes);
#endif
	    // Assumes sizeof(double) == 8:
	    if (bytes >= s_new_threshold)
		::operator delete(p);
	    else s_pools[s_pooltab[(bytes + 7) >> 3]].deallocate(p);
	    --s_blocks_allocated;
	    s_bytes_allocated -= bytes;
	}

	/** @brief Reorganise lists of free cells.
	 *
	 * This is done with a view to increasing the probability that
	 * successive allocations will lie within the same cache line
	 * or (less importantly nowadays) memory page.
	 */
	static void defragment();

#ifdef R_MEMORY_PROFILING
	/** Set a callback to monitor allocations exceeding a threshold size.
	 *
	 * @param monitor This is a pointer to a function that this
	 *          class will call when it allocates a block
	 *          exceeding a threshold size.  The function is
	 *          called with the first argument set to the number
	 *          of bytes allocated.  Alternatively, monitor can be
	 *          set to a null pointer to discontinue monitoring.
	 *
	 * @param threshold The monitor will only be called for
	 *          allocations of at least this many bytes.  Ignored
	 *          if monitor is a null pointer.
	 *
	 * @note This function is available only if R_MEMORY_PROFILING
	 * is defined, e.g. by specifying option
	 * --enable-memory-profiling to the configure script.
	 */
	static void setMonitor(void (*monitor)(size_t) = 0,
			       size_t threshold = 0);
#endif
    private:
	typedef CellPool Pool;
	static const size_t s_num_pools = 10;
	// We use ::operator new directly for allocations at least this big:
	static const size_t s_new_threshold;
	static size_t s_blocks_allocated;
	static size_t s_bytes_allocated;
	static Pool* s_pools;
	static const unsigned char s_pooltab[];
#ifdef R_MEMORY_PROFILING
	static void (*s_monitor)(size_t);
	static size_t s_monitor_threshold;
#endif

	// Not implemented.  Declared to stop the compiler generating
	// a constructor.
	MemoryBank();

	// Free memory used by the static data members:
	static void cleanup() {}

	// Initialize the static data members:
	static void initialize();

	friend class SchwarzCounter<MemoryBank>;
    };
}

namespace {
    CXXR::SchwarzCounter<CXXR::MemoryBank> memorybank_schwarz_ctr;
}

#endif /* MEMORYBANK_HPP */
