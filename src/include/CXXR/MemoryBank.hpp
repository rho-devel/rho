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
#include "CXXR/CellHeap.hpp"
#include "CXXR/SchwarzCounter.hpp"

namespace CXXR {
    /** @brief Class to manage memory allocation and deallocation for CXXR.
     * 
     * Small objects are quickly allocated from CellHeaps of various cell
     * sizes; large objects are obtained directly from the main heap.
     */
    class MemoryBank {
    public:
	/** @brief Allocate a block of memory.
	 *
	 * @param bytes Required size in bytes of the block.
	 *
	 * @param alloc_gc If false, the call will under no
	 *          circumstances cue garbage collection.
	 *
	 * @return a pointer to the allocated cell.
	 *
	 * @throws bad_alloc if a cell cannot be allocated.
	 */
	static void* allocate(size_t bytes, bool allow_gc = true)
	    throw (std::bad_alloc);

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
	 *
	 * @note If redzoning is operation (<tt>VALGRIND_LEVEL >=
	 * 2</tt>), the value returned does not include the size of the
	 * redzones.
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
	 */
	static void deallocate(void* p, size_t bytes)
	{
	    if (!p) return;
	    // Uncommenting this helps to diagnose premature GC:
	    // memset(p, 0x55, bytes);
#if VALGRIND_LEVEL >= 3
	    size_t blockbytes = bytes + 1;  // trailing redzone
	    char* c = static_cast<char*>(p);
	    VALGRIND_MAKE_MEM_UNDEFINED(c + bytes, 1);
#else
	    size_t blockbytes = bytes;
#endif
	    // Assumes sizeof(double) == 8:
	    if (blockbytes > s_max_cell_size) ::operator delete(p);
	    else s_pools[s_pooltab[blockbytes]]->deallocate(p);
	    --s_blocks_allocated;
	    s_bytes_allocated -= bytes;
	}

	/** @brief Set a callback to cue garbage collection.
	 *
	 * @param cue_gc This is a pointer, possibly null, to a
	 *          function that this class will call to cue garbage 
	 *          collection, either because the garbage collection
	 *          threshold has been exceeded, or because the class
	 *          has just failed to allocate memory from the main
	 *          heap.  The argument is set to the number of bytes
	 *          of memory currently being sought.  The function
	 *          should return the new value of the garbage
	 *          collection threshold.  If \a cue_gc is a null
	 *          pointer, then such callbacks are discontinued.
	 *
	 * @param initial_threshold The initial threshold for garbage
	 *          collection.  If garbage collection is allowed,
	 *          allocate() will call \a cue_gc when it looks as if
	 *          the number of bytes allocated via MemoryBank is
	 *          about to exceed the threshold.  The parameter is
	 *          ignored if \a cue_gc is a null pointer.
	 */
	static void setGCCuer(size_t (*cue_gc)(size_t),
			      size_t initial_threshold);

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
	static const size_t s_num_pools = 10;
	static const size_t s_max_cell_size = 128;
	static size_t s_blocks_allocated;
	static size_t s_bytes_allocated;
	static size_t s_gc_threshold;
	static size_t (*s_cue_gc)(size_t);
	static CellHeap* s_pools[];
	static const unsigned int s_pooltab[];
#ifdef R_MEMORY_PROFILING
	static void (*s_monitor)(size_t);
	static size_t s_monitor_threshold;
#endif

	// Not implemented.  Declared to stop the compiler generating
	// a constructor.
	MemoryBank();

	// First-line allocation attempt for small objects:
	static void* alloc1(size_t bytes) throw()
	{
	    CellHeap* pool = s_pools[s_pooltab[bytes]];
	    void* p = pool->easyAllocate();
	    if (p) {
		++s_blocks_allocated;
		s_bytes_allocated += bytes;
#if VALGRIND_LEVEL >= 2
		// Fence off supernumerary bytes:
		size_t surplus = pool->cellSize() - bytes;
		if (surplus > 0) {
		    char* tail = static_cast<char*>(p) + bytes;
		    VALGRIND_MAKE_MEM_NOACCESS(tail, surplus);
		}
#endif
	    }
#ifdef R_MEMORY_PROFILING
	    if (s_monitor && bytes >= s_monitor_threshold) s_monitor(bytes);
#endif
	    return p;
	}

	// Allocation of large objects, and second-line allocation
	// attempt for small objects:
	static void* alloc2(size_t bytes, bool alloc_gc)
	    throw (std::bad_alloc);

	// Free memory used by the static data members:
	static void cleanup();

	// Initialize the static data members:
	static void initialize();

	static void pool_out_of_memory(CellHeap* pool);

	friend class SchwarzCounter<MemoryBank>;
    };
}

namespace {
    CXXR::SchwarzCounter<CXXR::MemoryBank> memorybank_schwarz_ctr;
}

#endif /* MEMORYBANK_HPP */
