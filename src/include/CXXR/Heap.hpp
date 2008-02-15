/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  Andrew Runnalls
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

/** @file Heap.hpp
 *
 * @brief Class CXXR::Heap
 */

#ifndef CXXR_HEAP_HPP
#define CXXR_HEAP_HPP

#include "CXXR/CellPool.hpp"

namespace CXXR {
    /** @brief Class to manage memory allocation and deallocation for CXXR.
     * 
     * Small objects are quickly allocated from CellPools of various cell
     * sizes; large objects are obtained directly from the main heap.
     */
    class Heap {
    public:
	/** @brief Schwarz counter.
	 *
	 * The Schwarz counter (see for example Stephen C. Dewhurst's
	 * book 'C++ Gotchas') is a programming idiom to ensure that a
	 * class (including particularly its static members) is
	 * initialized before any client of the class requires to use
	 * it, and that on program exit the class's static resources
	 * are not cleaned up prematurely (e.g. while the class is
	 * still in use by another class's static members).  Devices
	 * such as this are necessitated by the fact that the standard
	 * does not prescribe the order in which objects of file and
	 * global scope in different compilation units are
	 * initialized: it only specifies that the order of
	 * destruction must be the reverse of the order of
	 * initialization.
	 *
	 * This is achieved by the unusual stratagem of including the
	 * \e definition of a lightweight data item within this header
	 * file.  This data item is of type Heap::SchwarzCtr, and is
	 * declared within an anonymous namespace.  Each file that
	 * <tt>\#include</tt>s this header file will therefore include
	 * a definition of a SchwarzCtr object, and this definition
	 * will precede any data definitions within the enclosing file
	 * that depend on class Heap.  Consequently, the SchwarzCtr
	 * object will be constructed before any data objects of the
	 * client file.  The constructor of SchwarzCtr is so defined
	 * that when the first such object is created, the class Heap
	 * will itself be initialized.
	 *
	 * Conversely, when the program exits, data items within each
	 * client file will have their destructors invoked before the
	 * file's SchwarzCtr object has its destructor invoked.  This
	 * SchwarzCtr destructor is so defined that only when the last
	 * SchwarzCtr object is destroyed is the Heap class itself
	 * cleaned up.
	 */
	class SchwarzCtr {
	public:
	    SchwarzCtr()
	    {
		if (!s_count++) Heap::initialize();
	    }

	    ~SchwarzCtr()
	    {
		if (!--s_count) Heap::cleanup();
	    }
	private:
	    static unsigned int s_count;
	};

	/** @brief Allocate a block of memory.
	 *
	 * @param bytes Required size in bytes of the block.
	 *
	 * @return a pointer to the allocated cell.
	 *
	 * @throws bad_alloc if a cell cannot be allocated.
	 */
	static void* allocate(size_t bytes) throw (std::bad_alloc)
	{
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
	    char* c = reinterpret_cast<char*>(p);
	    VALGRIND_MAKE_MEM_NOACCESS(c + bytes, 1);
	    s_bytes_allocated -= 1;
#endif
	    return p;
	}

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
	 *          by Heap::allocate(), or a null pointer (in which
	 *          case method does nothing).
	 */
	static void deallocate(void* p, size_t bytes)
	{
	    if (!p) return;
#if VALGRIND_LEVEL >= 3
	    size_t blockbytes = bytes + 1;  // trailing redzone
	    char* c = reinterpret_cast<char*>(p);
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
	 * @param cue_gc This is a pointer to a function that this
	 *         class will call before it attempts to allocate
	 *         memory from the main heap (second argument set to
	 *         false), or has just failed to allocate memory from
	 *         the heap (second argument set to true).  The first
	 *         argument is the amount of memory, in bytes, sought
	 *         from the main heap.  The function should return
	 *         false if it is certain that no release of memory
	 *         too place (e.g. because no garbage collection was
	 *         carried out on this occasion), and otherwise return
	 *         true.
	 */
	static void setGCCuer(bool (*cue_gc)(size_t, bool) = 0)
	{
	    s_cue_gc = cue_gc;
	}

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
	static const size_t s_max_cell_size = 128;
	static unsigned int s_blocks_allocated;
	static unsigned int s_bytes_allocated;
	static bool (*s_cue_gc)(size_t, bool);
	static CellPool* s_pools[];
	static unsigned int s_pooltab[];
#ifdef R_MEMORY_PROFILING
	static void (*s_monitor)(size_t);
	static size_t s_threshold;
#endif

	// Not implemented.  Declared to stop the compiler generating
	// a constructor.
	Heap();

	// First-line allocation attempt for small objects:
	static void* alloc1(size_t bytes) throw()
	{
	    CellPool* pool = s_pools[s_pooltab[bytes]];
	    void* p = pool->easyAllocate();
	    if (p) {
		++s_blocks_allocated;
		s_bytes_allocated += bytes;
#if VALGRIND_LEVEL >= 2
		// Fence off supernumerary bytes:
		size_t surplus = pool->cellSize() - bytes;
		if (surplus > 0) {
		    char* tail = reinterpret_cast<char*>(p) + bytes;
		    VALGRIND_MAKE_MEM_NOACCESS(tail, surplus);
		}
#endif
	    }
#ifdef R_MEMORY_PROFILING
	    if (bytes >= s_threshold && s_monitor) s_monitor(bytes);
#endif
	    return p;
	}

	// Allocation of large objects, and second-line allocation
	// attempt for small objects:
	static void* alloc2(size_t bytes) throw (std::bad_alloc);

	// Free memory used by the static data members:
	static void cleanup();

	// Initialize the static data members:
	static void initialize();

	static void pool_out_of_memory(CellPool* pool);

	friend class SchwarzCtr;
    };
}

namespace {
    CXXR::Heap::SchwarzCtr schwarz_ctr;
}

#endif /* CXXR_HEAP_HPP */
