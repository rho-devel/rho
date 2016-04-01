/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file CellPool.hpp
 *
 * @brief Class rho::CellPool.
 */

#ifndef CELLPOOL_HPP
#define CELLPOOL_HPP

#include <cstddef>
#include <new>
#include <vector>

#include "rho/config.hpp"

namespace rho {
    /** @brief Class to manage a pool of memory cells of a fixed size.
     * 
     * This class, based closely on Item 10 of Scott Meyers' 'Effective
     * C++ (2nd edition)' manages a collection of memory cells of a
     * specified size, and is intended as a back-end to implementations of
     * operator new and operator delete to enable the allocation and
     * deallocation of small objects quickly.
     *
     * Class CellPool operates a last-in-first-out allocation
     * policy; that is to say, if there are cells that have been
     * deallocated and not yet reallocated, the next one to be
     * reallocated will be the one that was most recently
     * deallocated.  This makes for efficient utilisation of the
     * processor caches.
     */
    class CellPool {
    public:
	/** @brief Constructor.
	 *
	 * Note that CellPool objects must be initialized by calling
	 * initialize() before being used.
	 */
	CellPool()
	    : m_free_cells(nullptr),
	      m_admin(nullptr)
	{}

	/** Destructor
	 *
	 * It is up to the user to check that any cells allocated from
	 * the pool have been freed before this destructor is
	 * invoked.  (Although the destructor could check this for
	 * itself and issue an error message, this message would
	 * probably be a nuisance if it occurred during program shutdown.)
	 */
        ~CellPool();

	/**
	 * @brief Allocate a cell from the pool.
	 *
	 * @return a pointer to the allocated cell.
	 *
	 * @throws bad_alloc if a cell cannot be allocated.
	 */
	void* allocate() throw (std::bad_alloc)
	{
	    if (!m_free_cells)
		m_free_cells = seekMemory();
	    Cell* c = m_free_cells;
	    m_free_cells = c->m_next;
	    return c;
	}

	/** @brief Size of cells.
	 *
	 * @return the size of each cell in bytes (well, strictly as a
	 * multiple of sizeof(char)).
	 */         
	size_t cellSize() const
	{
	    return m_admin->m_cellsize;
	}

	/** @brief Number of cells allocated from this CellPool.
	 *
	 * @return the number of cells currently allocated from this
	 * pool.
	 */
	size_t cellsAllocated() const
	{
	    return m_admin->cellsExisting() - cellsFree();
	}

	/** @brief Integrity check.
	 *
	 * Aborts the program with an error message if the object is
	 * found to be internally inconsistent.
	 *
	 * @return true, if it returns at all.  The return value is to
	 * facilitate use with \c assert .
	 */
	bool check() const;

	/** @brief Deallocate a cell
	 *
	 * @param p Pointer to a block of memory previously allocated
	 * from this pool, or a null pointer (in which case method
	 * does nothing).
	 */
	void deallocate(void* p)
	{
	    if (!p) return;
#ifdef DEBUG_RELEASE_MEM
	    checkAllocatedCell(p);
#endif
	    Cell* c = static_cast<Cell*>(p);
	    c->m_next = m_free_cells;
	    m_free_cells = c;
	}

	/** @brief Reorganise list of free cells within the CellPool.
	 *
	 * This is done with a view to increasing the probability that
	 * successive allocations will lie within the same cache line
	 * or (less importantly nowadays) memory page.
	 */
	void defragment();

	/** @brief Initialize the CellPool.
	 *
	 * This function must be called exactly once for each
	 * CellPool, before any allocation is made from it.
	 *
	 * @param dbls_per_cell (must be >= 1). Size of cells,
	 *         expressed as a multiple of sizeof(double).  For
	 *         example, if you require cells large enough to
	 *         contain one double, put dbls_per_cell as 1.  (NB:
	 *         cells can contain anything, not just doubles; we
	 *         work in doubles because these are likely to have
	 *         the most stringent address alignment requirements.)
	 *
	 * @param cells_per_superblock (must be >= 1).  Memory for cells is
	 *         obtained from the main heap in 'superblocks'
	 *         sufficient to contain this many cells.
	 */
	void initialize(size_t dbls_per_cell, size_t cells_per_superblock);

	/**
	 * @return The size in bytes of the superblocks from which
	 *         cells are allocated.
	 */
	size_t superblockSize() const
	{
	    return m_admin->m_superblocksize;
	}
    private:
	struct Cell {
	    Cell* m_next;

	    Cell(Cell* next = nullptr) : m_next(next) {}
	};

	// We put data fields that are used relatively rarely in a
	// separate data structure stored on the heap, so that an
	// array of CellPool objects, as used in MemoryBank, can be as
	// compact as possible.
	struct Admin {
	    const size_t m_cellsize;
	    const size_t m_cells_per_superblock;
	    const size_t m_superblocksize;
	    std::vector<void*> m_superblocks;

	    Admin(size_t dbls_per_cell, size_t cells_per_superblock)
		: m_cellsize(dbls_per_cell*sizeof(double)),
		  m_cells_per_superblock(cells_per_superblock),
		  m_superblocksize(m_cellsize*cells_per_superblock)
	    {}

	    size_t cellsExisting() const
	    {
		return m_cells_per_superblock*m_superblocks.size();
	    }
	};

	Cell* m_free_cells;

	Admin* m_admin;

	// Number of cells on the free list:
	unsigned int cellsFree() const;

	// Checks that p is either null or points to a cell belonging
	// to this pool; aborts if not.
	void checkCell(const void* p) const;

	// Calls checkCell, and further checks that the cell is not on
	// the free list:
	void checkAllocatedCell(const void* p) const;

	// Allocates a new superblock and returns a pointer to the
	// first free cell in it.
	Cell* seekMemory() throw (std::bad_alloc);
    };
}

#endif /* CELLPOOL_HPP */
