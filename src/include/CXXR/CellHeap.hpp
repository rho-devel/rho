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

/** @file CellHeap.hpp
 *
 * @brief Class CXXR::CellHeap.
 */

#ifndef CELLHEAP_HPP
#define CELLHEAP_HPP

#include <cstddef>
#include <new>
#include <vector>

#include "Rvalgrind.h"

namespace CXXR {
    /** Class to manage a pool of memory cells of a fixed size.
     * 
     * This class manages a collection of memory cells of a
     * specified size, and is intended as a back-end to implementations of
     * operator new and operator delete to enable the allocation and
     * deallocation of small objects quickly.  It differs from
     * CellPool in that it always allocates the cell with the minimum
     * address from among the currently available cells.  This is
     * achieved using a skew heap data structure (Sleator and Tarjan
     * 1986).  This data structure works most efficiently if cells are
     * as far as possible released in the reverse order of allocation.
     */
    class CellHeap {
    public:
	/**
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
	CellHeap(size_t dbls_per_cell, size_t cells_per_superblock)
	    : m_cellsize(dbls_per_cell*sizeof(double)),
	      m_cells_per_superblock(cells_per_superblock),
	      m_superblocksize(m_cellsize*cells_per_superblock),
	      m_free_cells(0),
	      m_cells_allocated(0)
	{
#if VALGRIND_LEVEL >= 2
	    VALGRIND_CREATE_MEMPOOL(this, 0, 0);
#endif
	}

	/** Destructor
	 *
	 * It is up to the user to check that any cells allocated from
	 * the heap have been freed before this destructor is
	 * invoked.  (Although the destructor could check this for
	 * itself and issue an error message, this message would
	 * probably be a nuisance if it occurred during program shutdown.)
	 */
        ~CellHeap();

	/**
	 * @brief Allocate a cell from the heap.
	 *
	 * @return a pointer to the allocated cell.
	 *
	 * @throws bad_alloc if a cell cannot be allocated.
	 */
	void* allocate() throw (std::bad_alloc)
	{
	    if (!m_free_cells) seekMemory();
	    // check();
	    Cell* c = m_free_cells;
	    m_free_cells = meld(c->m_l, c->m_r);
	    ++m_cells_allocated;
#if VALGRIND_LEVEL >= 2
	    VALGRIND_MEMPOOL_ALLOC(this, c, cellSize());
#endif
	    // check();
	    return c;
	}

	/**
	 * @return the size of each cell in bytes (well, strictly as a
	 * multiple of sizeof(char)).
	 */         
	size_t cellSize() const  {return m_cellsize;}

	/**
	 * @return the number of cells currently allocated from this
	 * heap.
	 */
	unsigned int cellsAllocated() const {return m_cells_allocated;}

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
	 * from this heap, or a null pointer (in which case method
	 * does nothing).
	 */
	void deallocate(void* p);
	/*
	{
	    if (!p) return;
#ifdef DEBUG_RELEASE_MEM
	    checkAllocatedCell(p);
#endif
#if VALGRIND_LEVEL >= 2
	    VALGRIND_MEMPOOL_FREE(this, p);
	    VALGRIND_MAKE_MEM_UNDEFINED(p, sizeof(Cell));
#endif
	    // check();
	    Cell* c = new (p) Cell;
	    m_free_cells = meld(c, m_free_cells);
	    --m_cells_allocated;
	    // check();
	}
	*/

	/** @brief Allocate a cell 'from stock'.
	 *
	 * Allocate a cell from the heap, provided it can be allocated
	 * 'from stock'.  Can be useful when called from other inlined
	 * functions in that it doesn't throw any exceptions.
	 *
	 * @return a pointer to the allocated cell, or 0 if the cell
	 * cannot be allocated from the current memory superblocks.
	 */
	void* easyAllocate() throw ()
	{
	    if (!m_free_cells) return 0;
	    // check();
	    Cell* c = m_free_cells;
	    m_free_cells = meld(c->m_l, c->m_r);
	    ++m_cells_allocated;
#if VALGRIND_LEVEL >= 2
	    VALGRIND_MEMPOOL_ALLOC(this, c, cellSize());
#endif
	    // check();
	    return c;
	}

	/**
	 * @return The size in bytes of the superblocks from which
	 *         cells are allocated.
	 */
	size_t superblockSize() const {return m_superblocksize;}
    private:
	struct Cell {
	    Cell* m_l;
	    Cell* m_r;

	    Cell(Cell* next = 0) : m_l(next), m_r(0) {}
	};

	const size_t m_cellsize;
	const size_t m_cells_per_superblock;
	const size_t m_superblocksize;
	std::vector<void*> m_superblocks;
	Cell* m_free_cells;
	unsigned int m_cells_allocated;

	// Checks that p is either null or points to a cell belonging
	// to this heap; aborts if not.
	void checkCell(const void* p) const;

	// Calls checkCell, and further checks that the cell is not on
	// the free list:
	void checkAllocatedCell(const void* p) const;

	// Return number of cells in the heap at root, in the process
	// checking that cells are organised in increasing address
	// order, and that no cell has a right child but no left child.
	static unsigned int countFreeCells(const Cell* root);

	// Return true iff c belongs to the heap at root:
	static bool isFreeCell(const Cell* root, const Cell* c);

	// Combine the heaps pointed to by a and b into a single heap.
	// a and b must pointers to disjoint skew heaps.  b may be a
	// null pointer, and a may be a null pointer provided b is too.
	static Cell* meld(Cell* a, Cell* b)
	{
	    if (!b) return a;
	    if (a > b) std::swap(a, b);
	    if (!a->m_l)
		a->m_l = b;
	    else meld_aux(a, b);
	    return a;
	}

	// Auxiliary function for meld:
	static void meld_aux(Cell* host, Cell* guest);

	void seekMemory() throw (std::bad_alloc);
    };
}

#endif /* CELLHEAP_HPP */
