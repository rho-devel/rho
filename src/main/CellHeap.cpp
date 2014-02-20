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

/** @file CellHeap.cpp
 *
 * Implementation of class CellHeap
 *
 * @todo Include valgrind instrumentation (conditionally compiled).
 */


#include "CXXR/CellHeap.hpp"

#include <cstdlib>
#include <iostream>

#ifndef __APPLE__
#define HAVE_POSIX_MEMALIGN
#endif

#ifdef HAVE_POSIX_MEMALIGN
#define _XOPEN_SOURCE 600
#endif

using namespace std;
using namespace CXXR;

CellHeap::~CellHeap()
{
#if VALGRIND_LEVEL >= 2
    VALGRIND_DESTROY_MEMPOOL(this);
#endif
    for (vector<void*>::iterator it = m_superblocks.begin();
	 it != m_superblocks.end(); ++it)
	free(*it);
}

bool CellHeap::check() const
{
    unsigned int free_cells = countFreeCells(m_free_cells);
    if (m_cells_allocated + free_cells
	!= m_cells_per_superblock*m_superblocks.size()) {
	cerr << "CellHeap::check(): internal inconsistency\n";
	abort();
    }
    return true;
}

void CellHeap::checkAllocatedCell(const void* p) const 
{
    checkCell(p);
    const Cell* cp = static_cast<const Cell*>(p);
    if (isFreeCell(m_free_cells, cp)) {
	cerr << "CellHeap::checkCell : designated block"
	    " is (already) free.\n";
	abort();
    }
}

void CellHeap::checkCell(const void* p) const
{
    if (!p) return;
    const char* pc = static_cast<const char*>(p);
    bool found = false;
    for (vector<void*>::const_iterator it = m_superblocks.begin();
	 !found && it != m_superblocks.end(); ++it) {
	ptrdiff_t offset = pc - static_cast<const char*>(*it);
	if (offset >= 0 && offset < static_cast<long>(m_superblocksize)) {
	    found = true;
	    if (offset%m_cellsize != 0) {
		cerr << "CellHeap::checkCell : designated block"
		        " is misaligned\n";
		abort();
	    }
	}
    }
    if (!found) {
	cerr << "CellHeap::checkCell : "
	    "designated block doesn't belong to this CellHeap\n";
	abort();
    }
    const Cell* c = static_cast<const Cell*>(p);
    if ((c->m_l && c->m_l < c) || (c->m_r && c->m_r < c)) {
	cerr << "CellHeap::checkCell : "
	    "child with lower address than parent.\n";
	abort();
    }
}

unsigned int CellHeap::countFreeCells(const Cell* root)
{
    if (!root) return 0;
    unsigned int ans = 1;
    if (root->m_l) {
	if (root >= root->m_l) abort();
	ans += countFreeCells(root->m_l);
    }
    if (root->m_r) {
	if (!root->m_l) abort();
	if (root >= root->m_r) abort();
	ans += countFreeCells(root->m_r);
    }
    return ans;
}

// Having this inlined was causing errors when optimising at -O2 with
// gcc (4.1.2 and 4.2.1) leading up to 2008/07/17.
void CellHeap::deallocate(void* p)
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

bool CellHeap::isFreeCell(const Cell* root, const Cell* c)
{
    if (!root || !c) return false;
    return c == root || isFreeCell(root->m_l, c)
	|| isFreeCell(root->m_r, c);
}

/*
CellHeap::Cell* CellHeap::meld(Cell* a, Cell* b)
{
    if (!b) return a;
    if (a > b) swap(a, b);
    if (!a->m_l)
	a->m_l = b;
    else meld_aux(a, b);
    return a;
}
*/

void CellHeap::meld_aux(Cell* host, Cell* guest)
{
    swap(host->m_l, host->m_r);
    while (host->m_l) {
	// if (host >= host->m_l) abort();
	// if (host->m_r && host >= host->m_r) abort();
	// if (guest->m_l && guest >= guest->m_l) abort();
	// if (guest->m_r && guest >= guest->m_r) abort();
	if (guest < host->m_l)
	    swap(host->m_l, guest);
	host = host->m_l;
	swap(host->m_l, host->m_r);
    }
    host->m_l = guest;
}

void CellHeap::seekMemory() throw (std::bad_alloc)
{
    if (!m_free_cells) {
#ifdef HAVE_POSIX_MEMALIGN
	void* memblock;
	// We assume the memory page size is some multiple of 4096 bytes:
	if (0 != posix_memalign(&memblock, 4096, m_superblocksize)) {
	    cerr << "Unable to allocate CellHeap memory.\n";
	    abort();
	}
	char* superblock = static_cast<char*>(memblock);
#else
	char* superblock = static_cast<char*>(malloc(m_superblocksize));
	if (!superblock) {
		cerr << "Unable to allocate CellHeap memory.\n";
	    abort();
	}
#endif
	//	cout << "Superblock at " << memblock << " for cell size "
	//     << m_cellsize << endl;
	m_superblocks.push_back(superblock);
	// Initialise cells:
	{
	    int offset = m_superblocksize - m_cellsize;
	    Cell* next = 0;
	    while (offset >= 0) {
		next = new (superblock + offset) Cell(next);
		// cout << "Cell created at " << next << "\n";
#if VALGRIND_LEVEL >= 2
		VALGRIND_MAKE_NOACCESS(next + 1, m_cellsize - sizeof(Cell));
#endif
		offset -= m_cellsize;
	    }
	    m_free_cells = next;
	}
    }
}
