/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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

#ifndef __APPLE__
#define HAVE_POSIX_MEMALIGN
#endif

#ifdef HAVE_POSIX_MEMALIGN
#define _XOPEN_SOURCE 600

#include <cstdlib>
#endif

#include <iostream>

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
    const Cell* cp = reinterpret_cast<const Cell*>(p);
    if (isFreeCell(m_free_cells, cp)) {
	cerr << "CellHeap::checkCell : designated block"
	    " is (already) free.\n";
	abort();
    }
}

void CellHeap::checkCell(const void* p) const
{
    if (!p) return;
    const char* pc = reinterpret_cast<const char*>(p);
    bool found = false;
    for (vector<void*>::const_iterator it = m_superblocks.begin();
	 !found && it != m_superblocks.end(); ++it) {
	ptrdiff_t offset = pc - reinterpret_cast<const char*>(*it);
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
    const Cell* c = reinterpret_cast<const Cell*>(p);
    if ((c->m_l && c->m_l < c) || (c->m_r && c->m_r < c)) {
	cerr << "CellHeap::checkCell : "
	    "child with lower address than parent.\n";
	abort();
    }
}

unsigned int CellHeap::countFreeCells(const Cell* root)
{
    return root
	? 1 + countFreeCells(root->m_l) + countFreeCells(root->m_r) : 0;
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
    Cell* al = a;
    Cell* bl = b;
    if (!al) return bl;
    if (!bl) return al;
    if (al > bl) std::swap(al, bl);
    std::swap(al->m_l, al->m_r);
    al->m_l = al->m_l ? meld(al->m_l, bl) : bl;
    return al;
}
*/

void CellHeap::meld_aux(Cell* host, Cell* guest)
{
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
    if (m_out_of_cells) (*m_out_of_cells)(this);
    if (!m_free_cells) {
#ifdef HAVE_POSIX_MEMALIGN
	void* memblock;
	// We assume the memory page size is some multiple of 4096 bytes:
	if (0 != posix_memalign(&memblock, 4096, m_superblocksize)) {
	    cerr << "Unable to allocate CellHeap memory.\n";
	    abort();
	}
	char* superblock = reinterpret_cast<char*>(memblock);
#else
	char* superblock = reinterpret_cast<char*>(malloc(m_superblocksize));
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
