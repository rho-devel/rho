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

/** @file CellPool.cpp
 *
 * Implementation of class CellPool
 *
 * @todo Include valgrind instrumentation (conditionally compiled).
 */

#include "CXXR/CellPool.hpp"

#include <iostream>

using namespace std;
using namespace CXXR;

CellPool::~CellPool()
{
#if VALGRIND_LEVEL >= 2
    VALGRIND_DESTROY_MEMPOOL(this);
#endif
    for (vector<void*>::iterator it = m_superblocks.begin();
	 it != m_superblocks.end(); ++it)
	::operator delete(*it);
}

bool CellPool::check() const
{
    unsigned int free_cells = 0;
    for (Cell* c = m_free_cells; c; c = c->m_next) {
	checkCell(c);
	++free_cells;
    }
    if (m_cells_allocated + free_cells
	!= m_cells_per_superblock*m_superblocks.size()) {
	cerr << "CellPool::check(): internal inconsistency\n";
	abort();
    }
    return true;
}

void CellPool::checkAllocatedCell(const void* p) const 
{
    checkCell(p);
    const Cell* cp = reinterpret_cast<const Cell*>(p);
    bool found = false;
    for (Cell* c = m_free_cells; !found && c; c = c->m_next)
	found = (c == cp);
    if (found) {
	cerr << "CellPool::checkCell : designated block"
	    " is (already) free.\n";
	abort();
    }
}

void CellPool::checkCell(const void* p) const
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
		cerr << "CellPool::checkCell : designated block"
		        " is misaligned\n";
		abort();
	    }
	}
    }
    if (!found) {
	cerr << "CellPool::checkCell : "
	    "designated block doesn't belong to this CellPool\n";
	abort();
    }
}

void CellPool::seekMemory() throw (std::bad_alloc)
{
    if (m_out_of_cells) (*m_out_of_cells)(this);
    if (!m_free_cells) {
	char* superblock
	    = reinterpret_cast<char*>(::operator new(m_superblocksize));
	m_superblocks.push_back(superblock);
	// Initialise cells:
	{
	    int offset = m_superblocksize - m_cellsize;
	    Cell* next = 0;
	    while (offset >= 0) {
		next = new (superblock + offset) Cell(next);
#if VALGRIND_LEVEL >= 2
		VALGRIND_MAKE_NOACCESS(next + 1, m_cellsize - sizeof(Cell));
#endif
		offset -= m_cellsize;
	    }
	    m_free_cells = next;
	}
    }
}
