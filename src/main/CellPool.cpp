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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
 */

/** @file CellPool.cpp
 *
 * Implementation of class CellPool
 */

#include "CXXR/CellPool.hpp"

#include <iostream>

using namespace std;
using namespace CXXR;

CellPool::~CellPool()
{
    for (vector<void*>::iterator it = m_blocks.begin();
	 it != m_blocks.end(); ++it)
	::operator delete(*it);
}

void CellPool::check() const
{
    unsigned int free_cells = 0;
    for (Cell* c = m_free_cells; c; c = c->m_next) ++free_cells;
    if (m_cells_allocated + free_cells != m_cells_per_block*m_blocks.size()) {
	cerr << "CellPool::check(): internal inconsistency\n";
	abort();
    }
}

void CellPool::seekMemory() throw (std::bad_alloc)
{
    if (m_out_of_cells) (*m_out_of_cells)(this);
    if (!m_free_cells) {
	char* block = reinterpret_cast<char*>(::operator new(m_blocksize));
	m_blocks.push_back(block);
	// Initialise cells:
	{
	    int offset = m_blocksize - m_cellsize;
	    Cell* next = 0;
	    while (offset >= 0) {
		next = new (block + offset) Cell(next);
		offset -= m_cellsize;
	    }
	    m_free_cells = next;
	}
    }
}
