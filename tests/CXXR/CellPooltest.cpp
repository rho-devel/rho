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

/** @file CellPooltest.cpp
 *
 * Test of class CellPool
 */

#include "CXXR/CellPool.hpp"

#include <iostream>

using namespace std;
using namespace CXXR;

namespace {
    double* dptrs[16];

    CellPool pool;
}

int main() {
    pool.initialize(1, 5);
    for (int i = 0; i < 16; ++i)
	dptrs[i] = 0;
    pool.check();
    cout << "Cell size: " << pool.cellSize()
	 << "\nSuperblock size: " << pool.superblockSize() << endl;
    for (int i = 0; i < 10; ++i) {
	cout << "Allocating dptrs[" << i << "]\n";
	dptrs[i] = static_cast<double*>(pool.allocate());
    }
    pool.check();
    cout << "Cells allocated: " << pool.cellsAllocated() << endl;
    for (int i = 3; i < 10; i += 2) {
	cout << "Deallocating dptrs[" << i << "]\n";
	pool.deallocate(dptrs[i]);
    }
    pool.check();
    cout << "Cells allocated: " << pool.cellsAllocated() << endl;
    for (int i = 11; i < 16; i += 2) {
	cout << "Allocating dptrs[" << i << "]\n";
	dptrs[i] = static_cast<double*>(pool.allocate());
    }
    pool.check();
    cout << "Cells allocated: " << pool.cellsAllocated() << endl;
    return 0;
}


    
