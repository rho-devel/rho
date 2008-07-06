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

/** @file CellHeaptest.cpp
 *
 * Test of class CellHeap
 */

#include "CXXR/CellHeap.hpp"

#include <iostream>

using namespace std;
using namespace CXXR;

namespace {
    double* dptrs[16];
    double* prev;  // Use to check ascending addresses.

    void out_of_memory(CellHeap*);

    CellHeap heap(1, 5, out_of_memory);

    void out_of_memory(CellHeap* /*ignored*/) {
	cout << "Out of memory";
	if (dptrs[1]) {
	    cout << ": freeing dptrs[1]";
	    heap.deallocate(dptrs[1]);
	    dptrs[1] = 0;
	}
	cout << endl;
	prev = 0;
    }

    void seq_check(double* block) {
	if (prev && prev > block) {
	    cerr << "Address sequence error.\n";
	    abort();
	}
	prev = block;
    }
}

int main() {
    for (int i = 0; i < 16; ++i) dptrs[i] = 0;
    heap.check();
    cout << "Cell size: " << heap.cellSize()
	 << "\nSuperblock size: " << heap.superblockSize() << endl;
    prev = 0;
    for (int i = 0; i < 10; ++i) {
	cout << "Allocating dptrs[" << i << "]\n";
	dptrs[i] = reinterpret_cast<double*>(heap.allocate());
	seq_check(dptrs[i]);
    }
    heap.check();
    cout << "Cells allocated: " << heap.cellsAllocated() << endl;
    for (int i = 3; i < 10; i += 2) {
	cout << "Deallocating dptrs[" << i << "]\n";
	heap.deallocate(dptrs[i]);
    }
    heap.check();
    cout << "Cells allocated: " << heap.cellsAllocated() << endl;
    prev = 0;
    for (int i = 1;
	 (dptrs[i] = reinterpret_cast<double*>(heap.easyAllocate()));
	 i += 2) {
	cout << "Allocated dptrs[" << i << "]\n";
	seq_check(dptrs[i]);
    }
    cout << "easyAllocate() failed\n";
    heap.check();
    for (int i = 11; i < 16; i += 2) {
	cout << "Allocating dptrs[" << i << "]\n";
	dptrs[i] = reinterpret_cast<double*>(heap.allocate());
	seq_check(dptrs[i]);
    }
    heap.check();
    cout << "Cells allocated: " << heap.cellsAllocated() << endl;
    return 0;
}


    
