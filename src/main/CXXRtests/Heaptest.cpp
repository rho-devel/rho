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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file Heaptest.cpp
 *
 * Test of class CXXR::Heap
 */

#include <cmath>
#include <cstdlib>
#include <iostream>
#include "CXXR/Heap.hpp"

using namespace std;
using namespace CXXR;

int main() {
    const size_t ARRAY_SIZE = 100;
    size_t sizes[ARRAY_SIZE];
    char* cptrs[ARRAY_SIZE];
    srandom(0);
    for (int i = 0; i < ARRAY_SIZE; ++i) {
	size_t bytes = size_t(pow(2.0, 10.0*random()/double(RAND_MAX)));
	sizes[i] = bytes;
	cout << "Allocating cptrs[" << i << "] with size "
	     << bytes << endl;
	cptrs[i] = reinterpret_cast<char*>(Heap::allocate(bytes));
    }
    Heap::check();
    cout << "Blocks allocated: " << Heap::blocksAllocated() << endl;
    for (int i = 1; i < ARRAY_SIZE; i += 2) {
	cout << "Deallocating cptrs[" << i << "]\n";
	Heap::deallocate(cptrs[i], sizes[i]);
    }
    Heap::check();
    cout << "Blocks allocated: " << Heap::blocksAllocated() << endl;
    for (int i = 1; i < ARRAY_SIZE; i += 2) {
	size_t bytes = size_t(pow(2.0, 10.0*random()/double(RAND_MAX)));
	sizes[i] = bytes;
	cout << "Allocating cptrs[" << i << "] with size "
	     << bytes << endl;
	cptrs[i] = reinterpret_cast<char*>(Heap::allocate(bytes));
    }
    Heap::check();
    cout << "Blocks allocated: " << Heap::blocksAllocated() << endl;
    for (int i = 0; i < ARRAY_SIZE; i += 2) {
	cout << "Deallocating cptrs[" << i << "]\n";
	Heap::deallocate(cptrs[i], sizes[i]);
    }
    Heap::check();
    cout << "Blocks allocated: " << Heap::blocksAllocated() << endl;
    for (int i = 1; i < ARRAY_SIZE; i += 2) {
	cout << "Deallocating cptrs[" << i << "]\n";
	Heap::deallocate(cptrs[i], sizes[i]);
    }
    Heap::check();
    cout << "Blocks allocated: " << Heap::blocksAllocated() << endl;
    return 0;
}


    
