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

/** @file Heaptest.cpp
 *
 * Test of class CXXR::Heap
 */

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include "CXXR/Heap.hpp"

using namespace std;
using namespace CXXR;

namespace {
    void usage(const char* cmd)
    {
	cerr << "Usage: " << cmd << " num_init_allocs num_churns\n";
	exit(1);
    }

    struct Tr {
	unsigned int serial;
	size_t size;
	char* cptr;

	Tr(unsigned int sr, size_t sz, char* cp)
	    : serial(sr), size(sz), cptr(cp)
	{}
    };

    vector<Tr> trs;

    // Allocate a block of a randomised size (approximately
    // exponentially distributed up to 1024 bytes) and clear its
    // memory. rnd is a number as returned by random().
    void alloc(long int rnd)
    {
	static int serial = 0;
	size_t bytes = size_t(pow(2.0, 10.0*double(rnd)/double(RAND_MAX)));
	cout << "Allocating #" << serial << " with size "
	     << bytes << endl;
	char* cptr = reinterpret_cast<char*>(Heap::allocate(bytes));
	memset(cptr, 0, bytes);
	trs.push_back(Tr(serial++, bytes, cptr));
    }
}

int main(int argc, char* argv[]) {
    if (argc != 3) usage(argv[0]);
    unsigned int num_init_allocs, num_churns;
    // Get number of initial allocations:
    {
	istringstream is(argv[1]);
	if (!(is >> num_init_allocs)) usage(argv[0]);
    }
    // Get number of churn operations:
    {
	istringstream is(argv[2]);
	if (!(is >> num_churns)) usage(argv[0]);
    }
    srandom(0);
    // Carry out initial allocations:
    {
	for (unsigned int i = 0; i < num_init_allocs; ++i) alloc(random());
	Heap::check();
	cout << "Blocks allocated: " << Heap::blocksAllocated()
	     << "\nBytes allocated: " << Heap::bytesAllocated() << endl;
    }
    // Carry out churns:
    {
	for (unsigned int i = 0; i < num_churns; ++i) {
	    long rnd = random();
	    if (rnd & 1 || trs.empty()) alloc(rnd);
	    else {
		// Select element to deallocate:
		unsigned int k
		    = int(double(rnd)*double(trs.size())/double(RAND_MAX));
		cout << "Deallocating #" << trs[k].serial << endl;
		Heap::deallocate(trs[k].cptr, trs[k].size);
		swap(trs[k], trs.back());
		trs.pop_back();
	    }
	}
	Heap::check();
	cout << "Blocks allocated: " << Heap::blocksAllocated()
	     << "\nBytes allocated: " << Heap::bytesAllocated() << endl;
    }
    return 0;
}


    
