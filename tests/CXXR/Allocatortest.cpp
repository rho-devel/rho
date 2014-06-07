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

/** @file Allocatortest.cpp
 *
 * Test of class CXXR::Allocator
 */

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <sstream>
#include <vector>
#include "CXXR/Allocator.hpp"

using namespace std;
using namespace CXXR;

namespace {
    void usage(const char* cmd)
    {
	cerr << "Usage: " << cmd << " num_init_allocs num_churns\n";
	exit(1);
    }

    list<int, Allocator<int> > ilist;

    vector<list<int>::iterator, Allocator<list<int>::iterator> > ilv;

    void alloc()
    {
	static int serial = 0;
	cout << "Allocating list item #" << serial << endl;
	ilv.push_back(ilist.insert(ilist.end(), serial++));
    }

	// Crude congruential generator in range 0 to 1023; repeatability
    // on different platforms is more important than randomness!
    // Deliberately the first value returned is 0.
    size_t qrnd()
    {
	static size_t r = 0;
	size_t ans = r;
	r = (r*633 + 633)&0x3ff;
	return ans;
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
    // Carry out initial allocations:
    {
	for (unsigned int i = 0; i < num_init_allocs; ++i)
		alloc();
	MemoryBank::check();
	cout << "Blocks allocated: " << MemoryBank::blocksAllocated() << endl;
    }
    // Carry out churns:
    {
	for (unsigned int i = 0; i < num_churns; ++i) {
	    long rnd = qrnd();
	    if (rnd & 2 || ilv.empty()) alloc();
	    else {
		// Select element to deallocate:
		unsigned int k
		    = int(double(rnd)*double(ilv.size())/1024.0);
		cout << "Deallocating list item #" << *ilv[k] << endl;
		ilist.erase(ilv[k]);
		swap(ilv[k], ilv.back());
		ilv.pop_back();
	    }
	}
	MemoryBank::check();
	cout << "Blocks allocated: " << MemoryBank::blocksAllocated() << endl;
    }
    // Clear up:
    {
	for (unsigned int k = 0; k < ilv.size(); ++k) {
	    cout << "Deallocating list item #" << *ilv[k] << endl;
	    ilist.erase(ilv[k]);
	}
    }
    return 0;
}


    
