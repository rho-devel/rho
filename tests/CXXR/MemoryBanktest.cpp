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

/** @file MemoryBanktest.cpp
 *
 * Test of class CXXR::MemoryBank
 */

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include "CXXR/MemoryBank.hpp"

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

    // Allocate a block of size bytes.
    void alloc(size_t bytes)
    {
	static int serial = 0;
	cout << "Allocating #" << serial << " with size "
	     << bytes << endl;
	char* cptr = reinterpret_cast<char*>(MemoryBank::allocate(bytes));
	memset(cptr, 0, bytes);
	trs.push_back(Tr(serial++, bytes, cptr));
    }

    void monitor(size_t bytes)
    {
	cout << "Monitored allocation of " << bytes << " bytes\n";
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
	MemoryBank::setMonitor(monitor, 100);
	for (unsigned int i = 0; i < num_init_allocs; ++i) alloc(qrnd());
	MemoryBank::check();
	cout << "Blocks allocated: " << MemoryBank::blocksAllocated()
	     << "\nBytes allocated: " << MemoryBank::bytesAllocated() << endl;
    }
    // Carry out churns:
    {
	MemoryBank::setMonitor(0);
	for (unsigned int i = 0; i < num_churns; ++i) {
	    long rnd = qrnd();
	    if (rnd & 2 || trs.empty()) alloc(rnd);
	    else {
		// Select element to deallocate:
		unsigned int k
		    = int(double(rnd)*double(trs.size())/1024.0);
		cout << "Deallocating #" << trs[k].serial << endl;
		MemoryBank::deallocate(trs[k].cptr, trs[k].size);
		swap(trs[k], trs.back());
		trs.pop_back();
	    }
	}
	MemoryBank::check();
	cout << "Blocks allocated: " << MemoryBank::blocksAllocated()
	     << "\nBytes allocated: " << MemoryBank::bytesAllocated() << endl;
    }
    // Clear up:
    {
	for (unsigned int k = 0; k < trs.size(); ++k) {
	    cout << "Deallocating #" << trs[k].serial << endl;
	    MemoryBank::deallocate(trs[k].cptr, trs[k].size);
	}
    }
    return 0;
}


    
