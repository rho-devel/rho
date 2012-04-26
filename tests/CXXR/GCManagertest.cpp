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

/** @file GCManagertest.cpp
 *
 * Test of class CXXR::GCManager
 */

#include <cmath>
#include <cstdarg>
#include <cstdlib>
#include <iostream>
#include <vector>
#include "CXXR/GCManager.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/WeakRef.h"

using namespace std;
using namespace CXXR;

namespace {
    vector<pair<size_t, void*> > allocs;

    // Crude congruential generator in range 1 to 1024; repeatability
    // on different platforms is more important than randomness!
    // Deliberately the first value returned is 0.
    size_t qrnd()
    {
	static size_t r = 0;
	r = (r*633 + 633)&0x3ff;
	return r;
    }

    // Random number uniform in [0,1].
    double uni01()
    {
	return double(qrnd())/1024.0;
    }

    // Exponential random number with mean 1.0:
    double rexp() {return -log(uni01());}

    bool cueGC(size_t bytes, bool force)
    {
	cout << "GC cued for " << bytes
	     << (force ? " (forced)\n" : " (not forced)\n");
	return false;
    }

    void gcstart()
    {
	cout << "GC started: Bytes allocated " << MemoryBank::bytesAllocated()
	     << "; threshold " << GCManager::triggerLevel() << '\n';
    }

    void gcend()
    {
	cout << "GC finished: Bytes allocated " << MemoryBank::bytesAllocated()
	     << "; threshold " << GCManager::triggerLevel() << '\n';
    }

    // Allocate an exponentially-distributed number of bytes:
    void alloc(double mean) {
	size_t bytes_allocated = MemoryBank::bytesAllocated();
	size_t bytes = size_t(mean*rexp());
	cout << " (" << bytes_allocated
	     << ")  Allocating " << bytes << " bytes\n";
	if (bytes_allocated + bytes > GCManager::triggerLevel())
	    GCManager::gc(bytes, false);
	allocs.push_back(make_pair(bytes, MemoryBank::allocate(bytes)));
    }
}

// Stubs for members of GCNode:

void GCNode::cleanup()
{
    cout << "GCNode::cleanup()\n";
}

void GCNode::gc()
{
    cout << "GCNode::gc()\n";
    // cout << MemoryBank::bytesAllocated() << " bytes allocated at start\n";
    double keptfrac = sqrt(uni01());
    size_t keptallocs = size_t(ceil(allocs.size()*keptfrac));
    while (allocs.size() > keptallocs) {
	pair<size_t, void*>& pr = allocs.back();
	MemoryBank::deallocate(pr.second, pr.first);
	allocs.pop_back();
	cout << " Released " << pr.first << " bytes ("
	     << MemoryBank::bytesAllocated() << ")\n";
    }
}

void GCNode::gclite()
{
}

void GCNode::initialize()
{
    cout << "GCNode::initialize()\n";
}

size_t GCNode::s_num_nodes = 42;

// Other stubs:

bool WeakRef::runFinalizers()
{
    bool success = (uni01() > 0.5);
    cout << "RunFinalizers():\n";
    if (success)
	alloc(500000.0);
    cout << "RunFinalizers returned " << (success ? "true\n" : "false\n");
    return success;
}

// Main program:

int main() {
    ios_base::sync_with_stdio();
    GCManager::setMonitors(gcstart, gcend);
    GCManager::setReporting(&cout);
    GCManager::setGCThreshold(1000000);
    //    GCManager::setMaxTrigger(8000000);
    for (int k = 0; k < 100; ++k)
	alloc(200000.0);
    return 0;
}


    
