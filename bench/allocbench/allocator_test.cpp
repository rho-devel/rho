/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#define COMPILING_RHO
#include "rho/IntVector.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/MemoryBank.hpp"
//#include "rho/BlockPool.hpp"

using namespace rho;

#ifdef DEBUG_INFO
void print_sparse_table();
#endif

#define NUM_ALLOCS (1000000)

// Allocates various sized int vectors before running each allocation test.
// This is meant to simulate non-uniform allocation patterns before we
// start allocating only same-size objects.
static void alloc_various_intvec() {
    for (int i = 1; i < 50; ++i) {
        if (i < 20) {
            IntVector::create(i);
        } else {
            IntVector::create(i * 10);
        }
    }
}

// Allocate NUM_ALLOCS * size int-vectors.
// Stress the allocator by allocating very many int-vectors.
// The allocations are not freed so this forces the allocator to continue
// allocating more and more memory.
extern "C"
SEXP alloc_intvec(int* num, int* size) {
    alloc_various_intvec();
    void* allocations[*num];
    for (int i = 0; i < *num; ++i) {
        allocations[i] = IntVector::create(*size);
    }
    return nullptr;
}

// Allocate NUM_ALLOCS * size int-vectors.
// Stress the allocator by allocating very many int-vectors.
// The allocations are not freed so this forces the allocator to continue
// allocating more and more memory.
extern "C"
SEXP alloc_reuse_intvec(int* num, int* each, int* size) {
    alloc_various_intvec();
    for (int i = 0; i < ((*num + *each - 1) / *each); ++i) {
        alloc_intvec(each, size);
    }
    return nullptr;
}

// Recursively calling a function that makes stack allocations tests the
// pointer lookup.
extern "C"
SEXP alloc_recursive_intvec(int* num, int* each, int* size) {
    alloc_various_intvec();
    if (*num > 0) {
        void* allocations[*each];
        for (int i = 0; i < *each; ++i) {
            allocations[i] = IntVector::create(*size);
        }
        *num -= *each;
        alloc_recursive_intvec(num, each, size);
    }
    return nullptr;
}

extern "C"
SEXP alloc_scalar(int* num) {
    alloc_various_intvec();
    void* allocations[*num];
    for (int i = 0; i < *num; ++i) {
        allocations[i] = ScalarInteger(i);
    }
    return nullptr;
}

// Allocate NUM_ALLOCS * size int-vectors.
// Stress the allocator by allocating very many int-vectors.
// The allocations are not freed so this forces the allocator to continue
// allocating more and more memory.
extern "C"
SEXP alloc_reuse_scalar(int* num, int* each) {
    alloc_various_intvec();
    for (int i = 0; i < ((*num + *each - 1) / *each); ++i) {
        alloc_scalar(each);
    }
    return nullptr;
}

// Recursively calling a function that makes stack allocations tests the
// pointer lookup.
extern "C"
SEXP alloc_recursive_scalar(int* num, int* each) {
    alloc_various_intvec();
    if (*num > 0) {
        void* allocations[*each];
        for (int i = 0; i < *each; ++i) {
            allocations[i] = ScalarInteger(i);
        }
        *num -= *each;
        alloc_recursive_scalar(num, each);
    }
    return nullptr;
}

extern "C"
SEXP alloc_lookup_scalar(int* num, int* lookups) {
    alloc_various_intvec();
    void* allocations[*num];
    for (int i = 0; i < *num; ++i) {
        allocations[i] = ScalarInteger(i);
    }
    for (int k = 0; k < *lookups; ++k) {
        // Exact pointer lookup.
        for (int i = 0; i < *num; ++i) {
            GCNode::asGCNode(allocations[i]);
        }
        // Lookup 1 byte inside allocation.
        for (int i = 0; i < *num; ++i) {
            GCNode::asGCNode(reinterpret_cast<char*>(allocations[i]) + 1);
        }
        // Lookup 40 bytes inside allocation.
        for (int i = 0; i < *num; ++i) {
            GCNode::asGCNode(reinterpret_cast<char*>(allocations[i]) + 40);
        }
    }
    // TODO: add test for lookup misses?
    return nullptr;
}

extern "C"
SEXP alloc_lookup_intvec(int* num, int* lookups, int* size) {
    alloc_various_intvec();
    void* allocations[*num];
    for (int i = 0; i < *num; ++i) {
        allocations[i] = IntVector::create(*size);
    }
    for (int k = 0; k < *lookups; ++k) {
        // Exact pointer lookup.
        for (int i = 0; i < *num; ++i) {
            GCNode::asGCNode(allocations[i]);
        }
        // Lookup 1 byte inside allocation.
        for (int i = 0; i < *num; ++i) {
            GCNode::asGCNode(reinterpret_cast<char*>(allocations[i]) + 1);
        }
        // Lookup at the end of the allocation.
        for (int i = 0; i < *num; ++i) {
            GCNode::asGCNode(reinterpret_cast<char*>(allocations[i]) + (*size - 1));
        }
    }
    // TODO: add test for lookup misses?
    return nullptr;
}

