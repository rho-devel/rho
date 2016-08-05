/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include "gtest/gtest.h"
#include "rho/GCNodeAllocator.hpp"
#include "rho/AddressSanitizer.hpp"

using namespace rho;

/** Helper function for computing pointer offsets. */
void* pointer_offset(void* pointer, int offset) {
    return reinterpret_cast<void*>(reinterpret_cast<uintptr_t>(pointer) + offset);
}

TEST(GCNodeAllocatorTest, ExactLookupSmall) {
    // Test that looking up exact object pointer works for small objects.
    for (int i = 4; i <= 256 / 8; ++i) {
        void* alloc = GCNodeAllocator::allocate(i * 8);
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(alloc));
    }
}

TEST(GCNodeAllocatorTest, InternalLookupSmall) {
    // Test that looking up internal object pointers works for small objects.
    for (int i = 4; i <= 256 / 8; ++i) {
        void* alloc = GCNodeAllocator::allocate(i * 8);
        // One past start:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, 1)));

        // Midpoint:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, i * 4)));

        // Pointer to end:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, i * 8 - 1)));
    }
}

#ifndef HAVE_ADDRESS_SANITIZER
// Address sanitizer messes with object sizes which makes these tests not work.

TEST(GCNodeAllocatorTest, OnePastEndLookupSmall) {
    // Test that looking up one past the end of a small 8-multiple allocation
    // does not find the same allocation.
    for (int i = 4; i <= 256 / 8; ++i) {
        void* alloc = GCNodeAllocator::allocate(i * 8);
        EXPECT_NE(alloc, pointer_offset(alloc, i * 8));
    }
}

TEST(GCNodeAllocatorTest, OneBeforeLookupSmall) {
    // Test that looking up a pointer before a small object allocation
    // does not find the same allocation.
    for (int i = 4; i <= 256 / 8; ++i) {
        void* alloc = GCNodeAllocator::allocate(i * 8);
        EXPECT_NE(alloc, pointer_offset(alloc, -1));
    }
}
#endif // HAVE_ADDRESS_SANITIZER

TEST(GCNodeAllocatorTest, ExactLookupMedium) {
    // Test that looking up exact object pointer works for medium objects.
    for (int i = 8; i < 18; ++i) {
        void* alloc = GCNodeAllocator::allocate(1 << i);
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(alloc));
    }
}

TEST(GCNodeAllocatorTest, InternalLookupMedium) {
    // Test that looking up internal object pointers works for medium objects.
    for (int i = 8; i < 18; ++i) {
        void* alloc = GCNodeAllocator::allocate(1 << i);
        // One past start:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, 1)));

        // Midpoint:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, 1 << (i - 1))));

        // Pointer to end:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, (1 << i) - 1)));
    }
}

TEST(GCNodeAllocatorTest, ExactLookupLarge) {
    // Test that looking up exact object pointer works for large objects.
    static constexpr int size = 1 << 19;
    static constexpr int num = 50;
    void* allocs[num];
    for (int i = 0; i < num; ++i) {
        allocs[i] = GCNodeAllocator::allocate(size);
    }
    for (void* alloc : allocs) {
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(alloc));
    }
}

TEST(GCNodeAllocatorTest, InternalLookupLarge) {
    // Test that looking up internal object pointer works for large objects.
    static constexpr int size = 1 << 20;
    static constexpr int num = 50;
    void* allocs[num];
    for (int i = 0; i < num; ++i) {
        allocs[i] = GCNodeAllocator::allocate(size);
    }
    for (void* alloc : allocs) {
        // One past start:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, 1)));

        // Midpoint:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, size / 2)));

        // Pointer to end:
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, size - 1)));
    }
}

#ifndef HAVE_ADDRESS_SANITIZER
// Address sanitizer messes with object sizes which makes these tests not work.

TEST(GCNodeAllocatorTest, OnePastEndLookupLarge) {
    static constexpr int size = 1 << 21;
    static constexpr int num = 50;
    void* allocs[num];
    for (int i = 0; i < num; ++i) {
        allocs[i] = GCNodeAllocator::allocate(size);
    }
    for (void* alloc : allocs) {
        EXPECT_NE(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, size + 1)));
    }
}

TEST(GCNodeAllocatorTest, OneBeforendLookupLarge) {
    static constexpr int size = 1 << 18;
    static constexpr int num = 50;
    void* allocs[num];
    for (int i = 0; i < num; ++i) {
        allocs[i] = GCNodeAllocator::allocate(size);
    }
    for (void* alloc : allocs) {
        EXPECT_NE(alloc, GCNodeAllocator::lookupPointer(pointer_offset(alloc, -1)));
    }
}

TEST(GCNodeAllocatorTest, FreeListSmall) {
    // Test freelist re-allocation ordering.
    // These three sizes are designed to be different size classes in the allocator.
    // They all will be allocated as small allocations in superblocks.
    int small = 32;
    int medium = 48;
    int large = 100;
    void* p1 = GCNodeAllocator::allocate(small);
    void* p2 = GCNodeAllocator::allocate(medium);
    void* p3 = GCNodeAllocator::allocate(medium);
    void* p4 = GCNodeAllocator::allocate(medium);
    void* p5 = GCNodeAllocator::allocate(large);

    GCNodeAllocator::free(p1);
    GCNodeAllocator::free(p2);
    GCNodeAllocator::free(p3);
    GCNodeAllocator::free(p4);
    GCNodeAllocator::free(p5);

    EXPECT_EQ(p4, GCNodeAllocator::allocate(medium));
    EXPECT_EQ(p3, GCNodeAllocator::allocate(medium));
    EXPECT_EQ(p2, GCNodeAllocator::allocate(medium));
    EXPECT_EQ(p5, GCNodeAllocator::allocate(large));
    EXPECT_EQ(p1, GCNodeAllocator::allocate(small));

    GCNodeAllocator::free(p1);
    GCNodeAllocator::free(p2);
    GCNodeAllocator::free(p3);
    GCNodeAllocator::free(p4);
    GCNodeAllocator::free(p5);
}

TEST(GCNodeAllocatorTest, FreeListMedium) {
    // Test freelist re-allocation ordering.
    // These three sizes are designed to be different size classes in the allocator.
    // They all will be allocated as medium allocations in superblocks.
    int small = 1 << 11;
    int medium = 1 << 12;
    int large = 1 << 13;
    void* p1 = GCNodeAllocator::allocate(small);
    void* p2 = GCNodeAllocator::allocate(medium);
    void* p3 = GCNodeAllocator::allocate(medium);
    void* p4 = GCNodeAllocator::allocate(medium);
    void* p5 = GCNodeAllocator::allocate(large);

    GCNodeAllocator::free(p1);
    GCNodeAllocator::free(p2);
    GCNodeAllocator::free(p3);
    GCNodeAllocator::free(p4);
    GCNodeAllocator::free(p5);

    EXPECT_EQ(p4, GCNodeAllocator::allocate(medium));
    EXPECT_EQ(p3, GCNodeAllocator::allocate(medium));
    EXPECT_EQ(p2, GCNodeAllocator::allocate(medium));
    EXPECT_EQ(p5, GCNodeAllocator::allocate(large));
    EXPECT_EQ(p1, GCNodeAllocator::allocate(small));

    GCNodeAllocator::free(p1);
    GCNodeAllocator::free(p2);
    GCNodeAllocator::free(p3);
    GCNodeAllocator::free(p4);
    GCNodeAllocator::free(p5);
}

#endif // HAVE_ADDRESS_SANITIZER

TEST(GCNodeAllocatorTest, AllocManySmall) {
    // Make many allocations to make sure superblock lookup works when the
    // superblock becomes full.
    static constexpr int max_small_block_size = 256;
    for (int i = 0; i < (1 << 19) / max_small_block_size; ++i) {
        void* alloc = GCNodeAllocator::allocate(256);
        EXPECT_EQ(alloc, GCNodeAllocator::lookupPointer(alloc));
    }
}

