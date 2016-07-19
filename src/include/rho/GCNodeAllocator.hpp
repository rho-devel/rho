/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

#ifndef RHO_GCNODEALLOCATOR_HPP
#define RHO_GCNODEALLOCATOR_HPP

#include <cstdint>
#include <functional>

#include "rho/AddressSanitizer.hpp"

// NUM_SMALL_POOLS = (256 / 8) + 1 = 33
#define NUM_SMALL_POOLS (33)

typedef std::uint32_t u32;

using std::size_t;
using std::function;

namespace rho {
  struct AllocationTable;
  struct AllocatorSuperblock;

  /**
   * Freelists are used to store superblock nodes and large allocations.
   * For large allocations the block and superblock members are unused.
   */
  struct FreeListNode {
    FreeListNode* next;
    u32 block;  // Used only for superblock free nodes.
    AllocatorSuperblock* superblock;  // Used only for superblock free nodes.
  };
}

#ifdef HAVE_ADDRESS_SANITIZER

/*
 * When compiling with the address sanitizer, we modify the allocation routines
 * to detect errors more reliably.  In particular:
 * - A redzone is added on both sides of the object to detect underflows and
 *   overflows.
 * - Memory is poisoned when it is freed to detect use-after-free errors.
 * - Freed objects are held in a quarantine to prevent the memory from
 *   being reallocated quickly.  This helps detect use-after-free errors.
 *
 * Note that in the context of GC, 'use after free' is really premature garbage
 * collection.
 */

// Maximum quarantine size = 10Mb.
#define MAX_QUARANTINE_SIZE (10<<20)

// Redzones are added before and after the allocation.
#define REDZONE_SIZE (16)

// Quarantines are used so objects are not quickly reused after free.
extern size_t small_quarantine_size;
extern size_t quarantine_size;
extern rho::FreeListNode* small_quarantine[NUM_SMALL_POOLS];
extern rho::FreeListNode* quarantine[64];

/** Helper function for address calculations. */
void* offset_pointer(void* pointer, size_t bytes);

/** Adds a small object to the small-object quarantine. */
void add_to_small_quarantine(rho::FreeListNode* free_node, int pool_index);

/** Adds an object to the quarantine freelist. */
void add_to_quarantine(rho::FreeListNode* free_node, unsigned size_log2);

#endif // HAVE_ADDRESS_SANITIZER

#ifdef ALLOCATION_CHECK
void add_to_allocation_map(void* allocation, size_t size);
void remove_from_allocation_map(void* allocation);
void* lookup_in_allocation_map(void* tentative_pointer);
#endif

extern rho::AllocationTable* alloctable;

// Pointers to small object superblocks with available blocks.
extern rho::AllocatorSuperblock* small_superblocks[NUM_SMALL_POOLS];

// Pointers to medium object superblocks with available blocks.
extern rho::AllocatorSuperblock* medium_superblocks[18];

// Arena for small-object superblocks:
extern void* superblock_arena;
extern uintptr_t arena_superblock_start;
extern uintptr_t arena_superblock_end;
extern uintptr_t arena_superblock_next;

/** Reports an allocation error and calls abort(). */
void allocerr(const char* message);

// Forward declarations for freelist functions.
void add_free_block(uintptr_t data, unsigned size_log2);
void* remove_free_block(unsigned size_log2);

namespace rho {
  /**
   * \brief An allocator for allocating and iterating GCNode objects of
   * different sizes.
   *
   * This allocator should only be used to allocate GCNodes, otherwise
   * you will trick the garbage collector to treat non-GCNodes as GCNodes
   * during the Sweep phase of garbage collection.
   */
  class GCNodeAllocator {
    public:
      GCNodeAllocator() = delete;

      /** \brief Must be called before any allocations can be made. */
      static void Initialize();

      static void* Allocate(size_t bytes);
      static void Free(void* p);

      /** \brief Apply function to all live allocations. */
      static void ApplyToAllAllocations(function<void(void*)> f);

      /** \brief Find heap allocation start pointer. */
      static void* Lookup(void* candidate);

      /** \brief Print allocation overview for debugging. */
      static void DebugPrint();
  };
}

#endif // RHO_GCNODEALLOCATOR_HPP

