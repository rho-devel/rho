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

#ifdef ALLOCATION_CHECK
// Helper functions for allocator consistency checking:
void add_to_allocation_map(void* allocation, std::size_t size);
void remove_from_allocation_map(void* allocation);
void* lookup_in_allocation_map(void* tentative_pointer);
#endif

/** Reports an allocation error and calls abort(). */
void allocerr(const char* message);

namespace rho {
  // Forward declarations.
  struct AllocationTable;
  struct AllocatorSuperblock;
  struct GCNode;

  /**
   * Freelists are used to store superblock nodes and large allocations.
   * For large allocations the block and superblock members are unused.
   */
  struct FreeListNode {
    FreeListNode* m_next;
    std::uint32_t m_block;  // Used only for superblock free nodes.
    AllocatorSuperblock* m_superblock;  // Used only for superblock free nodes.
  };

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

      static void* Allocate(std::size_t bytes);
      static void Free(void* p);

      /** \brief Apply function to all current allocations. */
      static void ApplyToAllAllocations(std::function<void(void*)> f);

      /** \brief Find heap allocation start pointer. */
      static GCNode* Lookup(void* candidate);

      /** \brief Print allocation overview for debugging. */
      static void DebugPrint();
    private:
      friend class AllocatorSuperblock;
      friend class AllocationTable;

      /** Samll object arena is 1Gb = 30 bits. */
      static constexpr unsigned s_arenasize = 1 << 30;

      /** Allocation table for medium and large allocations. */
      static AllocationTable* s_alloctable;

      /** Number of different object sizes for the small-object superblocks. */
      static constexpr int s_num_small_pools = (256 / 8) + 1;

      /** Pointers to small object superblocks with available blocks. */
      static AllocatorSuperblock* s_small_superblocks[s_num_small_pools];

      /** Pointers to medium object superblocks with available blocks. */
      static AllocatorSuperblock* s_medium_superblocks[18];

      /** Adds an allocation to a freelist. */
      static void AddToFreelist(uintptr_t data, unsigned size_log2);

      /**
       * Removes an allocation from a freelist, returns nullptr if there was no
       * available free object.
       */
      static void* RemoveFromFreelist(unsigned size_log2);

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

      /** Maximum quarantine size = 10Mb. */
      static constexpr int s_max_quarantine_size = 10 << 20;
      static FreeListNode* s_small_quarantine[s_num_small_pools];
      static FreeListNode* s_quarantine[64];

      /** Redzones are added before and after the allocation. */
      static constexpr int s_redzone_size = 16;

      /** Helper function for address calculations. */
      static void* OffsetPointer(void* pointer, std::size_t bytes);

      /** Adds a small object to the small-object quarantine. */
      static void AddToSmallQuarantine(FreeListNode* free_node, int pool_index);

      /** Adds an object to the quarantine freelist. */
      static void AddToQuarantine(FreeListNode* free_node, unsigned size_log2);

#endif // HAVE_ADDRESS_SANITIZER

  };
}

#endif // RHO_GCNODEALLOCATOR_HPP

