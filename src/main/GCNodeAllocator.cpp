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

#include <assert.h>
#include <stdio.h>

#include <cstdint>
#include <cstdlib>
#include <functional>
#include <limits>
#include <map>

#include "rho/AddressSanitizer.hpp"
#include "rho/AllocationTable.hpp"
#include "rho/GCNodeAllocator.hpp"

#ifdef HAVE_ADDRESS_SANITIZER
// Quarantine free lists are used to store freed objects for a while before
// they can be reused. Allocations are poisoned while in the quarantine. This
// makes it more likely that Address Sanitizer can detect use-after-free
// errors.
rho::FreeListNode* rho::GCNodeAllocator::s_quarantine[s_num_freelists];
#endif

// The allocation table tracks medium and large allocations.
rho::AllocationTable* rho::GCNodeAllocator::s_alloctable = nullptr;

// Superblock pools by size class. Each superblock in linked in this
// array has at least one untouched block available.
rho::AllocatorSuperblock* rho::GCNodeAllocator::s_superblocks[
    s_num_small_pools + s_num_medium_pools];

// Tracking heap bounds for fast rejection in pointer lookup.
uintptr_t rho::GCNodeAllocator::s_heap_start = UINTPTR_MAX;
uintptr_t rho::GCNodeAllocator::s_heap_end = 0;

// Free lists head pointers.
rho::FreeListNode* rho::GCNodeAllocator::s_freelists[s_num_freelists];

#ifdef ALLOCATION_CHECK
// Helper function for allocator consistency checking.
// An additional allocation map is added which shadows the state of the
// allocator. The extra allocation map is checked to verify each operation
// on the allocator.

namespace {
  void add_to_allocation_map(void* allocation, std::size_t size);
  void remove_from_allocation_map(void* allocation);

  /**
   * This should always return the same result as
   * GCNodeAllocator::lookupPointer().  If it does not, something is wrong.
   *
   * When allocation checking is enabled, each call to lookupPointer() is checked
   * against the result of this function.  This is also used to test if we try to
   * free something unalloated or allocate something which is already allocated.
   */
  void* lookup_in_allocation_map(void* tentative_pointer);
}

#endif

void allocerr(const char* message) {
  fprintf(stderr, "ERROR: %s\n", message);
  abort();
}

/**
 * Helper function for finding the the 2-log of the next power of two greater
 * than or equal to size, such that (1 << result) >= size.
 *
 * Handles 32-bit integers.
 */
static unsigned next_log2_32(unsigned size) {
  if (size == 0) {
    return 0;
  }

  // Feature test macro for GCC builtins. Also available in recent Clangs.
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

  // First we figure out the 2-log of the highest set bit.
  // Use builtin to count leading zeroes if available.
#if __has_builtin(__builtin_clz)
  unsigned log2 = 31 - __builtin_clz(size);
#else
  // Use manual log-2 finding implementation. The builtin version is available
  // for most compilers, but if not we fall back on this version.
  //
  // The algorithm below is inspired by this one for finding the number of
  // trailing zeroes:
  // https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightBinSearch
  // The code below works similarly by doing a binary search to find the
  // number of leading zeroes.
  unsigned log2 = 0;
  // This holds remaining bits after shifting out uninteresting bits:
  unsigned remaining = size;
  if (remaining & 0xFFFF0000) {
    // The high 16 bits have at least one bit set.
    log2 += 16;
    remaining >>= 16;
  }
  if (remaining & 0xFF00) {
    // The high 8 bits of remaining bits have at least one bit set.
    log2 += 8;
    remaining >>= 8;
  }
  if (remaining & 0xF0) {
    // The high 4 bits of remaining bits have at least one bit set.
    log2 += 4;
    remaining >>= 4;
  }
  // Test the upper 2 bits of the remaining bits.
  if (remaining & 0xC) {
    log2 += 2;
    remaining >>= 2;
  }
  // Adjust for the final bits.
  if (remaining > 0) {
    log2 += remaining - 1;
  }
#endif // __has_builtin(__builtin_clz)

  // Now make sure we go one above the highest set bit if the input is larger
  // than that power of two:
  if (size > (1 << log2)) {
    log2 += 1;
  }
  return log2;
}

/**
 * Allocate the small object arena, null out freelist head pointers,
 * and initialize the hashtable.
 */
void rho::GCNodeAllocator::initialize() {
  AllocatorSuperblock::allocateArena();

  for (int i = 0; i < s_num_small_pools + s_num_medium_pools; ++i) {
    // Initialize available superblock pointers.
    s_superblocks[i] = nullptr;
  }

  for (int i = 0; i < s_num_freelists; ++i) {
    // Initialize regular freelists.
    s_freelists[i] = nullptr;
#ifdef HAVE_ADDRESS_SANITIZER
    // Initialize quarantine freelists.
    s_quarantine[i] = nullptr;
#endif
  }

  // Use a 16 bit hash initially.
  s_alloctable = new rho::AllocationTable(16);
}

void* rho::GCNodeAllocator::allocate(size_t bytes) {
  void* result = nullptr;
#ifdef HAVE_ADDRESS_SANITIZER
  // Increase size to include redzones.
  bytes += 2 * s_redzone_size;
#endif
  unsigned actual_bytes;  // The actual allocation size
      // (may be more than requested).
  if (bytes <= 256) {
    int size_class = (bytes + 7) / 8; // Computes ceil(bytes / 8).
    if (size_class < 4) {
      // Ensure at least 32-byte blocks for the small-object arena. This is
      // required both to fit a FreeListNode (20 bytes), and to reduce the
      // number of bytes needed for the fixed size bitset (as part of the
      // constant SUPERBLOCK_HEADER_SIZE).
      size_class = 4;
    }
    actual_bytes = size_class * 8;
    result = removeFromFreelist(size_class);
    if (!result) {
      result = AllocatorSuperblock::allocateBlock(actual_bytes);
      // If allocating in the small object arena fails, we continue
      // on to using the medium block allocator. The allocation
      // size will be increased to the minimum allocation size 64 bytes.
    }
  }
  if (!result) {
    // Now we try to allocate in a large superblock or, if the size
    // exceeds the superblock threshold, use a separate allocation.
    // These allocations are rounded up to the next power of two size.
    unsigned size_log2 = next_log2_32(bytes);
    if (size_log2 < 6) {
      // Ensure a minimum allocation size of 64 bytes.
      size_log2 = 6;
    }
    actual_bytes = 1 << size_log2;
    unsigned size_class = AllocatorSuperblock::sizeClassFromSizeLog2(size_log2);
    result = removeFromFreelist(size_class);
    if (!result) {
      if (size_log2 < s_num_medium_pools) {
        result = AllocatorSuperblock::allocateLarge(size_log2);
      } else {
        result = new double[(1L << size_log2) / sizeof(double)];
        GCNodeAllocator::s_alloctable->insert(result, size_log2);
      }
      // Only update heap bounds if allocating a new block.
      updateHeapBounds(result, actual_bytes);
    }
  }
  if (!result) {
    allocerr("failed to allocate object");
  }

#ifdef ALLOCATION_CHECK
  if (lookup_in_allocation_map(result)) {
    allocerr("reusing live allocation");
  }
  add_to_allocation_map(result, actual_bytes);
  lookupPointer(result);  // Check lookup table consistency.
#endif

#ifdef HAVE_ADDRESS_SANITIZER
  // Poison the redzones.
  // The end redzone can be larger than s_redzone_size if the allocator
  // grew the allocation larger than the requested size.
  // The additional size (actual_bytes - bytes) is added to the default
  // redzone size.
  void* end_redzone = offsetPointer(result, bytes - s_redzone_size);
  unsigned end_redzone_size = s_redzone_size + (actual_bytes - bytes);
  ASAN_POISON_MEMORY_REGION(result, s_redzone_size);
  ASAN_POISON_MEMORY_REGION(end_redzone, end_redzone_size);

  // Offset the result pointer past first redzone.
  result = offsetPointer(result, s_redzone_size);
#endif // HAVE_ADDRESS_SANITIZER
  return result;
}

void rho::GCNodeAllocator::free(void* pointer) {
#ifdef HAVE_ADDRESS_SANITIZER
  // Adjust for redzone to find the true start of the allocation.
  pointer = offsetPointer(pointer, -s_redzone_size);

  // Unpoison the first redzone so we can link it into a freelist.
  ASAN_UNPOISON_MEMORY_REGION(pointer, sizeof(FreeListNode));
#endif
#ifdef ALLOCATION_CHECK
  if (!lookupPointer(pointer)) {
    allocerr("can not free unknown/already-freed pointer");
  }
  remove_from_allocation_map(pointer);
#endif
  uintptr_t pointer_uint = reinterpret_cast<uintptr_t>(pointer);
  AllocatorSuperblock* superblock =
      AllocatorSuperblock::arenaSuperblockFromPointer(pointer_uint);
  if (superblock) {
    superblock->freeBlock(pointer);
  } else {
    // Search for the allocation in the hashtable.  If the allocation is in a
    // superblock, the superblock is left as is but the superblock bitset is
    // updated to indicate that the block is free.
    // If the allocation is a large object (ie. not in a superblock), then
    // the object is taken out of the hashtable and inserted into a freelist.
    AllocationTable::Allocation* allocation =
        s_alloctable->search(pointer_uint);
    if (!allocation) {
      // Free failed: could not find block to free.
      allocerr("failed to free pointer - unallocated or double-free problem");
    }
    if (allocation->isSuperblock()) {
      AllocatorSuperblock* superblock = allocation->asSuperblock();
      superblock->freeBlock(pointer);
      // Done. Don't erase hash entries for the superblock.
    } else if (allocation->asPointer() == pointer) {
      // Erase all entries in hashtable for the allocation.
      s_alloctable->erase(pointer_uint, allocation->sizeLog2());
      // Add allocation to free list.
      addLargeAllocationToFreelist(pointer, allocation->sizeClass());
    } else {
      allocerr("failed to free pointer - unallocated or double-free problem");
    }
  }
}

void rho::GCNodeAllocator::applyToAllAllocations(
    std::function<void(void*)> fun) {
#ifdef HAVE_ADDRESS_SANITIZER
  // Create an intermediate lambda which adds redzone offsets.
  std::function<void(void*)> original = fun;
  fun = [=](void* pointer) {
    // Add redzone offset before calling the function.
    original(offsetPointer(pointer, s_redzone_size));
  };
#endif
  AllocatorSuperblock::applyToArenaAllocations(fun);
  s_alloctable->applyToAllAllocations(fun);
}

rho::GCNode* rho::GCNodeAllocator::lookupPointer(void* candidate) {
  uintptr_t candidate_uint = reinterpret_cast<uintptr_t>(candidate);
  void* result = AllocatorSuperblock::lookupAllocation(candidate_uint);
  if (!result
      && (candidate_uint >= s_heap_start && candidate_uint < s_heap_end)) {
    // Lookup a pointer in the hashtable.
    AllocationTable::Allocation* allocation =
        s_alloctable->search(candidate_uint);
    if (allocation) {
      if (allocation->isSuperblock()) {
        AllocatorSuperblock* superblock = allocation->asSuperblock();
        result = superblock->lookupBlock(candidate_uint);
      } else {
        result = allocation->asPointer();
      }
    }
  }
#ifdef ALLOCATION_CHECK
  if (result != lookup_in_allocation_map(candidate)) {
    if (!result) {
      allocerr("allocation map mismatch: did not find existing node");
    } else {
      allocerr("allocation map mismatch");
    }
  }
#endif
#ifdef HAVE_ADDRESS_SANITIZER
  // Adjust for redzone.
  if (result) {
    result = offsetPointer(result, s_redzone_size);
  }
#endif
  return static_cast<GCNode*>(result);
}

void rho::GCNodeAllocator::addLargeAllocationToFreelist(void* pointer,
    unsigned size_class) {
  FreeListNode* free_node = new (pointer)FreeListNode();
  addToFreelist(free_node, size_class);
}

void rho::GCNodeAllocator::addToFreelist(FreeListNode* free_node,
    unsigned size_class) {
#ifdef HAVE_ADDRESS_SANITIZER
  addToQuarantine(free_node, size_class);
#else
  free_node->m_next = s_freelists[size_class];
  s_freelists[size_class] = free_node;
#endif
}

void* rho::GCNodeAllocator::removeFromFreelist(unsigned size_class) {
  FreeListNode* node = s_freelists[size_class];
  if (node) {
#ifdef HAVE_ADDRESS_SANITIZER
    ASAN_UNPOISON_MEMORY_REGION(node, bytesFromSizeClass(size_class));
#endif
    s_freelists[size_class] = node->m_next;

    if (node->m_superblock) {
      // Tag the resued allocation as allocated since it is part of a
      // superblock.
      node->m_superblock->tagBlockAllocated(node->m_block);
    } else {
      // This is a non-superblock allocation. Insert it back into the
      // allocation table.
      s_alloctable->insert(static_cast<void*>(node),
          sizeLog2FromSizeClass(size_class));
    }
  }
  return static_cast<void*>(node);
}

void rho::GCNodeAllocator::updateHeapBounds(void* pointer, size_t size) {
  uintptr_t allocation = reinterpret_cast<uintptr_t>(pointer);
  if (allocation < s_heap_start) {
    s_heap_start = allocation;
  }
  uintptr_t allocation_end = allocation + size;
  if (allocation_end > s_heap_end) {
    s_heap_end = allocation_end;
  }
}

void rho::GCNodeAllocator::printSummary() {
  AllocatorSuperblock::debugPrintSmallSuperblocks();
  s_alloctable->printSummary();
}

#ifdef ALLOCATION_CHECK

namespace {
  /*
   * Defining ALLOCATION_CHECK adds a separate allocation-map to
   * track all current allocations. This map is used to double-check
   * each operation the GCNodeAllocator does to ensure it is consistent
   * with the state recorded in the ALLOCATION_CHECK map.
   */

  typedef std::map<void*, void*> allocation_map;

  /** Extra map for allocator sanity checking. */
  allocation_map allocations;

  /** Adds an allocation to the extra allocation map. */
  void add_to_allocation_map(void* allocation, std::size_t size) {
    void* allocation_end = static_cast<char*>(allocation) + size;
    allocations[allocation] = allocation_end;
  }

  /** Removes an allocation from the extra allocation map. */
  void remove_from_allocation_map(void* allocation) {
    allocations.erase(allocation);
  }

  /**
   * Check if a pointer is in the extra allocation map.
   * Returns the start pointer of the allocation if the pointer
   * is in the map, otherwise returns null.
   */
  void* lookup_in_allocation_map(void* tentative_pointer) {
    // Find the largest key less than or equal to tentative_pointer.
    allocation_map::const_iterator next_allocation =
        allocations.upper_bound(tentative_pointer);
    if (next_allocation != allocations.begin()) {
      allocation_map::const_iterator allocation = std::prev(next_allocation);

      // Check that tentative_pointer is before the end of the allocation.
      void* allocation_end = allocation->second;
      if (tentative_pointer < allocation_end) {
        return allocation->first;
      }
    }
    return nullptr;
  }
}
#endif

#ifdef HAVE_ADDRESS_SANITIZER
void* rho::GCNodeAllocator::offsetPointer(void* pointer, std::size_t bytes) {
    return reinterpret_cast<void*>(
        reinterpret_cast<uintptr_t>(pointer) + bytes);
}

void rho::GCNodeAllocator::addToQuarantine(rho::FreeListNode* free_node,
    unsigned size_class) {
  // Add an allocation to the quarantine and poison it.
  static unsigned quarantine_size = 0;

  size_t block_size = bytesFromSizeClass(size_class);
  free_node->m_next = s_quarantine[size_class];
  s_quarantine[size_class] = free_node;
  ASAN_POISON_MEMORY_REGION(free_node, block_size);

  // Clear the quarantine if it has grown too large.
  quarantine_size += block_size;
  if (quarantine_size > s_max_quarantine_size) {
    quarantine_size = 0;
    clearQuarantine();
  }
}

void rho::GCNodeAllocator::clearQuarantine() {
  // Remove all free nodes from quarantine and insert in corresponding
  // freelist.
  for (int i = 0; i < s_num_freelists; ++i) {
    FreeListNode* tail_node = nullptr;
    FreeListNode* next_node = s_quarantine[i];
    while (next_node) {
      tail_node = next_node;

      // Unpoison the quarantined allocation so we can read the next link.
      ASAN_UNPOISON_MEMORY_REGION(tail_node, sizeof(FreeListNode));
      next_node = tail_node->m_next;
      // Re-poison so the freelist nodes stay poisoned while free.
      ASAN_POISON_MEMORY_REGION(tail_node, sizeof(FreeListNode));
    }
    if (tail_node) {
      // Unpoison the last node so we can write the next link.
      ASAN_UNPOISON_MEMORY_REGION(tail_node, sizeof(FreeListNode));
      tail_node->m_next = s_freelists[i];
      // Re-poison so the freelist nodes stay poisoned while free.
      ASAN_POISON_MEMORY_REGION(tail_node, sizeof(FreeListNode));

      s_freelists[i] = s_quarantine[i];
      s_quarantine[i] = nullptr;
    }
  }
}

#endif // HAVE_ADDRESS_SANITIZER

