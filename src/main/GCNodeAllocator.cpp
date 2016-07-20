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
// The quarantine free lists are used to store freed objects for a while before
// they can be reused.
rho::FreeListNode* rho::GCNodeAllocator::s_small_quarantine[rho::GCNodeAllocator::s_num_small_pools];
rho::FreeListNode* rho::GCNodeAllocator::s_quarantine[64];
#endif

using std::size_t;

using rho::FreeListNode;
typedef rho::AllocatorSuperblock Superblock;
typedef rho::AllocationTable::Allocation Allocation;

#ifdef ALLOCATION_CHECK
typedef std::map<void*, void*> allocation_map;
static allocation_map allocations;
#endif

rho::AllocationTable* rho::GCNodeAllocator::s_alloctable = nullptr;
Superblock* rho::GCNodeAllocator::s_small_superblocks[rho::GCNodeAllocator::s_num_small_pools];
Superblock* rho::GCNodeAllocator::s_medium_superblocks[18];

// Arena for small-object superblocks:
void* superblock_arena = nullptr;
uintptr_t arena_superblock_start = 0;
uintptr_t arena_superblock_end = 0;
uintptr_t arena_superblock_next = 0;

// Tracking heap bounds for fast rejection in pointer lookup.
static void* heap_start = reinterpret_cast<void*>(UINTPTR_MAX);
static void* heap_end = reinterpret_cast<void*>(0);

static FreeListNode* freelists[64];

void allocerr(const char* message) {
  fprintf(stderr, "ERROR: %s\n", message);
  abort();
}

// Find the 2-log of the next power of two greater than or equal to size.
static unsigned next_log2_32(uint32_t size) {
  if (size == 0) {
    return 0;
  }
  unsigned log2 = 0;
  uint32_t temp = size;
  if (temp & 0xFFFF0000) {
    log2 += 16;
    temp >>= 16;
  }
  if (temp & 0xFF00) {
    log2 += 8;
    temp >>= 8;
  }
  if (temp & 0xF0) {
    log2 += 4;
    temp >>= 4;
  }
  if (temp & 0xC) {
    log2 += 2;
    temp >>= 2;
  }
  if (temp > 0) {
    log2 += temp - 1;
  }
  if ((size & (1 << log2)) && (size & ((1 << log2) - 1))) {
    log2 += 1;
  }
  return log2;
}

/** Update heap bounds for medium/large allocations. */
static void update_heap_bounds(void* allocation, size_t size) {
  if (allocation < heap_start) {
    heap_start = allocation;
  }
  void* allocation_end = static_cast<char*>(allocation) + size;
  if (allocation_end > heap_end) {
    heap_end = allocation_end;
  }
}

/**
 * Allocate the small object arena, null out freelist head pointers,
 * and initialize the hashtable.
 */
void rho::GCNodeAllocator::Initialize() {
  // We use new[] here, but one could also use sbrk(ARENASIZE).
  superblock_arena = new double[s_arenasize / sizeof(double)];
  uintptr_t start = reinterpret_cast<uintptr_t>(superblock_arena);
  uintptr_t end = start + s_arenasize;
  uintptr_t pad = Superblock::s_superblock_size
      - (start & (Superblock::s_superblock_size - 1));
  arena_superblock_start = start + pad;
  uintptr_t num_sb = (end - arena_superblock_start) >> Superblock::s_superblock_bits;
  arena_superblock_end = arena_superblock_start + num_sb * Superblock::s_superblock_size;
  arena_superblock_next = arena_superblock_start;

#ifdef HAVE_ADDRESS_SANITIZER
  // Poison the whole small object arena.
  ASAN_POISON_MEMORY_REGION(reinterpret_cast<void*>(start), s_arenasize);

  // Initialize quarantine freelists.
  for (int i = 0; i < s_num_small_pools; ++i) {
    s_small_quarantine[i] = nullptr;
  }

  for (int i = 0; i < 64; ++i) {
    s_quarantine[i] = nullptr;
  }
#endif

  for (int i = 0; i < s_num_small_pools; ++i) {
    s_small_superblocks[i] = nullptr;
  }

  for (int i = 0; i < 64; ++i) {
    freelists[i] = nullptr;
  }

  for (int i = 0; i < 18; ++i) {
    s_medium_superblocks[i] = nullptr;
  }

  s_alloctable = new rho::AllocationTable(16);
}

/**
 * Add a large allocation to a free list.
 */
void rho::GCNodeAllocator::AddToFreelist(uintptr_t data, unsigned size_log2) {
  FreeListNode* free_node = reinterpret_cast<FreeListNode*>(data);
#ifdef HAVE_ADDRESS_SANITIZER
  AddToQuarantine(free_node, size_log2);
#else
  free_node->m_next = freelists[size_log2];
  freelists[size_log2] = free_node;
#endif
}

/**
 * Reuse a large allocation from a freelist.
 * Returns null if no reusable allocation was found.
 */
void* rho::GCNodeAllocator::RemoveFromFreelist(unsigned size_log2) {
  FreeListNode* node = reinterpret_cast<FreeListNode*>(freelists[size_log2]);
  if (node) {
#ifdef HAVE_ADDRESS_SANITIZER
    ASAN_UNPOISON_MEMORY_REGION(node, 1 << size_log2);
#endif
    freelists[size_log2] = node->m_next;
  }
  return static_cast<void*>(node);
}

void* rho::GCNodeAllocator::Allocate(size_t bytes) {
  void* result = nullptr;
#ifdef HAVE_ADDRESS_SANITIZER
  // Increase size to include redzones.
  bytes += 2 * s_redzone_size;
#endif
  unsigned block_bytes;
  if (bytes <= 256) {
    int pool_index = (bytes + 7) / 8;
    if (pool_index < 4) {
      // Ensure at least 32-byte blocks. This is required both to fit a
      // FreeListNode (20 bytes), and to reduce the number of bytes needed for
      // the fixed size bitset (as part of the constant
      // SUPERBLOCK_HEADER_SIZE).
      pool_index = 4;
    }
    block_bytes = pool_index * 8;
    result = Superblock::AllocSmall(block_bytes);
    if (!result) {
      // Allocating in the small object arena failed, so we have to fall
      // back on using the medium block allocator. To make this work
      // the object size must be at least 64 bytes:
      if (bytes < 64) {
        bytes = 64;
      }
    }
  }
  if (!result) {
    // Default to separate allocation if block size is larger than small block
    // threshold.
    unsigned size_log2 = next_log2_32(bytes);
    block_bytes = 1 << size_log2;
    result = Superblock::AllocLarge(size_log2);
    update_heap_bounds(result, block_bytes);
  }
  if (!result) {
    allocerr("failed to allocate object");
  }

#ifdef ALLOCATION_CHECK
  if (lookup_in_allocation_map(result)) {
    allocerr("reusing live allocation");
  }
  add_to_allocation_map(result, block_bytes);
  Lookup(result);  // Check lookup table consistency.
#endif

#ifdef HAVE_ADDRESS_SANITIZER
  // Store allocation size in first redzone.
  void* start_redzone = result;
  void* end_redzone = OffsetPointer(result, bytes - s_redzone_size);

  // Poison the redzones.
  ASAN_POISON_MEMORY_REGION(start_redzone, s_redzone_size);
  ASAN_POISON_MEMORY_REGION(end_redzone, s_redzone_size);

  // Offset the result pointer past first redzone.
  result = OffsetPointer(result, s_redzone_size);
#endif  // HAVE_ADDRESS_SANITIZER
  return result;
}

void rho::GCNodeAllocator::Free(void* pointer) {
#ifdef HAVE_ADDRESS_SANITIZER
  // Adjust for redzone.
  pointer = OffsetPointer(pointer, -s_redzone_size);

  // Unpoison so we can link this block into a freelist.
  ASAN_UNPOISON_MEMORY_REGION(pointer, sizeof(FreeListNode));
#endif
#ifdef ALLOCATION_CHECK
  if (!Lookup(pointer)) {
    allocerr("can not free unknown/already-freed pointer");
  }
  remove_from_allocation_map(pointer);
#endif
  Superblock* superblock = Superblock::SmallSuperblockFromPointer(pointer);
  if (superblock) {
    superblock->FreeSmallBlock(pointer);
  } else if (!s_alloctable->free_allocation(reinterpret_cast<uintptr_t>(pointer))) {
    allocerr("failed to free pointer - unallocated or double-free problem");
  }
}

void rho::GCNodeAllocator::ApplyToAllAllocations(std::function<void(void*)> fun) {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    Superblock* superblock = reinterpret_cast<Superblock*>(next_superblock);
    superblock->ApplyToAllAllocations(fun);
    next_superblock += Superblock::s_superblock_size;
  }
  s_alloctable->ApplyToAllAllocations(fun);
}

/**
 * Finds the corresponding allocation for an internal or exact object pointer.
 *
 * If the pointer is inside the bounds of the small object arena, then
 * the corresponding superblock is found via pointer manipulation.
 *
 * If the pointer is inside the heap bounds, then we iterate over all hash
 * collisions for the pointer to find the corresponding allocation.
 */
rho::GCNode* rho::GCNodeAllocator::Lookup(void* candidate) {
  void* result = nullptr;
  uintptr_t candidate_uint = reinterpret_cast<uintptr_t>(candidate);
  Superblock* superblock = Superblock::SmallSuperblockFromPointer(candidate);
  if (superblock) {
    uintptr_t first_block = superblock->FirstBlockPointer();
    unsigned index = (candidate_uint - first_block) / superblock->m_block_size;
    if (superblock->IsBlockAllocated(index)) {
      result = reinterpret_cast<char*>(first_block)
          + index * superblock->m_block_size;
    } else {
      // The block is not allocated.
      result = nullptr;
    }
  } else if (candidate >= heap_start && candidate < heap_end) {
    Allocation* bucket = s_alloctable->search(candidate_uint);
    if (bucket) {
      if (bucket->is_superblock()) {
        Superblock* superblock = bucket->as_superblock();
        uintptr_t first_block = superblock->FirstBlockPointer();
        unsigned size_log2 = superblock->m_block_size;
        unsigned index = (candidate_uint - first_block) >> size_log2;
        // Check that the block is actually allocated.
        if (superblock->IsBlockAllocated(index)) {
          result = reinterpret_cast<char*>(first_block) + (index << size_log2);
        }
      } else {
        result = bucket->as_pointer();
      }
    }
  }
#ifdef ALLOCATION_CHECK
  if (result != lookup_in_allocation_map(candidate)) {
    allocerr("allocation map mismatch");
  }
#endif
#ifdef HAVE_ADDRESS_SANITIZER
  // Adjust for redzone.
  if (result) {
    result = OffsetPointer(result, s_redzone_size);
  }
#endif
  return static_cast<GCNode*>(result);
}

#ifdef ALLOCATION_CHECK
void add_to_allocation_map(void* allocation, size_t size) {
  void* allocation_end = static_cast<char*>(allocation) + size;
  allocations[allocation] = allocation_end;
}

void remove_from_allocation_map(void* allocation) {
  allocations.erase(allocation);
}

void* lookup_in_allocation_map(void* tentative_pointer) {
  // Find the largest key less than or equal to tentative_pointer.
  if (allocations.find(tentative_pointer) != allocations.end()) {
    return tentative_pointer;
  }
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
#endif

void rho::GCNodeAllocator::DebugPrint() {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    reinterpret_cast<Superblock*>(next_superblock)->DebugPrint();
    next_superblock += Superblock::s_superblock_size;
  }
  s_alloctable->PrintTable();
}

#ifdef HAVE_ADDRESS_SANITIZER
void* rho::GCNodeAllocator::OffsetPointer(void* pointer, size_t bytes) {
    return static_cast<char*>(pointer) + bytes;
}

void rho::GCNodeAllocator::AddToSmallQuarantine(rho::FreeListNode* free_node, int pool_index) {
  static unsigned small_quarantine_size = 0;
  // Add to quarantine and poison.
  unsigned block_size = pool_index * 8;
  small_quarantine_size += block_size;
  free_node->m_next = s_small_quarantine[pool_index];
  s_small_quarantine[pool_index] = free_node;
  ASAN_POISON_MEMORY_REGION(free_node, block_size);
  // Maybe clear the quarantine.
  if (small_quarantine_size > s_max_quarantine_size) {
    // Remove all small ojbects from quarantine and insert in freelists.
    for (int i = 4; i < s_num_small_pools; ++i) {
      while (s_small_quarantine[i]) {
        FreeListNode* quarantined_node = s_small_quarantine[i];
        ASAN_UNPOISON_MEMORY_REGION(quarantined_node, i * 8);
        s_small_quarantine[i] = quarantined_node->m_next;
        Superblock* superblock;
        if (s_small_superblocks[i]) {
          superblock = s_small_superblocks[i];
        } else {
          superblock = quarantined_node->m_superblock;
          s_small_superblocks[i] = superblock;
        }
        quarantined_node->m_next = superblock->m_free_list;
        superblock->m_free_list = quarantined_node;
        // Re-poison so the freelist nodes stay poisoned while free.
        ASAN_POISON_MEMORY_REGION(quarantined_node, i * 8);
      }
    }
    small_quarantine_size = 0;
  }
}

void rho::GCNodeAllocator::AddToQuarantine(rho::FreeListNode* free_node, unsigned size_log2) {
  static unsigned quarantine_size = 0;
  // Add to quarantine and poison.
  unsigned block_size = 1 << size_log2;
  quarantine_size += block_size;
  free_node->m_next = s_quarantine[size_log2];
  s_quarantine[size_log2] = free_node;
  ASAN_POISON_MEMORY_REGION(free_node, block_size);
  // Maybe clear the quarantine.
  if (quarantine_size > s_max_quarantine_size) {
    // Clear the medium/large object quarantine.
    // Remove all medium ojbects from quarantine and insert in superblock freelists.
    for (int i = 0; i < 18; ++i) {
      while (s_quarantine[i]) {
        FreeListNode* quarantined_node = s_quarantine[i];
        ASAN_UNPOISON_MEMORY_REGION(quarantined_node, 1 << i);
        s_quarantine[i] = quarantined_node->m_next;
        Superblock* superblock;
        if (s_medium_superblocks[i]) {
          superblock = s_medium_superblocks[i];
        } else {
          superblock = quarantined_node->m_superblock;
          s_medium_superblocks[i] = superblock;
        }
        quarantined_node->m_next = superblock->m_free_list;
        superblock->m_free_list = quarantined_node;
        // Re-poison so the freelist nodes stay poisoned while free.
        ASAN_POISON_MEMORY_REGION(quarantined_node, 1 << i);
      }
    }
    // Remove all large ojbects from quarantine and insert in freelists.
    for (int i = 18; i < 64; ++i) {
      while (s_quarantine[i]) {
        FreeListNode* quarantined_node = s_quarantine[i];
        ASAN_UNPOISON_MEMORY_REGION(quarantined_node, 1 << i);
        s_quarantine[i] = quarantined_node->m_next;
        quarantined_node->m_next = freelists[i];
        freelists[i] = quarantined_node;
        // Re-poison so the freelist nodes stay poisoned while free.
        ASAN_POISON_MEMORY_REGION(quarantined_node, 1 << i);
      }
    }
    quarantine_size = 0;
  }
}

#endif // HAVE_ADDRESS_SANITIZER

