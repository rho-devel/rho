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

#include "rho/AllocationTable.hpp"
#include "rho/GCNodeAllocator.hpp"
#include "rho/AddressSanitizer.hpp"

// The low pointer bits are masked out in the hash function.
#define LOW_BITS (19)
#define MAX_COLLISIONS (6)
#define MAX_REBALANCE_COLLISIONS (3)

// Constant indicating a deleted bucket.
#define EMPTY_KEY (0)
#define DELETED_KEY (UINTPTR_MAX)

// Samll object arena is 1Gb = 30 bits.
#define ARENASIZE (1 << 30)

/*
 * A small object superblock has size 2^18.
 * Minimum object size = 32 bytes.
 */
#define SUPERBLOCK_BITS (18)
#define SUPERBLOCK_SIZE (1 << SUPERBLOCK_BITS)

/*
 * Medium object superblocks use 2^19 bytes.
 * Minimum object size = 64 bytes.
 */
#define MEDIUM_SUPERBLOCK_SIZE_LOG2 (19)
#define MEDIUM_SUPERBLOCK_SIZE (1 << MEDIUM_SUPERBLOCK_SIZE_LOG2)

#ifdef HAVE_ADDRESS_SANITIZER
static size_t small_quarantine_size = 0;
static size_t quarantine_size = 0;

// The quarantine free lists are used to store freed objects for a while before
// they can be reused.
static rho::FreeListNode* small_quarantine[NUM_SMALL_POOLS];
static rho::FreeListNode* quarantine[64];

void* offset_pointer(void* pointer, size_t bytes) {
    return static_cast<char*>(pointer) + bytes;
}
#endif

typedef std::uint64_t u64;
typedef std::uint32_t u32;

using std::function;
using std::size_t;

using rho::FreeListNode;
typedef rho::AllocatorSuperblock Superblock;
typedef rho::AllocationTable::HashBucket HashBucket;

#ifdef ALLOCATION_CHECK
typedef std::map<void*, void*> allocation_map;
static allocation_map allocations;
#endif

// Pointers to small object superblocks with available blocks.
Superblock* small_superblocks[NUM_SMALL_POOLS];

// Pointers to medium object superblocks with available blocks.
Superblock* medium_superblocks[18];

// Medium- and large-object hashtable:
rho::AllocationTable* alloctable = nullptr;

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
static unsigned next_log2_32(u32 size) {
  if (size == 0) {
    return 0;
  }
  unsigned log2 = 0;
  u32 temp = size;
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

/**
 * Allocate the small object arena, null out freelist head pointers,
 * and initialize the hashtable.
 */
void rho::GCNodeAllocator::Initialize() {
  // We use new[] here, but one could also use sbrk(ARENASIZE).
  superblock_arena = new double[ARENASIZE / sizeof(double)];
  uintptr_t start = reinterpret_cast<uintptr_t>(superblock_arena);
  uintptr_t end = start + ARENASIZE;
  uintptr_t pad = SUPERBLOCK_SIZE - (start & (SUPERBLOCK_SIZE - 1));
  arena_superblock_start = start + pad;
  uintptr_t num_sb = (end - arena_superblock_start) >> SUPERBLOCK_BITS;
  arena_superblock_end = arena_superblock_start + num_sb * SUPERBLOCK_SIZE;
  arena_superblock_next = arena_superblock_start;

#ifdef HAVE_ADDRESS_SANITIZER
  // Poison the whole small object arena.
  ASAN_POISON_MEMORY_REGION(reinterpret_cast<void*>(start), ARENASIZE);

  // Initialize quarantine freelists.
  for (int i = 0; i < NUM_SMALL_POOLS; ++i) {
    small_quarantine[i] = nullptr;
  }

  for (int i = 0; i < 64; ++i) {
    quarantine[i] = nullptr;
  }
#endif

  for (int i = 0; i < NUM_SMALL_POOLS; ++i) {
    small_superblocks[i] = nullptr;
  }

  for (int i = 0; i < 64; ++i) {
    freelists[i] = nullptr;
  }

  for (int i = 0; i < 18; ++i) {
    medium_superblocks[i] = nullptr;
  }

  alloctable = new rho::AllocationTable(16);
}

#ifdef HAVE_ADDRESS_SANITIZER

void add_to_small_quarantine(rho::FreeListNode* free_node, int pool_index) {
  // Add to quarantine and poison.
  unsigned block_size = pool_index * 8;
  small_quarantine_size += block_size;
  free_node->next = small_quarantine[pool_index];
  small_quarantine[pool_index] = free_node;
  ASAN_POISON_MEMORY_REGION(free_node, block_size);
  // Maybe clear the quarantine.
  if (small_quarantine_size > MAX_QUARANTINE_SIZE) {
    // Remove all small ojbects from quarantine and insert in freelists.
    for (int i = 4; i < NUM_SMALL_POOLS; ++i) {
      while (small_quarantine[i]) {
        FreeListNode* quarantined_node = small_quarantine[i];
        ASAN_UNPOISON_MEMORY_REGION(quarantined_node, i * 8);
        small_quarantine[i] = quarantined_node->next;
        Superblock* superblock;
        if (small_superblocks[i]) {
          superblock = small_superblocks[i];
        } else {
          superblock = quarantined_node->superblock;
          small_superblocks[i] = superblock;
        }
        quarantined_node->next = superblock->free_list;
        superblock->free_list = quarantined_node;
        // Re-poison so the freelist nodes stay poisoned while free.
        ASAN_POISON_MEMORY_REGION(quarantined_node, i * 8);
      }
    }
    small_quarantine_size = 0;
  }
}

void add_to_quarantine(rho::FreeListNode* free_node, unsigned size_log2) {
  // Add to quarantine and poison.
  unsigned block_size = 1 << size_log2;
  quarantine_size += block_size;
  free_node->next = quarantine[size_log2];
  quarantine[size_log2] = free_node;
  ASAN_POISON_MEMORY_REGION(free_node, block_size);
  // Maybe clear the quarantine.
  if (quarantine_size > MAX_QUARANTINE_SIZE) {
    // Clear the medium/large object quarantine.
    // Remove all medium ojbects from quarantine and insert in superblock freelists.
    for (int i = 0; i < 18; ++i) {
      while (quarantine[i]) {
        FreeListNode* quarantined_node = quarantine[i];
        ASAN_UNPOISON_MEMORY_REGION(quarantined_node, 1 << i);
        quarantine[i] = quarantined_node->next;
        Superblock* superblock;
        if (medium_superblocks[i]) {
          superblock = medium_superblocks[i];
        } else {
          superblock = quarantined_node->superblock;
          medium_superblocks[i] = superblock;
        }
        quarantined_node->next = superblock->free_list;
        superblock->free_list = quarantined_node;
        // Re-poison so the freelist nodes stay poisoned while free.
        ASAN_POISON_MEMORY_REGION(quarantined_node, 1 << i);
      }
    }
    // Remove all large ojbects from quarantine and insert in freelists.
    for (int i = 18; i < 64; ++i) {
      while (quarantine[i]) {
        FreeListNode* quarantined_node = quarantine[i];
        ASAN_UNPOISON_MEMORY_REGION(quarantined_node, 1 << i);
        quarantine[i] = quarantined_node->next;
        quarantined_node->next = freelists[i];
        freelists[i] = quarantined_node;
        // Re-poison so the freelist nodes stay poisoned while free.
        ASAN_POISON_MEMORY_REGION(quarantined_node, 1 << i);
      }
    }
    quarantine_size = 0;
  }
}

#endif // HAVE_ADDRESS_SANITIZER

/**
 * Add a large allocation to a free list.
 */
void add_free_block(uintptr_t data, unsigned size_log2) {
  FreeListNode* free_node = reinterpret_cast<FreeListNode*>(data);
#ifdef HAVE_ADDRESS_SANITIZER
  add_to_quarantine(free_node, size_log2);
#else
  free_node->next = freelists[size_log2];
  freelists[size_log2] = free_node;
#endif
}

/**
 * Reuse a large allocation from a freelist.
 * Returns null if no reusable allocation was found.
 */
void* remove_free_block(unsigned size_log2) {
  FreeListNode* node = reinterpret_cast<FreeListNode*>(freelists[size_log2]);
  if (node) {
#ifdef HAVE_ADDRESS_SANITIZER
    ASAN_UNPOISON_MEMORY_REGION(node, 1 << size_log2);
#endif
    freelists[size_log2] = node->next;
  }
  return static_cast<void*>(node);
}

static void update_heap_bounds(void* allocation, size_t size) {
  if (allocation < heap_start) {
    heap_start = allocation;
  }
  void* allocation_end = static_cast<char*>(allocation) + size;
  if (allocation_end > heap_end) {
    heap_end = allocation_end;
  }
}

void* rho::GCNodeAllocator::Allocate(size_t bytes) {
  void* result = nullptr;
#ifdef HAVE_ADDRESS_SANITIZER
  // Increase size to include redzones.
  bytes += 2 * REDZONE_SIZE;
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
  void* end_redzone = offset_pointer(result, bytes - REDZONE_SIZE);

  // Poison the redzones.
  ASAN_POISON_MEMORY_REGION(start_redzone, REDZONE_SIZE);
  ASAN_POISON_MEMORY_REGION(end_redzone, REDZONE_SIZE);

  // Offset the result pointer past first redzone.
  result = offset_pointer(result, REDZONE_SIZE);
#endif  // HAVE_ADDRESS_SANITIZER
  return result;
}

void rho::GCNodeAllocator::Free(void* pointer) {
#ifdef HAVE_ADDRESS_SANITIZER
  // Adjust for redzone.
  pointer = offset_pointer(pointer, -REDZONE_SIZE);

  // Unpoison so we can link this block into a freelist.
  ASAN_UNPOISON_MEMORY_REGION(pointer, sizeof(FreeListNode));
#endif
#ifdef ALLOCATION_CHECK
  if (!Lookup(pointer)) {
    allocerr("can not free unknown/already-freed pointer");
  }
  remove_from_allocation_map(pointer);
#endif
  Superblock* superblock = Superblock::SuperblockFromPointer(pointer);
  if (superblock) {
    superblock->FreeSmallBlock(pointer);
  } else if (!alloctable->free_allocation(reinterpret_cast<uintptr_t>(pointer))) {
    allocerr("failed to free pointer - unallocated or double-free problem");
  }
}

/*
 * Iterates over all live objects and calls the argument function on
 * their pointers.
 * It is okay to free objects during the iteration, but adding objects
 * means they may not be found during the current iteration pass.
 */
void rho::GCNodeAllocator::ApplyToAllAllocations(std::function<void(void*)> fun) {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    Superblock* superblock = reinterpret_cast<Superblock*>(next_superblock);
    uintptr_t block = next_superblock + SUPERBLOCK_HEADER_SIZE;
    uintptr_t block_end = next_superblock + SUPERBLOCK_SIZE;
    unsigned block_size = superblock->block_size;
    unsigned superblock_size =
        (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) / block_size;
    unsigned bitset_entries = (superblock_size + 63) / 64;
    for (int i = 0; i < bitset_entries; ++i) {
      if (superblock->free[i] != ~0ull) {
        for (int index = 0; index < 64; ++index) {
          if (!(superblock->free[i] & (u64{1} << index))) {
#ifdef ALLOCATION_CHECK
            if (!lookup_in_allocation_map(reinterpret_cast<void*>(block))) {
              allocerr(
                  "apply to all blocks iterating over non-alloc'd pointer");
            }
#endif
#ifdef HAVE_ADDRESS_SANITIZER
            fun(offset_pointer(reinterpret_cast<void*>(block), REDZONE_SIZE));
#else
            fun(reinterpret_cast<void*>(block));
#endif
          }
          block += block_size;
        }
      } else {
        block += block_size * 64;
      }
    }
    next_superblock += SUPERBLOCK_SIZE;
  }
  alloctable->ApplyToAllBlocks(fun);
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
void* rho::GCNodeAllocator::Lookup(void* candidate) {
  void* result = nullptr;
  uintptr_t candidate_uint = reinterpret_cast<uintptr_t>(candidate);
  Superblock* superblock = Superblock::SuperblockFromPointer(candidate);
  if (superblock) {
    uintptr_t first_block =
        reinterpret_cast<uintptr_t>(superblock) + SUPERBLOCK_HEADER_SIZE;
    unsigned index = (candidate_uint - first_block) / superblock->block_size;
    unsigned bitset = index / 64;
    if (!(superblock->free[bitset] & (u64{1} << (index & 63)))) {
      result = reinterpret_cast<char*>(first_block)
          + index * superblock->block_size;
    } else {
      // The block is not allocated.
      result = nullptr;
    }
  } else if (candidate >= heap_start && candidate < heap_end) {
    HashBucket* bucket = alloctable->search(candidate_uint);
    if (bucket) {
      uintptr_t pointer = bucket->DataPointer();
      if (bucket->data & 2) {
        uintptr_t first_block = pointer + SUPERBLOCK_HEADER_SIZE;
        Superblock* superblock = reinterpret_cast<Superblock*>(pointer);
        unsigned index =
            (candidate_uint - first_block) >> superblock->block_size;
        unsigned bitset = index / 64;
        // Check that the block is actually allocated.
        if (!(superblock->free[bitset] & (u64{1} << (index & 63)))) {
          result = reinterpret_cast<char*>(first_block)
              + (index << superblock->block_size);
        }
      } else {
        result = reinterpret_cast<void*>(pointer);
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
    result = offset_pointer(result, REDZONE_SIZE);
  }
#endif
  return result;
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

void print_superblock(Superblock* superblock) {
  unsigned superblock_size =
      (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) / superblock->block_size;
  printf(">>>>>>>>>> SUPERBLOCK (blocksize=%d, num block=%d)\n",
      superblock->block_size, superblock_size);
  unsigned bitset_entries = (superblock_size + 63) / 64;
  for (int i = 0; i < bitset_entries; ++i) {
    int num_free = 0;
    for (int index = 0; index < 64; ++index) {
      if (superblock->free[i] & (u64{1} << index)) {
        num_free += 1;
      }
    }
    switch (num_free) {
      case 0:
        printf("#");  // Full.
        break;
      case 64:
        printf(".");  // Empty.
        break;
      default:
        printf("%d", (64 - num_free) / 7);
        break;
    }
    if (i + 1 < bitset_entries) {
      printf(" ");
    }
  }
  printf("\n");
}

void rho::GCNodeAllocator::DebugPrint() {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    print_superblock(reinterpret_cast<Superblock*>(next_superblock));
    next_superblock += SUPERBLOCK_SIZE;
  }
  alloctable->PrintTable();
}

