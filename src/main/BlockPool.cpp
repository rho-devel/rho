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

#include "rho/BlockPool.hpp"

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

#include <cstdint>
#include <cstdlib>
#include <functional>
#include <limits>
#include <map>

typedef std::uint64_t u64;
typedef std::uint32_t u32;
using std::function;

#ifdef ALLOCATION_CHECK
typedef std::map<void*, void*> allocation_map;
static allocation_map allocations;

static bool iterating = false;

static void add_to_allocation_map(void* allocation, size_t size);
static void remove_from_allocation_map(void* allocation);
static void* lookup_in_allocation_map(void* tentative_pointer);
#endif

static void allocerr(const char* message) {
  fprintf(stderr, "ERROR: %s\n", message);
  abort();
}

using std::size_t;

// Tracking heap bounds for fast rejection in pointer lookup.
void* heap_start = reinterpret_cast<void*>(UINTPTR_MAX);
void* heap_end = reinterpret_cast<void*>(0);

// Hash bucket for the sparse block table and free lists.
// The two lowest bits of the data pointer are flags indicating
// if the hash bucket contains a superblock (2) and whether this
// hash bucket is the first representing that allocation (1).
struct HashBucket {
  uintptr_t data;
  unsigned size;
};

struct Superblock;

// Freelists are used to store superblock nodes and large allocations.
// For large allocations the block and superblock members are unused.
struct FreeListNode {
  FreeListNode* next;
  u32 block;  // Used only for superblock free nodes.
  Superblock* superblock;  // Used only for superblock free nodes.
};

// Superblocks are used to allocate small and medium sized objects.
// The bitset tracks which blocks are currently allocated.
// For small objects block_size is the actual object size.
// For medium objects block_size is the 2-log of the object size.
struct Superblock {
  u32 block_size;
  u32 next_untouched;
  FreeListNode* free_list;
  u64 free[];  // Free bitset.

  Superblock(u32 block_size, unsigned bitset_entries):
      block_size(block_size),
      next_untouched(0),
      free_list(nullptr) {
    // Here we mark all bitset entries as free, later we don't have to do
    // precise range checking while iterating allocated blocks when
    // the number of blocks is not evenly divisible by 64.
    for (int i = 0; i < bitset_entries; ++i) {
      free[i] = ~0ull;
    }
  }
};

unsigned sparse_bits = 16;
unsigned num_sparse_buckets = 1 << sparse_bits;
unsigned hash_mask = num_sparse_buckets - 1;

// The low pointer bits are masked out in the hash function.
#define LOW_BITS (19)
#define MAX_COLLISIONS (6)
#define MAX_REBALANCE_COLLISIONS (3)

// Computes the hash key for a pointer.
static unsigned hash_ptr(uintptr_t ptr, unsigned hash_mask) {
  unsigned low = (ptr << 4) & 0xFFFFFFFF;
  unsigned hi = (ptr >> 32) & 0xFFFFFFFF;
  unsigned hash = low ^ hi;
  low = hash & 0xFFFF;
  hi = (hash >> 16) & 0xFFFF;
  hash = low ^ hi ^ (ptr & (~0xFFFF));
  return hash & hash_mask;
}

// Computes the next key in a probe chain.
static unsigned probe_func(unsigned hash, int i, unsigned hash_mask) {
  return (hash + i + 1) & hash_mask;
}

// NUM_SMALL_POOLS = (256 / 8) + 1 = 33
#define NUM_SMALL_POOLS (33)

HashBucket* sparse_buckets = nullptr;
FreeListNode* small_freelists[NUM_SMALL_POOLS];
FreeListNode* freelists[64];
Superblock* medium_superblocks[18];
Superblock* small_superblocks[NUM_SMALL_POOLS];

// Constant indicating a deleted bucket.
#define DELETED_KEY (UINTPTR_MAX)

unsigned add_collision = 0;
unsigned remove_collision = 0;
unsigned lookup_collision = 0;

// Find the 2-log of the next power of two greater than or equal to size.
static unsigned next_log2_32(int size) {
  if (size == 0) {
    return 0;
  }
  int log2 = 0;
  int temp = size;
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

// Arena is 1Gb = 30 bits.
#define ARENASIZE (1 << 30)

#define SUPERBLOCK_BITS (18)
#define SUPERBLOCK_SIZE (1 << SUPERBLOCK_BITS)

// NOTE: We use a fixed superblock header size, regardless of block size. This
// leaves some unused bitset entries for larger block sizes.
#define SUPERBLOCK_HEADER_SIZE (1040)

// 1 Mbyte for medium block arenas.
#define LARGE_ARENA_SIZE_LOG2 (19)
#define LARGE_ARENA_SIZE (1 << LARGE_ARENA_SIZE_LOG2)
#define LARGE_HEADER_SIZE (1040)

void* superblock_arena = nullptr;
uintptr_t arena_superblock_start;
uintptr_t arena_superblock_end;
uintptr_t arena_superblock_next;

// Allocates a new superlbock from the small object arena.  If the arena is
// full, we call abort with the message "out of superblock space".
Superblock* new_superblock(int block_size) {
  if (arena_superblock_next >= arena_superblock_end) {
    allocerr("out of superblock space");
  }
  void* pointer = reinterpret_cast<void*>(arena_superblock_next);
  unsigned superblock_size =
      (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) / block_size;
  unsigned bitset_entries = (superblock_size + 63) / 64;
  Superblock* superblock = new (pointer)Superblock(block_size, bitset_entries);
  arena_superblock_next += SUPERBLOCK_SIZE;
  return superblock;
}

// Compute the address of the superblock header from a pointer.
// Returns nullptr if candidate pointer is not inside the small object arena
// or if the pointer points inside the superblock header.
Superblock* superblock_from_pointer(void* pointer) {
  uintptr_t candidate = reinterpret_cast<uintptr_t>(pointer);
  if (candidate >= arena_superblock_start && candidate < arena_superblock_next) {
    if ((candidate & (SUPERBLOCK_SIZE - 1)) < SUPERBLOCK_HEADER_SIZE) {
      // The pointer points inside the superblock header.
      return nullptr;
    }
    return reinterpret_cast<Superblock*>(
        candidate & ~uintptr_t{SUPERBLOCK_SIZE - 1});
  } else {
    return nullptr;
  }
}

// Free a pointer inside a given superblock. The block MUST be in the given
// superblock.
void free_small_block(void* pointer, Superblock* superblock) {
  uintptr_t block = reinterpret_cast<uintptr_t>(pointer);
  uintptr_t superblock_start = reinterpret_cast<uintptr_t>(superblock)
      + SUPERBLOCK_HEADER_SIZE;
  unsigned index = (block - superblock_start) / superblock->block_size;
  unsigned bitset = index / 64;
  superblock->free[bitset] |= u64{1} << (index & 63);

  // Use the block as a free list node and prepend to the free list.
  FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
  free_node->block = index;
  free_node->superblock = superblock;
  int pool_index = superblock->block_size / 8;
  if (small_superblocks[pool_index]) {
    superblock = small_superblocks[pool_index];
  } else {
    small_superblocks[pool_index] = superblock;
  }
  free_node->next = superblock->free_list;
  superblock->free_list = free_node;
}

// Tag a block in a superblock as allocated.
void tag_block_allocated(Superblock* superblock, unsigned block) {
  unsigned bitset = block / 64;
  superblock->free[bitset] &= ~(u64{1} << (block & 63));
}

// Allocate the small object arena, null out freelist head pointers,
// and initialize the hashtable.
void BlockPool::Initialize() {
  superblock_arena = sbrk(ARENASIZE);
  uintptr_t start = reinterpret_cast<uintptr_t>(superblock_arena);
  uintptr_t end = start + ARENASIZE;
  uintptr_t pad = SUPERBLOCK_SIZE - (start & (SUPERBLOCK_SIZE - 1));
  uintptr_t num_sb = (end - arena_superblock_start) >> SUPERBLOCK_BITS;
  arena_superblock_start = start + pad;
  arena_superblock_end = arena_superblock_start + num_sb * SUPERBLOCK_SIZE;
  arena_superblock_next = arena_superblock_start;

  for (int i = 0; i < NUM_SMALL_POOLS; ++i) {
    small_superblocks[i] = nullptr;
    small_freelists[i] = nullptr;
  }

  for (int i = 0; i < 64; ++i) {
    freelists[i] = nullptr;
  }

  for (int i = 0; i < 18; ++i) {
    medium_superblocks[i] = nullptr;
  }

  sparse_buckets = new HashBucket[num_sparse_buckets];
  for (int i = 0; i < num_sparse_buckets; ++i) {
    sparse_buckets[i].data = 0;
  }
}

// Attempts to rebalance the hashtable.
// Returns false if the rebalance failed.
bool rebalance_sparse_table(unsigned new_sparse_bits) {
  if (new_sparse_bits > 29) {
    allocerr("allocation hashtable is too large");
  }
  unsigned new_table_size = 1 << new_sparse_bits;
  unsigned new_mask = new_table_size - 1;
  HashBucket* new_table = new HashBucket[new_table_size];
  for (int i = 0; i < new_table_size; ++i) {
    new_table[i].data = 0;
  }
  // Build new table.
  for (int i = 0; i < num_sparse_buckets; ++i) {
    HashBucket& bucket = sparse_buckets[i];
    if (bucket.data != 0 && bucket.data != DELETED_KEY) {
      unsigned hash = hash_ptr(bucket.data >> LOW_BITS, new_mask);
      bool placed = false;
      for (int j = 0; j < MAX_REBALANCE_COLLISIONS; ++j) {
        if (new_table[hash].data) {
          new_table[hash].data = bucket.data;
          new_table[hash].size = bucket.size;
          placed = true;
          break;
        }
        hash = probe_func(hash, j, new_mask);
      }
      if (!placed) {
        delete new_table;
        return false;
      }
    }
  }
  // Replace the old table.
  delete sparse_buckets;
  sparse_buckets = new_table;
  sparse_bits = new_sparse_bits;
  num_sparse_buckets = new_table_size;
  hash_mask = new_mask;
  return true;
}

// Add an allocation (large object or medium object superblock) to the
// sparse allocations hashtable.
void add_sparse_block(uintptr_t pointer, size_t size) {
  uintptr_t key = pointer >> LOW_BITS;
  uintptr_t key_end = (pointer + (1 << size) + ((1 << LOW_BITS) - 1))
      >> LOW_BITS;
  bool first = true;
  do {
    unsigned hash = hash_ptr(key, hash_mask);
    bool added = false;
    for (int i = 0; i < MAX_COLLISIONS; ++i) {
      HashBucket& bucket = sparse_buckets[hash];
      if (bucket.data == 0 || (bucket.data == DELETED_KEY)) {
        if (first) {
          // Tag this as the first hash entry.
          first = false;
          sparse_buckets[hash].data = pointer | 1;
        } else {
          sparse_buckets[hash].data = pointer;
        }
        sparse_buckets[hash].size = size;
        added = true;
        break;
      }
      add_collision += 1;
      hash = probe_func(hash, i, hash_mask);
    }
    if (!added) {
      // Too many collisions, rebalance the hash table.
      unsigned new_sparse_bits = sparse_bits;
      do {
        new_sparse_bits += 1;
      } while (!rebalance_sparse_table(new_sparse_bits));
    }
    key += 1;
  } while (key < key_end);
}

// Add a large allocation to a free list.
void add_free_block(uintptr_t data, unsigned size_log2) {
  FreeListNode* new_node = reinterpret_cast<FreeListNode*>(data);
  new_node->next = freelists[size_log2];
  freelists[size_log2] = new_node;
}

// Reuse a large allocation from a freelist.
// Returns null if no reusable allocation was found.
void* remove_free_block(unsigned size_log2) {
  FreeListNode* node = reinterpret_cast<FreeListNode*>(freelists[size_log2]);
  if (node) {
    freelists[size_log2] = node->next;
  }
  return static_cast<void*>(node);
}

/*
 * Remove an allocation from the hashtable.  If the allocation is in a
 * superblock, the superblock is left as is but the superblock bitset is
 * updated to indicate that the block is free.
 */
bool remove_sparse_block(uintptr_t pointer) {
  uintptr_t key = pointer >> LOW_BITS;
  unsigned hash = hash_ptr(key, hash_mask);
  unsigned size_log2 = 0;
  HashBucket* bucket = nullptr;
  for (int i = 0; i < MAX_COLLISIONS; ++i) {
    bucket = &sparse_buckets[hash];
    if (bucket->data != 0) {
      uintptr_t data = bucket->data & ~uintptr_t{3};
      uintptr_t first_block = data + LARGE_HEADER_SIZE;
      if ((bucket->data & 2) && first_block <= pointer
          && (data + LARGE_ARENA_SIZE) > pointer) {
        Superblock* superblock = reinterpret_cast<Superblock*>(data);
        unsigned index = (pointer - first_block) >> superblock->block_size;
        unsigned bitset = index / 64;
        superblock->free[bitset] |= u64{1} << (index & 63);
        FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
        free_node->block = index;
        free_node->superblock = superblock;
        if (medium_superblocks[superblock->block_size]) {
          superblock = medium_superblocks[superblock->block_size];
        } else {
          medium_superblocks[superblock->block_size] = superblock;
        }
        free_node->next = superblock->free_list;
        superblock->free_list = free_node;
        return true;
      } else if (data == pointer) {
        bucket->data = DELETED_KEY;
        size_log2 = bucket->size;
        add_free_block(pointer, bucket->size);
        break;
      }
    } else {
      // Free failed.
      return false;
    }
    remove_collision += 1;
    hash = probe_func(hash, i, hash_mask);
  }
  key += 1;
  uintptr_t key_end = (pointer + (1 << size_log2) + ((1 << LOW_BITS) - 1))
      >> LOW_BITS;
  while (key < key_end) {
    unsigned hash = hash_ptr(key, hash_mask);
    for (int i = 0; i < MAX_COLLISIONS; ++i) {
      HashBucket& bucket = sparse_buckets[hash];
      if (bucket.data == pointer) {
        bucket.data = DELETED_KEY;
        break;
      }
      if (!bucket.data) {
        // Free failed.
        return false;
      }
      remove_collision += 1;
      hash = probe_func(hash, i, hash_mask);
    }
    key += 1;
  }
  return true;
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

void* BlockPool::AllocBlock(size_t bytes) {
  void* result;
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
    result = AllocSmall(block_bytes);
  } else {
    // Default to separate allocation if block size is larger than small block
    // threshold.
    unsigned log2 = next_log2_32(bytes);
    block_bytes = 1 << log2;
    result = AllocLarge(log2);
    update_heap_bounds(result, block_bytes);
  }
  if (!result) {
    allocerr("returning null from alloc");
  }

#ifdef ALLOCATION_CHECK
  if (lookup_in_allocation_map(result)) {
    allocerr("reusing live allocation");
  }
  add_to_allocation_map(result, block_bytes);
  Lookup(result);  // Check lookup table consistency.
#endif
  return result;
}

// Allocate a medium or large object.
void* BlockPool::AllocLarge(unsigned size_log2) {
  void* result = nullptr;
  if (size_log2 <= 17) {
    unsigned num_blocks = (LARGE_ARENA_SIZE - LARGE_HEADER_SIZE) >> size_log2;
    Superblock* superblock;
    if (medium_superblocks[size_log2]) {
      // Reuse existing superblock for this allocation size.
      superblock = medium_superblocks[size_log2];
    } else {
      // Allocate a midsize superblock.
      void* arena = new double[LARGE_ARENA_SIZE / sizeof(double)];
      unsigned bitset_entries =
          ((1 << (LARGE_ARENA_SIZE_LOG2 - size_log2)) + 63) / 64;
      superblock = new (arena)Superblock(size_log2, bitset_entries);
      add_sparse_block(reinterpret_cast<uintptr_t>(arena) | 2,
          LARGE_ARENA_SIZE_LOG2);
      medium_superblocks[size_log2] = superblock;
    }
    if (superblock->free_list) {
      FreeListNode* free_node = superblock->free_list;
      unsigned index = free_node->block;
      tag_block_allocated(free_node->superblock, index);
      superblock->free_list = free_node->next;
      if (!superblock->free_list && superblock->next_untouched == num_blocks) {
        medium_superblocks[size_log2] = nullptr;
      }
      result = free_node;
    } else {
      unsigned index = superblock->next_untouched;
      tag_block_allocated(superblock, index);
      superblock->next_untouched += 1;
      if (superblock->next_untouched == num_blocks) {
        medium_superblocks[size_log2] = nullptr;
      }
      result = reinterpret_cast<char*>(superblock) + LARGE_HEADER_SIZE
          + (index << size_log2);
    }
  } else {
    result = remove_free_block(size_log2);
    if (!result) {
      result = new double[(1L << size_log2) / sizeof(double)];
    }
    add_sparse_block(reinterpret_cast<uintptr_t>(result), size_log2);
  }
  return result;
}

void BlockPool::FreeBlock(void* p) {
#ifdef ALLOCATION_CHECK
  if (!Lookup(p)) {
    allocerr("can not free unknown/already-freed pointer");
  }
  remove_from_allocation_map(p);
#endif
  Superblock* superblock = superblock_from_pointer(p);
  if (superblock) {
    free_small_block(p, superblock);
  } else if (!remove_sparse_block(reinterpret_cast<uintptr_t>(p))) {
    allocerr("failed to free pointer - unallocated or double-free problem");
  }
}

// Allocate a small object (<= 256 bytes).
void* BlockPool::AllocSmall(size_t block_size) {
  unsigned pool_index = (block_size + 7) / 8;
  block_size = pool_index * 8;
  assert(block_size >= 32 && block_size <= 256
      && "Only use AllocSmall to allocate objects between 32 and 256 bytes.");

  unsigned num_blocks = (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) / block_size;
  Superblock* superblock;
  if (small_superblocks[pool_index]) {
    superblock = small_superblocks[pool_index];
  } else {
    superblock = new_superblock(block_size);
    if (!superblock) {
      return nullptr;
    }
    small_superblocks[pool_index] = superblock;
  }
  if (superblock->free_list) {
    FreeListNode* free_node = superblock->free_list;
    u32 index = free_node->block;
    tag_block_allocated(free_node->superblock, index);
    superblock->free_list = free_node->next;
    if (!superblock->free_list && superblock->next_untouched == num_blocks) {
      small_superblocks[pool_index] = nullptr;
    }
    return free_node;
  } else {
    u32 index = superblock->next_untouched;
    tag_block_allocated(superblock, index);
    superblock->next_untouched += 1;
    if (superblock->next_untouched == num_blocks) {
      small_superblocks[pool_index] = nullptr;
    }
    return reinterpret_cast<char*>(superblock) + SUPERBLOCK_HEADER_SIZE
        + (index * block_size);
  }
}

// Iterates over all live objects and calls the argument function on
// their pointers.
// It is okay to free objects during the iteration, but adding objects
// means they may not be found during the current iteration pass.
void BlockPool::ApplyToAllBlocks(std::function<void(void*)> fun) {
#ifdef ALLOCATION_CHECK
  iterating = true;
#endif
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
            fun(reinterpret_cast<void*>(block));
          }
          block += block_size;
        }
      } else {
        block += block_size * 64;
      }
    }
    next_superblock += SUPERBLOCK_SIZE;
  }
  for (int i = 0; i < num_sparse_buckets; ++i) {
    HashBucket& bucket = sparse_buckets[i];
    if (bucket.data && bucket.data != DELETED_KEY) {
      if (bucket.data & 1) {
        void* pointer = reinterpret_cast<void*>(bucket.data & ~uintptr_t{3});
        if (bucket.data & 2) {
          // This is a large superblock.
          Superblock* superblock = reinterpret_cast<Superblock*>(pointer);
          uintptr_t block =
              reinterpret_cast<uintptr_t>(pointer) + LARGE_HEADER_SIZE;
          uintptr_t block_end =
              reinterpret_cast<uintptr_t>(pointer) + LARGE_ARENA_SIZE;
          unsigned block_size = 1 << superblock->block_size;
          unsigned superblock_size = (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE)
              / superblock->block_size;
          unsigned bitset_entries =
              ((1 << (LARGE_ARENA_SIZE_LOG2 - superblock->block_size)) + 63)
              / 64;
          for (int i = 0; i < bitset_entries; ++i) {
            if (superblock->free[i] != ~0ull) {
              for (int index = 0; index < 64; ++index) {
                if (!(superblock->free[i] & (u64{1} << index))) {
#ifdef ALLOCATION_CHECK
                  if (!lookup_in_allocation_map(reinterpret_cast<void*>(
                      block))) {
                    allocerr("apply to all blocks iterating over "
                        "non-alloc'd pointer");
                  }
#endif
                  fun(reinterpret_cast<void*>(block));
                }
                block += block_size;
              }
            } else {
              block += block_size * 64;
            }
          }
        } else {
          fun(pointer);
        }
      }
    }
  }
#ifdef ALLOCATION_CHECK
  iterating = false;
#endif
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
void* BlockPool::Lookup(void* candidate) {
  void* result = nullptr;
  uintptr_t candidate_uint = reinterpret_cast<uintptr_t>(candidate);
  Superblock* superblock = superblock_from_pointer(candidate);
  if (superblock) {
    uintptr_t first_block =
        reinterpret_cast<uintptr_t>(superblock) + SUPERBLOCK_HEADER_SIZE;
    if (candidate_uint < first_block) {
      // The candidate pointer points inside the superblock header.
      return nullptr;
    }
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
    unsigned hash = hash_ptr(candidate_uint >> LOW_BITS, hash_mask);
    for (int i = 0; i < MAX_COLLISIONS; ++i) {
      HashBucket& bucket = sparse_buckets[hash];
      if (bucket.data && bucket.data != DELETED_KEY) {
        uintptr_t pointer = bucket.data & ~uintptr_t{3};
        if (bucket.data & 2) {
          uintptr_t first_block = pointer + LARGE_HEADER_SIZE;
          if (first_block <= candidate_uint
              && pointer + LARGE_ARENA_SIZE > candidate_uint) {
            Superblock* superblock = reinterpret_cast<Superblock*>(pointer);
            unsigned index =
                (candidate_uint - first_block) >> superblock->block_size;
            unsigned bitset = index / 64;
            if (!(superblock->free[bitset] & (u64{1} << (index & 63)))) {
              result = reinterpret_cast<char*>(first_block)
                  + (index << superblock->block_size);
            }
            break;
          }
        } else if (pointer <= candidate_uint
              && (pointer + (1L << bucket.size)) > candidate_uint) {
          result = reinterpret_cast<void*>(pointer);
          break;
        }
      }
      if (!bucket.data) {
        break;
      }
      lookup_collision += 1;
      hash = probe_func(hash, i, hash_mask);
    }
  }
#ifdef ALLOCATION_CHECK
  if (result != lookup_in_allocation_map(candidate)) {
    allocerr("allocation map mismatch");
  }
#endif
  return result;
}

#ifdef ALLOCATION_CHECK
void add_to_allocation_map(void* allocation, size_t size) {
  if (iterating) {
    allocerr("allocating node during GC pass");
  }
  void* allocation_end = static_cast<char*>(allocation) + size;
  allocations[allocation] = allocation_end;
}

void remove_from_allocation_map(void* allocation) {
  if (iterating) {
    allocerr("freeing node during GC pass");
  }
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

static void print_sparse_table() {
  printf(">>>>> SPARSE TABLE\n");
  int size = 0;
  for (int i = 0; i < num_sparse_buckets; i += 16) {
    int count = 0;
    for (int j = 0; j < 16 && i + j < num_sparse_buckets; ++j) {
      HashBucket& bucket = sparse_buckets[i + j];
      if (bucket.data && bucket.data != DELETED_KEY) {
        count += 1;
      }
    }
    if (count) {
      printf("%X", count);
    } else {
      printf(".");
    }
    size += count;
  }
  printf("\n");
  printf("table size: %d\n", size);
}

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

void BlockPool::DebugPrint() {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    print_superblock(reinterpret_cast<Superblock*>(next_superblock));
    next_superblock += SUPERBLOCK_SIZE;
  }
  print_sparse_table();
}

void BlockPool::DebugRebalance(int new_bits) {
  rebalance_sparse_table(new_bits);
  print_sparse_table();
}

