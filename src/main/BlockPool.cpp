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
#include <unistd.h>

#include <cstdint>
#include <cstdlib>
#include <functional>
#include <limits>
#include <map>

#include "rho/BlockPool.hpp"

#ifdef HAVE_ADDRESS_SANITIZER
#include "rho/AddressSanitizer.hpp"
#endif

// The low pointer bits are masked out in the hash function.
#define LOW_BITS (19)
#define MAX_COLLISIONS (6)
#define MAX_REBALANCE_COLLISIONS (3)

// NUM_SMALL_POOLS = (256 / 8) + 1 = 33
#define NUM_SMALL_POOLS (33)

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

/*
 * NOTE: We use a fixed superblock header size, regardless of block size. This
 * leaves some unused bitset entries for larger block sizes.
 *
 * The fixed header size is 1168 bytes. This is the breakdown:
 *    block_size     =   32 bytes
 *    next_untouched =   32 bytes
 *    free_list      =   64 bytes
 *    free bitset    = 1040 bytes
 *    total          = 1168 bytes
 *
 * The bitset can map 1040 * 8 = 8320 blocks.  The minimum block size for a
 * 2^18 superblock is 32 bytes, meaning the bitset covers 8320 * 8 = 266240
 * bytes, which is more than the required range 2^18 - 1168 = 260976.
 *
 * For a 2^19 size superblock the minimum object size is 64 bytes,
 * and 1040 * 8 * 64 = 532480 < 2^19 - 1168. So all objects are mappable
 * for a 2^19 superblock.
 */
#define SUPERBLOCK_HEADER_SIZE (1168)

typedef std::uint64_t u64;
typedef std::uint32_t u32;

using std::function;
using std::size_t;

#ifdef ALLOCATION_CHECK
typedef std::map<void*, void*> allocation_map;
static allocation_map allocations;

static void add_to_allocation_map(void* allocation, size_t size);
static void remove_from_allocation_map(void* allocation);
static void* lookup_in_allocation_map(void* tentative_pointer);
#endif

static void allocerr(const char* message) {
  fprintf(stderr, "ERROR: %s\n", message);
  abort();
}

// Arena for small-object superblocks:
static void* superblock_arena = nullptr;
static uintptr_t arena_superblock_start;
static uintptr_t arena_superblock_end;
static uintptr_t arena_superblock_next;

struct Superblock;

// Pointers to small object superblocks with available blocks.
static Superblock* small_superblocks[NUM_SMALL_POOLS];

// Pointers to medium object superblocks with available blocks.
static Superblock* medium_superblocks[18];

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
static size_t small_quarantine_size = 0;
static size_t quarantine_size = 0;
static FreeListNode* small_quarantine[NUM_SMALL_POOLS];
static FreeListNode* quarantine[64];

// Helper function for address calculations.
static void* offset_pointer(void* pointer, size_t bytes) {
    return static_cast<char*>(pointer) + bytes;
}

#endif // HAVE_ADDRESS_SANITIZER

/**
 * Hash bucket for the allocation table.
 * The two lowest bits of the data pointer are flags indicating
 * if the hash bucket contains a superblock (2) and whether this
 * hash bucket is the first representing that allocation (1).
 */
struct HashBucket {
  uintptr_t data;
  unsigned size;

  /* Masks away the pointer bits from the data field. */
  uintptr_t DataPointer() {
    return data & ~uintptr_t{3};
  }
};

struct Superblock;

/**
 * Freelists are used to store superblock nodes and large allocations.
 * For large allocations the block and superblock members are unused.
 */
struct FreeListNode {
  FreeListNode* next;
  u32 block;  // Used only for superblock free nodes.
  Superblock* superblock;  // Used only for superblock free nodes.
};

// Forward declarations (used in struct Superblock).
static void alloctable_insert(uintptr_t pointer, size_t size);
static bool alloctable_rebalance(unsigned new_alloctable_bits);
static bool alloctable_remove_maybe(uintptr_t pointer);
static void alloctable_erase(uintptr_t pointer, size_t size);
static bool hashtable_try_insert(HashBucket* table, uintptr_t pointer,
    size_t size);
static void* remove_free_block(unsigned size_log2);

/**
 * Superblocks are used to allocate small and medium sized objects.
 * The bitset tracks which blocks are currently allocated.
 * For small objects block_size is the actual object size.
 * For medium objects block_size is the 2-log of the object size.
 */
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

  /**
   * Allocates a new superblock from the small object arena.
   * Returns nullptr if the arena space is full.
   * If nullptr is returned we need to fall back on using a separately
   * allocated superblock.
   */
  static Superblock* NewSuperblock(int block_size) {
    if (arena_superblock_next >= arena_superblock_end) {
      return nullptr;
    }
    void* pointer = reinterpret_cast<void*>(arena_superblock_next);
#ifdef HAVE_ADDRESS_SANITIZER
    // Unpoison only the header.
    ASAN_UNPOISON_MEMORY_REGION(pointer, SUPERBLOCK_HEADER_SIZE);
#endif
    unsigned superblock_size =
        (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) / block_size;
    unsigned bitset_entries = (superblock_size + 63) / 64;
    Superblock* superblock = new (pointer)Superblock(block_size, bitset_entries);
    arena_superblock_next += SUPERBLOCK_SIZE;
    return superblock;
  }

  /**
   * Compute the address of the superblock header from a pointer.
   * Returns nullptr if candidate pointer is not inside the small object arena
   * or if the pointer points inside the superblock header.
   */
  static Superblock* SuperblockFromPointer(void* pointer) {
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

  /**
   * Allocate a small block (bytes >= 32 && bytes <= 256).
   * Returns nullptr if there is no more space left for this object
   * size in the small object arena.
   */
  static void* AllocSmall(size_t block_size) {
    unsigned pool_index = (block_size + 7) / 8;
    block_size = pool_index * 8;
    assert(block_size >= 32 && block_size <= 256
        && "Only use AllocSmall to allocate objects between 32 and 256 bytes.");

    unsigned num_blocks =
        (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) / block_size;
    Superblock* superblock;
    if (small_superblocks[pool_index]) {
      superblock = small_superblocks[pool_index];
    } else {
      superblock = Superblock::NewSuperblock(block_size);
      if (!superblock) {
        return nullptr;
      }
      small_superblocks[pool_index] = superblock;
    }
    if (superblock->free_list) {
      FreeListNode* free_node = superblock->free_list;
#ifdef HAVE_ADDRESS_SANITIZER
      ASAN_UNPOISON_MEMORY_REGION(free_node, block_size);
#endif
      u32 index = free_node->block;
      Superblock::TagBlockAllocated(free_node->superblock, index);
      superblock->free_list = free_node->next;
      if (!superblock->free_list && superblock->next_untouched == num_blocks) {
        small_superblocks[pool_index] = nullptr;
      }
      return free_node;
    } else {
      u32 index = superblock->next_untouched;
      Superblock::TagBlockAllocated(superblock, index);
      superblock->next_untouched += 1;
      if (superblock->next_untouched == num_blocks) {
        small_superblocks[pool_index] = nullptr;
      }
      void* result = reinterpret_cast<char*>(superblock)
          + SUPERBLOCK_HEADER_SIZE + (index * block_size);
#ifdef HAVE_ADDRESS_SANITIZER
      ASAN_UNPOISON_MEMORY_REGION(result, block_size);
#endif
      return result;
    }
  }

  /**
   * Free a pointer inside a given superblock. The block MUST be in the given
   * superblock.
   */
  static void FreeSmallBlock(void* pointer, Superblock* superblock) {
    uintptr_t block = reinterpret_cast<uintptr_t>(pointer);
    uintptr_t superblock_start = reinterpret_cast<uintptr_t>(superblock)
        + SUPERBLOCK_HEADER_SIZE;
    unsigned block_size = superblock->block_size;
    unsigned index = (block - superblock_start) / block_size;
    unsigned bitset = index / 64;
    superblock->free[bitset] |= u64{1} << (index & 63);

    // Use the block as a free list node and prepend to the free list.
    FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
    free_node->block = index;
    free_node->superblock = superblock;
    int pool_index = block_size / 8;
#ifdef HAVE_ADDRESS_SANITIZER
    // Add to quarantine and poison.
    small_quarantine_size += block_size;
    free_node->next = small_quarantine_size[pool_index];
    small_quarantine[pool_index] = superblock->free_list;
    ASAN_POISON_MEMORY_REGION(free_node, block_size);
    // Maybe clear the quarantine.
    if (small_quarantine_size > MAX_QUARANTINE_SIZE) {
      // Remove all small ojbects from quarantine and insert in freelists.
      for (int i = 0; i < NUM_SMALL_POOLS; ++i) {
        while (small_quarantine[i]) {
          FreeNode* quarantined_node = small_quarantine[i];
          ASAN_UNPOISON_MEMORY_REGION(quarantined_node, block_size);
          small_quarantine[i] = quarantined_node->next;
          if (small_superblocks[i]) {
            superblock = small_superblocks[i];
          } else {
            small_superblocks[i] = superblock;
          }
          quarantined_node->next = superblock->free_list;
          superblock->free_list = quarantined_node;
          // Re-poison so the freelist nodes stay poisoned while free.
          ASAN_POISON_MEMORY_REGION(quarantined_node, block_size);
        }
      }
      small_quarantine_size = 0;
    }
#else
    if (small_superblocks[pool_index]) {
      superblock = small_superblocks[pool_index];
    } else {
      small_superblocks[pool_index] = superblock;
    }
    free_node->next = superblock->free_list;
    superblock->free_list = free_node;
#endif
  }

  /** Allocate a medium or large object. */
  static void* AllocLarge(unsigned size_log2) {
    assert(size_log2 >= 6
        && "Can not allocate objects smaller than 64 bytes using AllocLarge");
    void* result = nullptr;
    if (size_log2 <= 17) {
      unsigned num_blocks =
          (MEDIUM_SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) >> size_log2;
      Superblock* superblock;
      if (medium_superblocks[size_log2]) {
        // Reuse existing superblock for this allocation size.
        superblock = medium_superblocks[size_log2];
      } else {
        // Allocate a midsize superblock.
        void* arena = new double[MEDIUM_SUPERBLOCK_SIZE / sizeof(double)];
        unsigned bitset_entries =
            ((1 << (MEDIUM_SUPERBLOCK_SIZE_LOG2 - size_log2)) + 63) / 64;
        superblock = new (arena)Superblock(size_log2, bitset_entries);
        alloctable_insert(reinterpret_cast<uintptr_t>(arena) | 2,
            MEDIUM_SUPERBLOCK_SIZE_LOG2);
        medium_superblocks[size_log2] = superblock;
      }
      if (superblock->free_list) {
        FreeListNode* free_node = superblock->free_list;
#ifdef HAVE_ADDRESS_SANITIZER
        ASAN_UNPOISON_MEMORY_REGION(free_node, 1 << size_log2);
#endif
        unsigned index = free_node->block;
        Superblock::TagBlockAllocated(free_node->superblock, index);
        superblock->free_list = free_node->next;
        if (!superblock->free_list && superblock->next_untouched == num_blocks) {
          medium_superblocks[size_log2] = nullptr;
        }
        result = free_node;
      } else {
        unsigned index = superblock->next_untouched;
        Superblock::TagBlockAllocated(superblock, index);
        superblock->next_untouched += 1;
        if (superblock->next_untouched == num_blocks) {
          medium_superblocks[size_log2] = nullptr;
        }
        result = reinterpret_cast<char*>(superblock) + SUPERBLOCK_HEADER_SIZE
            + (index << size_log2);
#ifdef HAVE_ADDRESS_SANITIZER
        ASAN_UNPOISON_MEMORY_REGION(result, 1 << size_log2);
#endif
      }
    } else {
      result = remove_free_block(size_log2);
      if (!result) {
        result = new double[(1L << size_log2) / sizeof(double)];
      }
      alloctable_insert(reinterpret_cast<uintptr_t>(result), size_log2);
    }
    return result;
  }

  /** Tag a block in a superblock as allocated. */
  static void TagBlockAllocated(Superblock* superblock, unsigned block) {
    unsigned bitset = block / 64;
    superblock->free[bitset] &= ~(u64{1} << (block & 63));
  }
};

static unsigned alloctable_bits = 16;
static unsigned num_alloctable_buckets = 1 << alloctable_bits;

// Mask deciding which bits of a hash key are used to index the hash table.
static unsigned alloctable_hash_mask = num_alloctable_buckets - 1;

static unsigned add_collisions = 0;
static unsigned remove_collisions = 0;
static unsigned lookup_collisions = 0;

/**
 * Computes the hash key for a pointer.
 *
 * The lowest 19 bits (LOW_BITS) of the pointer are discarded before
 * generating a hash key. Consecutive pointers are not hashed directly
 * sequentially, instead spread by 16 entries. This is to allow hash
 * collisions to be stored in the neighboring buckets.
 *
 * The hash function divides the pointer into quarters and XORs them
 * together. This ensures that any single bit flip will hash to
 * a different bucket.
 */
static unsigned pointer_hash(uintptr_t pointer) {
  pointer >>= LOW_BITS;
  pointer <<= 4; // Spread out hash keys to leave room for collision buckets.
  unsigned low = pointer & 0xFFFFFFFF;
  unsigned hi = pointer >> 32;
  unsigned hash = low ^ hi;
  low = hash & 0xFFFF;
  hi = (hash >> 16) & 0xFFFF;
  hash = (low >> 16) ^ low ^ hi ^ (pointer & (~0xFFFF));
  return hash & alloctable_hash_mask;
}

// Computes the next key in a probe chain.
static unsigned probe_func(unsigned hash, int i) {
  return (hash + i + 1) & alloctable_hash_mask;
}

// Tracking heap bounds for fast rejection in pointer lookup.
static void* heap_start = reinterpret_cast<void*>(UINTPTR_MAX);
static void* heap_end = reinterpret_cast<void*>(0);

static HashBucket* alloctable_buckets = nullptr;
static FreeListNode* freelists[64];

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
void rho::BlockPool::Initialize() {
  //superblock_arena = sbrk(ARENASIZE);
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

  alloctable_buckets = new HashBucket[num_alloctable_buckets];
  for (int i = 0; i < num_alloctable_buckets; ++i) {
    alloctable_buckets[i].data = EMPTY_KEY;
  }
}

/**
 * Attempts to rebalance the hashtable.
 * Returns false if the rebalance failed.
 */
bool alloctable_rebalance(unsigned new_alloctable_bits) {
  if (new_alloctable_bits > 29) {
    allocerr("allocation hashtable is too large");
  }
  unsigned new_table_size = 1 << new_alloctable_bits;
  unsigned old_mask = alloctable_hash_mask;
  alloctable_hash_mask = new_table_size - 1;
  HashBucket* new_table = new HashBucket[new_table_size];
  for (int i = 0; i < new_table_size; ++i) {
    new_table[i].data = EMPTY_KEY;
  }
  // Build new table.
  for (int i = 0; i < num_alloctable_buckets; ++i) {
    HashBucket& bucket = alloctable_buckets[i];
    uintptr_t data = bucket.data;
    if (data != EMPTY_KEY && data != DELETED_KEY && data & 1) {
      // This is the initial hash bucket for the corresponding allocation.
      // Rebuild the hash entries in the new table based only on this entry.
      if (!hashtable_try_insert(new_table, data & ~uintptr_t{1}, bucket.size)) {
        // Failed ot insert in new table. Abort the rebuild.
        delete[] new_table;
        alloctable_hash_mask = old_mask;
        return false;
      }
    }
  }
  // Replace the old table.
  delete[] alloctable_buckets;
  alloctable_buckets = new_table;
  alloctable_bits = new_alloctable_bits;
  num_alloctable_buckets = new_table_size;
  return true;
}

/**
 * Add an allocation (large object or medium object superblock) to the
 * allocations hashtable.
 */
void alloctable_insert(uintptr_t pointer, size_t size) {
  while (!hashtable_try_insert(alloctable_buckets, pointer, size)) {
    // Remove partially inserted entries.
    alloctable_erase(pointer, size);
    // Too many collisions, rebalance the hash table.
    unsigned new_table_bits = alloctable_bits + 1;
    while (!alloctable_rebalance(new_table_bits)) {
      new_table_bits += 1;
    }
  }
}

bool hashtable_try_insert(HashBucket* table, uintptr_t pointer, size_t size) {
  uintptr_t key = pointer >> LOW_BITS;
  uintptr_t key_end = (pointer + (1 << size) + ((1 << LOW_BITS) - 1))
      >> LOW_BITS;
  bool first = true;
  do {
    unsigned hash = pointer_hash(key << LOW_BITS);
    bool added = false;
    for (int i = 0; i < MAX_COLLISIONS; ++i) {
      HashBucket& bucket = table[hash];
      if (bucket.data == EMPTY_KEY || bucket.data == DELETED_KEY) {
        if (first) {
          // Tag this as the first hash entry.
          first = false;
          table[hash].data = pointer | 1;
        } else {
          table[hash].data = pointer;
        }
        table[hash].size = size;
        added = true;
        break;
      }
      add_collisions += 1;
      hash = probe_func(hash, i);
    }
    if (!added) {
      return false;
    }
    key += 1;
  } while (key < key_end);
  return true;
}

/*
 * Erases all hashtable entries for a pointer. The size determines
 * how many hash buckets are removed.
 */
void alloctable_erase(uintptr_t pointer, size_t size) {
  uintptr_t key = pointer >> LOW_BITS;
  uintptr_t key_end = (pointer + (1 << size) + ((1 << LOW_BITS) - 1))
      >> LOW_BITS;
  bool first = true;
  pointer &= ~uintptr_t{3};  // Mask out the flag bits for equality comparison.
  do {
    unsigned hash = pointer_hash(key << LOW_BITS);
    for (int i = 0; i < MAX_COLLISIONS; ++i) {
      HashBucket& bucket = alloctable_buckets[hash];
      if (bucket.DataPointer() == pointer) {
        bucket.data = DELETED_KEY;
        break;
      }
      if (bucket.data == EMPTY_KEY) {
        // No more partial entries inserted, we are done.
        return;
      }
      hash = probe_func(hash, i);
    }
    key += 1;
  } while (key < key_end);
}

/*
 * Find a bucket matching the pointer. Will find internal pointers.
 */
HashBucket* alloctable_search(uintptr_t candidate) {
  unsigned hash = pointer_hash(candidate);
  for (int i = 0; i < MAX_COLLISIONS; ++i) {
    HashBucket* bucket = &alloctable_buckets[hash];
    if (bucket->data != EMPTY_KEY && bucket->data != DELETED_KEY) {
      uintptr_t pointer = bucket->DataPointer();
      if (bucket->data & 2) {
        if (pointer <= candidate
            && pointer + MEDIUM_SUPERBLOCK_SIZE > candidate) {
          // Pointer is inside this superblock. Test if it is after the header.
          if (candidate >= pointer + SUPERBLOCK_HEADER_SIZE) {
            return bucket;
          } else {
            return nullptr;
          }
        }
      } else if (pointer <= candidate
            && (pointer + (1L << bucket->size)) > candidate) {
        return bucket;
      }
    }
    if (bucket->data == EMPTY_KEY) {
      return nullptr;
    }
    lookup_collisions += 1;
    hash = probe_func(hash, i);
  }
  return nullptr;
}
/**
 * Add a large allocation to a free list.
 */
static void add_free_block(uintptr_t data, unsigned size_log2) {
  FreeListNode* free_node = reinterpret_cast<FreeListNode*>(data);
#ifdef HAVE_ADDRESS_SANITIZER
  // Add to quarantine and poison.
  quarantine_size += 1 << size_log2;
  free_node->next = quarantine_size[size_log2];
  quarantine[size_log2] = superblock->free_list;
  ASAN_POISON_MEMORY_REGION(free_node, 1 << size_log2);
  // Maybe clear the quarantine.
  if (quarantine_size > MAX_QUARANTINE_SIZE) {
    // Remove all small ojbects from quarantine and insert in freelists.
    for (int i = 0; i < 64; ++i) {
      while (quarantine[i]) {
        FreeNode* quarantined_node = quarantine[i];
        ASAN_UNPOISON_MEMORY_REGION(quarantined_node, 1 << size_log2);
        quarantine[i] = quarantined_node->next;
        quarantined_node->next = freelists[i];
        freelists[i] = quarantined_node;
        // Re-poison so the freelist nodes stay poisoned while free.
        ASAN_POISON_MEMORY_REGION(quarantined_node, 1 << size_log2);
      }
    }
    quarantine_size = 0;
  }
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

/*
 * Remove an allocation from the hashtable.  If the allocation is in a
 * superblock, the superblock is left as is but the superblock bitset is
 * updated to indicate that the block is free.
 * If the allocation is a large object (ie. not in a superblock), then
 * the object is taken out of the hashtable and inserted into a freelist.
 */
bool alloctable_remove_maybe(uintptr_t pointer) {
  assert(pointer != EMPTY_KEY && pointer != DELETED_KEY
      && "Trying to remove invalid key from alloctable.");
  unsigned hash = pointer_hash(pointer);
  unsigned size_log2 = 0;
  HashBucket* bucket = alloctable_search(pointer);
  if (!bucket) {
    // Free failed: could not find block to free.
    return false;
  }
  uintptr_t data = bucket->DataPointer();
  uintptr_t first_block = data + SUPERBLOCK_HEADER_SIZE;
  if (bucket->data & 2) {
    Superblock* superblock = reinterpret_cast<Superblock*>(data);
    size_log2 = superblock->block_size;
    unsigned index = (pointer - first_block) >> size_log2;
    unsigned bitset = index / 64;
    superblock->free[bitset] |= u64{1} << (index & 63);
    FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
    free_node->block = index;
    free_node->superblock = superblock;
#ifdef HAVE_ADDRESS_SANITIZER
    // Add to quarantine and poison.
    quarantine_size += 1 << size_log2;
    free_node->next = quarantine_size[size_log2];
    quarantine[size_log2] = superblock->free_list;
    ASAN_POISON_MEMORY_REGION(free_node, 1 << size_log2);
    // Maybe clear the quarantine.
    if (quarantine_size > MAX_QUARANTINE_SIZE) {
      // Remove all small ojbects from quarantine and insert in freelists.
      for (int i = 0; i < 64; ++i) {
        while (quarantine[i]) {
          FreeNode* quarantined_node = quarantine[i];
          ASAN_UNPOISON_MEMORY_REGION(quarantined_node, 1 << size_log2);
          quarantine[i] = quarantined_node->next;
          if (medium_superblocks[i]) {
            superblock = medium_superblocks[i];
          } else {
            medium_superblocks[i] = superblock;
          }
          quarantined_node->next = superblock->free_list;
          superblock->free_list = quarantined_node;
          // Re-poison so the freelist nodes stay poisoned while free.
          ASAN_POISON_MEMORY_REGION(quarantined_node, 1 << size_log2);
        }
      }
      quarantine_size = 0;
    }
#else
    if (medium_superblocks[size_log2]) {
      superblock = medium_superblocks[size_log2];
    } else {
      medium_superblocks[size_log2] = superblock;
    }
    free_node->next = superblock->free_list;
    superblock->free_list = free_node;
#endif
    // Done. Don't erase hash entries for the superblock.
    return true;
  } else if (data == pointer) {
    // Erase all entries in hashtable for the allocation.
    alloctable_erase(pointer, bucket->size);
    // Add allocation to free list.
    add_free_block(pointer, bucket->size);
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

void* rho::BlockPool::AllocBlock(size_t bytes) {
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

void rho::BlockPool::FreeBlock(void* pointer) {
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
    Superblock::FreeSmallBlock(pointer, superblock);
  } else if (!alloctable_remove_maybe(reinterpret_cast<uintptr_t>(pointer))) {
    allocerr("failed to free pointer - unallocated or double-free problem");
  }
}

// Iterates over all live objects and calls the argument function on
// their pointers.
// It is okay to free objects during the iteration, but adding objects
// means they may not be found during the current iteration pass.
void rho::BlockPool::ApplyToAllBlocks(std::function<void(void*)> fun) {
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
  for (int i = 0; i < num_alloctable_buckets; ++i) {
    HashBucket& bucket = alloctable_buckets[i];
    if (bucket.data != EMPTY_KEY && bucket.data != DELETED_KEY) {
      if (bucket.data & 1) {
        void* pointer = reinterpret_cast<void*>(bucket.data & ~uintptr_t{3});
        if (bucket.data & 2) {
          // This is a large superblock.
          Superblock* superblock = reinterpret_cast<Superblock*>(pointer);
          uintptr_t block =
              reinterpret_cast<uintptr_t>(pointer) + SUPERBLOCK_HEADER_SIZE;
          uintptr_t block_end =
              reinterpret_cast<uintptr_t>(pointer) + MEDIUM_SUPERBLOCK_SIZE;
          unsigned block_size = 1 << superblock->block_size;
          unsigned superblock_size = (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE)
              / superblock->block_size;
          unsigned bitset_entries =
              ((1 << (MEDIUM_SUPERBLOCK_SIZE_LOG2 - superblock->block_size)) + 63)
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
#ifdef HAVE_ADDRESS_SANITIZER
                  fun(offset_pointer(reinterpret_cast<void*>(block),
                      REDZONE_SIZE));
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
        } else {
#ifdef HAVE_ADDRESS_SANITIZER
          fun(offset_pointer(reinterpret_cast<void*>(block), REDZONE_SIZE));
#else
          fun(pointer);
#endif
        }
      }
    }
  }
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
void* rho::BlockPool::Lookup(void* candidate) {
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
    HashBucket* bucket = alloctable_search(candidate_uint);
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
  result = offset_pointer(result, REDZONE_SIZE);
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

static void print_alloctable() {
  printf(">>>>> SPARSE TABLE\n");
  int size = 0;
  for (int i = 0; i < num_alloctable_buckets; i += 16) {
    int count = 0;
    for (int j = 0; j < 16 && i + j < num_alloctable_buckets; ++j) {
      HashBucket& bucket = alloctable_buckets[i + j];
      if (bucket.data != EMPTY_KEY && bucket.data != DELETED_KEY) {
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

void rho::BlockPool::DebugPrint() {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    print_superblock(reinterpret_cast<Superblock*>(next_superblock));
    next_superblock += SUPERBLOCK_SIZE;
  }
  print_alloctable();
}

void rho::BlockPool::DebugRebalance(int new_bits) {
  alloctable_rebalance(new_bits);
  print_alloctable();
}

void rho::BlockPool::RebalanceAllocationTable(int new_bits) {
  alloctable_rebalance(new_bits);
}

