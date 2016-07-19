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

typedef std::uint64_t u64;
typedef std::uint32_t u32;

using std::function;
using std::size_t;

using rho::FreeListNode;
using rho::AllocationTable;
typedef rho::AllocationTable::HashBucket HashBucket;

typedef rho::AllocatorSuperblock Superblock;

AllocationTable::AllocationTable(unsigned num_bits): num_bits(num_bits) {
  num_buckets = 1 << num_bits;
  hash_mask = num_buckets - 1;
  buckets = new HashBucket[num_buckets];
  for (int i = 0; i < num_buckets; ++i) {
    buckets[i].data = EMPTY_KEY;
  }
}

void AllocationTable::insert(uintptr_t pointer, size_t size) {
  while (!try_insert(buckets, pointer, size)) {
    // Remove partially inserted entries.
    erase(pointer, size);
    // Too many collisions, resize the hash table.
    unsigned new_table_bits = num_bits + 1;
    while (!resize(new_table_bits)) {
      new_table_bits += 1;
    }
  }
}

void AllocationTable::erase(uintptr_t pointer, size_t size) {
  uintptr_t key = pointer >> LOW_BITS;
  uintptr_t key_end = (pointer + (1 << size) + ((1 << LOW_BITS) - 1))
      >> LOW_BITS;
  bool first = true;
  pointer &= ~uintptr_t{3};  // Mask out the flag bits for equality comparison.
  do {
    unsigned hash = pointer_hash(key << LOW_BITS);
    for (int i = 0; i < MAX_COLLISIONS; ++i) {
      HashBucket& bucket = buckets[hash];
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

HashBucket* AllocationTable::search(uintptr_t candidate) {
  unsigned hash = pointer_hash(candidate);
  for (int i = 0; i < MAX_COLLISIONS; ++i) {
    HashBucket* bucket = &buckets[hash];
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

bool AllocationTable::resize(unsigned new_alloctable_bits) {
  unsigned new_table_size = 1 << new_alloctable_bits;
  unsigned old_mask = hash_mask;
  hash_mask = new_table_size - 1;
  HashBucket* new_table = new HashBucket[new_table_size];
  for (int i = 0; i < new_table_size; ++i) {
    new_table[i].data = EMPTY_KEY;
  }
  // Build new table.
  for (int i = 0; i < num_buckets; ++i) {
    HashBucket& bucket = buckets[i];
    uintptr_t data = bucket.data;
    if (data != EMPTY_KEY && data != DELETED_KEY && (data & 1)) {
      // This is the initial hash bucket for the corresponding allocation.
      // Rebuild the hash entries in the new table based only on this entry.
      if (!try_insert(new_table, data & ~uintptr_t{1}, bucket.size)) {
        // Failed ot insert in new table. Abort the rebuild.
        delete[] new_table;
        hash_mask = old_mask;
        return false;
      }
    }
  }
  // Replace the old table.
  delete[] buckets;
  buckets = new_table;
  num_bits = new_alloctable_bits;
  num_buckets = new_table_size;
  return true;
}

bool AllocationTable::free_allocation(uintptr_t pointer) {
  assert(pointer != EMPTY_KEY && pointer != DELETED_KEY
      && "Trying to remove invalid key from alloctable.");
  unsigned hash = pointer_hash(pointer);
  unsigned size_log2 = 0;
  HashBucket* bucket = search(pointer);
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
    add_to_quarantine(free_node, size_log2);
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
    erase(pointer, bucket->size);
    // Add allocation to free list.
    add_free_block(pointer, bucket->size);
  }
  return true;
}

void AllocationTable::ApplyToAllBlocks(std::function<void(void*)> fun) {
  for (int i = 0; i < num_buckets; ++i) {
    HashBucket& bucket = buckets[i];
    if (bucket.data != EMPTY_KEY && bucket.data != DELETED_KEY) {
      if (bucket.data & 1) {
        uintptr_t pointer = bucket.DataPointer();
        if (bucket.data & 2) {
          // This is a large superblock.
          Superblock* superblock = reinterpret_cast<Superblock*>(pointer);
          uintptr_t block = pointer + SUPERBLOCK_HEADER_SIZE;
          uintptr_t block_end = pointer + MEDIUM_SUPERBLOCK_SIZE;
          unsigned block_size = 1 << superblock->block_size;
          unsigned superblock_size =
              (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE)
              / superblock->block_size;
          unsigned bitset_entries =
              ((1 << (MEDIUM_SUPERBLOCK_SIZE_LOG2 - superblock->block_size))
                  + 63) / 64;
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
          fun(offset_pointer(reinterpret_cast<void*>(pointer), REDZONE_SIZE));
#else
          fun(reinterpret_cast<void*>(pointer));
#endif
        }
      }
    }
  }
}

void AllocationTable::PrintTable() {
  printf(">>>>> SPARSE TABLE\n");
  int size = 0;
  for (int i = 0; i < num_buckets; i += 16) {
    int count = 0;
    for (int j = 0; j < 16 && i + j < num_buckets; ++j) {
      HashBucket& bucket = buckets[i + j];
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

bool AllocationTable::try_insert(HashBucket* table, uintptr_t pointer, size_t size) {
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

unsigned AllocationTable::pointer_hash(uintptr_t pointer) {
  pointer >>= LOW_BITS;
  pointer <<= 4;  // Spread out hash keys to leave room for collision buckets.
  unsigned low = pointer & 0xFFFFFFFF;
  unsigned hi = pointer >> 32;
  unsigned hash = low ^ hi;
  low = hash & 0xFFFF;
  hi = (hash >> 16) & 0xFFFF;
  hash = (low >> 16) ^ low ^ hi ^ (pointer & (~0xFFFF));
  return hash & hash_mask;
}

unsigned AllocationTable::probe_func(unsigned hash, int i) {
  return (hash + i + 1) & hash_mask;
}

