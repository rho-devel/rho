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

typedef std::uint64_t u64;

using std::size_t;

using rho::FreeListNode;
using rho::AllocationTable;

typedef rho::AllocatorSuperblock Superblock;

AllocationTable::AllocationTable(unsigned num_bits): m_num_bits(num_bits) {
  m_num_buckets = 1 << m_num_bits;
  m_capacity = m_num_buckets / 2;  // Max load factor = 50%.
  m_hash_mask = m_num_buckets - 1;
  m_buckets = new Allocation[m_num_buckets];
}

void AllocationTable::insert(uintptr_t pointer, size_t size) {
  if (m_capacity == 0) {
    if (!resize(m_num_bits + 1)) {
      allocerr("failed to increase allocation table size");
    }
  }
  m_capacity -= 1;
  if (!try_insert(m_buckets, pointer, size)) {
    allocerr("failed to increase allocation table size");
  }
}

void AllocationTable::erase(uintptr_t pointer, size_t size) {
  uintptr_t key = pointer >> LOW_BITS;
  uintptr_t key_end = (pointer + (1 << size) + ((1 << LOW_BITS) - 1))
      >> LOW_BITS;
  bool first = true;
  pointer &= ~uintptr_t{3};  // Mask out the flag bits for equality comparison.
  // A do-while loop is used here because we always have to delete at least one
  // key. See also the comment in try_insert().
  do {
    unsigned hash = pointer_hash(key << LOW_BITS);
    for (int i = 0; i < m_num_buckets; ++i) {
      Allocation& bucket = m_buckets[hash];
      if (bucket.data_pointer() == pointer) {
        m_capacity += 1;
        bucket.m_data = Allocation::s_deleted_key;
        break;
      }
      if (bucket.is_empty()) {
        // No more partial entries inserted, we are done.
        return;
      }
      m_num_erase_collisions += 1;
      hash = probe_func(hash, i);
    }
    key += 1;
  } while (key < key_end);
}

AllocationTable::Allocation* AllocationTable::search(uintptr_t candidate) {
  unsigned hash = pointer_hash(candidate);
  for (int i = 0; i < m_num_buckets; ++i) {
    Allocation* bucket = &m_buckets[hash];
    if (!bucket->is_empty() && !bucket->is_deleted()) {
      if (bucket->is_superblock()) {
        Superblock* superblock = bucket->as_superblock();
        if (superblock->IsInternalPointer(candidate)) {
          // Pointer is inside this superblock. Test if it is after the header.
          if (superblock->IsBlockPointer(candidate)) {
            return bucket;
          } else {
            return nullptr;
          }
        }
      } else if (bucket->is_internal_pointer(candidate)) {
        return bucket;
      }
    }
    if (bucket->is_empty()) {
      return nullptr;
    }
    m_num_search_collisions += 1;
    hash = probe_func(hash, i);
  }
  return nullptr;
}

bool AllocationTable::resize(unsigned new_alloctable_bits) {
  unsigned new_table_size = 1 << new_alloctable_bits;
  unsigned old_mask = m_hash_mask;
  unsigned new_capacity = new_table_size / 2;
  m_hash_mask = new_table_size - 1;
  Allocation* new_table = new Allocation[new_table_size];
  // Build new table.
  for (int i = 0; i < m_num_buckets; ++i) {
    Allocation& bucket = m_buckets[i];
    if (!bucket.is_empty() && !bucket.is_deleted() && bucket.is_first()) {
      // This is the initial hash bucket for the corresponding allocation.
      // Rebuild the hash entries in the new table based only on this entry.
      new_capacity -= 1;
      if (!try_insert(new_table, bucket.m_data & ~uintptr_t{1},
          bucket.m_size_log2)) {
        // Failed to insert in new table. Abort the rebuild.
        delete[] new_table;
        m_hash_mask = old_mask;
        return false;
      }
    }
  }
  // Replace the old table.
  delete[] m_buckets;
  m_buckets = new_table;
  m_num_bits = new_alloctable_bits;
  m_num_buckets = new_table_size;
  m_capacity = new_capacity;
  return true;
}

bool AllocationTable::free_allocation(uintptr_t pointer) {
  assert(pointer != Allocation::s_empty_key
      && pointer != Allocation::s_deleted_key
      && "Trying to remove invalid key from alloctable.");
  unsigned hash = pointer_hash(pointer);
  unsigned size_log2 = 0;
  Allocation* bucket = search(pointer);
  if (!bucket) {
    // Free failed: could not find block to free.
    return false;
  }
  uintptr_t data = bucket->data_pointer();
  if (bucket->is_superblock()) {
    Superblock* superblock = bucket->as_superblock();
    size_log2 = superblock->m_block_size;
    unsigned index = (pointer - superblock->FirstBlockPointer()) >> size_log2;
    superblock->TagBlockUnallocated(index);
    FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
    free_node->m_block = index;
    free_node->m_superblock = superblock;
#ifdef HAVE_ADDRESS_SANITIZER
    GCNodeAllocator::AddToQuarantine(free_node, size_log2);
#else
    if (GCNodeAllocator::s_medium_superblocks[size_log2]) {
      superblock = GCNodeAllocator::s_medium_superblocks[size_log2];
    } else {
      GCNodeAllocator::s_medium_superblocks[size_log2] = superblock;
    }
    free_node->m_next = superblock->m_free_list;
    superblock->m_free_list = free_node;
#endif
    // Done. Don't erase hash entries for the superblock.
    return true;
  } else if (data == pointer) {
    // Erase all entries in hashtable for the allocation.
    erase(pointer, bucket->m_size_log2);
    // Add allocation to free list.
    GCNodeAllocator::AddToFreelist(pointer, bucket->m_size_log2);
  }
  return true;
}

void AllocationTable::ApplyToAllAllocations(std::function<void(void*)> fun) {
  for (int i = 0; i < m_num_buckets; ++i) {
    Allocation& bucket = m_buckets[i];
    if (!bucket.is_empty() && !bucket.is_deleted()) {
      if (bucket.is_first()) {
        if (bucket.is_superblock()) {
          // This is a large superblock.
          Superblock* superblock = bucket.as_superblock();
          uintptr_t data = bucket.data_pointer();
          uintptr_t block = superblock->FirstBlockPointer();
          uintptr_t block_end = data + Superblock::s_medium_superblock_size;
          unsigned size_log2 = superblock->m_block_size;
          unsigned block_size = 1 << size_log2;
          unsigned bitset_entries =
              ((1 << (Superblock::s_medium_superblock_size_log2 - size_log2)) + 63) / 64;
          for (int i = 0; i < bitset_entries; ++i) {
            if (superblock->m_free[i] != ~0ull) {
              for (int index = 0; index < 64; ++index) {
                if (!(superblock->m_free[i] & (u64{1} << index))) {
#ifdef ALLOCATION_CHECK
                  if (!lookup_in_allocation_map(reinterpret_cast<void*>(
                      block))) {
                    allocerr("apply to all blocks iterating over "
                        "non-alloc'd pointer");
                  }
#endif
#ifdef HAVE_ADDRESS_SANITIZER
                  fun(GCNodeAllocator::OffsetPointer(
                      reinterpret_cast<void*>(block),
                      GCNodeAllocator::s_redzone_size));
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
          fun(GCNodeAllocator::OffsetPointer(
              bucket.as_pointer(),
              GCNodeAllocator::s_redzone_size));
#else
          fun(bucket.as_pointer());
#endif
        }
      }
    }
  }
}

void AllocationTable::PrintTable() {
  printf(">>>>> SPARSE TABLE\n");
  int size = 0;
  for (int i = 0; i < m_num_buckets; i += 16) {
    int count = 0;
    for (int j = 0; j < 16 && i + j < m_num_buckets; ++j) {
      Allocation& bucket = m_buckets[i + j];
      if (!bucket.is_empty() && !bucket.is_deleted()) {
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

bool AllocationTable::try_insert(Allocation* table, uintptr_t pointer, size_t size) {
  uintptr_t key = pointer >> LOW_BITS;
  uintptr_t key_end = (pointer + (1 << size) + ((1 << LOW_BITS) - 1))
      >> LOW_BITS;
  bool first = true;
  // A do-while loop is used here because we always want to insert at least one key,
  // however if we have inserted one key then we skip the last key (key_end) because
  // it maps to one-past-the-end for the allocation range.
  do {
    unsigned hash = pointer_hash(key << LOW_BITS);
    bool added = false;
    for (int i = 0; i < m_num_buckets; ++i) {
      Allocation& bucket = table[hash];
      if (bucket.is_empty() || bucket.is_deleted()) {
        new (&table[hash])Allocation(reinterpret_cast<void*>(pointer), size, first);
        first = false;
        added = true;
        break;
      }
      m_num_insert_collisions += 1;
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
  return hash & m_hash_mask;
}

unsigned AllocationTable::probe_func(unsigned hash, int i) {
  return (hash + i + 1) & m_hash_mask;
}

