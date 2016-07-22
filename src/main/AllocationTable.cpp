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
#include <functional>
#include <limits>

#include "rho/AddressSanitizer.hpp"
#include "rho/AllocationTable.hpp"
#include "rho/GCNodeAllocator.hpp"

rho::AllocationTable::AllocationTable(unsigned num_bits): m_num_bits(num_bits) {
  m_num_buckets = 1 << m_num_bits;
  m_capacity = m_num_buckets / 2;  // Sets max load factor to 50%.
  m_hash_mask = m_num_buckets - 1;
  m_buckets = new Allocation[m_num_buckets];
}

rho::AllocationTable::~AllocationTable() {
  delete[] m_buckets;
  m_buckets = nullptr;
}

void rho::AllocationTable::insert(void* pointer, std::size_t size) {
  if (m_capacity == 0) {
    if (!resize(m_num_bits + 1)) {
      allocerr("failed to increase allocation table size");
    }
  }
  m_capacity -= 1;
  if (!tryInsert(m_buckets, reinterpret_cast<uintptr_t>(pointer), size)) {
    allocerr("failed to insert allocation into allocation table");
  }
}

void* rho::AllocationTable::lookup_pointer(uintptr_t candidate) {
  Allocation* bucket = search(candidate);
  if (bucket) {
    if (bucket->isSuperblock()) {
      AllocatorSuperblock* superblock = bucket->asSuperblock();
      return superblock->lookupBlock(candidate);
    } else {
      return bucket->asPointer();
    }
  }
  return nullptr;
}

void rho::AllocationTable::erase(uintptr_t pointer, size_t size) {
  pointer &= ~uintptr_t{3};  // Mask out the flag bits for equality comparison.
  size_t key_end = endKey(pointer, size);
  bool first = true;
  for (size_t key = startKey(pointer); key < key_end; ++key) {
    size_t hash = pointerHash(key << LOW_BITS);
    for (int i = 0; i < m_num_buckets; ++i) {
      Allocation& bucket = m_buckets[hash];
      if (bucket.dataPointer() == pointer) {
        m_capacity += 1;
        bucket.m_data = Allocation::s_deleted_key;
        break;
      }
      if (bucket.isEmpty()) {
        // No more partial entries inserted, we are done.
        return;
      }
      m_num_erase_collisions += 1;
      hash = probeFunc(hash, i);
    }
  }
}

rho::AllocationTable::Allocation* rho::AllocationTable::search(uintptr_t candidate) {
  size_t hash = pointerHash(candidate);
  for (int i = 0; i < m_num_buckets; ++i) {
    Allocation* bucket = &m_buckets[hash];
    if (!bucket->isEmpty() && !bucket->isDeleted()) {
      if (bucket->isSuperblock()) {
        AllocatorSuperblock* superblock = bucket->asSuperblock();
        if (superblock->isInternalPointer(candidate)) {
          // Pointer is inside this superblock. Test if it is after the header.
          if (superblock->isBlockPointer(candidate)) {
            return bucket;
          } else {
            return nullptr;
          }
        }
      } else if (bucket->isInternalPointer(candidate)) {
        return bucket;
      }
    }
    if (bucket->isEmpty()) {
      return nullptr;
    }
    m_num_search_collisions += 1;
    hash = probeFunc(hash, i);
  }
  return nullptr;
}

bool rho::AllocationTable::resize(unsigned new_alloctable_bits) {
  unsigned new_table_size = 1 << new_alloctable_bits;
  unsigned old_mask = m_hash_mask;
  unsigned new_capacity = new_table_size / 2;  // Sets max load factor to 50%.
  m_hash_mask = new_table_size - 1;
  Allocation* new_table = new Allocation[new_table_size];
  // Build new table.
  for (int i = 0; i < m_num_buckets; ++i) {
    Allocation& bucket = m_buckets[i];
    if (!bucket.isEmpty() && !bucket.isDeleted() && bucket.isFirst()) {
      // This is the initial hash bucket for the corresponding allocation.
      // Rebuild the hash entries in the new table based only on this entry.
      new_capacity -= 1;
      if (!tryInsert(new_table, bucket.m_data & ~uintptr_t{1},
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

bool rho::AllocationTable::freeAllocation(uintptr_t pointer) {
  assert(pointer != Allocation::s_empty_key
      && pointer != Allocation::s_deleted_key
      && "Trying to remove invalid key from alloctable.");
  size_t hash = pointerHash(pointer);
  unsigned size_log2 = 0;
  Allocation* bucket = search(pointer);
  if (!bucket) {
    // Free failed: could not find block to free.
    return false;
  }
  uintptr_t data = bucket->dataPointer();
  if (bucket->isSuperblock()) {
    AllocatorSuperblock* superblock = bucket->asSuperblock();
    superblock->freeBlock(reinterpret_cast<void*>(pointer));
    // Done. Don't erase hash entries for the superblock.
    return true;
  } else if (data == pointer) {
    // Erase all entries in hashtable for the allocation.
    erase(pointer, bucket->m_size_log2);
    // Add allocation to free list.
    GCNodeAllocator::addToFreelist(pointer,
        AllocatorSuperblock::largeSizeClass(bucket->m_size_log2));
  }
  return true;
}

void rho::AllocationTable::applyToAllAllocations(std::function<void(void*)> fun) {
  for (int i = 0; i < m_num_buckets; ++i) {
    Allocation& bucket = m_buckets[i];
    if (!bucket.isEmpty() && !bucket.isDeleted()) {
      if (bucket.isFirst()) {
        if (bucket.isSuperblock()) {
          // This is a large superblock.
          AllocatorSuperblock* superblock = bucket.asSuperblock();
          superblock->applyToBlocks(fun);
        } else {
#ifdef ALLOCATION_CHECK
          // Extra consistency check.
          if (!GCNodeAllocator::lookupPointer(bucket.asPointer())) {
            allocerr("apply to all blocks iterating over non-alloc'd pointer");
          }
#endif
          fun(bucket.asPointer());
        }
      }
    }
  }
}

void rho::AllocationTable::printSummary() {
  printf("Allocation Table Summary:\n");
  int size = 0;
  for (int i = 0; i < m_num_buckets; i += 16) {
    int count = 0;
    for (int j = 0; j < 16 && i + j < m_num_buckets; ++j) {
      Allocation& bucket = m_buckets[i + j];
      if (!bucket.isEmpty() && !bucket.isDeleted()) {
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

bool rho::AllocationTable::tryInsert(Allocation* table, uintptr_t pointer, size_t size) {
  size_t key_end = endKey(pointer, size);
  bool first = true;
  for (size_t key = startKey(pointer); key < key_end; ++key) {
    size_t hash = pointerHash(key << LOW_BITS);
    bool added = false;
    for (int i = 0; i < m_num_buckets; ++i) {
      Allocation& bucket = table[hash];
      if (bucket.isEmpty() || bucket.isDeleted()) {
        new (&table[hash])Allocation(reinterpret_cast<void*>(pointer), size, first);
        first = false;
        added = true;
        break;
      }
      m_num_insert_collisions += 1;
      hash = probeFunc(hash, i);
    }
    if (!added) {
      // Failed to insert the current key.
      return false;
    }
  }
  // All keys inserted.
  return true;
}

size_t rho::AllocationTable::pointerHash(uintptr_t pointer) {
  pointer >>= LOW_BITS;
  pointer <<= 4;  // Spread out hash keys to leave room for collision buckets.
  size_t low = pointer & 0xFFFFFFFF;
  size_t hi = pointer >> 32;
  size_t hash = low ^ hi;
  low = hash & 0xFFFF;
  hi = (hash >> 16) & 0xFFFF;
  hash = (low >> 16) ^ low ^ hi ^ (pointer & (~0xFFFF));
  return hash & m_hash_mask;
}

size_t rho::AllocationTable::probeFunc(size_t prev_hash, int i) {
  return (prev_hash + i + 1) & m_hash_mask;
}

size_t rho::AllocationTable::startKey(uintptr_t pointer) {
  return pointer >> LOW_BITS;
}

size_t rho::AllocationTable::endKey(uintptr_t pointer, size_t size) {
  size_t key_end = (pointer + (1 << size) + ((1 << LOW_BITS) - 1)) >> LOW_BITS;
  if (startKey(pointer) == key_end) {
    // Ensure that at least one entry is added to the table.
    // If the pointer was exactly aligned to LOW_BITS then
    // key_end will be equal to key, otherwise key_end will point
    // to the next hash bucket past the allocation range.
    key_end += 1;
  }
  return key_end;
}

