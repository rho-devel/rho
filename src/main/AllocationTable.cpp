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

void rho::AllocationTable::insert(const Allocation& allocation) {
  while (m_capacity < requiredCapacity(allocation)) {
    resize(m_num_bits + 1);
  }
  insertUnchecked(allocation);
}

void rho::AllocationTable::insertUnchecked(const Allocation& allocation) {
  m_capacity -= requiredCapacity(allocation);
  bool first = true;
  size_t key_end = endKey(allocation.dataPointer(), allocation.sizeLog2());
  for (size_t key = startKey(allocation.dataPointer()); key < key_end; ++key) {
    insertSingleKey(allocation, key << LOW_BITS, first);
    first = false;
  }
}

void rho::AllocationTable::insertSingleKey(const Allocation& allocation,
    size_t key, bool first) {
  size_t hash = pointerHash(key);
  // Insert will always succed because we have a precondition testing that
  // there is enough space in the table.
  for (int i = 0; true;
      hash = probeFunc(hash, i),
      m_num_insert_collisions += 1,
      i += 1) {
    Allocation& bucket = m_buckets[hash];
    if (bucket.isEmpty() || bucket.isDeleted()) {
      // Can insert here.
      m_buckets[hash] = allocation;
      if (first) {
        // Set the first flag for this entry.
        m_buckets[hash].m_data |= Allocation::s_first_flag;
      }
      return;
    }
  }
}

void rho::AllocationTable::erase(uintptr_t pointer, size_t size) {
  assert(pointer != Allocation::s_empty_key
      && pointer != Allocation::s_deleted_key
      && "Trying to remove invalid key from alloctable.");
  pointer &= ~uintptr_t{3};  // Mask out the flag bits for equality comparison.
  size_t key_end = endKey(pointer, size);
  for (size_t key = startKey(pointer); key < key_end; ++key) {
    eraseSingleKey(pointer, key);
  }
}

void rho::AllocationTable::eraseSingleKey(uintptr_t pointer, size_t key) {
  size_t hash = pointerHash(key << LOW_BITS);
  // We don't have precondition ensuring the key is in the table before trying
  // to delete, so we can rely on always finding the key. A fatal error is
  // generated if the key is not found.
  for (int i = 0; i < m_num_buckets;
      hash = probeFunc(hash, i),
      m_num_erase_collisions += 1,
      i += 1) {
    Allocation& bucket = m_buckets[hash];
    if (bucket.dataPointer() == pointer) {
      m_capacity += 1;
      bucket.m_data = Allocation::s_deleted_key;
      return;
    }
    if (bucket.isEmpty()) {
      // We didn't find the correct hash entry.
      allocerr("trying to erase non-existent allocation entry");
    }
  }
  allocerr("trying to erase non-existent allocation entry");
}

rho::AllocationTable::Allocation* rho::AllocationTable::search(
    uintptr_t candidate) {
  size_t hash = pointerHash(candidate);
  for (int i = 0; i < m_num_buckets;
      hash = probeFunc(hash, i),
      m_num_search_collisions += 1,
      i += 1) {
    Allocation* bucket = &m_buckets[hash];
    if (bucket->isEmpty()) {
      return nullptr;
    }
    if (bucket->isDeleted()) {
      continue;
    }
    if (bucket->isSuperblock()) {
      AllocatorSuperblock* superblock = bucket->asSuperblock();
      if (superblock->isInternalPointer(candidate)) {
        // Pointer is inside this superblock. Test if it is after the header.
        if (superblock->isBlockPointer(candidate)) {
          return bucket;
        } else {
          // The pointer mapped to this superblock, but was inside the header.
          return nullptr;
        }
      }
    } else if (bucket->isInternalPointer(candidate)) {
      return bucket;
    }
  }
  return nullptr;
}

void rho::AllocationTable::resize(unsigned new_alloctable_bits) {
  assert(new_alloctable_bits > m_num_bits
      && "can not shrink this table");
  AllocationTable new_table(new_alloctable_bits);
  // Build new table.
  for (int i = 0; i < m_num_buckets; ++i) {
    const Allocation& bucket = m_buckets[i];
    if (!bucket.isEmpty() && !bucket.isDeleted() && bucket.isFirst()) {
      // This is the initial hash bucket for the corresponding allocation.
      // Rebuild the hash entries in the new table based only on this entry.
      new_table.insertUnchecked(bucket);
    }
  }
  // Copy everything except collision counts from the new table.
  m_capacity = new_table.m_capacity;
  m_num_bits = new_table.m_num_bits;
  m_num_buckets = new_table.m_num_buckets;
  m_hash_mask = new_table.m_hash_mask;

  // Swap to ensure the old bucket array is freed with the temporary table.
  std::swap(m_buckets, new_table.m_buckets);
}

void rho::AllocationTable::applyToAllAllocations(
    std::function<void(void*)> fun) {
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

size_t rho::AllocationTable::endKey(uintptr_t pointer, size_t size_log2) {
  // We add (1 << LOW_BITS) - 1 to ensure that the end key is one-past the last
  // valid internal pointer hash key.
  return (pointer + (1 << size_log2) + ((1 << LOW_BITS) - 1)) >> LOW_BITS;
}

