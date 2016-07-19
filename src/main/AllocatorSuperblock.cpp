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

#include "rho/AllocationTable.hpp"
#include "rho/AllocatorSuperblock.hpp"

typedef std::uint64_t u64;
typedef std::uint32_t u32;

using std::function;
using std::size_t;

using rho::FreeListNode;

typedef rho::AllocatorSuperblock Superblock;

Superblock* Superblock::NewSuperblock(int block_size) {
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

Superblock* Superblock::SuperblockFromPointer(void* pointer) {
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

void* Superblock::AllocSmall(size_t block_size) {
  unsigned pool_index = (block_size + 7) / 8;
  block_size = pool_index * 8;
  assert(block_size >= 32 && block_size <= 256
      && "Only use AllocSmall to allocate objects between 32 and 256 bytes.");

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
  return superblock->AllocateSmallBlock();
}

void* Superblock::AllocateSmallBlock() {
  unsigned pool_index = (block_size + 7) / 8;
  unsigned num_blocks = (SUPERBLOCK_SIZE - SUPERBLOCK_HEADER_SIZE) / block_size;
  if (free_list) {
    FreeListNode* free_node = free_list;
#ifdef HAVE_ADDRESS_SANITIZER
    ASAN_UNPOISON_MEMORY_REGION(free_node, block_size);
#endif
    u32 index = free_node->block;
    free_node->superblock->TagBlockAllocated(index);
    free_list = free_node->next;
    if (!free_list && next_untouched == num_blocks) {
      // This superblock can not allocate any more blocks,
      // so take it out of the superblock pool.
      small_superblocks[pool_index] = nullptr;
    }
    return free_node;
  } else {
    u32 index = next_untouched;
    TagBlockAllocated(index);
    next_untouched += 1;
    if (next_untouched == num_blocks) {
      small_superblocks[pool_index] = nullptr;
    }
    void* result = reinterpret_cast<char*>(this)
        + SUPERBLOCK_HEADER_SIZE + (index * block_size);
#ifdef HAVE_ADDRESS_SANITIZER
    ASAN_UNPOISON_MEMORY_REGION(result, block_size);
#endif
    return result;
  }
}

void Superblock::FreeSmallBlock(void* pointer) {
  uintptr_t block = reinterpret_cast<uintptr_t>(pointer);
  uintptr_t superblock_start = reinterpret_cast<uintptr_t>(this)
      + SUPERBLOCK_HEADER_SIZE;
  unsigned index = (block - superblock_start) / block_size;
  unsigned bitset = index / 64;
  free[bitset] |= u64{1} << (index & 63);

  // Use the block as a free list node and prepend to the free list.
  FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
  free_node->block = index;
  free_node->superblock = this;
  int pool_index = block_size / 8;
#ifdef HAVE_ADDRESS_SANITIZER
  add_to_small_quarantine(free_node, pool_index);
#else
  Superblock* superblock;
  if (small_superblocks[pool_index]) {
    superblock = small_superblocks[pool_index];
  } else {
    superblock = this;
    small_superblocks[pool_index] = superblock;
  }
  free_node->next = superblock->free_list;
  superblock->free_list = free_node;
#endif
}

void* Superblock::AllocLarge(unsigned size_log2) {
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
      alloctable->insert(reinterpret_cast<uintptr_t>(arena) | 2,
          MEDIUM_SUPERBLOCK_SIZE_LOG2);
      medium_superblocks[size_log2] = superblock;
    }
    if (superblock->free_list) {
      FreeListNode* free_node = superblock->free_list;
#ifdef HAVE_ADDRESS_SANITIZER
      ASAN_UNPOISON_MEMORY_REGION(free_node, 1 << size_log2);
#endif
      unsigned index = free_node->block;
      free_node->superblock->TagBlockAllocated(index);
      superblock->free_list = free_node->next;
      if (!superblock->free_list && superblock->next_untouched == num_blocks) {
        medium_superblocks[size_log2] = nullptr;
      }
      result = free_node;
    } else {
      unsigned index = superblock->next_untouched;
      superblock->TagBlockAllocated(index);
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
    alloctable->insert(reinterpret_cast<uintptr_t>(result), size_log2);
  }
  return result;
}

void Superblock::TagBlockAllocated(unsigned block) {
  unsigned bitset = block / 64;
  free[bitset] &= ~(u64{1} << (block & 63));
}

