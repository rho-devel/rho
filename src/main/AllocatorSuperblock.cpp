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

#include "rho/AddressSanitizer.hpp"
#include "rho/AllocationTable.hpp"
#include "rho/AllocatorSuperblock.hpp"

typedef std::uint64_t u64;
typedef std::uint32_t u32;

using std::size_t;

using rho::FreeListNode;

typedef rho::AllocatorSuperblock Superblock;

// Arena for small-object superblocks. Initialized in GCNodeAllocator.cpp.
extern void* superblock_arena;
extern uintptr_t arena_superblock_start;
extern uintptr_t arena_superblock_end;
extern uintptr_t arena_superblock_next;

Superblock* Superblock::NewSuperblock(int block_size) {
  if (arena_superblock_next >= arena_superblock_end) {
    return nullptr;
  }
  void* pointer = reinterpret_cast<void*>(arena_superblock_next);
#ifdef HAVE_ADDRESS_SANITIZER
  // Unpoison only the header.
  ASAN_UNPOISON_MEMORY_REGION(pointer, s_superblock_header_size);
#endif
  unsigned superblock_size =
      (s_superblock_size - s_superblock_header_size) / block_size;
  unsigned bitset_entries = (superblock_size + 63) / 64;
  Superblock* superblock = new (pointer)Superblock(block_size, bitset_entries);
  arena_superblock_next += s_superblock_size;
  return superblock;
}

Superblock* Superblock::SmallSuperblockFromPointer(void* pointer) {
  uintptr_t candidate = reinterpret_cast<uintptr_t>(pointer);
  if (candidate >= arena_superblock_start && candidate < arena_superblock_next) {
    if ((candidate & (s_superblock_size - 1)) < s_superblock_header_size) {
      // The pointer points inside the superblock header.
      return nullptr;
    }
    return reinterpret_cast<Superblock*>(
        candidate & ~uintptr_t{s_superblock_size - 1});
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
  if (GCNodeAllocator::s_small_superblocks[pool_index]) {
    superblock = GCNodeAllocator::s_small_superblocks[pool_index];
  } else {
    superblock = Superblock::NewSuperblock(block_size);
    if (!superblock) {
      return nullptr;
    }
    GCNodeAllocator::s_small_superblocks[pool_index] = superblock;
  }
  return superblock->AllocateSmallBlock();
}

void* Superblock::AllocateSmallBlock() {
  unsigned pool_index = (m_block_size + 7) / 8;
  unsigned num_blocks =
      (s_superblock_size - s_superblock_header_size) / m_block_size;
  if (m_free_list) {
    FreeListNode* free_node = m_free_list;
#ifdef HAVE_ADDRESS_SANITIZER
    ASAN_UNPOISON_MEMORY_REGION(free_node, m_block_size);
#endif
    u32 index = free_node->m_block;
    free_node->m_superblock->TagBlockAllocated(index);
    m_free_list = free_node->m_next;
    if (!m_free_list && m_next_untouched == num_blocks) {
      // This superblock can not allocate any more blocks,
      // so take it out of the superblock pool.
      GCNodeAllocator::s_small_superblocks[pool_index] = nullptr;
    }
    return free_node;
  } else {
    u32 index = m_next_untouched;
    TagBlockAllocated(index);
    m_next_untouched += 1;
    if (m_next_untouched == num_blocks) {
      GCNodeAllocator::s_small_superblocks[pool_index] = nullptr;
    }
    void* result = reinterpret_cast<char*>(this)
        + s_superblock_header_size + (index * m_block_size);
#ifdef HAVE_ADDRESS_SANITIZER
    ASAN_UNPOISON_MEMORY_REGION(result, m_block_size);
#endif
    return result;
  }
}

void Superblock::FreeSmallBlock(void* pointer) {
  uintptr_t block = reinterpret_cast<uintptr_t>(pointer);
  uintptr_t superblock_start = reinterpret_cast<uintptr_t>(this)
      + s_superblock_header_size;
  unsigned index = (block - superblock_start) / m_block_size;
  unsigned bitset = index / 64;
  m_free[bitset] |= u64{1} << (index & 63);

  // Use the block as a free list node and prepend to the free list.
  FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
  free_node->m_block = index;
  free_node->m_superblock = this;
  int pool_index = m_block_size / 8;
#ifdef HAVE_ADDRESS_SANITIZER
  GCNodeAllocator::AddToSmallQuarantine(free_node, pool_index);
#else
  Superblock* superblock;
  if (GCNodeAllocator::s_small_superblocks[pool_index]) {
    superblock = GCNodeAllocator::s_small_superblocks[pool_index];
  } else {
    superblock = this;
    GCNodeAllocator::s_small_superblocks[pool_index] = superblock;
  }
  free_node->m_next = superblock->m_free_list;
  superblock->m_free_list = free_node;
#endif
}

void* Superblock::AllocLarge(unsigned size_log2) {
  assert(size_log2 >= 6
      && "Can not allocate objects smaller than 64 bytes using AllocLarge");
  void* result = nullptr;
  if (size_log2 <= 17) {
    unsigned num_blocks =
        (s_medium_superblock_size - s_superblock_header_size) >> size_log2;
    Superblock* superblock;
    if (GCNodeAllocator::s_medium_superblocks[size_log2]) {
      // Reuse existing superblock for this allocation size.
      superblock = GCNodeAllocator::s_medium_superblocks[size_log2];
    } else {
      // Allocate a midsize superblock.
      void* arena = new double[s_medium_superblock_size / sizeof(double)];
      unsigned bitset_entries =
          ((1 << (s_medium_superblock_size_log2 - size_log2)) + 63) / 64;
      superblock = new (arena)Superblock(size_log2, bitset_entries);
      GCNodeAllocator::s_alloctable->insert(
          reinterpret_cast<uintptr_t>(arena) | 2,
          s_medium_superblock_size_log2);
      GCNodeAllocator::s_medium_superblocks[size_log2] = superblock;
    }
    if (superblock->m_free_list) {
      FreeListNode* free_node = superblock->m_free_list;
#ifdef HAVE_ADDRESS_SANITIZER
      ASAN_UNPOISON_MEMORY_REGION(free_node, 1 << size_log2);
#endif
      unsigned index = free_node->m_block;
      free_node->m_superblock->TagBlockAllocated(index);
      superblock->m_free_list = free_node->m_next;
      if (!superblock->m_free_list
          && superblock->m_next_untouched == num_blocks) {
        GCNodeAllocator::s_medium_superblocks[size_log2] = nullptr;
      }
      result = free_node;
    } else {
      unsigned index = superblock->m_next_untouched;
      superblock->TagBlockAllocated(index);
      superblock->m_next_untouched += 1;
      if (superblock->m_next_untouched == num_blocks) {
        GCNodeAllocator::s_medium_superblocks[size_log2] = nullptr;
      }
      result = reinterpret_cast<char*>(superblock) + s_superblock_header_size
          + (index << size_log2);
#ifdef HAVE_ADDRESS_SANITIZER
      ASAN_UNPOISON_MEMORY_REGION(result, 1 << size_log2);
#endif
    }
  } else {
    result = GCNodeAllocator::RemoveFromFreelist(size_log2);
    if (!result) {
      result = new double[(1L << size_log2) / sizeof(double)];
    }
    GCNodeAllocator::s_alloctable->insert(
        reinterpret_cast<uintptr_t>(result), size_log2);
  }
  return result;
}

void Superblock::TagBlockAllocated(unsigned block) {
  unsigned bitset = block / 64;
  m_free[bitset] &= ~(u64{1} << (block & 63));
}

void Superblock::TagBlockUnallocated(unsigned block) {
  unsigned bitset = block / 64;
  m_free[bitset] |= u64{1} << (block & 63);
}

void Superblock::ApplyToAllAllocations(std::function<void(void*)> fun) {
  uintptr_t header_start = reinterpret_cast<uintptr_t>(this);
  uintptr_t block = header_start + s_superblock_header_size;
  uintptr_t block_end = header_start + s_superblock_size;
  unsigned num_blocks =
      (s_superblock_size - s_superblock_header_size) / m_block_size;
  unsigned bitset_entries = (num_blocks + 63) / 64;
  for (int i = 0; i < bitset_entries; ++i) {
    if (m_free[i] != ~0ull) {
      for (int index = 0; index < 64; ++index) {
        if (!(m_free[i] & (u64{1} << index))) {
#ifdef ALLOCATION_CHECK
          if (!lookup_in_allocation_map(reinterpret_cast<void*>(block))) {
            allocerr(
                "apply to all blocks iterating over non-alloc'd pointer");
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
        block += m_block_size;
      }
    } else {
      block += m_block_size * 64;
    }
  }
}

void Superblock::DebugPrint() {
  unsigned superblock_size =
      (s_superblock_size - s_superblock_header_size) / m_block_size;
  printf(">>>>>>>>>> SUPERBLOCK (blocksize=%d, num block=%d)\n",
      m_block_size, superblock_size);
  unsigned bitset_entries = (superblock_size + 63) / 64;
  for (int i = 0; i < bitset_entries; ++i) {
    int num_free = 0;
    for (int index = 0; index < 64; ++index) {
      if (m_free[i] & (u64{1} << index)) {
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

