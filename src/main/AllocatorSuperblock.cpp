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

unsigned rho::AllocatorSuperblock::s_size_class[] = {
       0,      0,      0,      0,  4 * 8,  5 * 8,  6 * 8,  7 * 8,
   8 * 8,  9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8,
  16 * 8, 17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8,
  24 * 8, 25 * 8, 26 * 8, 27 * 8, 28 * 8, 29 * 8, 30 * 8, 31 * 8,
  32 * 8,
  1 << 0, 1 << 1, 1 << 2, 1 << 3, 1 << 4, 1 << 5, 1 << 6, 1 << 7,
  1 << 8, 1 << 9, 1 <<10, 1 <<11, 1 <<12, 1 <<13, 1 <<14, 1 <<15,
  1 <<16, 1 <<17,
};

// Arena for small-object superblocks:
void* superblock_arena = nullptr;
uintptr_t arena_superblock_start = 0;
uintptr_t arena_superblock_end = 0;
uintptr_t arena_superblock_next = 0;

void rho::AllocatorSuperblock::allocateArena() {
  // We use new[] here, but one could also use sbrk(ARENASIZE).
  superblock_arena = new double[s_arenasize / sizeof(double)];
  uintptr_t start = reinterpret_cast<uintptr_t>(superblock_arena);
  uintptr_t end = start + s_arenasize;
  uintptr_t pad = s_small_superblock_size - (start & (s_small_superblock_size - 1));
  arena_superblock_start = start + pad;
  uintptr_t num_sb = (end - arena_superblock_start) >> s_small_superblock_bits;
  arena_superblock_end = arena_superblock_start + num_sb * s_small_superblock_size;
  arena_superblock_next = arena_superblock_start;

#ifdef HAVE_ADDRESS_SANITIZER
  // Poison the whole small object arena.
  ASAN_POISON_MEMORY_REGION(reinterpret_cast<void*>(start), s_arenasize);
#endif
}

rho::AllocatorSuperblock* rho::AllocatorSuperblock::newSuperblockFromArena(unsigned block_size) {
  if (arena_superblock_next >= arena_superblock_end) {
    return nullptr;
  }
  void* pointer = reinterpret_cast<void*>(arena_superblock_next);
#ifdef HAVE_ADDRESS_SANITIZER
  // The whole arena is poisoned on allocation, now we just unpoison this superblock header.
  ASAN_UNPOISON_MEMORY_REGION(pointer, s_superblock_header_size);
#endif
  unsigned superblock_size =
      (s_small_superblock_size - s_superblock_header_size) / block_size;
  unsigned bitset_entries = (superblock_size + 63) / 64;
  AllocatorSuperblock* superblock =
      new (pointer)AllocatorSuperblock(smallSizeClass(block_size), bitset_entries);
  arena_superblock_next += s_small_superblock_size;
  return superblock;
}

rho::AllocatorSuperblock* rho::AllocatorSuperblock::newLargeSuperblock(unsigned size_log2) {
  void* memory = new double[s_large_superblock_size / sizeof(double)];
  unsigned bitset_entries =
      ((1 << (s_large_superblock_size_log2 - size_log2)) + 63) / 64;
  AllocatorSuperblock* superblock = new (memory)AllocatorSuperblock(largeSizeClass(size_log2), bitset_entries);
  GCNodeAllocator::s_alloctable->insertSuperblock(superblock,
      s_large_superblock_size_log2);
#ifdef HAVE_ADDRESS_SANITIZER
  // Poison all blocks in the superblock. They are unpoisoned one at a time
  // later, when allocated.
  ASAN_POISON_MEMORY_REGION(
      reinterpret_cast<void*>(superblock->firstBlockPointer()),
      s_large_superblock_size - s_superblock_header_size);
#endif
  return superblock;
}

rho::AllocatorSuperblock* rho::AllocatorSuperblock::arenaSuperblockFromPointer(uintptr_t candidate) {
  if (candidate >= arena_superblock_start && candidate < arena_superblock_next) {
    if ((candidate & (s_small_superblock_size - 1)) < s_superblock_header_size) {
      // The pointer points inside the superblock header.
      return nullptr;
    } else {
      // The pointer points inside a block in the superblock.
      return reinterpret_cast<AllocatorSuperblock*>(
          candidate & ~uintptr_t{s_small_superblock_size - 1});
    }
  } else {
    return nullptr;
  }
}

void* rho::AllocatorSuperblock::lookupAllocation(uintptr_t candidate) {
  AllocatorSuperblock* superblock = arenaSuperblockFromPointer(candidate);
  if (superblock) {
    return superblock->lookupBlock(candidate);
  }
  return nullptr;
}

void* rho::AllocatorSuperblock::lookupBlock(uintptr_t candidate) {
  uintptr_t first_block = firstBlockPointer();
  unsigned index = (candidate - first_block) / blockSize();
  // Check that the block is actually allocated.
  if (isBlockAllocated(index)) {
    return reinterpret_cast<char*>(first_block) + index * blockSize();
  } else {
    return nullptr;
  }
}

void* rho::AllocatorSuperblock::allocateBlock(size_t block_size) {
  assert(block_size >= 32 && block_size <= 256
      && "Only use allocateBlock() to allocate objects between 32 and 256 bytes.");
  assert((block_size & 7) == 0
      && "The size argument must be a multiple of 8 bytes");

  unsigned size_class = smallSizeClass(block_size);
  AllocatorSuperblock* superblock;
  if (GCNodeAllocator::s_superblocks[size_class]) {
    superblock = GCNodeAllocator::s_superblocks[size_class];
  } else {
    superblock = newSuperblockFromArena(block_size);
    if (!superblock) {
      return nullptr;
    }
    GCNodeAllocator::s_superblocks[size_class] = superblock;
  }
  return superblock->allocateNextUntouched();
}

void* rho::AllocatorSuperblock::allocateNextUntouched() {
  unsigned block_size = blockSize();
  unsigned num_blocks = (superblockSize() - s_superblock_header_size) / block_size;
  uint32_t index = m_next_untouched;
  tagBlockAllocated(index);
  m_next_untouched += 1;
  if (m_next_untouched == num_blocks) {
    GCNodeAllocator::s_superblocks[m_size_class] = nullptr;
  }
  void* result = reinterpret_cast<void*>(firstBlockPointer() + (index * block_size));
#ifdef HAVE_ADDRESS_SANITIZER
  ASAN_UNPOISON_MEMORY_REGION(result, block_size);
#endif
  return result;
}

void rho::AllocatorSuperblock::freeBlock(void* pointer) {
  uintptr_t block = reinterpret_cast<uintptr_t>(pointer);
  uintptr_t first_block = firstBlockPointer();
  unsigned index = (block - first_block) / blockSize();

  // Mark the block as not allocated.
  tagBlockUnallocated(index);

  // Use the block as a free list node and prepend to the free list.
  FreeListNode* free_node = reinterpret_cast<FreeListNode*>(pointer);
  free_node->m_block = index;
  free_node->m_superblock = this;
  GCNodeAllocator::addToFreelist(free_node, m_size_class);
}

void* rho::AllocatorSuperblock::allocateLarge(unsigned size_log2) {
  assert(size_log2 >= 6 && size_log2 < GCNodeAllocator::s_num_medium_pools
      && "Can not allocate objects smaller than 64 bytes using allocateLarge()");
  unsigned size_class = largeSizeClass(size_log2);
  unsigned num_blocks =
      (s_large_superblock_size - s_superblock_header_size) >> size_log2;
  AllocatorSuperblock* superblock;
  if (GCNodeAllocator::s_superblocks[size_class]) {
    // Reuse existing superblock for this allocation size.
    // All superblocks in s_superblocks have at least one untouched block.
    superblock = GCNodeAllocator::s_superblocks[size_class];
  } else {
    // Allocate a new large superblock.
    superblock = newLargeSuperblock(size_log2);
    GCNodeAllocator::s_superblocks[size_class] = superblock;
  }
  return superblock->allocateNextUntouched();
}

void rho::AllocatorSuperblock::tagBlockAllocated(unsigned block) {
  unsigned bitset = block / 64;
  m_free[bitset] &= ~(uint64_t{1} << (block & 63));
}

void rho::AllocatorSuperblock::tagBlockUnallocated(unsigned block) {
  unsigned bitset = block / 64;
  m_free[bitset] |= uint64_t{1} << (block & 63);
}

void rho::AllocatorSuperblock::applyToArenaAllocations(std::function<void(void*)> fun) {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    AllocatorSuperblock* superblock = reinterpret_cast<AllocatorSuperblock*>(next_superblock);
    superblock->applyToBlocks(fun);
    next_superblock += s_small_superblock_size;
  }
}

void rho::AllocatorSuperblock::applyToBlocks(std::function<void(void*)> fun) {
  uintptr_t block = firstBlockPointer();
  unsigned block_size = blockSize();
  unsigned num_blocks =
      (superblockSize() - s_superblock_header_size) / block_size;
  unsigned bitset_entries = (num_blocks + 63) / 64;
  for (int i = 0; i < bitset_entries; ++i) {
    if (m_free[i] != ~0ull) {
      for (int index = 0; index < 64; ++index) {
        if (!(m_free[i] & (uint64_t{1} << index))) {
#ifdef ALLOCATION_CHECK
          // Extra consistency check.
          if (!GCNodeAllocator::lookupPointer(reinterpret_cast<void*>(block))) {
            allocerr("apply to all blocks iterating over non-alloc'd pointer");
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
}

void rho::AllocatorSuperblock::debugPrintSmallSuperblocks() {
  uintptr_t next_superblock = arena_superblock_start;
  while (next_superblock < arena_superblock_next) {
    reinterpret_cast<AllocatorSuperblock*>(next_superblock)->printSummary();
    next_superblock += s_small_superblock_size;
  }
}

void rho::AllocatorSuperblock::printSummary() {
  unsigned superblock_size =
      (superblockSize() - s_superblock_header_size) / blockSize();
  printf("Superblock Summary (blocksize=%d, num block=%d):\n",
      blockSize(), superblockSize());
  unsigned bitset_entries = (superblock_size + 63) / 64;
  for (int i = 0; i < bitset_entries; ++i) {
    int num_free = 0;
    for (int index = 0; index < 64; ++index) {
      if (m_free[i] & (uint64_t{1} << index)) {
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

