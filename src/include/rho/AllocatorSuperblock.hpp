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

#ifndef RHO_ALLOCATORSUPERBLOCK_HPP
#define RHO_ALLOCATORSUPERBLOCK_HPP

#include <cstdint>
#include <functional>

#include "GCNodeAllocator.hpp"

typedef std::uint64_t u64;
typedef std::uint32_t u32;

using std::function;
using std::size_t;

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
 * Superblocks are either 2^18 or 2^19 bytes.
 *
 * The fixed header size is 1168 bytes. This is the breakdown:
 *    block_size     =   32 bytes
 *    next_untouched =   32 bytes
 *    free_list      =   64 bytes
 *    free bitset    = 1024 bytes
 *    total          = 1152 bytes
 *
 * The bitset can map 1024 * 8 = 8192 blocks.  We use these two combinations
 * of superblock size and minimum block size:
 *
 * sb_size    min. block_size      max. num. blocks
 *    2^18                 32      2^18 / 32 = 8192
 *    2^19                 64      2^19 / 64 = 8192
 *
 * Note that the max number of blocks in the table above is computed without
 * accounting for the header size, so there is actually a little bit of slack,
 * i.e. wasted bits in the bitset.
 *
 * The required header size can be calculated with the following equation:
 *
 * header_size =
 *   128 + 8 * ( ( (sb_size + block_size - 1) / block_size ) + 63 ) / 64 )
 */
#define SUPERBLOCK_HEADER_SIZE (128 + 8 * ((((1<<18) + 31) / 32 + 63) / 64))

namespace rho {

  struct FreeListNode;

  /**
   * Superblocks are used to allocate small and medium sized objects.
   * The bitset tracks which blocks are currently allocated.
   * For small objects block_size is the actual object size.
   * For medium objects block_size is the 2-log of the object size.
   */
  struct AllocatorSuperblock {
    u32 block_size;
    u32 next_untouched;
    FreeListNode* free_list;
    u64 free[];  // Free bitset.

    AllocatorSuperblock(u32 block_size, unsigned bitset_entries):
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
    static AllocatorSuperblock* NewSuperblock(int block_size);

    /**
     * Compute the address of the superblock header from a pointer.
     * Returns nullptr if candidate pointer is not inside the small object arena
     * or if the pointer points inside the superblock header.
     */
    static AllocatorSuperblock* SuperblockFromPointer(void* pointer);

    /**
     * Allocate a small block (bytes >= 32 && bytes <= 256).
     * Returns nullptr if there is no more space left for this object
     * size in the small object arena.
     */
    static void* AllocSmall(size_t block_size);

    /**
     * Allocates a small block in this superblock (either from freelist or using
     * the next untouched block).
     */
    void* AllocateSmallBlock();

    /**
     * Free a pointer inside a given superblock. The block MUST be in the given
     * superblock.
     */
    void FreeSmallBlock(void* pointer);

    /** Allocate a medium or large object. */
    static void* AllocLarge(unsigned size_log2);

    /** Tag a block in a superblock as allocated. */
    void TagBlockAllocated(unsigned block);
  };
}

#endif // RHO_ALLOCATORSUPERBLOCK_HPP

