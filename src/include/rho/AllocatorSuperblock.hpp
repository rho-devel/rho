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

namespace rho {

  /**
   * Superblocks are used to allocate small and medium sized objects.
   * The bitset tracks which blocks are currently allocated.
   * For small objects block_size is the actual object size.
   * For medium objects block_size is the 2-log of the object size.
   *
   * Superblock size is determined by the size_class member.
   */
  class AllocatorSuperblock {
  public:

    /**
     * @param size_class an index into the s_size_class array giving the block size.
     * @param bitset_entries the number of bitset entries used for this superblock.
     */
    AllocatorSuperblock(unsigned size_class, unsigned bitset_entries):
        m_size_class(size_class),
        m_next_untouched(0) {
      // Here we mark all bitset entries as free so that we don't have to do
      // precise range checking while iterating over currently allocated blocks
      // when the number of blocks is not evenly divisible by 64.
      for (int i = 0; i < bitset_entries; ++i) {
        m_free[i] = ~0ull;
      }
    }

    /**
     * Initialize the arena used to allocate small-object superblocks.
     */
    static void allocateArena();

    /**
     * Allocate a small block (bytes >= 32 && bytes <= 256).
     * Returns nullptr if there is no more space left for this object
     * size in the small object arena.
     */
    static void* allocateBlock(size_t block_size);

    /**
     * Allocates the next untouched block in this superblock.
     * The superblock must have an untouched block when calling this function.
     */
    void* allocateNextUntouched();

    /** Allocate a medium or large object. */
    static void* allocateLarge(unsigned size_log2);

    /** \brief Apply function to all current allocations. */
    static void applyToArenaAllocations(std::function<void(void*)> fun);

    /** \brief Apply function to all current blocks in this superblock. */
    void applyToBlocks(std::function<void(void*)> fun);

    /**
     * Free a pointer inside a given superblock. The block MUST be in the given
     * superblock.
     */
    void freeBlock(void* pointer);

    /**
     * Tests the bitset if a block index is allocated.
     * Returns true if the given block is currently allocated.
     */
    bool isBlockAllocated(unsigned block) {
      unsigned bitset = block / 64;
      return !(m_free[bitset] & (uint64_t{1} << (block & 63)));
    }

    /**
     * Returns true if the pointer is inside the superblock and not inside
     * the superblock header.
     */
    bool isBlockPointer(uintptr_t pointer) {
      return pointer >= firstBlockPointer() && pointer < endPointer();
    }

    /**
     * Returns true if the pointer is inside the superblock (may be inside the
     * superblock header).
     */
    bool isInternalPointer(uintptr_t pointer) {
      return pointer >= reinterpret_cast<uintptr_t>(this)
          && pointer < endPointer();
    }

    /** Print debug info about this superblock. */
    void printSummary();

    /**
     * Tag a block in a superblock as allocated.
     *
     * @param block the index of the block to tag.
     */
    void tagBlockAllocated(unsigned block);

    /**
     * Tag a block in a superblock as unallocated.
     *
     * @param block the index of the block to tag.
     */
    void tagBlockUnallocated(unsigned block);

  private:
    friend class AllocationTable;
    friend class GCNodeAllocator;
    friend class FreeListNode;

    /**
     * NOTE: We use a fixed superblock header size, regardless of block size. This
     * leaves some unused bitset entries for larger block sizes.
     * Superblocks are either 2^18 or 2^19 bytes.
     *
     * The fixed header size is 1168 bytes. This is the breakdown:
     *    size_class     =   32 bytes
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
    static constexpr unsigned s_superblock_header_size =
        128 + 8 * ((((1 << 18) + 31) / 32 + 63) / 64);

    /**
     * The number of 64-bit bitset entries needed to store the free bitset.
     * Must be declared here so it comes
     */
    static constexpr unsigned s_max_bitset_entries =
        (s_superblock_header_size - 128) / 8;

    // These are the superblock header members (1152 bytes total):
    std::uint32_t m_size_class;
    std::uint32_t m_next_untouched;
    std::uint64_t m_free[s_max_bitset_entries];  // Free bitset, using 64-bit entries.

    /** Samll object arena is 1Gb = 30 bits. */
    static constexpr unsigned s_arenasize = 1 << 30;

    /**
     * A small object superblock has size 2^18.
     * Minimum object size = 32 bytes.
     */
    static constexpr unsigned s_small_superblock_bits = 18;
    static constexpr unsigned s_small_superblock_size = 1 << s_small_superblock_bits;

    /**
     * Medium object superblocks use 2^19 bytes.
     * Minimum object size = 64 bytes.
     */
    static constexpr unsigned s_large_superblock_size_log2 = 19;
    static constexpr unsigned s_large_superblock_size = 1 << s_large_superblock_size_log2;

    /**
     * This array maps size classes to block sizes.
     */
    static unsigned s_size_class[];

    /**
     * Get the block size for this superblock. Depends on m_size_class.
     */
    unsigned blockSize() {
      return s_size_class[m_size_class];
    }

    /**
     * Returns size in bytes of this superblock. Determined by m_size_class.
     */
    unsigned superblockSize() {
      if (m_size_class < GCNodeAllocator::s_num_small_pools) {
        return s_small_superblock_size;
      } else {
        return s_large_superblock_size;
      }
    }

    /**
     * \brief Returns a pointer to one-past the end of this superblock.
     *
     * This is used to do a quick check if a pointer is inside this superblock.
     *
     * Due to block alignment there may not be a block spanning right up to the
     * end of the superblock, however for pointer lookup operations this is
     * handled later when the bitset is tested to see if the indexed block is
     * currently allocated.
     */
    uintptr_t endPointer() {
      return reinterpret_cast<uintptr_t>(this) + superblockSize();
    }

    /** Returns a pointer to the first block in this superblock. */
    uintptr_t firstBlockPointer() {
      return reinterpret_cast<uintptr_t>(this) + s_superblock_header_size;
    }

    /**
     * Lookup a pointer to a small allocation in the small object arena.
     * Returns nullptr if the pointer does not point inside a small object,
     * i.e. if the pointer is outside the active part of the arena or
     * if the pointer points inside a superblock header instead of a
     * valid block, or if the pointer points to a not-currently-allocated
     * block.
     */
    static void* lookupAllocation(uintptr_t candidate);

    /**
     * Lookup the start of a block via a (possibly internal) block pointer.
     */
    void* lookupBlock(uintptr_t candidate);

    /**
     * Allocates a new superblock from the small object arena.
     * Returns nullptr if the arena space is full.
     * If nullptr is returned we need to fall back on using a separately
     * allocated superblock.
     */
    static AllocatorSuperblock* newSuperblockFromArena(unsigned block_size);

    /**
     * Allocates a new large superblock for medium-sized allocations.
     * Large superblocks are allocated outside the small block arena.
     */
    static AllocatorSuperblock* newLargeSuperblock(unsigned size_log2);

    /**
     * Compute the address of the superblock header from a pointer.
     * Returns nullptr if candidate pointer is not inside the small object arena
     * or if the pointer points inside the superblock header.
     */
    static AllocatorSuperblock* arenaSuperblockFromPointer(uintptr_t pointer);

    /** Print debug info about all small-object superblocks. */
    static void debugPrintSmallSuperblocks();

    /**
     * Get the size class index for a small allocation using the allocation
     * size in bytes.
     */
    static unsigned smallSizeClass(unsigned block_size) {
      return block_size / 8;
    }

    /**
     * Get the size class index for a large allocation using the 2-log
     * of the allocation size in bytes.
     */
    static unsigned largeSizeClass(unsigned size_log2) {
      return GCNodeAllocator::s_num_small_pools + size_log2;
    }

  };

  class LargeSuperblock : public AllocatorSuperblock {
  };
}

#endif // RHO_ALLOCATORSUPERBLOCK_HPP

