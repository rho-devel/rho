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

#ifndef RHO_ALLOCATIONTABLE_HPP
#define RHO_ALLOCATIONTABLE_HPP

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

#include <cstdint>
#include <cstdlib>
#include <functional>
#include <limits>
#include <map>

#include "rho/AllocatorSuperblock.hpp"

namespace rho {
/*
 * Special-purpose hashtable implementation for tracking medium and large
 * allocations.
 *
 * The table uses open addressing with quadratic probing to resolve
 * collisions. This hashtable differs from dense_hash_table by
 * allowing asymmetric insertion and searching - allocation ranges are
 * inserted but the lookup function searches on a pointer so the search
 * will look for a range covering the pointer.
 *
 * Another difference from most other types of generic maps is that this
 * hashtable will map a single allocation to multiple hash buckets - the
 * reason being that each hash bucket represents addresses that hash into
 * that bucket, and all internal pointers of an allocation must be able
 * to be found via the search() function.
 *
 * For hashing pointers, we shift away LOW_BITS bits from the pointer before
 * hashing, and this limits the range of hash keys one allocation can hash
 * to.
 */
class AllocationTable {
public:

  /** @brief A hash bucket for the allocation table.
   *
   * Allocations are stored in multiple Allocation entries in the allocation
   * table, however only one representative entry needs to be created and
   * passed to insert(). The constructors of this class make it easy to
   * construct that representative Allocation.
   *
   * The two lowest bits of the data pointer are flags indicating
   * if the hash bucket contains a superblock (2) and whether this
   * hash bucket is the first representing that allocation (1).
   */
  class Allocation {
  public:
    /** Initializes to an empty hash bucket. */
    Allocation(): m_data(s_empty_key), m_size_log2(0) {}

    /** @brief Creates a new hash bucket for a superblock.
     *
     * @param superblock the superblock to insert in the table.
     * @param size_log2 the 2-log of the size of the superblock.
     */
    Allocation(AllocatorSuperblock* superblock, unsigned size_log2):
        m_data(reinterpret_cast<uintptr_t>(superblock) | s_superblock_flag),
        m_size_log2(size_log2) { }

    /**
     * @brief Creates a new hash bucket for a large allocation
     * (non-superblock).
     */
    Allocation(void* allocation, unsigned size_log2):
        m_data(reinterpret_cast<uintptr_t>(allocation)),
        m_size_log2(size_log2) { }

    /** @breif Returns true if this allocation represents a superblock. */
    bool isSuperblock() const {
      return (m_data & s_superblock_flag) == s_superblock_flag;
    }

    /** @brief Returns the data pointer as a void pointer. */
    void* asPointer() const {
      return reinterpret_cast<void*>(dataPointer());
    }

    /** @brief Returns the allocation size 2-log. */
    unsigned sizeLog2() const {
      return m_size_log2;
    }

    /** Returns the size class for this allocation. */
    unsigned sizeClass() const {
      return AllocatorSuperblock::sizeClassFromSizeLog2(m_size_log2);
    }

    /**
     * Returns the data pointer as a superblock pointer.
     */
    AllocatorSuperblock* asSuperblock() {
      return reinterpret_cast<AllocatorSuperblock*>(dataPointer());
    }

  private:
    friend AllocationTable;

    /** @brief Constant key for an empty hash bucket. */
    static constexpr uintptr_t s_empty_key = 0;

    /** @brief Constant key for a deleted hash bucket. */
    static constexpr uintptr_t s_deleted_key = UINTPTR_MAX;

    /** @brief The flag bit indicating the first bucket of an allocation. */
    static constexpr int s_first_flag = 1;

    /** @breif The flag bit indicating that the allocation is a superblock. */
    static constexpr int s_superblock_flag = 2;

    uintptr_t m_data;

    /** @breif The 2-log of the allocation size. */
    unsigned m_size_log2;

    /** @brief Returns the pointer to the allocation.
     *
     * Masks away the pointer bits from the data field.
     */
    uintptr_t dataPointer() const {
      return m_data & ~uintptr_t{3};
    }

    /** @breif Returns true if this hash bucket represents a deleted
     * allocation.
     */
    bool isDeleted() const {
      return m_data == s_deleted_key;
    }

    /** @brief Returns true if this hash bucket is unused. */
    bool isEmpty() const {
      return m_data == s_empty_key;
    }

    /**
     * Returns true if this allocation bucket is the first entry for the
     * corresponding allocation in the allocation table.
     */
    bool isFirst() const {
      return (m_data & s_first_flag) == s_first_flag;
    }

    /** @brief Returns true if the argument pointer points inside this
     * allocation.
     */
    bool isInternalPointer(uintptr_t pointer) const {
      uintptr_t data = dataPointer();
      return data <= pointer && (data + (1L << m_size_log2)) > pointer;
    }

  };


  /**
   * @brief Allocates a new allocation table of the give size.
   *
   * @param num_bits the number of bits to use for hashing. The number of
   * buckets is 2^num_bits.
   */
  AllocationTable(unsigned num_bits);

  /** @brief Frees the memory used by the hashtable, but not the allocations
   * in it.
   */
  ~AllocationTable();

  AllocationTable(const AllocationTable& table) = delete;

  AllocationTable& operator=(const AllocationTable& table) = delete;

  /** @brief Iterates over all current allocations and calls the argument
   * function on their pointers.
   *
   * Eeven though multiple entries are stored per allocation, the argument
   * function is only called once per allocation. The first flag of the
   * allocation buckets are used to ensure this. Only the first entry is
   * used as the representative entry of each allocation.
   *
   * It is okay to free objects during the iteration, but adding objects
   * means they may not be found during the current iteration pass.
   */
  void applyToAllAllocations(std::function<void(void*)> fun);

  /** @breif Erases all hashtable entries for an allocation.
   *
   * @param size_log2 the 2-log of the allocation size. Determines how many
   * hash buckets are removed.
   */
  void erase(uintptr_t pointer, size_t size_log2);

  /** @brief Add a large object allocation, not a superblock, into the
   * allocations hashtable.
   */
  void insert(void* allocation, size_t size_log2) {
    insert(Allocation(allocation, size_log2));
  }

  /** @brief Inserts a superblock a superblock allocation into the
   * allocations hashtable.
   */
  void insertSuperblock(AllocatorSuperblock* superblock, size_t size_log2) {
    insert(Allocation(superblock, size_log2));
  }

  /** @brief Find a bucket matching the argument pointer.
   *
   * This function does find internal pointers. This is supported by mapping
   * all possible internal hash keys into the allocation table.
   */
  Allocation* search(uintptr_t candidate);

  /** @brief Prints a summary of the table illustrating the bucket
   * utilization.
   */
  void printSummary();

  /**
   * Attempts to resize the hashtable to use a new number of bits
   * for hash keys. The resize fails if the number of collisions
   * for a single entry exceeds MAX_REBALANCE_COLLISIONS.
   * @return true if the resize succeeded.
   */
  void resize(unsigned new_alloctable_bits);

private:
  /** The low pointer bits are shifted out in the hash function. */
  static constexpr unsigned LOW_BITS = 19;

  /** Additional entries that can be inserted before resize. */
  unsigned m_capacity;

  unsigned m_num_bits;
  unsigned m_num_buckets;

  /**
   * Bit mask deciding which bits of a hash key are used to index the hash
   * table.
   */
  unsigned m_hash_mask;

  Allocation* m_buckets = nullptr;

  // Counters logging collision statistics:
  unsigned m_num_insert_collisions = 0;
  unsigned m_num_erase_collisions = 0;
  unsigned m_num_search_collisions = 0;

  /** Erase a single key entry from the table. */
  void eraseSingleKey(uintptr_t pointer, size_t key);

  /** Insert an allocation with capacity checking. */
  void insert(const Allocation& allocation);

  /**
   * Insert an allocation without checking if there is room in the hash table
   * for all the required new entries.  This must only be called after
   * checking that there is enough room.
   *
   * @param allocation the allocation to insert.
   */
  void insertUnchecked(const Allocation& allocation);

  /**
   * This is used to insert a single entry for an allocation.
   * Must only be called after checking that there is room to insert
   * the given key!
   *
   * @param key the hash key for the entry.
   * @param first set to true if this is the first hash entry for the
   * allocation.
   */
  void insertSingleKey(const Allocation& allocation, size_t key, bool first);

  /** Returns the start key for iterating over allocation keys. */
  static size_t startKey(uintptr_t pointer);

  /**
   * Returns the end key for iterating over keys in a single allocation.
   *
   * @param size the allocation size in bytes
   */
  static size_t endKey(uintptr_t pointer, size_t size_log2);

  /**
   * Calculate the number of hash table entries required for an allocation.
   */
  static size_t requiredCapacity(const Allocation& allocation) {
    return endKey(allocation.dataPointer(), allocation.sizeLog2())
        - startKey(allocation.dataPointer());
  }

  /** @brief Computes the hash key for a pointer.
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
  size_t pointerHash(uintptr_t pointer);

  /** @brief Computes the next key in a probe chain.
   *
   * The probe function should be applied in a recurrence equation style:
   *
   * hash = probeFunc(hash, i);
   *
   * If the initial hash is 0, then we get the following probe chain:
   *
   * i    = NA, 0, 1, 2,  3,  4,  5,  6, ...
   * hash =  0, 1, 3, 6, 10, 15, 21, 28, ...
   */
  size_t probeFunc(size_t prev_hash, int i);

};
}

#endif // RHO_ALLOCATIONTABLE_HPP

