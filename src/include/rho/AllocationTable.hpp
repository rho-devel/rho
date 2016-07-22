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
   * to be found via the lookup_pointer() member function.
   *
   * For hashing pointers, we shift away LOW_BITS bits from the pointer before
   * hashing, and this limits the range of hash keys one allocation can hash
   * to.
   */
  class AllocationTable {
  private:
    class Allocation;

  public:

    /**
     * \brief Allocates a new allocation table of the give size.
     *
     * @param num_bits the number of bits to use for hashing. The number of
     * buckets is 2^num_bits.
     */
    AllocationTable(unsigned num_bits);

    /**
     * \brief Frees the memory used by the hashtable, but not the allocations in it.
     */
    ~AllocationTable();

    /**
     * Iterates over all current allocations and calls the argument function on
     * their pointers.
     *
     * It is okay to free objects during the iteration, but adding objects
     * means they may not be found during the current iteration pass.
     */
    void applyToAllAllocations(std::function<void(void*)> fun);

    /**
     * Erases all hashtable entries for a pointer. The size determines
     * how many hash buckets are removed.
     */
    void erase(uintptr_t pointer, size_t size);

    /**
     * Remove an allocation from the hashtable.  If the allocation is in a
     * superblock, the superblock is left as is but the superblock bitset is
     * updated to indicate that the block is free.
     * If the allocation is a large object (ie. not in a superblock), then
     * the object is taken out of the hashtable and inserted into a freelist.
     */
    bool freeAllocation(uintptr_t pointer);

    /**
     * Add an allocation (large object or medium object superblock) to the
     * allocations hashtable.
     */
    void insert(void* pointer, size_t size_log2);

    /**
     * Add a superblock allocation to the allocations hashtable.
     */
    void insertSuperblock(AllocatorSuperblock* superblock, size_t size_log2) {
      insert(reinterpret_cast<void*>(
          reinterpret_cast<uintptr_t>(superblock) | Allocation::s_superblock_flag),
          size_log2);
    }

    /**
     * Lookup a pointer in this hash table. Returns the start
     * of the allocation if the pointer is a current allocation.
     * Otherwise returns nullptr.
     */
    void* lookup_pointer(uintptr_t candidate);

    /*
     * Find a bucket matching the pointer. Will find internal pointers.
     */
    Allocation* search(uintptr_t candidate);

    /**
     * Prints a summary of the table illustrating the bucket utilization.
     */
    void printSummary();

    /**
     * Attempts to resize the hashtable to use a new number of bits
     * for hash keys. The resize fails if the number of collisions
     * for a single entry exceeds MAX_REBALANCE_COLLISIONS.
     * @return true if the resize succeeded.
     */
    bool resize(unsigned new_alloctable_bits);

  private:
    /**
     * Hash bucket for the allocation table.
     * The two lowest bits of the data pointer are flags indicating
     * if the hash bucket contains a superblock (2) and whether this
     * hash bucket is the first representing that allocation (1).
     */
    class Allocation {
    public:
      /** Initializes to an empty hash bucket. */
      Allocation(): m_data(s_empty_key), m_size_log2(0) {}

      /**
       * Creates a new hash bucket for a superblock.
       */
      Allocation(rho::AllocatorSuperblock* superblock, unsigned size, bool first):
          m_data(reinterpret_cast<uintptr_t>(superblock) | s_superblock_flag),
          m_size_log2(size) {
        if (first) {
          m_data |= s_first_flag;
        }
      }

      /**
       * Creates a new hash bucket for a separate allocation (non-superblock).
       */
      Allocation(void* allocation, unsigned size_log2, bool first):
          m_data(reinterpret_cast<uintptr_t>(allocation)),
          m_size_log2(size_log2) {
        if (first) {
          m_data |= s_first_flag;
        }
      }

      /**
       * Returns true if this allocation represents a superblock.
       */
      bool isSuperblock() {
        return (m_data & s_superblock_flag) == s_superblock_flag;
      }

      /**
       * Returns the data pointer as a void*.
       */
      void* asPointer() {
        return reinterpret_cast<void*>(dataPointer());
      }

      /**
       * Returns the data pointer as a superblock pointer.
       */
      rho::AllocatorSuperblock* asSuperblock() {
        return reinterpret_cast<rho::AllocatorSuperblock*>(dataPointer());
      }

    private:
      friend AllocationTable;

      /** Constant key for an empty hash bucket. */
      static constexpr uintptr_t s_empty_key = 0;

      /** Constant key for a deleted hash bucket. */
      static constexpr uintptr_t s_deleted_key = UINTPTR_MAX;

      /** The flag bit indicating the first bucket for an allocation. */
      static constexpr int s_first_flag = 1;

      /** The flag bit indicating that the allocation is a superblock. */
      static constexpr int s_superblock_flag = 2;

      uintptr_t m_data;

      /** The size in bytes for small objects, or the 2-log for large objects. */
      unsigned m_size_log2;

      /** Masks away the pointer bits from the data field. */
      uintptr_t dataPointer() {
        return m_data & ~uintptr_t{3};
      }

      /** Returns true if this hash bucket represents a deleted allocation. */
      bool isDeleted() {
        return m_data == s_deleted_key;
      }

      /** Returns true if this hash bucket is unused. */
      bool isEmpty() {
        return m_data == s_empty_key;
      }

      /**
       * Returns true if this allocation bucket is the first entry for the
       * corresponding allocation in the allocation table.
       */
      bool isFirst() {
        return (m_data & s_first_flag) == s_first_flag;
      }

      /**
       * Returns true if the argument pointer points inside this allocation.
       */
      bool isInternalPointer(uintptr_t pointer) {
        uintptr_t data = dataPointer();
        return data <= pointer && (data + (1L << m_size_log2)) > pointer;
      }

    };

    /** The low pointer bits are shifted out in the hash function. */
    static constexpr unsigned LOW_BITS = 19;

    /** Additional entries that can be inserted before resize. */
    unsigned m_capacity;

    unsigned m_num_bits;
    unsigned m_num_buckets;

    /** Mask deciding which bits of a hash key are used to index the hash table. */
    unsigned m_hash_mask;

    Allocation* m_buckets = nullptr;

    // Counters logging collision statistics:
    unsigned m_num_insert_collisions = 0;
    unsigned m_num_erase_collisions = 0;
    unsigned m_num_search_collisions = 0;

    /**
     * Unlike most other member functions this one takes a table pointer,
     * because it is used to tentatively insert new nodes into a temporary
     * resized table.
     *
     * @return true if the insert succeeded.
     */
    bool tryInsert(Allocation* table, uintptr_t pointer, size_t size);

    /**
     * Gets the start key for iterating over allocation keys
     */
    static size_t startKey(uintptr_t pointer);

    /**
     * Get the end key for iterating over keys in a single allocation.
     *
     * @param size the allocation size in bytes
     */
    static size_t endKey(uintptr_t pointer, size_t size);

    /**
     * Computes the hash key for a pointer.
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

    /**
     * Computes the next key in a probe chain.
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

