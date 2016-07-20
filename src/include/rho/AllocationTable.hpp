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
   * collisions. This hashtable is different from dense_hash_table by
   * allowing asymmetric insertion and searching - allocation ranges are
   * inserted but the lookup function searches on a pointer so the search
   * will look for a range covering the pointer.
   */
  class AllocationTable {
  public:

    /**
     * Hash bucket for the allocation table.
     * The two lowest bits of the data pointer are flags indicating
     * if the hash bucket contains a superblock (2) and whether this
     * hash bucket is the first representing that allocation (1).
     */
    class Allocation {
    public:
      // Initializes to the empty key.
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
      bool is_superblock() {
        return (m_data & s_superblock_flag) == s_superblock_flag;
      }

      void* as_pointer() {
        return reinterpret_cast<void*>(data_pointer());
      }

      rho::AllocatorSuperblock* as_superblock() {
        return reinterpret_cast<rho::AllocatorSuperblock*>(data_pointer());
      }

    private:
      friend AllocationTable;

      // Constants indicating an empty/deleted bucket:
      static constexpr uintptr_t s_empty_key = 0;
      static constexpr uintptr_t s_deleted_key = UINTPTR_MAX;

      static constexpr int s_first_flag = 1;
      static constexpr int s_superblock_flag = 2;

      uintptr_t m_data;

      // The size in bytes for small objects, or the 2-log for large objects.
      unsigned m_size_log2;

      /** Masks away the pointer bits from the data field. */
      uintptr_t data_pointer() {
        return m_data & ~uintptr_t{3};
      }

      bool is_deleted() {
        return m_data == s_deleted_key;
      }

      bool is_empty() {
        return m_data == s_empty_key;
      }

      /**
       * Returns true if this allocation bucket is the first entry for the
       * corresponding allocation in the allocation table.
       */
      bool is_first() {
        return (m_data & s_first_flag) == s_first_flag;
      }

      /**
       * Returns true if the argument pointer points inside this allocation.
       */
      bool is_internal_pointer(uintptr_t pointer) {
        uintptr_t data = data_pointer();
        return data <= pointer && (data + (1L << m_size_log2)) > pointer;
      }

    };

    /**
     * @param num_bits the number of bits to use for hashing. The number of
     * buckets is 2^num_bits.
     */
    AllocationTable(unsigned num_bits);

    /**
     * Add an allocation (large object or medium object superblock) to the
     * allocations hashtable.
     */
    void insert(uintptr_t pointer, std::size_t size);

    /*
     * Erases all hashtable entries for a pointer. The size determines
     * how many hash buckets are removed.
     */
    void erase(uintptr_t pointer, std::size_t size);

    /*
     * Find a bucket matching the pointer. Will find internal pointers.
     */
    Allocation* search(uintptr_t candidate);

    /*
     * Remove an allocation from the hashtable.  If the allocation is in a
     * superblock, the superblock is left as is but the superblock bitset is
     * updated to indicate that the block is free.
     * If the allocation is a large object (ie. not in a superblock), then
     * the object is taken out of the hashtable and inserted into a freelist.
     */
    bool free_allocation(uintptr_t pointer);

    /**
     * Iterates over all current allocations and calls the argument function on
     * their pointers.
     *
     * It is okay to free objects during the iteration, but adding objects
     * means they may not be found during the current iteration pass.
     */
    void ApplyToAllAllocations(std::function<void(void*)> fun);

    /**
     * Prints a summary of the table illustrating the bucket utilization.
     */
    void PrintTable();

    /**
     * Attempts to resize the hashtable to use a new number of bits
     * for hash keys. The resize fails if the number of collisions
     * for a single entry exceeds MAX_REBALANCE_COLLISIONS.
     * @return true if the resize succeeded.
     */
    bool resize(unsigned new_alloctable_bits);

  private:
    /** The low pointer bits are shifted out in the hash function. */
    static constexpr unsigned LOW_BITS = 19;

    /** Additional entries that can be inserted before resize. */
    unsigned m_capacity;

    unsigned m_num_bits;
    unsigned m_num_buckets;

    // Mask deciding which bits of a hash key are used to index the hash table.
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
    bool try_insert(Allocation* table, uintptr_t pointer, std::size_t size);

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
    unsigned pointer_hash(uintptr_t pointer);

    /** Computes the next key in a probe chain. */
    unsigned probe_func(unsigned hash, int i);

  };
}

#endif // RHO_ALLOCATIONTABLE_HPP

