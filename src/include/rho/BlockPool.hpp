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

#include <functional>
#include <cstdint>

using std::size_t;
using std::function;

class BlockPool {
  public:
    BlockPool() = delete;

    /** @brief Must be called before any allocations can be made. */
    static void Initialize();

    static void* AllocBlock(size_t bytes);
    static void FreeBlock(void* p);

    /** @brief Apply function to all allocated blocks (small + large). */
    static void ApplyToAllBlocks(function<void(void*)> f);

    /** @brief Find heap allocation start pointer. */
    static void* Lookup(void* candidate);

    /** @brief Print allocation overview for debugging. */
    static void DebugPrint();

    static void DebugRebalance(int low_bits);

  private:
    /** Allocate a small block. The pool index is the allocation size divided by 8. */
    static void* AllocSmall(size_t bytes);

    /** Free a block in this block pool. */
    static void FreeSmall(void* p, unsigned superblock_id);

    /** Allocate a large block in the sparse block table. */
    static void* AllocLarge(unsigned bytes);
};

