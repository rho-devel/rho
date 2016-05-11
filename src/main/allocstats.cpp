/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016 and onwards the Rho Project Authors.
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

#include "rho/IntVector.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/MemoryBank.hpp"

using namespace rho;

#ifdef ALLOC_STATS
extern size_t alloc_counts[32];
extern size_t free_counts[32];
#endif

// Returns a vector list with columns 'size', 'alloc', 'free' representing alloc and free stats.
extern "C"
SEXP allocstats(void) {
#ifdef ALLOC_STATS

    // Copy frequency tables to avoid concurrent modifications affecting result.
    size_t allocs[32];
    size_t frees[32];
    std::copy(std::begin(alloc_counts), std::end(alloc_counts), std::begin(allocs));
    std::copy(std::begin(free_counts), std::end(free_counts), std::begin(frees));

    GCStackRoot<ListVector> ans(ListVector::create(3));
    GCStackRoot<IntVector> size_column(IntVector::create(32));
    GCStackRoot<IntVector> alloc_column(IntVector::create(32));
    GCStackRoot<IntVector> free_column(IntVector::create(32));

    for (int i = 0; i < 32; ++i) {
        (*size_column)[i] = (i + 1) * 8;
        (*alloc_column)[i] = allocs[i];
        (*free_column)[i] = frees[i];
    }
    (*ans)[0] = size_column.get();
    (*ans)[1] = alloc_column.get();
    (*ans)[2] = free_column.get();
    return ans;
#else
    return nullptr;
#endif // ALLOC_STATS
}
