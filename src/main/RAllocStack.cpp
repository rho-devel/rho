/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
 *  http://www.r-project.org/Licenses/
 */

/** @file RAllocStack.cpp
 *
 * Implementation of class RAllocStack and related functions.
 */

#include "CXXR/RAllocStack.h"

#include <cstring>
#include <stdexcept>
#include "R_ext/Error.h"
#include "localization.h"
#include "CXXR/MemoryBank.hpp"

using namespace std;
using namespace CXXR;

// Force generation of non-inline embodiments of functions in the C
// interface:
namespace CXXR {
    namespace ForceNonInline {
	void* (*vmaxgetp)(void) = vmaxget;
	void (*vmaxsetp)(const void*) = vmaxset;
    }
}

RAllocStack::Stack* RAllocStack::s_stack = nullptr;
#ifndef NDEBUG
RAllocStack::Scope* RAllocStack::s_innermost_scope = 0;
#endif

void* RAllocStack::allocate(size_t sz)
{
    Pair pr(sz, MemoryBank::allocate(sz));
    s_stack->push(pr);
    return s_stack->top().second;
}

void RAllocStack::initialize()
{
    s_stack = new Stack();
}

void RAllocStack::restoreSize(size_t new_size)
{
    if (new_size > s_stack->size())
	throw out_of_range("RAllocStack::restoreSize: requested size"
			   " greater than current size.");
#ifndef NDEBUG
    if (s_innermost_scope && new_size < s_innermost_scope->startSize())
	throw out_of_range("RAllocStack::restoreSize: requested size"
			   " too small for current scope.");
#endif
    trim(new_size);
}

void RAllocStack::trim(size_t new_size)
{
    while (s_stack->size() > new_size) {
	Pair& top = s_stack->top();
	MemoryBank::deallocate(top.second, top.first);
	s_stack->pop();
    }
}

// ***** C interface *****

char* R_alloc(size_t num_elts, int elt_size)
{
    if (elt_size <= 0)
	Rf_error(_("R_alloc: element size must be positive."));
    size_t uelt_size = size_t(elt_size);
    size_t size = num_elts*uelt_size;
    // Check for integer overflow:
    if (size/uelt_size != num_elts)
	Rf_error(_("R_alloc: requested allocation is impossibly large."));
    return static_cast<char*>(RAllocStack::allocate(size));
}

char* S_alloc(long num_elts, int elt_size)
{
    if (num_elts < 0)
	Rf_error(_("S_alloc: number of elements must be non-negative."));
    if (elt_size <= 0)
	Rf_error(_("S_alloc: element size must be positive."));
    size_t unum_elts = size_t(num_elts);
    size_t uelt_size = size_t(elt_size);
    size_t size = unum_elts*uelt_size;
    // Check for integer overflow:
    if (size/uelt_size != unum_elts)
	Rf_error(_("R_alloc: requested allocation is impossibly large."));
    char* ans = static_cast<char*>(RAllocStack::allocate(size));
    memset(ans, 0, size);
    return ans;
}
    
char* S_realloc(char *prev_block, long new_sz, long old_sz, int elt_size)
{
    if (new_sz <= old_sz)
	return prev_block;
    char* ans = S_alloc(new_sz, elt_size);
    size_t uelt_size = size_t(elt_size);
    size_t old_bytes = size_t(old_sz)*uelt_size;
    memcpy(ans, prev_block, old_bytes);
    memset(ans + old_sz, 0, size_t(new_sz)*uelt_size - old_bytes);
    return ans;
}
