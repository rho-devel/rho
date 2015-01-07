/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file GCStackRoot.cpp
 *
 * Implementation of class GCStackRootBase.
 */

#include "CXXR/GCStackRoot.hpp"
#include "CXXR/AddressSanitizer.h"
#include "CXXR/GCStackFrameBoundary.hpp"
#include "Defn.h"
#include "gc.h"
#include "private/gcconfig.h"  // For STACK_GROWS_UP

#include <cstdlib>

extern "C" {
    // Declared in src/extra/gc/include/private/gc_priv.h.
    void GC_with_callee_saves_pushed(void (*fn)(char*, void *),
				     char* arg);
}

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	auto ensureReachableP = GCStackRootBase::ensureReachable;
    }
}

// As of 2014, std::align isn't available widely enough, so we use our own
// alignment function here.
static void* align(void* pointer)
{
    const size_t alignment = alignof(void*);
    uintptr_t value = reinterpret_cast<uintptr_t>(pointer);
    value = (value + alignment - 1) & ~(alignment - 1);
    return reinterpret_cast<void*>(value);
}

// TODO: should only be defined if we have asan.
extern "C"
const char* __asan_default_options()
{
    // R installs it's own signal handlers.
    return "--allow_user_segv_handler=1";
}

void* GCStackRootBase::getStackBase()
{
    struct GC_stack_base base;
    if (GC_get_stack_base(&base) == GC_SUCCESS) {
#ifdef STACK_GROWS_UP
	return base.mem_base;
#else
	// GC_get_stack_base() returns one past the end of the stack.
	return reinterpret_cast<void*>(
	    reinterpret_cast<char*>(base.mem_base) - sizeof(void*));
#endif
    }
    return align(reinterpret_cast<void*>(R_CStackStart));
}

NO_SANITIZE_ADDRESS 
void GCStackRootBase::visitRoots(GCNode::const_visitor* visitor,
				 const void* start_ptr,
				 const void* end_ptr)
{
    uintptr_t start = start_ptr ? reinterpret_cast<uintptr_t>(start_ptr)
	: reinterpret_cast<uintptr_t>(getStackBase());
    uintptr_t end = reinterpret_cast<uintptr_t>(end_ptr);


#ifdef STACK_GROWS_UP
    for (uintptr_t stack_pointer = start; stack_pointer < end;
	 stack_pointer += alignof(void*))
#else
    for (uintptr_t stack_pointer = start; stack_pointer > end;
	 stack_pointer -= alignof(void*))
#endif
    {
        void* candidate_pointer = *reinterpret_cast<void**>(stack_pointer);
	GCNode* node = GCNode::asGCNode(candidate_pointer);
	if (node) {
	    (*visitor)(node);
	}
    }
}

void GCStackRootBase::visitRoots(GCNode::const_visitor* v)
{
    GC_with_callee_saves_pushed(visitRootsImpl,
				reinterpret_cast<char*>(v));
}

void GCStackRootBase::visitRootsImpl(char* p, void*)
{
    GCStackRoot<GCNode> top;
    GCNode::const_visitor* v = reinterpret_cast<GCNode::const_visitor*>(p);
    visitRoots(v, nullptr, &top);
}

void GCStackRootBase::withAllStackNodesProtected(std::function<void()> function)
{
    // Push all callee-save registers onto the stack.
    GC_with_callee_saves_pushed(withAllStackNodesProtectedImpl,
				reinterpret_cast<char*>(&function));
}

void GCStackRootBase::withAllStackNodesProtectedImpl(char* pointer, void*)
{
    std::function<void()>* function
	= reinterpret_cast<std::function<void()>*>(pointer);

    // Protect all values on the stack above the barrier.  Those below the
    // barrier have already been protected.
    GCStackFrameBoundary::withStackFrameBoundary([=]() {
	    GCStackFrameBoundary::advanceBarrier();
	    // Call the user function.
	    (*function)();
	    return nullptr;
	});
}
