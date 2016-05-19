/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file GCStackRoot.cpp
 *
 * Implementation of class GCStackRootBase.
 */

#include "rho/GCStackRoot.hpp"
#include "rho/AddressSanitizer.hpp"
#include "rho/GCStackFrameBoundary.hpp"
#include "Defn.h"
#include "gc.h"

#include <cstdlib>

extern "C" {
    // Declared in src/extra/gc/include/private/gc_priv.h.
    void GC_with_callee_saves_pushed(void (*fn)(char*, void *),
				     char* arg);
}

using namespace std;
using namespace rho;

namespace rho {
    namespace ForceNonInline {
	auto ensureReachableP = GCStackRootBase::ensureReachable;
    }
}

#ifdef HAVE_ADDRESS_SANITIZER
extern "C"
const char* __asan_default_options()
{
    // Disables leak detection and ASAN signal segv handler.
    // R installs it's own signal handlers.
    // Need to set both allow_user_segv_handler and handle_segv because
    // different ASAN versions have different options for this.
    return "allow_user_segv_handler=1:handle_segv=0:detect_leaks=0";
}
#endif

extern "C"
void R_GetStackLimits();

void* GCStackRootBase::getStackBase()
{
    if (R_CStackStart == -1) {
	R_GetStackLimits();
    }
    return reinterpret_cast<void*>(R_CStackStart);
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
