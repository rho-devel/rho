/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include "CXXR/GCNode.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <limits>
#include "CXXR/AddressSanitizer.h"
#include "CXXR/ByteCode.hpp"
#include "CXXR/GCManager.hpp"
#include "CXXR/GCRoot.h"
#include "CXXR/GCStackFrameBoundary.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/ProtectStack.h"
#include "CXXR/RAllocStack.h"
#include "CXXR/WeakRef.h"
#include "gc.h"
extern "C" {
#  include "private/gc_priv.h"
}

using namespace std;
using namespace CXXR;

vector<const GCNode*>* GCNode::s_moribund = 0;
unsigned int GCNode::s_num_nodes = 0;
bool GCNode::s_on_stack_bits_correct = false;
const unsigned char GCNode::s_decinc_refcount[]
= {0,    2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x1e,
   0x1e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x3e,
   0x3e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 2, 0x1e,
   0x1e, 2, 2, 6, 6, 2, 2, 0xe, 0xe, 2, 2, 6, 6, 2, 0,    0};
unsigned char GCNode::s_mark = 0;

#ifdef HAVE_ADDRESS_SANITIZER
static void* asan_allocate(size_t bytes);
static void asan_free(void* object);
static const int kRedzoneSize = 16;
#else
static const int kRedzoneSize = 0;
#endif  // HAVE_ADDRESS_SANITIZER

static void* offset(void* p, size_t bytes) {
    return static_cast<char*>(p) + bytes;
}

static void* get_object_pointer_from_allocation(void* allocation) {
    return offset(allocation, kRedzoneSize);
}

static void* get_allocation_from_object_pointer(void* object) {
    return offset(object, -kRedzoneSize);
}

// We repurpose the BDW collector's mark bit as an allocated flag.  This
// works since the mark-sweep functionality of the BDW GC isn't used at all.
static void set_allocated_bit(void* allocation) {
    GC_set_mark_bit(allocation);
}

static void clear_allocated_bit(void* allocation) {
    GC_clear_mark_bit(allocation);
}

static bool test_allocated_bit(void* allocation) {
    return GC_is_marked(allocation);
}

HOT_FUNCTION void* GCNode::operator new(size_t bytes)
{
    GCManager::maybeGC();
    MemoryBank::notifyAllocation(bytes);
    void *result;

#ifdef HAVE_ADDRESS_SANITIZER
    result = asan_allocate(bytes);
#else
    result = GC_malloc_atomic(bytes);
    set_allocated_bit(result);
#endif

    // Because garbage collection may occur between this point and the GCNode's
    // constructor running, we need to ensure that this space is at least
    // minimally initialized.
    // We construct a GCNode with a ficticious reference count of 1, to prevent
    // it from getting marked as moribund or garbage collected (the mark-sweep
    // GC uses the reference counts, so this is always effective).
    // It will be overwritten by the real constructor.
    new (result)GCNode(static_cast<CreateAMinimallyInitializedGCNode*>(nullptr));
    return result;
}

GCNode::GCNode(CreateAMinimallyInitializedGCNode*) : m_rcmms(0) {
    incRefCount(this);
}

void GCNode::operator delete(void* p, size_t bytes)
{
    MemoryBank::notifyDeallocation(bytes);
#ifdef HAVE_ADDRESS_SANITIZER
    asan_free(p);
#else
    clear_allocated_bit(p);
    GC_free(p);
#endif
}

bool GCNode::check()
{
    // Check moribund list:
    for (const GCNode* node: *s_moribund) {
	if (!(node->m_rcmms & s_moribund_mask)) {
	    cerr << "GCNode::check() : "
		"Node on moribund list without moribund bit set.\n";
	    abort();
	}
    }

    return true;
}

void GCNode::destruct_aux()
{
    // Erase this node from the moribund list:
    typedef std::vector<const GCNode*>::iterator Iter;
    Iter it = std::find(s_moribund->begin(), s_moribund->end(), this);
    if (it == s_moribund->end())
	abort();  // Should never happen!
    s_moribund->erase(it);
}
    
void GCNode::gc(bool markSweep)
{
    if (GCManager::GCInhibitor::active())
	return;
    GCManager::GCInhibitor inhibitor;

    ProtectStack::protectAll();
    ByteCode::protectAll();
    if (markSweep) {
	GCStackRootBase::withAllStackNodesProtected(markSweepGC);
    } else {
	GCStackRootBase::withAllStackNodesProtected(gclite);
    }
}

void GCNode::markSweepGC()
{
    // NB: setting this flag implies that the garbage collection will ignore
    // any new stack nodes.  To ensure correctness, this function must not call
    // any code that depends on normal operation of the garbage collector.
    s_on_stack_bits_correct = true;

    mark();
    sweep();

    s_on_stack_bits_correct = false;
}

void GCNode::gclite()
{
    s_on_stack_bits_correct = true;

    while (!s_moribund->empty()) {
	// Last in, first out, for cache efficiency:
	const GCNode* node = s_moribund->back();
	s_moribund->pop_back();
	// Clear moribund bit.  Beware ~ promotes to unsigned int.
	node->m_rcmms &= static_cast<unsigned char>(~s_moribund_mask);

	if (node->maybeGarbage())
	    delete node;
    }
    s_on_stack_bits_correct = false;
}

void GCNode::initialize()
{
    s_moribund = new vector<const GCNode*>();

    // Initialize the Boehm GC.
    GC_set_all_interior_pointers(1);
    GC_set_dont_precollect(1);
    GC_set_no_dls(1);
    GC_set_pages_executable(0);

    GC_INIT();

    // We do our own garbage collection, so disable the Boehm GC's collector.
    // The collector library is used to do allocations, deallocations, find
    // stack roots and walk the heap in the mark/sweep cleanup, but no actual
    // garbage collection.
    GC_disable();
}

void GCNode::makeMoribund() const
{
    if (s_on_stack_bits_correct) {
	// In this case, the node can be deleted immediately.
	delete this;
    } else {
	m_rcmms |= s_moribund_mask;
	s_moribund->push_back(this);
    }
}

void GCNode::mark()
{
    // In the first mark-sweep collection, the marking of a node is
    // indicated by the mark bit being set; in the second mark sweep,
    // marking is indicated by the bit being clear, and so on in
    // alternation.  This avoids the need for the sweep phase to
    // iterate through the surviving nodes simply to remove marks.
    s_mark ^= s_mark_mask;
    GCNode::Marker marker;
    GCRootBase::visitRoots(&marker);
    GCStackRootBase::visitRoots(&marker);
    ProtectStack::visitRoots(&marker);
    ByteCode::visitRoots(&marker);
    WeakRef::markThru();
}

static GCNode* getNodePointerFromAllocation(void* allocation)
{
    return reinterpret_cast<GCNode*>(
	get_object_pointer_from_allocation(allocation));
}

void GCNode::detachReferentsOfObjectIfUnmarked(GCNode* object,
					       vector<GCNode*> *unmarked_and_saturated)
{
    if (!object->isMarked()) {
	int ref_count = object->getRefCount();
	incRefCount(object);
	if (object->getRefCount() == ref_count) {
	    // The reference count has saturated.
	    object->detachReferents();
	    unmarked_and_saturated->push_back(object);
	} else {
	    object->detachReferents();
	    decRefCount(object);
	}
    }
}

void GCNode::sweep()
{
    // Detach the referents of nodes that haven't been marked.
    // Once this is done, all of the nodes in the cycle will be unreferenced
    // and they will have been deleted unless their reference count is
    // saturated.
    vector<GCNode*> unmarked_and_saturated;
    applyToAllAllocatedNodes([&](GCNode* node) {
	    detachReferentsOfObjectIfUnmarked(node, &unmarked_and_saturated);
	});
    // At this point, the only unmarked objects are GCNodes with saturated
    // reference counts.  Delete them.
    for (GCNode* node : unmarked_and_saturated) {
	delete node;
    }
}

static void applyToAllAllocatedNodesInBlock(struct hblk* block, GC_word fn)
{
    auto function = reinterpret_cast<std::function<void(GCNode*)>*>(fn);
    hdr* block_header = HDR(block);
    size_t object_size = block_header->hb_sz;
    char *start = block->hb_body;
    char* end = start + HBLKSIZE - object_size + 1;
    
    for (char* allocation = start; allocation < end; allocation += object_size)
    {
	if (test_allocated_bit(allocation)) {
	    (*function)(getNodePointerFromAllocation(allocation));
	}
    }
}

void GCNode::applyToAllAllocatedNodes(std::function<void(GCNode*)> f)
{
    GC_apply_to_all_blocks(
	applyToAllAllocatedNodesInBlock, reinterpret_cast<GC_word>(&f));
}


void GCNode::Marker::operator()(const GCNode* node)
{
    if (node->isMarked()) {
	return;
    }
    // Update mark  Beware ~ promotes to unsigned int.
    node->m_rcmms &= static_cast<unsigned char>(~s_mark_mask);
    node->m_rcmms |= s_mark;
    ++m_marks_applied;
    node->visitReferents(this);
}

void CXXR::initializeMemorySubsystem()
{
    static bool initialized = false;
    if (!initialized) {
	MemoryBank::initialize();
	GCNode::initialize();
	ProtectStack::initialize();
	RAllocStack::initialize();

	initialized = true;
    }
}


GCNode* GCNode::asGCNode(void* candidate_pointer)
{
    if (candidate_pointer < GC_least_plausible_heap_addr
	|| candidate_pointer > GC_greatest_plausible_heap_addr)
	return nullptr;

    void* base_pointer = GC_base(candidate_pointer);
    if (base_pointer && test_allocated_bit(base_pointer)) {
	return getNodePointerFromAllocation(base_pointer);
    }
    return nullptr;
}


#ifdef HAVE_ADDRESS_SANITIZER

/*
 * When compiling with the address sanitizer, we modify the allocation routines
 * to detect errors more reliably.  In particular:
 * - A redzone is added on both sides of the object to detect underflows and
 *   overflows.
 * - Memory is poisoned when it is freed to detect use-after-free errors.
 * - Freed objects are held in a quarantine to prevent the memory from being
 *   reallocated quickly.  This helps detect use-after-free errors.
 *
 * Note that in the context of GC, 'use after free' is really premature garbage
 * collection.
 */

static void* asan_allocate(size_t bytes)
{
    size_t allocation_size = bytes + 2 * kRedzoneSize;
    void* allocation = GC_malloc_atomic(allocation_size);
    set_allocated_bit(allocation);

    void *start_redzone = allocation;
    void* storage = offset(allocation, kRedzoneSize);
    void* end_redzone = offset(storage, bytes);

    ASAN_UNPOISON_MEMORY_REGION(allocation, allocation_size);

    // Store the allocation size for later use.
    *static_cast<size_t*>(allocation) = allocation_size;

    // Poison the redzones
    ASAN_POISON_MEMORY_REGION(start_redzone, kRedzoneSize);
    ASAN_POISON_MEMORY_REGION(end_redzone, kRedzoneSize);
    
    return storage;
}

static void defered_free(void* allocation, size_t allocation_size)
{
    // Rather than immediately freeing the pointer, hold it in a queue for a
    // while.  This allows use-after-free errors to be detected.
    static const int kMaxQuarantineSize = 10 * 1024 * 1024; // 10MB

    static std::deque<void*> quarantine;
    static size_t quarantine_size = 0;

    quarantine.push_back(allocation);
    quarantine_size += allocation_size;

    while (quarantine_size > kMaxQuarantineSize) {
	allocation = quarantine.front();
	quarantine.pop_front();

	// We need to unpoison the memory now, as the allocator may choose to reuse it in
	// whatever way it deems fit. Any following use-after-free errors will not be caught.
	ASAN_UNPOISON_MEMORY_REGION(allocation, sizeof(size_t));
	allocation_size = *static_cast<size_t*>(allocation);
	ASAN_UNPOISON_MEMORY_REGION(allocation, allocation_size);

	quarantine_size -= allocation_size;
	GC_free(allocation);
    }
}

static void asan_free(void* object) {
    void* allocation = get_allocation_from_object_pointer(object);
    clear_allocated_bit(allocation);

    void* start_redzone = allocation;
    ASAN_UNPOISON_MEMORY_REGION(start_redzone, kRedzoneSize);
    size_t allocation_size = *static_cast<size_t*>(allocation);

    // Poison the entire region.
    ASAN_POISON_MEMORY_REGION(allocation, allocation_size);

    defered_free(allocation, allocation_size);
}

#endif  // HAVE_ADDRESS_SANITIZER
