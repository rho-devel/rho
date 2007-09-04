/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2007
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file GCNode.h
 * Class GCNode and associated C-callable routines.
 */

#ifndef GCNODE_H
#define GCNODE_H

#ifdef __cplusplus

//#include <cstddef>
//#include <cstring>

#include "CXXR/Heap.hpp"

namespace CXXR {
    struct GCNode {
	GCNode();

	/** Allocate memory.
         *
	 * Allocates memory for a new object of a class derived from
	 * GCNode, and zero the memory thus allocated.
	 *
	 * @param bytes Number of bytes of memory required.
	 *
	 * @note Since objects of classes derived from RObject \e must
	 * be allocated on the heap, constructors of these classes may
	 * rely on the fact that operator new zeroes the allocated
	 * memory to elide member initializations.
	 */
	static void* operator new(size_t bytes)
	{
	    return memset(Heap::allocate(bytes), 0, bytes);
	}

	static void operator delete(void* p, size_t bytes)
	{
	    Heap::deallocate(p, bytes);
	}

	/** Delete a GCNode
	 *
	 * @note Because the class destructors are not public, objects
	 * of classes derived from GCNode must be deleted by calling
	 * this method.
	 */
	void destroy() const {delete this;}

        // To be protected in future:

	/** Destructor
	 *
	 * @note The destructor is protected to ensure that GCNodes
	 * are allocated on the heap.  (See Meyers 'More Effective
	 * C++' Item 27.) Derived classes should likewise declare
	 * their constructors private or protected.
	 */
	virtual ~GCNode();

	// To be private in future:

	mutable const GCNode *gengc_prev_node, *gengc_next_node;
	mutable unsigned int m_gcgen : 2;
	mutable bool m_marked        : 1;

	// Special constructor for pegs.  The parameter is simply to
	// give this constructor a distinct signature. Note that the
	// node count isn't altered.
	explicit GCNode(int /*ignored*/)
	    : gengc_prev_node(this), gengc_next_node(this)
	{}

	// Make t the successor of s:
	static void link(const GCNode* s, const GCNode* t)
	{
	    s->gengc_next_node = t;
	    t->gengc_prev_node = s;
	}
    };
}  // namespace CXXR

// Remember extern "C" for any C stuff.

#endif /* __cplusplus */

#endif /* GCNODE_H */
