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

#include "CXXR/Heap.hpp"

namespace CXXR {
    struct GCNode {
	struct const_visitor {
	    virtual ~const_visitor() {}

	    /** Perform visit
	     *
	     * @param node Node to be visited.
	     *
	     * @return true if the visitor wishes to visit the
	     * children of this node, otherwise false.
	     *
	     * @note The const in the name refers to the fact that the
	     * visitor does not modify the node it visits (or modifies
	     * only mutable fields).  There is currently no provision
	     * for the visitor object itself to be be considered const
	     * during a visit.
	     */
	    virtual bool operator()(const GCNode* node) = 0;
	};

	struct visitor {
	    virtual ~visitor() {}

	    /** Perform visit
	     *
	     * @param node Node to be visited.
	     *
	     * @return true if the visitor wishes to visit the
	     * children of this node, otherwise false.
	     */
	    virtual bool operator()(GCNode* node) = 0;
	};

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

	/** Present this node to a visitor and, if the visitor so
	 * wishes, conduct the visitor to the children of this node.
	 * 
	 * @param v Pointer to the visitor object.
	 */
	void conductVisitor(const_visitor* v) const
	{
	    if ((*v)(this)) visitChildren(v);
	}

	/** Present this node to a visitor and, if the visitor so
	 * wishes, conduct the visitor to the children of this node.
	 * 
	 * @param v Pointer to the visitor object.
	 */
	void conductVisitor(visitor* v)
	{
	    if ((*v)(this)) visitChildren(v);
	}

	/** Delete a GCNode
	 *
	 * @note Because the class destructors are not public, objects
	 * of classes derived from GCNode must be deleted by calling
	 * this method.
	 */
	void destroy() const {delete this;}

	/** Initialize static members.
	 *
	 * This method must be called before any GCNodes are created.
	 * If called more than once in a single program run, the
	 * second and subsequent calls do nothing.
	 */
	static void initialize();

	/**
	 * @return the number of GCNodes currently in existence.
	 */
	static unsigned int numNodes() {return s_num_nodes;}

        // To be protected in future:

	/** Destructor
	 *
	 * @note The destructor is protected to ensure that GCNodes
	 * are allocated on the heap.  (See Meyers 'More Effective
	 * C++' Item 27.) Derived classes should likewise declare
	 * their constructors private or protected.
	 */
	virtual ~GCNode();

	/** Conduct a visitor to the children of this node.
	 *
	 * @param Pointer to the visitor object.
	 */
	virtual void visitChildren(const_visitor* v) const {}

	/** Conduct a visitor to the children of this node.
	 *
	 * @param Pointer to the visitor object.
	 */
	virtual void visitChildren(visitor* v) {}

	// To be private in future:

	static const unsigned int s_num_old_generations = 2;
	static GCNode* s_oldpeg[s_num_old_generations];
	static unsigned int s_oldcount[s_num_old_generations];
#ifndef EXPEL_OLD_TO_NEW
	static GCNode* s_old_to_new_peg[s_num_old_generations];
#endif
	static GCNode* s_newpeg;
	static unsigned int s_num_nodes;

	mutable const GCNode *gengc_prev_node, *gengc_next_node;
	mutable unsigned int m_gcgen : 2;
	mutable bool m_marked        : 1;

	// Special constructor for pegs.  The parameter is simply to
	// give this constructor a distinct signature. Note that the
	// node count isn't altered.
	explicit GCNode(int /*ignored*/)
	    : gengc_prev_node(this), gengc_next_node(this)
	{}

	bool isMarked() const {return m_marked;}

	// Make t the successor of s:
	static void link(const GCNode* s, const GCNode* t)
	{
	    s->gengc_next_node = t;
	    t->gengc_prev_node = s;
	}

	void mark() const {m_marked = true;}

	const GCNode* next() const {return gengc_next_node;}

	const GCNode* prev() const {return gengc_prev_node;}

	/** Transfer a node so as to precede this node.
	 * 
	 * @param n Pointer to node to be moved.
	 */
	void splice(const GCNode* n) const
	{
	    // Doing things in this order is innocuous if n is already
	    // this node's predecessor:
	    link(n->prev(), n->next());
	    link(prev(), n);
	    link(n, this);
	}

	void unmark() const {m_marked = false;}
    };
}  // namespace CXXR

// Remember extern "C" for any C stuff.

#endif /* __cplusplus */

#endif /* GCNODE_H */
