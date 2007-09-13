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

/** @file GCNode.hpp
 * Class GCNode.
 */

#ifndef GCNODE_HPP
#define GCNODE_HPP

#include "CXXR/Heap.hpp"

/* Comment formerly in memory.c:

   The Heap Structure.  Nodes for each generation are arranged in
   circular doubly-linked lists.  The double linking allows nodes to
   be removed in constant time; this is used by the collector to move
   reachable nodes out of free space and into the appropriate
   generation.  The circularity eliminates the need for end checks.
   In addition, each link is anchored at an artificial node called a
   peg, which simplifies pointer maintenance.  The circular
   doubly-linked arrangement is taken from Baker's in-place
   incremental collector design; see
   ftp://ftp.netcom.com/pub/hb/hbaker/NoMotionGC.html or the Jones and
   Lins GC book.  The linked lists are implemented by adding two
   pointer fields to the SEXPREC structure, which increases its size
   from 5 to 7 words. Other approaches are possible but don't seem
   worth pursuing for R.

   There are two options for dealing with old-to-new pointers.  The
   first option is to make sure they never occur by transferring all
   referenced younger objects to the generation of the referrer when a
   reference to a newer object is assigned to an older one.  This is
   enabled by defining EXPEL_OLD_TO_NEW.  The second alternative is to
   keep track of all nodes that may contain references to newer nodes
   and to "age" the nodes they refer to at the beginning of each
   collection.  This is the default.  The first option is simpler in
   some ways, but will create more floating garbage and add a bit to
   the execution time, though the difference is probably marginal on
   both counts.*/

namespace CXXR {
    /** Abstract base class for all objects managed by the garbage collector.
     * 
     * @note Because this base class is used purely for housekeeping
     * by the garbage collector, and does not contribute to the
     * 'meaning' of an object of a derived class, all of its data
     * members are mutable.
     */
    struct GCNode {
	/** Abstract base class for the Visitor design pattern.
	 *
	 * See Gamma et al 'Design Patterns' Ch. 5 for a description
	 * of the Visitor design pattern.
	 *
	 * The const in the name refers to the fact that the visitor
	 * does not modify the node it visits (or modifies only
	 * mutable fields).  There is currently no provision for the
	 * visitor object itself to be be considered const during a
	 * visit.
	 */
	struct const_visitor {
	    virtual ~const_visitor() {}

	    /** Perform visit
	     *
	     * @param node Node to be visited.
	     *
	     * @return true if the visitor wishes to visit the
	     * children of this node, otherwise false.
	     */
	    virtual bool operator()(const GCNode* node) = 0;
	};

	/** Abstract base class for the Visitor design pattern.
	 *
	 * See Gamma et al 'Design Patterns' Ch. 5 for a description
	 * of the Visitor design pattern.
	 */
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
	 *
	 * @return the result of applying the visitor to \e this node.
	 */
	bool conductVisitor(const_visitor* v) const
	{
	    if (!(*v)(this)) return false;
	    visitChildren(v);
	    return true;
	}

	/** Present this node to a visitor and, if the visitor so
	 * wishes, conduct the visitor to the children of this node.
	 * 
	 * @param v Pointer to the visitor object.
	 *
	 * @return the result of applying the visitor to \e this node.
	 */
	bool conductVisitor(visitor* v)
	{
	    if (!(*v)(this)) return false;
	    visitChildren(v);
	    return true;
	}

	/** Delete a GCNode
	 *
	 * @note Because the class destructors are not public, objects
	 * of classes derived from GCNode must be deleted by calling
	 * this method.
	 */
	void destroy() const {delete this;}

	/** Initiate a garbage collection.
	 *
	 * @param num_old_gens The number of old generations to collect.
	 */
	static void gc(unsigned int num_old_gens);

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

	static const unsigned int s_num_old_generations = 2;
	static GCNode* s_oldpeg[s_num_old_generations];
	static unsigned int s_oldcount[s_num_old_generations];
#ifndef EXPEL_OLD_TO_NEW
	static GCNode* s_old_to_new_peg[s_num_old_generations];
#endif
	static GCNode* s_newpeg;
	static unsigned int s_num_nodes;

	mutable const GCNode *m_prev, *m_next;
	mutable unsigned int m_gcgen : 2;
	mutable bool m_marked        : 1;

	// Special constructor for pegs.  The parameter is simply to
	// give this constructor a distinct signature. Note that the
	// node count isn't altered.
	explicit GCNode(int /*ignored*/)
	    : m_prev(this), m_next(this)
	{}

	bool isMarked() const {return m_marked;}

	// Make t the successor of s:
	static void link(const GCNode* s, const GCNode* t)
	{
	    s->m_next = t;
	    t->m_prev = s;
	}

	void mark() const {m_marked = true;}

	const GCNode* next() const {return m_next;}

	const GCNode* prev() const {return m_prev;}

	/** Transfer a node so as to precede this node.
	 * 
	 * @param n Pointer to node to be moved, which may be in the
	 * same (circularly linked) list as '*this', or in a different
	 * list.  It is permissible for n to point to what is already
	 * the predecessor of '*this', in which case the function
	 * amounts to a no-op.  It is also permissible for n to point
	 * to '*this' itself; beware however that in that case the
	 * function will detach '*this' from its current list, and turn
	 * it into a singleton list.
	 */
	void splice(const GCNode* n) const
	{
	    // Doing things in this order is innocuous if n is already
	    // this node's predecessor:
	    link(n->prev(), n->next());
	    link(prev(), n);
	    link(n, this);
	}

	/** Transfer a sublist so as to precede this node.
	 *
	 * @param beg Pointer to the first node in the sublist to be
	 * moved.  The sublist may be a sublist of the same (circularly
	 * linked) list of which '*this' forms a part, or of another
	 * list.  Note however that in the former case, the sublist to
	 * be moved must not contain '*this'.
	 *
	 * @param end Pointer to the successor of the last node of the
	 * sublist to be moved.  It is permissible for it be identical
	 * to beg, or to point to '*this': in either case the function
	 * amounts to a no-op.
	 */
	void splice(const GCNode* beg, const GCNode* end) const
	{
	    if (beg != end) {
		const GCNode* last = end->prev();
		link(beg->prev(), end);
		link(prev(), beg);
		link(last, this);
	    }
	}

	void unmark() const {m_marked = false;}
    };
}  // namespace CXXR

#endif /* GCNODE_HPP */
