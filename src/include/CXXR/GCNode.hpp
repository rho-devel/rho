/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2007-8
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file GCNode.hpp
 * @brief Class CXXR::GCNode.
 */

#ifndef GCNODE_HPP
#define GCNODE_HPP

#include <vector>
#include "CXXR/Heap.hpp"

#define EXPEL_OLD_TO_NEW

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
*/

namespace CXXR {
    /** @brief Base class for objects managed by the garbage collector.
     *
     * Abstract base class for all objects managed by the garbage
     * collector.
     *
     * \par Derived class checklist:
     * Classes derived directly or indirectly from GCNode should do
     * the following to ensure the integrity of the garbage collection
     * scheme:
     * <ol>
     * <li>Explicitly declare a destructor (even if it does nothing)
     * as either protected or (if no further derivation from the class
     * is envisaged) private.  This ensures that objects of classes
     * derived from GCNode can be created only using 'new'.</li>
     *
     * <li>If the derived class contains any pointers or references to
     * other objects derived from GCNode, it should reimplement the
     * methods visitChildren() appropriately.</li>
     *
     * <li>If the derived class contains any pointers to other objects
     * derived from GCNode, then any post-construction operation that
     * modifies the pointer must invoke the method devolveAge() with the
     * new destination of the pointer as its argument, to ensure that
     * no old-to-new references arise.  There is no need to do this
     * during the operation of a constructor for the derived class,
     * because then the object under construction will necessarily be
     * newer than anything to which it refers.</li>
     * </ol>
     *
     * \par Infant immunity:
     * While a GCNode or an object of a class derived from GCNode is
     * under construction, it is effectively immune from the garbage
     * collector.  Not only does this greatly simplify the coding of
     * the constructors themselves, it also means that in implementing
     * the virtual methods visitChildren(), it is not necessary to
     * consider the possibility that the garbage collector will invoke
     * this method for a node whose construction is not yet complete.
     *
     * \par
     * The private method expose() is used to end this immunity once
     * construction of an object is complete.  However, there appears
     * to be no clean and general way in C++ of calling expose() \e
     * exactly when construction is complete.  Consequently, a node's
     * infant immunity will in fact continue until one of the
     * following events occurs:
     * <ul>
     * <li>The node is explicitly protected from the garbage
     * collector, either by encapsulating a pointer to it in a
     * GCRoot, or by the CR PROTECT() mechanism.  For this reason, it
     * is important that constructors <em>do not</em> attempt
     * explicitly to protect '<tt>this</tt>'; a particular risk with
     * this is that constructors of derived classes will not be able
     * to rely on infant immunity.</li>
     *
     * <li>The node is marked by the garbage collector.</li>
     *
     * <li>The node is visited by a \b GCNode::Ager object (as part of
     * write barrier enforcement).</li>
     * </ul>
     * 
     * @note Because this base class is used purely for housekeeping
     * by the garbage collector, and does not contribute to the
     * 'meaning' of an object of a derived class, all of its data
     * members are mutable.
     */
    class GCNode {
    public:
	/** @brief Abstract base class for the Visitor design pattern.
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

	    /** @brief Perform visit
	     *
	     * @param node Node to be visited.
	     *
	     * @return true if the visitor wishes to visit the
	     * children of this node, otherwise false.
	     */
	    virtual bool operator()(const GCNode* node) = 0;
	};

	/** @brief Abstract base class for the Visitor design pattern.
	 *
	 * See Gamma et al 'Design Patterns' Ch. 5 for a description
	 * of the Visitor design pattern.
	 */
	struct visitor {
	    virtual ~visitor() {}

	    /** @brief Perform visit
	     *
	     * @param node Node to be visited.
	     *
	     * @return true if the visitor wishes to visit the
	     * children of this node, otherwise false.
	     */
	    virtual bool operator()(GCNode* node) = 0;
	};

	GCNode()
	{
	    ++s_num_nodes;
	}

	/** @brief Allocate memory.
         *
	 * Allocates memory for a new object of a class derived from
	 * GCNode, and zero the memory thus allocated.
	 *
	 * @param bytes Number of bytes of memory required.
	 *
	 * @return Pointer to the allocated memory block.
	 *
	 * @note Since objects of classes derived from RObject \e must
	 * be allocated on the heap, constructors of these classes may
	 * elide some member initializations by relying on the fact
	 * that operator new zeroes the allocated memory.
	 */
	static void* operator new(size_t bytes)
	{
	    return memset(Heap::allocate(bytes), 0, bytes);
	}

	/** @brief Deallocate memory
	 *
	 * Deallocate memory previously allocated by operator new.
	 *
	 * @param p Pointer to the allocated memory block.
	 *
	 * @param bytes Size in bytes of the memory block, as
	 * requested when the block was allocated.
	 */
	static void operator delete(void* p, size_t bytes)
	{
	    Heap::deallocate(p, bytes);
	}

	/** @brief Integrity check.
	 *
	 * Aborts the program with an error message if the class is
	 * found to be internally inconsistent.
	 *
	 * @return true, if it returns at all.  The return value is to
	 * facilitate use with \c assert.
	 */
	static bool check();

	/** @brief Present this node, and maybe its children, to a
	 * visitor.
	 *
	 * Present this node to a visitor and, if the visitor so
	 * requests, conduct the visitor to the children of this node.
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

	/** @brief Present this node, and maybe its children, to a
	 * visitor.
	 *
	 * Present this node to a visitor and, if the visitor so
	 * requests, conduct the visitor to the children of this node.
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

	/** @brief Prevent old-to-new references.
	 * 
	 * If \a node points to a node of a younger generation than
	 * this node, then raise it to this node's generation, and
	 * propagate this change to the nodes to which \a node refers,
	 * and so on recursively.
	 * @param node Pointer to the node whose generation is to be
	 *          adjusted if necessary.  If this is a null pointer,
	 *          the method does nothing.
	 * @todo Make this protected once CHECK_OLD_TO_NEW in
	 * memory.cpp is no longer required.
	 */
	void devolveAge(const GCNode* node);

	/** @brief Initiate a garbage collection.
	 *
	 * @param num_old_gens The number of old generations to collect.
	 */
	static void gc(unsigned int num_old_gens);

	/** @brief Initialize static members.
	 *
	 * This method must be called before any GCNodes are created.
	 * If called more than once in a single program run, the
	 * second and subsequent calls do nothing.
	 *
	 * @param num_old_generations One fewer than the number of
	 * generations into which GCNode objects are to be ranked.
	 *
	 * @todo Have this method called automatically by a Schwarz
	 * counter, and make it private.
	 */
	static void initialize(unsigned int num_old_generations);

	/** @brief Number of generations used by garbage collector.
	 *
	 * @return The number of generations into which GCNode objects
	 * are ranked by the garbage collector.
	 */
	static size_t numGenerations() {return s_genpeg.size();}

	/** @brief Number of GCNode objects in existence.
	 *
	 * @return the number of GCNode objects currently in
	 * existence.
	 */
	static size_t numNodes() {return s_num_nodes;}

	/** @brief Conduct a visitor to the children of this node.
	 *
	 * @param v Pointer to the visitor object.
	 * @note If this method is reimplemented in a derived class,
	 * the reimplemented version must remember to invoke
	 * visitChildren for the immediate base class of the derived
	 * class, to ensure that \e all children of the object get
	 * visited.
	 */
	virtual void visitChildren(const_visitor* v) const {}

	/** @brief Conduct a visitor to the children of this node.
	 *
	 * @param v Pointer to the visitor object.
	 * @note If this method is reimplemented in a derived class,
	 * the reimplemented version must remember to invoke
	 * visitChildren for the immediate base class of the derived
	 * class, to ensure that \e all children of the object get
	 * visited.
	 */
	virtual void visitChildren(visitor* v) {}
    protected:
	/**
	 * @note The destructor is protected to ensure that GCNode
	 * objects are allocated on the heap.  (See Meyers 'More
	 * Effective C++' Item 27.) Derived classes should likewise
	 * declare their destructors private or protected.
	 */
	virtual ~GCNode();
    private:
	friend class WeakRef;
	friend class GCRootBase;
	template <class T> friend class GCEdge;

	/** Visitor class used to impose a minimum generation number.
	 *
	 * This visitory class is used to ensure that a node and its
	 * descendants all have generation numbers that exceed a
	 * specified minimum value, and is used in implementing the
	 * write barrier in the generational garbage collector.
	 */
	class Ager : public const_visitor {
	public:
	    /**
	     * @param min_gen The minimum generation number that the
	     * visitor is to apply.
	     */
	    Ager(unsigned int min_gen)
		: m_mingen(min_gen)
	    {}

	    // Virtual function of const_visitor:
	    bool operator()(const GCNode* node);
	private:
	    unsigned int m_mingen;
	};

	/** Visitor class used to mark nodes.
	 *
	 * This visitor class is used during the mark phase of garbage
	 * collection to ensure that a node and its descendants are
	 * marked.  However, nodes with generation numbers exceeding a
	 * specified level are left unmarked.  It is assumed that no
	 * node references a node with a younger generation number.
	 */
	class Marker : public const_visitor {
	public:
	    /**
	     * @param max_gen Nodes with a generation number exceeding
	     *          this are not to be marked.
	     */
	    Marker(unsigned int max_gen)
		: m_maxgen(max_gen)
	    {}
	    
	    // Virtual function of const_visitor:
	    bool operator()(const GCNode* node);
	private:
	    unsigned int m_maxgen;
	};

	/** Visitor class used to abort the program if old-to-new
	 * references are found.
	 */
	class OldToNewChecker : public const_visitor {
	public:
	    /**
	     * @param min_gen The minimum generation number that is
	     * acceptable in visited nodes.
	     */
	    OldToNewChecker(unsigned int min_gen)
		: m_mingen(min_gen)
	    {}

	    // Virtual function of const_visitor:
	    bool operator()(const GCNode* node);
	private:
	    unsigned int m_mingen;
	};

	static unsigned int s_last_gen;
	static std::vector<const GCNode*> s_genpeg;
	static std::vector<unsigned int> s_gencount;
	static size_t s_num_nodes;

	mutable const GCNode *m_prev, *m_next;
	mutable unsigned int m_gcgen : 2;
	mutable bool m_marked        : 1;

	// Special constructor for pegs.  The parameter is simply to
	// give this constructor a distinct signature. Note that the
	// node count isn't altered.
	explicit GCNode(int /*ignored*/)
	    : m_prev(this), m_next(this)
	{}

	// Make the node known to the garbage collector (if it isn't
	// already).
	void expose() const
	{
	    if (!m_prev) expose_aux();
	}

	// Does the business for expose():
	void expose_aux() const;

	bool isMarked() const {return m_marked;}

	// Make t the successor of s:
	static void link(const GCNode* s, const GCNode* t)
	{
	    s->m_next = t;
	    t->m_prev = s;
	}

	void mark() const
	{
	    expose();
	    m_marked = true;
	}

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
