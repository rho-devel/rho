/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

#include "CXXR/MemoryBank.hpp"

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
     * method visitChildren() appropriately.</li>
     *
     * <li>If the derived class contains any pointers to other objects
     * derived from GCNode, then any post-construction operation that
     * modifies the pointer must invoke the method propagateAge() with
     * the new destination of the pointer as its argument, to ensure
     * that no old-to-new references arise.  There is no need to do
     * this during the operation of a constructor for the derived
     * class, because then the object under construction will
     * necessarily be newer than anything to which it refers.</li>
     * </ol>
     *
     * \par Infant immunity:
     * While a GCNode or an object of a class derived from GCNode is
     * under construction, it is effectively immune from the garbage
     * collector.  Not only does this greatly simplify the coding of
     * the constructors themselves, it also means that in implementing
     * the virtual method visitChildren(), it is not necessary to
     * consider the possibility that the garbage collector will invoke
     * this method for a node whose construction is not yet complete.
     *
     * \par
     * The method expose() is used to end this immunity once
     * construction of an object is complete.  However, there appears
     * to be no clean and general way in C++ of calling expose() \e
     * exactly when construction is complete.  Consequently, a node
     * N's infant immunity will in fact continue until one of the
     * following events occurs:
     *
     * <ul>
     *
     * <li>The method expose() is called explicitly for N, or for a
     * node that refers to N (and so on recursively).</li>
     *
     * <li>N is visited by a \b GCNode::Ager object (as part of write
     * barrier enforcement).  This will happen if a node that is
     * already exposed to the garbage collector is modified so that it
     * refers to N (or a node that refers to N, and so on
     * recursively).</li>
     *
     * <li>A pointer to N (or a node that refers to N, and so on
     * recursively) is specified in the constructor of a GCRoot
     * object, and the optional argument \a expose is set to
     * true.</li>
     *
     * <li>N is designated as the key, value or R finalizer of a weak
     * reference object.</li>
     *
     * <li>N is itself a weak reference object (in which case it is
     * exposed to garbage collection during construction).</li>
     *
     * </ul>
     * 
     * \par
     * It is the responsibility of any code that creates an object of
     * a class derived from GCNode to ensure that, under normal
     * operation, the object is in due course exposed to the garbage
     * collector.  However, if exceptions occur, the static member
     * function slaughterInfants() can be used to delete \e all the
     * nodes currently enjoying infant immunity.
     *
     * \par Nested <tt>new</tt>:
     * Consider the following code to create a PairList of two
     * elements, with \c first as the 'car' of the first element and
     * \c second as the 'car' of the second element:
     * \code
     * CXXR::GCRoot<CXXR::PairList>
     *   pl2(new CXXR::PairList(first, new CXXR::PairList(second)));
     * \endcode
     * Is this code sound?  You might suppose that there is a risk
     * that the second element of the list will be garbage-collected
     * when \c new is invoked to allocate space for the first element.
     * But this is not so: at this stage, the second element will
     * still enjoy infant immunity.
     *
     * \par
     * However, a potential problem arises from a different quarter.
     * Suppose that the \c new to allocate space for the second
     * element succeeds, but that the \c new to allocate space for the
     * first element fails because of shortage of memory, and throws
     * <tt>std::bad_alloc</tt>.  Then the second element will not be
     * exposed to the garbage collector, and the space it occupies is
     * potentially lost.  An easy workaround is for the handler that
     * catches the exception to invoke slaughterInfants().
     *
     * @note Because this base class is used purely for housekeeping
     * by the garbage collector, and does not contribute to the
     * 'meaning' of an object of a derived class, all of its data
     * members are mutable.
     *
     * @todo The (private) cleanup() method needs to address the
     * possibility that derived classes may have destructors that
     * release some external resource (e.g. a lock).  Maybe a garbage
     * collection without a 'mark' phase would do the trick.
     */
    class GCNode {
    public:
	/** @brief Schwarz counter for managing static data associated
	 * with garbage collection.
	 *
	 * (See MemoryBank::SchwarzCtr for a description of how
	 * Schwarz counters work.)
	 */
	class SchwarzCtr {
	public:
	    SchwarzCtr();

	    ~SchwarzCtr();
	private:
	    static unsigned int s_count;
	};

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

	GCNode()
	    : m_next(s_generation[0]), m_gcgen(0), m_marked(false),
	      m_aged(false)
	{
	    s_generation[0] = this;
	    ++s_gencount[0];
	    ++s_num_nodes;
	}

	/** @brief Allocate memory.
         *
	 * Allocates memory for a new object of a class derived from
	 * GCNode.
	 *
	 * @param bytes Number of bytes of memory required.
	 *
	 * @return Pointer to the allocated memory block.
	 */
	static void* operator new(size_t bytes)
	{
	    return MemoryBank::allocate(bytes);
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
	    MemoryBank::deallocate(p, bytes);
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

	/** @brief Make node known to the garbage collector.
	 *
	 * Makes this node known to the garbage collector (if it isn't
	 * already), and so on recursively for nodes to which this
	 * node refers.
	 *
	 * @note The generation change initiated by expose() must be
	 * recursively propagated immediately, rather than having the
	 * recursive propagation deferred until the next garbage
	 * collection as is the case with propagateAge().  Otherwise,
	 * if some element A of a structure S were replaced by B, and
	 * this happened after the top-level element of S was exposed
	 * but before the next following garbage collection, then at
	 * that garbage collection propagateAges() would expose B to
	 * the garbage collector but not A.
	 */
	void expose() const
	{
	    Ager exposer(1);
	    conductVisitor(&exposer);
	}

	/** @brief Initiate a garbage collection.
	 *
	 * @param num_old_gens The number of old generations to
	 * collect.  Must be strictly smaller than numGenerations().
	 */
	static void gc(unsigned int num_old_gens);

	/** @brief Perform sanity checks on a GCNode.
	 *
	 * This function performs simple sanity checks on a GCNode,
	 * and is typically used to detect premature garbage
	 * collection.  In this regard, it is particularly effective
	 * when MemoryBank.hpp is configured to fill freed blocks with
	 * 0x55 bytes.  If the sanity check fails, the function aborts
	 * the program.
	 *
	 * @param node Either a null pointer (in which case the check
	 *          succeeds) or a pointer to the GCNode to be
	 *          checked.
	 */
	static void nodeCheck(const GCNode* node);

	/** @brief Number of generations used by garbage collector.
	 *
	 * @return The number of generations into which GCNode objects
	 * are ranked by the garbage collector.
	 */
	static size_t numGenerations() {return s_num_generations;}

	/** @brief Number of GCNode objects in existence.
	 *
	 * @return the number of GCNode objects currently in
	 * existence.
	 */
	static size_t numNodes() {return s_num_nodes;}

	/** @brief Prevent old-to-new references.
	 * 
	 * If \a node points to a node of a younger generation than
	 * this node, then raise it to this node's generation, and
	 * propagate this change to the nodes to which \a node refers,
	 * and so on recursively.
	 *
	 * @param node Pointer to the node whose generation is to be
	 *          adjusted if necessary.  If this is a null pointer,
	 *          the method does nothing.
	 *
	 * @note In practice only \a node has its generation changed
	 * as a direct result of this call; the recursive propagation
	 * of the change is deferred until the next garbage
	 * collection.
	 *
	 * @todo Make this protected once CHECK_OLD_TO_NEW in
	 * memory.cpp is no longer required.
	 */
	void propagateAge(const GCNode* node) const
	{
	    if (node) node->ageTo(m_gcgen);
	}

	/** @brief Delete all nodes with infant immunity.
	 *
	 * This function is typically called during error recovery,
	 * and deletes all nodes in Generation 0.
	 *
	 * @return The number of nodes deleted.
	 */
	static size_t slaughterInfants();

	/** @brief Conduct a visitor to the children of this node.
	 *
	 * The children of this node are those objects derived from
	 * GCNode to which this node contains a pointer or a
	 * reference.
	 *
	 * @param v Pointer to the visitor object.
	 *
	 * @note The metaphor 'children' in this function's name is
	 * rather at odds with the metaphor of garbage-collection
	 * 'generations': the children of a node will always be of a
	 * generation at least as old as the node itself!
	 *
	 * @note If this method is reimplemented in a derived class,
	 * the reimplemented version must remember to invoke
	 * visitChildren() for the immediate base class of the derived
	 * class, to ensure that \e all children of the object get
	 * visited.
	 */
	virtual void visitChildren(const_visitor* v) const {}
    protected:
	/**
	 * @note The destructor is protected to ensure that GCNode
	 * objects are allocated using 'new'.  (See Meyers 'More
	 * Effective C++' Item 27.) Derived classes should likewise
	 * declare their destructors private or protected.
	 */
	virtual ~GCNode()
	{
	    --s_num_nodes;
	    --s_gencount[m_gcgen];
	}
    private:
	friend class WeakRef;
	friend class GCRootBase;

	/** Visitor class used to impose a minimum generation number.
	 *
	 * This visitor class is used to ensure that a node and its
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

	// Class used to marshal nodes awaiting transfer to a
	// different generation.  Defined in GCNode.cpp.
	class XferList;
		
	static unsigned int s_num_generations;
	static const GCNode** s_generation;  // s_generation[g] is
				// nominally a list of the nodes in
				// generation g; however, some nodes
				// in the list may subsequently have
				// changed generation.
	static unsigned int* s_next_gen; // Look-up table used to
			       // advance generation number following
			       // a garbage collection.
	static unsigned int* s_gencount;
	static size_t s_num_nodes;

	typedef std::vector<const GCNode*> AgedList;
	static AgedList* s_aged_list; // List of nodes whose
			   // generation numbers have been changed (by
			   // setting the m_aged flag) since the last
			   // garbage collection.

	mutable const GCNode *m_next;
	mutable unsigned char m_gcgen;
	mutable bool m_marked : 1;
	mutable bool m_aged   : 1;  // true if the generation number
		       // of this node has been changed (otherwise
		       // than from 0 to 1) since the last garbage
		       // collection, and this change has not yet been
		       // propagated recursively.

	// Special constructor for pegs (i.e. dummy nodes used to
	// simplify list management).  The parameter is simply to 
	// give this constructor a distinct signature.
	explicit GCNode(int /*ignored*/);

	// Not implemented.  Declared to prevent compiler-generated
	// versions:
	GCNode(const GCNode&);
	GCNode& operator=(const GCNode&);

	// Not implemented.  Declared private to prevent clients
	// allocating arrays of GCNode.
	static void* operator new[](size_t);

	// Force the generation number of this node up to mingen, and
	// if the generation number is changed, flag up the change for
	// recursive propagation by propagateAges().
	void ageTo(unsigned int mingen) const;

	// Clean up static data at end of run:
	static void cleanup();

	/** @brief Initialize static members.
	 *
	 * This method must be called before any GCNodes are created.
	 * If called more than once in a single program run, the
	 * second and subsequent calls do nothing.
	 *
	 * @param num_old_generations One fewer than the number of
	 * generations into which GCNode objects are to be ranked.
	 */
	static void initialize();

	bool isMarked() const {return m_marked;}

	void mark() const
	{
	    m_marked = true;
	}

	const GCNode* next() const {return m_next;}

	// This is the first stage of garbage collection.  It
	// propagates the generation changes initiated by
	// propagateAge() recursively through the node graph.
	static void propagateAges();

	/** @brief Carry out the sweep phase of garbage collection.
	 *
	 * Exit conditions: (i) All nodes that on entry were within
	 * the singly-linked lists of the swept generations will have
	 * been moved, if necessary, to the singly-linked list
	 * appropriate to their generation.  (This generation is subject to
	 * further change as under (iv) below.)  (ii) all nodes within the
	 * swept generations that were not marked on entry, apart from
	 * those in Generation 0, will have been deleted.  (iii) all
	 * nodes will be unmarked.  (iv) Each node within the swept
	 * generations, other than Generation 0, will have been moved
	 * to the next higher generation, unless it was already in the
	 * highest generation.
	 *
	 * @param max_generation The highest generation number to be
	 *          swept.
	 */
	static void sweep(unsigned int max_generation);

	void unmark() const {m_marked = false;}

	friend class SchwarzCtr;
    };
}  // namespace CXXR

namespace {
    CXXR::GCNode::SchwarzCtr gcnode_schwarz_ctr;
}

#endif /* GCNODE_HPP */
