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

#include "CXXR/Allocator.hpp"
#include "CXXR/HeterogeneousList.hpp"
#include "CXXR/MemoryBank.hpp"
#include "CXXR/SchwarzCounter.hpp"

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
     * other objects derived from GCNode, these should be encapsulated
     * within an object of the templated class GCEdge, and the class
     * should reimplement the method visitReferents()
     * appropriately.  (This does not necessarily apply to pointers
     * where other logic ensures that the pointer does not outlive the
     * thing pointed to.)</li>
     * </ol>
     *
     * \par Infant immunity:
     * Garbage collection is disabled while any object of a class
     * derived from GCNode is under construction.  This greatly
     * simplifies the coding of constructors.  Such an object is
     * considered to be under construction from the moment the
     * constructor is invoked until the node is exposed by by calling
     * the member function expose() (in one of its two forms), or
     * until the node is deleted, whichever is sooner.  It is the
     * responsibility of any code that creates an object of a class
     * derived from GCNode to ensure that the object is exposed as
     * soon as possible, e.g. before a pointer to the node is returned
     * as the value of a function.  In particular, a node must be
     * exposed before it is designated as the target of a GCEdge or a
     * GCRoot, or protected with PROTECT() or REPROTECT(): if the
     * preprocessor variable CHECK_EXPOSURE is defined, runtime checks
     * for this are inserted into the code.
     *
     * The simplest way of ensuring timely exposure is always to wrap
     * the \c new call in a call to expose():
     * e.g. <tt>GCNode::expose(new FooNode(<i>args</i>)</tt>.
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
    class GCNode : public HeterogeneousListBase::Link {
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

	/** @brief Not for general use.
	 *
	 * All garbage collection will be inhibited while any object
	 * of this type exists.
	 *
	 * @deprecated This class is provided for use in implementing
	 * functions (such as SET_ATTRIB()) in the Rinternals.h
	 * interface which would not give rise to any memory
	 * allocations as implemented in CR but may do so as
	 * implemented in CXXR.  Its use for other purposes is
	 * deprecated: use instead more selective protection against
	 * garbage collection such as that provided by class
	 * GCStackRoot<T>.
	 *
	 * @note GC inhibition is implemented as an object type to
	 * facilitate reinstatement of garbage collection when an
	 * exception is thrown.
	 */
	struct GCInhibitor {
	    GCInhibitor()
	    {
		++GCNode::s_inhibitor_count;
	    }

	    ~GCInhibitor()
	    {
		--GCNode::s_inhibitor_count;
	    }
	};

	GCNode()
	    : m_bits(s_mark)
	{
	    s_generation[1].splice_back(this);
	    ++s_num_nodes;
	    ++s_under_construction;
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
	static void* operator new(size_t bytes);

	/** @brief Placement new for GCNode.
	 */
	static void* operator new(size_t, void* where)
	{
	    return where;
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
	    visitReferents(v);
	    return true;
	}

	/** @brief Record that construction of a node is complete.
	 *
	 * See the description of the templated form of expose.
	 */
	void expose() const
	{
	    if (!(m_bits & GENERATION)) {
		m_bits |= 1;
		--s_under_construction;
	    }
	}

	/** @brief Record that construction of a node is complete.
	 *
	 * In normal operation (i.e. unless the object's constructor
	 * throws an exception), this function - or its non-templated
	 * form - should be called for each object derived from
	 * GCNode, and this should be done as soon as possible after
	 * construction of the object is complete.  In particular, a
	 * node must be exposed before it is designated as the target
	 * of a GCEdge, a GCStackRoot or a GCRoot, or protected with
	 * PROTECT() or REPROTECT(): if the preprocessor variable
	 * CHECK_EXPOSURE is defined, runtime checks for this are
	 * inserted into the code.
	 *
	 * The simplest way of ensuring timely exposure is always to wrap
	 * the \c new call in a call to expose():
	 * e.g. <tt>GCNode::expose(new FooNode(<i>args</i>)</tt>.
	 *
	 * It is permissible (but pointless) for a node to be exposed
	 * more than once.
	 *
	 * @param T GCNode or any class derived from it, possibly
	 *          qualified by const.
	 *
	 * @param node Pointer to the node whose construction has been
	 *          completed.
	 *
	 * @return the pointer \a node itself.
	 *
	 * @note The name of this function reflects an earlier design
	 * in which GCNode objects were individually exposed to
	 * mark-sweep garbage collection once their construction was
	 * complete.  In the current design, mark-sweep garbage
	 * collection is inhibited entirely whilst any GCNode object
	 * is under construction.
	 */
	template <class T>
	static T* expose(T* node)
	{
	    node->expose();
	    return node;
	}

	/** @brief Initiate a garbage collection.
	 *
	 * @param num_old_gens The number of old generations to
	 * collect.  Must be strictly smaller than numGenerations().
	 */
	static void gc(unsigned int num_old_gens);

	/** @brief Subject to configuration, check that a GCNode is exposed.
	 *
	 * Normally, this function is an inlined no-op.  However, if
	 * the preprocessor variable CHECK_EXPOSURE is defined, it
	 * checks that a GCNode is exposed to garbage collection, and
	 * aborts the program if not.  This can be a useful diagnostic
	 * aid.
	 *
	 * @param node Pointer to the node to be checked, or a null
	 *          pointer in which case the check passes.
	 */
	static void maybeCheckExposed(const GCNode* node)
	{
#ifdef CHECK_EXPOSURE
	    abortIfNotExposed(node);
#endif
	}

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

	/** @brief Conduct a visitor to the nodes referred to by this
	 * one.
	 *
	 * The referents of this node are those objects (derived from
	 * GCNode) designated by a GCEdge within this object.
	 *
	 * @param v Pointer to the visitor object.
	 *
	 * @note If this method is reimplemented in a derived class,
	 * the reimplemented version must remember to invoke
	 * visitReferents() for the immediate base class of the
	 * derived class, to ensure that \e all referents of the
	 * object get visited.  It is recommended that implementations
	 * set up stack-based pointers to all the referents of a node
	 * before visiting any of them; in that case, if the
	 * (recursive) visiting pushes the node out of the processor
	 * cache, there is no need to fetch it back in.
	 */
	virtual void visitReferents(const_visitor* v) const {}
    protected:
	/**
	 * @note The destructor is protected to ensure that GCNode
	 * objects are allocated using 'new'.  (See Meyers 'More
	 * Effective C++' Item 27.) Derived classes should likewise
	 * declare their destructors private or protected.
	 */
	virtual ~GCNode()
	{
	    if (!(m_bits & GENERATION))
		--s_under_construction;
	    --s_num_nodes;
	}

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
	 */
	void propagateAge(const GCNode* node) const
	{
	    if (node) node->ageTo(generation());
	}
    private:
	friend class GCInhibitor;
	friend class GCRootBase;
	friend class WeakRef;

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

	typedef HeterogeneousList<GCNode> List;

	static unsigned int s_num_generations;
	static List* s_generation;  // s_generation[g] is a list of
		       // the nodes in generation g, excluding those
		       // on s_aged_list.
	static List* s_aged_list; // List of nodes whose generation has
		       // been changed since the last garbage
		       // collection.  They are here awaiting
		       // propagation of the generation change to
		       // their referents (and so on recursively),
		       // which will happen at the start of the next
		       // garbage collection.
	static List* s_reachable;  // During the mark phase of garbage
		       // collection, if a node within the generations
		       // being collected is found to be reachable
		       // from the roots, it is moved to the list
		       // s_reachable[g], where g is the generation
		       // that this node will be promoted to following
		       // the garbage collection.  (Consequently,
		       // s_reachable[1] is unused.)  Between garbage
		       // collections, all these lists should be empty.
	static List* s_lists[];  // Look-up table used to determine
			// from the m_bits field which list a node is
			// currently on.
	static unsigned int* s_next_gen; // Look-up table used to
			       // advance generation number following
			       // a garbage collection.
	static unsigned char s_mark;  // During garbage collection, a
			       // node is considered marked if its
			       // MARK field matches the corresponding
			       // bits of s_mark.
	static unsigned int s_num_nodes;
	static unsigned int s_under_construction;  // Number of nodes
	                      // currently under construction
	                      // (i.e. not yet exposed).
	static unsigned int s_inhibitor_count;  // Number of GCInhibitor
	                      // objects in existence.

	// Masks applicable to the m_bits field:
	enum {GENERATION = 3, AGED = 4, LIST = 7, MARK = 0x70};

	mutable unsigned char m_bits;

	// m_bits&AGED is non-zero if the generation number of the
	// node has been changed (otherwise than from 0 to 1) since
	// the last garbage collection, and this change has not yet
	// been propagated recursively.

	// Not implemented.  Declared to prevent compiler-generated
	// versions:
	GCNode(const GCNode&);
	GCNode& operator=(const GCNode&);

	// Not implemented.  Declared private to prevent clients
	// allocating arrays of GCNode.
	static void* operator new[](size_t);

	// Abort program if 'node' is not exposed to GC.
	static void abortIfNotExposed(const GCNode* node);

	// Force the generation number of this node up to mingen, and
	// if the generation number is changed, flag up the change for
	// recursive propagation by propagateAges().
	void ageTo(unsigned int mingen) const;

	// Clean up static data at end of run:
	static void cleanup();

	unsigned char generation() const
	{
	    return m_bits & GENERATION;
	}

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

	bool isMarked() const {return (m_bits & MARK) == s_mark;}

	// List on which this node is currently recorded.
	List* list() const
	{
	    return s_lists[m_bits & LIST];
	}

	/** @brief Carry out the mark phase of garbage collection.
	 *
	 * @param max_generation The highest generation number to be
	 *          swept.
	 */
	static void mark(unsigned int max_generation);

	// This is the first stage of garbage collection.  It
	// propagates the generation changes initiated by
	// propagateAge() recursively through the node graph.
	static void propagateAges();

	/** @brief Carry out the sweep phase of garbage collection.
	 *
	 * @param max_generation The highest generation number to be
	 *          swept.
	 */
	static void sweep(unsigned int max_generation);

	friend class GCEdgeBase;
	friend class SchwarzCounter<GCNode>;
    };
}  // namespace CXXR

namespace {
    CXXR::SchwarzCounter<CXXR::GCNode> gcnode_schwarz_ctr;
}

#endif /* GCNODE_HPP */
