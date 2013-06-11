/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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

#include <sstream>

#include "CXXR/Allocator.hpp"
#include "CXXR/HeterogeneousList.hpp"
#include "CXXR/MemoryBank.hpp"
#include "CXXR/SchwarzCounter.hpp"

// According to various web postings (and arr's experience) it is
// necessary for the compiler to have seen the headers for the archive
// types in use before it encounters any of the BOOST_CLASS_EXPORT_*
// macros.  So we include them here, along with export.hpp itself.
#include <boost/archive/xml_oarchive.hpp>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/export.hpp>
#include <boost/serialization/version.hpp>

/** @def GC_FIND_LOOPS
 *
 * If the preprocessor variable GC_FIND_LOOPS is defined, extra code
 * is inserted which, during a mark-sweep garbage collection, writes
 * to the standard output information about any cycles encountered in
 * the GCNode-GCEdge graph.
 */
#ifdef DOXYGEN
#define GC_FIND_LOOPS
#endif

/** @brief Syntactic sugar for creating CXXR::GCNode objects.
 *
 * The argument of this macro must be a constructor expression for an
 * object of a class derived from CXXR::GCNode.  The macro expansion
 * returns a pointer to a new object created by that constructor
 * expression.
 */
#define CXXR_NEW(T) CXXR::GCNode::expose(new T)

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
     * should reimplement the methods detachReferents() and visitReferents()
     * appropriately.  (This does not necessarily apply to pointers
     * where other logic ensures that the pointer does not outlive the
     * thing pointed to.)</li>
     * </ol>
     *
     * \par Infant immunity:
     * Mark-sweep garbage collection is disabled while any GCNode or
     * object of a class derived from GCNode is under construction.
     * This greatly simplifies the coding of the constructors.  Such
     * an object is considered to be under construction from the
     * moment the constructor is invoked until the node is exposed by
     * calling the member function expose() (in one of its two forms),
     * or until the node is deleted, whichever is sooner.  It is the
     * responsibility of any code that creates an object of a class
     * derived from GCNode to ensure that the object is exposed as
     * soon as possible, e.g. before a pointer to the node is returned
     * as the value of a function.  In particular, a node must be
     * exposed before it is designated as the target of a GCEdge, a
     * GCStackRoot or a GCRoot, or protected with PROTECT() or
     * REPROTECT(): if the preprocessor variable CHECK_EXPOSURE is
     * defined, runtime checks for this are inserted into the code.
     *
     * The simplest way of ensuring timely exposure is always to wrap
     * the \c new call in a call to expose():
     * e.g. <tt>GCNode::expose(new FooNode(<i>args</i>)</tt>.  This
     * can be further simplified using the CXXR_NEW macro to
     * CXXR_NEW(FooNode).
     *
     * @note Because this base class is used purely for housekeeping
     * by the garbage collector, and does not contribute to the
     * 'meaning' of an object of a derived class, its data members are
     * mutable.
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

	    /** @brief Perform visit.
	     *
	     *
	     * In the light of what the visitor discovers, it may
	     * elect also to visit the referents of \a node, by
	     * calling visitReferents().
	     *
	     * @param node Node to be visited.
	     */
	    virtual void operator()(const GCNode* node) = 0;
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
	 * implemented in CXXR.  It is also used within the GCNode
	 * class to handle reentrant calls to gclite() and gc().  Its
	 * use for other purposes is deprecated: use instead more
	 * selective protection against garbage collection such as
	 * that provided by class GCStackRoot<T>.
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

	    /** @brief Is inhibition currently in effect?
	     *
	     * @return true iff garbage collection is currently inhibited.
	     */
	    static bool active()
	    {
		return s_inhibitor_count != 0;
	    }
	};

	// Serialization of pointers to GCNodes.  Defined in
	// GCNode_PtrS11n.hpp .
	class PtrS11n;

	GCNode()
            : HeterogeneousListBase::Link(s_live),
#ifdef GCID
	      m_id(++s_last_id),
#endif
	      m_rcmmu(s_mark | s_moribund_mask | 1)
	{
	    ++s_num_nodes;
	    ++s_inhibitor_count;
	    s_moribund->push_back(this);
#ifdef GCID
	    watch();
#endif
	}

	/** @brief Allocate memory.
         *
	 * Allocates memory for a new object of a class derived from
	 * GCNode.
	 *
	 * @param bytes Number of bytes of memory required.
	 *
	 * @return Pointer to the allocated memory block.
	 *
	 * @note This function will often carry out garbage collection
	 * of some kind before allocating memory.  However, no
	 * mark-sweep collection will be performed if another GCNode
	 * object is currently under construction, or if at least one
	 * GCInhibitor object is in existence.
	 */
#ifdef __GNUC__
	__attribute__((hot,fastcall))
#endif
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

	/** @brief Null out all references from this node to other nodes.
	 *
	 * The referents of this node are those objects (derived from
	 * GCNode) designated by a GCEdge within this object.  This
	 * function changes all GCEdges within this object to
	 * encapsulate a null pointer.  It is used during the sweep
	 * phase of a mark-sweep garbage collection to break up
	 * unreachable subgraphs, and in particular to remove
	 * reference loops from them.  After the application of this
	 * method, the GCNode should be regarded as a 'zombie', kept
	 * in existence only so other nodes can detach their
	 * references to it cleanly (using decRefCount()).
	 *
	 * @note If this method is reimplemented in a derived class,
	 * the reimplemented version must remember to invoke
	 * detachReferents() for the immediate base class of the
	 * derived class, to ensure that \e all referents of the
	 * object get detached.
	 */
	virtual void detachReferents()  {}

	/** @brief Record that construction of a node is complete.
	 *
	 * See the description of the templated form of expose.
	 */
	void expose() const
	{
#ifndef NDEBUG
	    if (isExposed())
		alreadyExposedError();
#endif
	    m_rcmmu &= ~1;
	    --s_inhibitor_count;
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
	 * The simplest way of ensuring timely exposure is always to
	 * wrap the \c new call in a call to expose():
	 * e.g. <tt>GCNode::expose(new FooNode(<i>args</i>)</tt>.
	 * This can be further simplified using the CXXR_NEW macro to
	 * CXXR_NEW(FooNode).
	 *
	 * It is not permissible for a node to be exposed more than
	 * once, and this is checked unless \c NDEBUG is defined.
	 *
	 * @tparam T GCNode or any class derived from it, possibly
	 *           qualified by const.
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
	 */
	static void gc();

	/** @brief Lightweight garbage collection.
	 *
	 * This function deletes nodes whose reference counts have
	 * fallen to zero: if the deletion of these nodes in turn
	 * causes the reference counts of other nodes to fall to zero,
	 * those nodes are also deleted, and so on recursively.
	 *
	 * @note This function does not delete nodes whose reference
	 * counts have never have risen above zero.
	 */
	static void gclite();

	/** @brief Has this node been exposed to garbage collection?
	 *
	 * @return true iff this node has been exposed to garbage
	 * collection.
	 */
	bool isExposed() const
	{
	    return (m_rcmmu & 1) == 0;
	}

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
	 * object get visited.  It is suggested that implementations
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
#ifdef GCID
	    watch();
#endif
	    // Is the node still under construction?
	    if (m_rcmmu & 1)
		destruct_aux();
	    --s_num_nodes;
	}
    private:
	friend class boost::serialization::access;
	friend class GCRootBase;
	friend class GCStackRootBase;
	friend class NodeStack;
	friend class WeakRef;

	/** Visitor class used to mark nodes.
	 *
	 * This visitor class is used during the mark phase of garbage
	 * collection to ensure that a node and its descendants are
	 * marked.
	 */
	class Marker : public const_visitor {
	public:
	    Marker()
		: m_marks_applied(0)
	    {}

	    unsigned int marksApplied() const
	    {
		return m_marks_applied;
	    }

	    // Virtual function of const_visitor:
	    void operator()(const GCNode* node);
	private:
	    unsigned int m_marks_applied;
#ifdef GC_FIND_LOOPS
	    std::vector<const GCNode*> m_ariadne;
#endif
	};

	typedef HeterogeneousList<GCNode> List;

	static List* s_live;  // Except during mark-sweep garbage
	  // collection, all existing nodes are threaded on this
	  // list.
	static std::vector<const GCNode*>* s_moribund;  // Vector of
	  // pointers to nodes whose reference count has fallen to
	  // zero (but may subsequently have increased again).
	static List* s_reachable;  // During the mark phase of garbage
	  // collection, if a node is found to be reachable from the
	  // roots, it is moved to this list. Between garbage
	  // collections, this list should be empty.
	static const size_t s_gclite_margin;  // operator new will
	  // invoke gclite() when MemoryBank::bytesAllocated() exceeds
	  // by at least s_gclite_margin the number of bytes that were
	  // allocated following the previous gclite().  This is a
	  // tuning parameter.
	static size_t s_gclite_threshold;  // operator new calls
	  // gclite() when the number of bytes allocated reaches this
	  // level.
	static unsigned int s_num_nodes;  // Number of nodes in existence
	static unsigned int s_inhibitor_count;  // Number of GCInhibitor
	  // objects in existence, plus the number of nodes currently
	  // under construction (i.e. not yet exposed).
#ifdef GCID
	// If GCID is defined, each GCNode is given an identity
	// number.  The numbers are not unique: they wrap around
	// eventually.  This is the number that was most recently
	// assigned to a node:
	static unsigned int s_last_id;

	// Using a debugger, the following can be set to non-null values
	// to monitor operations on nodes at a particular address, or
	// a node with a particular id:
	static const GCNode* s_watch_addr;
	static unsigned int s_watch_id;
#endif
	// Bit patterns XORd into m_rcmmu to decrement or increment the
	// reference count.  Patterns 0, 2, 4, ... are used to
	// decrement; 1, 3, 5, .. to increment.
	static const unsigned char s_decinc_refcount[];
	static unsigned char s_mark;  // During garbage collection, a
	  // node is considered marked if its s_mark_mask bit matches the
	  // corresponding bit of s_mark.  (Only this bit will ever be
	  // set in s_mark.)
#ifdef GCID
	unsigned int m_id;
#endif

	static const unsigned char s_mark_mask = 0x80;
	static const unsigned char s_moribund_mask = 0x40;
	static const unsigned char s_refcount_mask = 0x3e;
	mutable unsigned char m_rcmmu;
	  // Refcount/moribund/marked/under-construction.  The least
	  // significant bit is set to signify that the node is under
	  // construction.  The reference count is held in the next 5
	  // bits, and saturates at 31.  The 0x40 bit is set to
	  // signify that the node is on the moribund list.  The most
	  // significant bit is set to s_mark on construction; this
	  // bit is then toggled in the mark phase of a mark-sweep
	  // garbage collection to identify reachable nodes.

	// Not implemented.  Declared to prevent compiler-generated
	// versions:
	GCNode(const GCNode&);
	GCNode& operator=(const GCNode&);

	// Not implemented.  Declared private to prevent clients
	// allocating arrays of GCNode.
	//
	// But boost::serialization doesn't like this.
	// static void* operator new[](size_t);

	// Abort program if 'node' is not exposed to GC.
	static void abortIfNotExposed(const GCNode* node);

	// Abort program with an error message if an attempt is made
	// to expose a node more than once.
	static void alreadyExposedError();

	// Clean up static data at end of run:
	static void cleanup();

	// Decrement the reference count (subject to the stickiness of
	// its MSB).  If as a result the reference count falls to
	// zero, mark the node as moribund.
	static void decRefCount(const GCNode* node)
	{
	    if (node) {
		unsigned char& rcmmu = node->m_rcmmu;
		rcmmu ^= s_decinc_refcount[rcmmu & s_refcount_mask];
		if ((rcmmu & (s_refcount_mask | s_moribund_mask)) == 0)
		    node->makeMoribund();
	    }
	}

	// Helper function for the destructor, handling the case where
	// the node is still under construction.  This should happen
	// only in the case where a derived class constructor has
	// thrown an exception.
#ifdef __GNUC__
	__attribute__((cold))
#endif
	void destruct_aux();

	// Increment the reference count.  Overflow is handled by the
	// stickiness of the MSB.
	static void incRefCount(const GCNode* node)
	{
	    if (node) {
		unsigned char& rcmmu = node->m_rcmmu;
		rcmmu ^= s_decinc_refcount[(rcmmu & s_refcount_mask) + 1];
	    }
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

	bool isMarked() const
	{
	    return (m_rcmmu & s_mark_mask) == s_mark;
	}

	// Mark this node as moribund:
#ifdef __GNUC__
	__attribute__((hot,fastcall))
#endif
	void makeMoribund() const;

	/** @brief Carry out the mark phase of garbage collection.
	 */
	static void mark();

	// boost::serialization.  Version 0 is for debugging, and will
	// be used for output if the preprocessor variable DEBUG_S11N
	// is defined.  It writes the GCNode's address and id to the
	// archive; these fields are parsed but ignored on input.
	// Version 1 is the default version.
	template <class Archive>
	void serialize(Archive & ar, const unsigned int version);

	/** @brief Carry out the sweep phase of garbage collection.
	 */
	static void sweep();
#ifdef GCID
	// Used to monitor a particular node (or nodes at a particular
	// address) using a debugger.
	void watch() const;
#endif

	friend class GCEdgeBase;
	friend class SchwarzCounter<GCNode>;
    };
}  // namespace CXXR

template <class Archive>
void CXXR::GCNode::serialize(Archive & ar, const unsigned int version) {
    if (version == 0) {
	std::ostringstream oss;
	oss << this;
	std::string addr = oss.str();
	ar & BOOST_SERIALIZATION_NVP(addr);
	unsigned int id = 0;
#ifdef GCID
	id = m_id;
#endif
	ar & BOOST_SERIALIZATION_NVP(id);
    }
}

#ifndef DEBUG_S11N
BOOST_CLASS_VERSION(CXXR::GCNode, 1)
#endif

namespace {
    CXXR::SchwarzCounter<CXXR::GCNode> gcnode_schwarz_ctr;
}

#endif /* GCNODE_HPP */
