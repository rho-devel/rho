/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
#include <vector>

#include "CXXR/config.hpp"

// According to various web postings (and arr's experience) it is
// necessary for the compiler to have seen the headers for the archive
// types in use before it encounters any of the BOOST_CLASS_EXPORT_*
// macros.  So we include them here, along with export.hpp itself.
#include <boost/archive/xml_oarchive.hpp>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/export.hpp>
#include <boost/serialization/version.hpp>

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

     * @note Because this base class is used purely for housekeeping
     * by the garbage collector, and does not contribute to the
     * 'meaning' of an object of a derived class, its data members are
     * mutable.
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

	// Serialization of pointers to GCNodes.  Defined in
	// GCNode_PtrS11n.hpp .
	class PtrS11n;

	GCNode()
            : m_rcmmu(s_mark | s_moribund_mask)
	{
	    ++s_num_nodes;
	    s_moribund->push_back(this);
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
	 * garbage collection will be performed if at least one
	 * GCInhibitor object is in existence.
	 */
	static void* operator new(size_t bytes) HOT_FUNCTION;

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
	static void operator delete(void* p, size_t bytes);

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

	/** @brief Initiate a full mark-sweep garbage collection.
	 */
	static void markSweepGC();

	/** @brief Lightweight garbage collection.
	 *
	 * This function deletes nodes whose reference counts are
	 * zero: if the deletion of these nodes in turn
	 * causes the reference counts of other nodes to fall to zero,
	 * those nodes are also deleted, and so on recursively.
	 */
	static void gclite();

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

	// If candidate_pointer is a (possibly internal) pointer to a GCNode,
	// returns the pointer to that node.
	// Otherwise returns nullptr.
	static GCNode* asGCNode(void* candidate_pointer);

    protected:
	/**
	 * @note The destructor is protected to ensure that GCNode
	 * objects are allocated using 'new'.  (See Meyers 'More
	 * Effective C++' Item 27.) Derived classes should likewise
	 * declare their destructors private or protected.
	 */
	virtual ~GCNode()
	{
	    if (m_rcmmu & s_moribund_mask)
		destruct_aux();
	    --s_num_nodes;
	}
    private:
	friend class boost::serialization::access;
	friend class GCRootBase;
	friend class GCStackFrameBoundary;
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
	    void operator()(const GCNode* node) override;
	private:
	    unsigned int m_marks_applied;
	};

	static std::vector<const GCNode*>* s_moribund;  // Vector of
	  // pointers to nodes whose reference count has fallen to
	  // zero (but may subsequently have increased again).
	static unsigned int s_num_nodes;  // Number of nodes in existence

	// Bit patterns XORd into m_rcmmu to decrement or increment the
	// reference count.  Patterns 0, 2, 4, ... are used to
	// decrement; 1, 3, 5, .. to increment.
	static const unsigned char s_decinc_refcount[];
	static unsigned char s_mark;  // During garbage collection, a
	  // node is considered marked if its s_mark_mask bit matches the
	  // corresponding bit of s_mark.  (Only this bit will ever be
	  // set in s_mark.)

	static const unsigned char s_mark_mask = 0x80;
	static const unsigned char s_moribund_mask = 0x40;
	static const unsigned char s_refcount_mask = 0x3e;
	mutable unsigned char m_rcmmu;
	  // Refcount/moribund/marked/unused.  The least
	  // significant bit is currently unused.
	  // The reference count is held in the next 5
	  // bits, and saturates at 31.  The 0x40 bit is set to
	  // signify that the node is on the moribund list.  The most
	  // significant bit is set to s_mark on construction; this
	  // bit is then toggled in the mark phase of a mark-sweep
	  // garbage collection to identify reachable nodes.

	static void gcliteImpl();

	GCNode(const GCNode&) = delete;
	GCNode& operator=(const GCNode&) = delete;

	// Not implemented.  Declared private to prevent clients
	// allocating arrays of GCNode.
	//
	// But boost::serialization doesn't like this.
	// static void* operator new[](size_t);

	// Returns the stored reference count.
	unsigned char getRefCount() const
	{
	    return (m_rcmmu & s_refcount_mask) >> 1;
	}

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

	/** @brief Initialize the entire memory subsystem.
	 *
	 * This method must be called before any GCNodes are created.
	 * If called more than once in a single program run, the
	 * second and subsequent calls do nothing.
	 */
	static void initialize();
	friend void initializeMemorySubsystem();

	bool isMarked() const
	{
	    return (m_rcmmu & s_mark_mask) == s_mark;
	}

	// Mark this node as moribund:
	void makeMoribund() const HOT_FUNCTION;

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
	static void detachReferentsOfObjectIfUnmarked(GCNode*);

	static void applyToAllAllocatedNodes(void (*function)(GCNode*));

	friend class GCEdgeBase;
	friend class GCTestHelper;
    };

    /** @brief Initialize the entire memory subsystem.
     *
     * This method must be called before any GCNodes are created.
     * If called more than once in a single program run, the
     * second and subsequent calls do nothing.
     */
    void initializeMemorySubsystem();
}  // namespace CXXR

template <class Archive>
void CXXR::GCNode::serialize(Archive & ar, const unsigned int version) {
    if (version == 0) {
	std::ostringstream oss;
	oss << this;
	std::string addr = oss.str();
	ar & BOOST_SERIALIZATION_NVP(addr);
	unsigned int id = 0;
	ar & BOOST_SERIALIZATION_NVP(id);
    }
}

#ifndef DEBUG_S11N
BOOST_CLASS_VERSION(CXXR::GCNode, 1)
#endif

#endif /* GCNODE_HPP */
