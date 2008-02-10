/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  Andrew Runnalls
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef GCEDGE_HPP
#define GCEDGE_HPP 1

#include "CXXR/GCNode.hpp"

namespace CXXR {
    class RObject;

    /**
     * This class encapsulates a pointer from one GCNode to another,
     * and carries out housekeeping required by the garbage collection
     * scheme.  The class name reflects the fact that these objects
     * represent directed edges in the directed graph with the GCNode
     * objects as its nodes.
     *
     * Whenever an object of a type derived from GCNode needs to refer
     * to another such object, it should do so by containing a GCEdge
     * object, rather than by containing a pointer or reference
     * directly.
     *
     * @param Ptr This should be a pointer or const pointer to GCNode or
     *          (more usually) a type derived from GCNode.
     */
    template <class Ptr = RObject*>
    class GCEdge {
    public:
	/** @brief Destination comparator template.
	 *
	 * This templated class converts an STL-compatible comparator
	 * type for object of the type to which Ptr points to into an
	 * STL-comparator for GCEdge<Ptr>.  The comparison is based on
	 * the destinations of the GCEdge objects being compared.
	 * @param BinaryPredicate STL-compatible comparator type for
	 *          objects of the type to which Ptr points.
	 */
	template <class BinaryPredicate> class DestComparator {
	public:
	    /**
	     * @param tcomp The constructed object will compare two
	     *          GCEdge<Ptr> objects according to how \a tcomp
	     *          compares their destinations.
	     */
	    explicit DestComparator(const BinaryPredicate& tcomp)
		: m_tcomp(tcomp)
	    {}

	    /** @brief Comparison operation.
	     * @param l const reference to a GCEdge<Ptr>.
	     * @param r const reference to a GCEdge<Ptr>.
	     * @return true iff \a l < \a r in the defined ordering.
	     */
	    bool operator()(const GCEdge<Ptr>& l, const GCEdge<Ptr>& r) const
	    {
		return m_tcomp(*l, *r);
	    }
	private:
	    const BinaryPredicate& m_tcomp;
	};

	/** @fn GCEdge(GCNode* from, Ptr to = 0)
	 * @param from Pointer to the GCNode which needs to refer to
	 *          \a to.  Usually the constructed GCEdge object will
	 *          form part of the object to which \a from points.
	 *          (In the present implementation this parameter is
	 *          ignored, but it should be set correctly to allow
	 *          for future changes in implementation.)
	 *
	 * @param to Pointer to the object to which reference is to be
	 *           made.
	 *
	 * @note This constructor does not carry out an old-to-new
	 * check, because normally the GCEdge being constructed will
	 * form part of newly-constructed object of a type derived
	 * from GCNode, so will automatically be newer than any GCNode
	 * it refers to.  If an old-to-new check is required, it is
	 * recommended to create the GCEdge will a null 'to' pointer,
	 * and then to redirect it to the desired target.
	 */
	explicit GCEdge(GCNode* /*from*/, Ptr to = 0)
	    : m_to(to)
	{}

	/** @fn GCEdge(const GCEdge<Ptr>& source)
	 * @brief Copy constructor
	 * @param source const reference to the GCEdge to be copied.
	 *
	 * @note This constructor does not carry out an old-to-new
	 * check, because normally the GCEdge being constructed will
	 * form part of newly-constructed object of a type derived
	 * from GCNode, so will automatically be newer than any GCNode
	 * it refers to.
	 */

	/** @fn GCEdge<Ptr>& operator=(const GCEdge<Ptr>& rhs)
	 * @brief Assignment operator.
	 * @param rhs Right-hand side of the assignment.  It is
	 *          <em>essential</em> that the origin ('from') of \a
	 *          rhs be the same as the origin of the GCEdge being
	 *          assigned to, but this is not checked in the
	 *          default implementation.  Failure to observe this
	 *          rule will result in garbage collection chaos.
	 * @return A reference to this object.
	 */

	/**
	 * @return the pointer which this GCEdge object encapsulates.
	 */
	operator Ptr const() const {return m_to;}

	/** Redirect the GCEdge to point at a (possibly) different node.
	 *
	 * @param from This \e must point to the same node as that
	 *          pointed to by the \a from parameter used to
	 *          construct this GCEdge object.
	 *
	 * @param to Pointer to the object to which reference is now
	 *           to be made.
	 *
	 * @note An alternative implementational approach would be to
	 * save the \a from pointer within the GCEdge object.
	 * However, this would double the space occupied by a GCEdge
	 * object.
	 */
	void redirect(GCNode* from, Ptr to)
	{
	    m_to = to;
	    if (m_to) {
		GCNode::Ager ager(from->m_gcgen);
		m_to->conductVisitor(&ager);
	    }
	}
    private:
	Ptr m_to;
    };
}

#endif  // GCEDGE_HPP
