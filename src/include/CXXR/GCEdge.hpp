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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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
     * @param T This should be a pointer or const pointer to GCNode or
     *          (more usually) a type derived from GCNode.
     */
    template <class T = RObject*>
    class GCEdge {
    public:
	/**
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
	GCEdge(GCNode* /*from*/, T to = 0)
	    : m_to(to)
	{}

	/**
	 * @return the pointer which this GCEdge object encapsulates.
	 */
	operator T const() const {return m_to;}

	/** Redirect the GCEdge to point at a (possibly) different node.
	 *
	 * @param from This \e must point to the same node as that
	 *          pointed to by the \a from parameter used construct
	 *          this GCEdge object.
	 *
	 * @param to Pointer to the object to which reference is now
	 *           to be made.
	 *
	 * @note An alternative implementational approach would be to
	 * save the \a from pointer within the GCEdge object.
	 * However, this would double the space occupied by a GCEdge
	 * object.
	 */
	 
	void redirect(GCNode* from, T to)
	{
	    GCNode::Ager ager(from->m_gcgen);
	    m_to = to;
	    if (m_to) m_to->conductVisitor(&ager);
	}
    private:
	T m_to;

	// Not implemented:
	GCEdge(const GCEdge&);
	GCEdge& operator=(const GCEdge&);
    };
}

#endif  // GCEDGE_HPP
