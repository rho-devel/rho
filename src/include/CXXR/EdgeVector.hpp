/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008  Andrew Runnalls
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

/** @file EdgeVector.hpp
 *
 * @brief Templated class EdgeVector.
 */

#ifndef EDGEVECTOR_HPP
#define EDGEVECTOR_HPP 1

#include <algorithm>

#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCRoot.h"
#include "CXXR/VectorBase.h"

namespace CXXR {
    /** @brief Vector of GCEdge objects.
     *
     * This is a templated class to represent a vector whose members
     * are of a type instantiated from the template GCEdge<>.
     * @param T The type of pointer to be encapsulated by the GCEdge
     *          objects.  This should be pointer or const pointer to
     *          GCNode or to a type (publicly) derived from GCNode.
     *          The vector elements will be of type GCEdge<T>.
     * @param ST The required \c SEXPTYPE of the vector.
     */
    template <class T, SEXPTYPE ST>
    class EdgeVector : public VectorBase {
    public:
	/** Proxy object for an element of an EdgeVector<T, ST>.
	 *
	 * Objects of this class are used to allow the elements of an
	 * EdgeVector<T, ST> to be examined and modified using the
	 * same syntax as would be used for accessing an array of
	 * <tt>T</tt>, whilst nevertheless enforcing the write
	 * barrier.  See Item 30 of Scott Meyers's 'More Effective
	 * C++' for a general discussion of proxy objects, but see the
	 * <a
	 * href="http://www.aristeia.com/BookErrata/mec++-errata_frames.html">errata</a>.
	 * (It may look complicated, but an optimising compiler should
	 * be able to distil an invocation of EdgeVector<T,
	 * ST>::operator[] into very few instructions.)
	 */
	class ElementProxy {
	public:
	    /** Copy the value of the proxied element from another
	     *  proxied element.
	     * @param rhs Proxied element whose value is to be copied.
	     * @return Reference to this ElementProxy.
	     */
	    ElementProxy& operator=(const ElementProxy& rhs)
	    {
		return operator=(static_cast<T>(rhs));
	    }

	    /** Redirect the pointer encapsulated by the proxied element.
	     * @param rhs New pointer value.
	     * @return Reference to this ElementProxy.
	     */
	    ElementProxy& operator=(T s)
	    {
		(m_ev->m_data)[m_index].redirect(m_ev, s);
		return *this;
	    }

	    /**
	     * @return The pointer encapsulated by the proxied
	     *         element.
	     */
	    operator T const() const
	    {
		return (m_ev->m_data)[m_index];
	    }
	private:
	    EdgeVector<T, ST>* m_ev;
	    unsigned int m_index;

	    ElementProxy(EdgeVector<T, ST>* ev, unsigned int index)
		: m_ev(ev), m_index(index)
	    {}

	    // Not implemented:
	    ElementProxy(const ElementProxy&);

	    friend class EdgeVector<T, ST>;
	};

	/** @brief Create a vector.
         *
         * Create a vector.
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 * @param init Initial value for the destination of each
	 *          GCEdge<T> in the EdgeVector.
	 */
	explicit EdgeVector(size_t sz, T init = 0);

	/** @brief Element access.
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 * @return Proxy for the specified element, via which the
	 *         element can be examined or modified.
	 */
	ElementProxy operator[](unsigned int index)
	{
	    return ElementProxy(this, index);
	}

	/** @brief Read-only element access.
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 * @return \c the specified element.
	 */
	T const operator[](unsigned int index) const
	{
	    return m_data[index];
	}

	/** @brief Sort the EdgeVector.
	 * @param tcomp The vector will be sorted according to the
	 *          ordering defined by the binary predicate \a tcomp
	 *          as applied to the destinations of the constituent
	 *          GCEdge<T> objects.
	 */
	template <class BinaryPredicate>
	void sort(const BinaryPredicate& tcomp)
	{
	    if (size() > 0) {
		std::sort(m_data, m_data + size(),
			  GCEdge<T>::DestComparator(tcomp));
	    }
	}

	/**
	 * @return the name by which this type is known in R.
	 *
	 * @note This function is declared but not defined as part of
	 * the EdgeVector template.  It must be defined as a
	 * specialization for each instantiation of the template for
	 * which it or typeName() is used.
	 */
	inline static const char* staticTypeName();

	// Virtual function of RObject:
	const char* typeName() const;

	// Virtual functions of GCNode:
	void visitChildren(const_visitor* v) const;
	void visitChildren(visitor* v);
    protected:
	// Declared protected to ensure that EdgeVectors are
	// allocated only using 'new'.
	~EdgeVector()
	{
	    if (m_data)
		Heap::deallocate(m_data, m_databytes);
	}
    private:
	size_t m_databytes;  // used only if > 0 elements
	GCEdge<T>* m_data;  // pointer to the vector's data block.

	// Not implemented.  Declared to prevent
	// compiler-generated versions:
	EdgeVector(const EdgeVector&);
	EdgeVector& operator=(const EdgeVector&);

	friend class ElementProxy;
    };

    template <class T, SEXPTYPE ST>
    EdgeVector<T, ST>::EdgeVector(size_t sz, T init)
	: VectorBase(ST, sz)
    {
	if (sz > 0) {
	    m_databytes = sz*sizeof(GCEdge<T>);
	    // Check for integer overflow:
	    if (m_databytes/sizeof(GCEdge<T>) != sz)
		Rf_error(_("Request to create impossibly large vector."));
	    GCRoot<> thisroot(this);
	    m_data = reinterpret_cast<GCEdge<T>*>(Heap::allocate(m_databytes));
	    GCEdge<T>* p = m_data;
	    for (unsigned int i = 0; i < sz; ++i)
		new (p++) GCEdge<T>(this, init);
	}
    }

    template <class T, SEXPTYPE ST>
    const char* EdgeVector<T, ST>::typeName() const
    {
	return EdgeVector<T, ST>::staticTypeName();
    }

    template <class T, SEXPTYPE ST>
    void EdgeVector<T, ST>::visitChildren(GCNode::const_visitor* v) const
    {
	VectorBase::visitChildren(v);
	// Check that this node is fully constructed before attempting
	// to visit children:
	if (m_data) {
	    for (unsigned int i = 0; i < size(); ++i) {
		T ptr = (*this)[i];
		if (ptr) ptr->conductVisitor(v);
	    }
	}
    }
		    
    template <class T, SEXPTYPE ST>
    void EdgeVector<T, ST>::visitChildren(GCNode::visitor* v)
    {
	VectorBase::visitChildren(v);
	// Check that this node is fully constructed before attempting
	// to visit children:
	if (m_data) {
	    for (unsigned int i = 0; i < size(); ++i) {
		T ptr = (*this)[i];
		if (ptr) ptr->conductVisitor(v);
	    }
	}
    }
}  // namespace CXXR

#endif  // EDGEVECTOR_HPP
