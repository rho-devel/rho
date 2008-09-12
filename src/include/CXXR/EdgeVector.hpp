/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file EdgeVector.hpp
 *
 * @brief Templated class CXXR::EdgeVector.
 */

#ifndef EDGEVECTOR_HPP
#define EDGEVECTOR_HPP 1

#include <algorithm>

#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/Allocator.hpp"
#include "CXXR/GCRoot.h"
#include "CXXR/VectorBase.h"

namespace CXXR {
    /** @brief Vector of pointers to RObject.
     *
     * This is a templated class to represent a vector whose members
     * are pointers to other GCNode objects.
     * @param Ptr This should be pointer or const pointer to
     *          GCNode or to a type (publicly) derived from GCNode.
     *          The vector elements will be of type \a Ptr.
     * @param ST The required ::SEXPTYPE of the vector.
     */
    template <typename Ptr, SEXPTYPE ST>
    class EdgeVector : public VectorBase {
    public:
	/** @brief Proxy object for an element of an EdgeVector<Ptr, ST>.
	 *
	 * Objects of this class are used to allow the elements of an
	 * EdgeVector<Ptr, ST> to be examined and modified using the
	 * same syntax as would be used for accessing an array of
	 * \a Ptr, whilst nevertheless enforcing the write
	 * barrier.  See Item 30 of Scott Meyers's 'More Effective
	 * C++' for a general discussion of proxy objects, but see the
	 * <a
	 * href="http://www.aristeia.com/BookErrata/mec++-errata_frames.html">errata</a>.
	 * (It may look complicated, but an optimising compiler should
	 * be able to distil an invocation of EdgeVector<Ptr,
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
		*m_it = *rhs.m_it;
		if (rhs.m_ev != m_ev)
		    m_ev->propagateAge(*m_it);
		return *this;
	    }

	    /** Redirect the pointer encapsulated by the proxied element.
	     * @param s New pointer value.
	     * @return Reference to this ElementProxy.
	     */
	    ElementProxy& operator=(Ptr s)
	    {
		m_ev->propagateAge(s);
		*m_it = s;
		return *this;
	    }

	    /**
	     * @return The pointer encapsulated by the proxied
	     *         element.
	     */
	    operator Ptr const() const
	    {
		return *m_it;
	    }
	private:
	    EdgeVector<Ptr, ST>* m_ev;
	    typename std::vector<Ptr, Allocator<Ptr> >::iterator m_it;

	    ElementProxy(EdgeVector<Ptr, ST>* ev, unsigned int index)
		: m_ev(ev), m_it(m_ev->m_data.begin() + index)
	    {}

	    // Not implemented:
	    ElementProxy(const ElementProxy&);

	    friend class EdgeVector<Ptr, ST>;
	};

	/** @brief Create a vector.
         *
         * Create a vector.
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 * @param init Initial value for the destination of each
	 *          \a Ptr in the EdgeVector.
	 */
	explicit EdgeVector(size_t sz, Ptr init = 0)
	    : VectorBase(ST, sz), m_data(sz, init)
	{}

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
	 * @return the specified element.
	 */
	Ptr const operator[](unsigned int index) const
	{
	    return m_data[index];
	}

	/**
	 * @return pointer to the start of this object's data,
	 * interpreted (riskily) as an array of \a Ptr.
	 * @deprecated This function puts the integrity of the write barrier
	 * at the mercy of class clients.  (It also assumes that the
	 * data of a std::vector are stored contiguously, which isn't
	 * guaranteed by the standard.)
	 */
	Ptr* dataPtr()
	{
	    return &m_data[0];
	}

	/** @brief Name by which this type is known in R.
	 *
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

	// Virtual function of GCNode:
	void visitChildren(const_visitor* v) const;
    protected:
	/**
	 * Declared protected to ensure that EdgeVector objects are
	 * allocated only using 'new'.
	 */
	~EdgeVector() {}
    private:
	std::vector<Ptr, Allocator<Ptr> > m_data;

	// Not implemented.  Declared to prevent
	// compiler-generated versions:
	EdgeVector(const EdgeVector&);
	EdgeVector& operator=(const EdgeVector&);

	friend class ElementProxy;
    };

    template <typename Ptr, SEXPTYPE ST>
    const char* EdgeVector<Ptr, ST>::typeName() const
    {
	return EdgeVector<Ptr, ST>::staticTypeName();
    }

    template <typename Ptr, SEXPTYPE ST>
    void EdgeVector<Ptr, ST>::visitChildren(const_visitor* v) const
    {
	VectorBase::visitChildren(v);
	for (unsigned int i = 0; i < size(); ++i) {
	    Ptr ptr = (*this)[i];
	    if (ptr) ptr->conductVisitor(v);
	}
    }
}  // namespace CXXR

#endif  // EDGEVECTOR_HPP
