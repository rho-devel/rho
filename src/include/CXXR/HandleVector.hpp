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

/** @file HandleVector.hpp
 *
 * @brief Templated class CXXR::HandleVector.
 */

#ifndef HANDLEVECTOR_HPP
#define HANDLEVECTOR_HPP 1

#include <algorithm>

#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/Allocator.hpp"
#include "CXXR/GCRoot.h"
#include "CXXR/VectorBase.h"

namespace CXXR {
    /** @brief Vector of RObject::Handle smart pointers.
     *
     * This is a templated class to represent a vector whose elements
     * are are smart pointers of type \c RObject::Handle<T>.  As
     * explained in the documentation for \c RObject::Handle, copying
     * the vector will copy the objects pointed to, provided that they
     * are clonable.
     *
     * @param T This should be RObject or a type (publicly) derived
     * from RObject.  The vector elements will be of type \c Handle<T>.
     *
     * @param ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class HandleVector : public VectorBase {
    public:
	/** @brief Proxy object for an element of an HandleVector<T, ST>.
	 *
	 * Objects of this class are used to allow the elements of an
	 * HandleVector<T, ST> to be examined and modified using
	 * the same syntax as would be used for accessing an array of
	 * \a T*, whilst nevertheless enforcing the write barrier.
	 * See Item 30 of Scott Meyers's 'More Effective C++' for a
	 * general discussion of proxy objects, but see the
	 * <a href="http://www.aristeia.com/BookErrata/mec++-errata_frames.html">errata</a>.
	 * (It may look complicated, but an optimising compiler should
	 * be able to distil an invocation of HandleVector<T,
	 * ST>::operator[] into very few instructions.)
	 */
	class ElementProxy {
	public:
	    /** Copy the value of the proxied element from another
	     *  proxied element.
	     *
	     * @param rhs Proxied element whose value is to be copied.
	     *
	     * @return Reference to this ElementProxy.
	     */
	    ElementProxy& operator=(const ElementProxy& rhs)
	    {
		(*m_it).retarget(m_ev, *rhs.m_it);
		return *this;
	    }

	    /** Redirect the pointer encapsulated by the proxied
	     * element.
	     *
	     * @param s New pointer value.
	     *
	     * @return Reference to this ElementProxy.
	     */
	    ElementProxy& operator=(T* s)
	    {
		(*m_it).retarget(m_ev, s);
		return *this;
	    }

	    /**
	     * @return The pointer encapsulated by the proxied
	     *         element.
	     */
	    operator T* const() const
	    {
		return *m_it;
	    }
	private:
	    HandleVector<T, ST>* m_ev;
	    typename std::vector<Handle<T>,
				 Allocator<Handle<T> > >::iterator m_it;

	    ElementProxy(HandleVector<T, ST>* ev, unsigned int index)
		: m_ev(ev), m_it(m_ev->m_data.begin() + index)
	    {}

	    // Not implemented:
	    ElementProxy(const ElementProxy&);

	    friend class HandleVector<T, ST>;
	};

	/** @brief Create a vector.
         *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 *
	 * @param init Initial value for the destination of each
	 *          \a T* in the HandleVector.
	 */
	explicit HandleVector(size_t sz, T* init = 0)
	    : VectorBase(ST, sz), m_data(sz, Handle<T>(init))
	{}

	/** @brief Copy constructor.
	 *
	 * @param pattern HandleVector to be copied.  Beware that if
	 *          any of the elements of \a pattern are unclonable,
	 *          they will be shared between \a pattern and the
	 *          created object.  This is necessarily prejudicial
	 *          to the constness of the \a pattern parameter.
	 */
	HandleVector(const HandleVector<T, ST>& pattern)
	    : VectorBase(pattern), m_data(pattern.m_data)
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
	const T* operator[](unsigned int index) const
	{
	    return m_data[index];
	}

	/**
	 * @return pointer to the start of this object's data,
	 * interpreted (riskily) as an array of \a T*.
	 *
	 * @deprecated This function puts the integrity of the write barrier
	 * at the mercy of class clients.
	 */
	T** dataPtr()
	{
	    return reinterpret_cast<T**>(&m_data[0]);
	}

	/** @brief Name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 *
	 * @note This function is declared but not defined as part of
	 * the HandleVector template.  It must be defined as a
	 * specialization for each instantiation of the template for
	 * which it or typeName() is used.
	 */
	inline static const char* staticTypeName();

	// Virtual function of RObject:
	const char* typeName() const;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	/**
	 * Declared protected to ensure that HandleVector objects are
	 * allocated only using 'new'.
	 */
	~HandleVector() {}
    private:
	typedef std::vector<Handle<T>, Allocator<Handle<T> > > Vector;
	Vector m_data;

	// Not implemented.  Declared to prevent
	// compiler-generated version:
	HandleVector& operator=(const HandleVector&);

	friend class ElementProxy;
    };

    template <typename T, SEXPTYPE ST>
    const char* HandleVector<T, ST>::typeName() const
    {
	return HandleVector<T, ST>::staticTypeName();
    }

    template <typename T, SEXPTYPE ST>
    void HandleVector<T, ST>::visitReferents(const_visitor* v) const
    {
	VectorBase::visitReferents(v);
	for (typename Vector::const_iterator it = m_data.begin();
	     it != m_data.end(); ++it)
	    (*it).conductVisitor(v);
    }
}  // namespace CXXR

#endif  // HANDLEVECTOR_HPP
