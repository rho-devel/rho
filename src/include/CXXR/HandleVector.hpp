/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
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
#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/split_member.hpp>

#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/Allocator.hpp"
#include "CXXR/BSerializer.hpp"
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
    private:
	typedef std::vector<Handle<T>, Allocator<Handle<T> > > Vector;
    public:
	typedef typename Vector::const_iterator const_iterator;

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
	    : VectorBase(pattern), m_data(pattern.m_data.size())
	{
	    for (unsigned int i = 0; i < m_data.size(); ++i) {
		// Use copy constructor to apply object copying logic:
		Handle<T> handle(pattern.m_data[i]);
		m_data[i] = handle;
	    }
	}

	/** @brief default constructor
	 * Used by boost::serialization
	 */
	HandleVector() { }

	/** @brief Element access.
	 *
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 *
	 * @return the specified element.
	 */
        Handle<T>& operator[](unsigned int index)
	{
	    return m_data[index];
	}

	/** @brief Read-only element access.
	 *
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 *
	 * @return the specified element.
	 */
	const T* operator[](unsigned int index) const
	{
	    return m_data[index];
	}

	/** @brief Iterator designating first element.
	 *
	 * @return An iterator designating the first element of the
	 * HandleVector.  Returns end() if the vector is empty.
	 */
	const_iterator begin() const
	{
	    return m_data.begin();
	}

	/** @brief One-past-the-end iterator.
	 *
	 * @return An iterator designating a position 'one past the
	 * end' of the HandleVector.
	 */
	const_iterator end() const
	{
	    return m_data.end();
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

	// Virtual function of GCNode:
	void detachReferents();
    private:
    	friend class boost::serialization::access;
	Vector m_data;

	// Not implemented.  Declared to prevent
	// compiler-generated version:
	HandleVector& operator=(const HandleVector&);

	template<class Archive>
	void load(Archive & ar, const unsigned int version) {
	    ar >> boost::serialization::base_object<VectorBase>(*this);
	    ar >> m_data;
	}

	template<class Archive>
	void save(Archive & ar, const unsigned int version) const {
	    ar << boost::serialization::base_object<VectorBase>(*this);
	    ar << m_data;
	}

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("HandleVector");
	    boost::serialization::split_member(ar, *this, version);
	}

	friend class ElementProxy;
    };

    template <typename T, SEXPTYPE ST>
    const char* HandleVector<T, ST>::typeName() const
    {
	return HandleVector<T, ST>::staticTypeName();
    }

    template <typename T, SEXPTYPE ST>
    void HandleVector<T, ST>::detachReferents()
    {
	VectorBase::detachReferents();
	m_data.clear();
    }

    template <typename T, SEXPTYPE ST>
    void HandleVector<T, ST>::visitReferents(const_visitor* v) const
    {
	VectorBase::visitReferents(v);
	typename Vector::const_iterator end = m_data.end();
	for (typename Vector::const_iterator it = m_data.begin();
	     it != end; ++it) {
	    T* node = *it;
	    if (node)
		node->conductVisitor(v);
	}
    }
}  // namespace CXXR

#endif  // HANDLEVECTOR_HPP
