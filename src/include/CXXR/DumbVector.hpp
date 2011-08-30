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

/** @file DumbVector.hpp
 *
 * @brief Templated class CXXR::DumbVector.
 */

#ifndef DUMBVECTOR_HPP
#define DUMBVECTOR_HPP 1

#include <cstring>
#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/split_member.hpp>
#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/BSerializer.hpp"
#include "CXXR/GCRoot.h"
#include "CXXR/VectorBase.h"

namespace CXXR {
    /** @brief Vector of 'plain old data'.
     *
     * This is a templated class to represent an R data vector.
     * @param T The type of the elements of the vector.  This should
     *          be either (i) a C++ built-in data type, or (ii) a
     *          class type with a trivial destructor, and a copy
     *          constructor implemented by bitwise copy.
     * @param ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class DumbVector : public VectorBase {
    public:
	/** @brief Create a vector, leaving its contents
	 *         uninitialized. 
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 */
	DumbVector(size_t sz)
	    : VectorBase(ST, sz), m_data(&m_singleton)
	{
	    if (sz > 1) allocData(sz);
#if VALGRIND_LEVEL >= 1
	    else VALGRIND_MAKE_MEM_UNDEFINED(&m_singleton, sizeof(T));
#endif
	}

	/** @brief Create a vector, and fill with a specified initial
	 *         value. 
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 * @param initializer Initial value to be assigned to every
	 *          element.
	 */
	DumbVector(size_t sz, const T& initializer)
	    : VectorBase(ST, sz), m_data(&m_singleton),
	      m_singleton(initializer)
	{
	    if (sz > 1) allocData(sz, true);
	}

	/** @brief Default constructor
	 * Mainly for boost::serialization
	 */
	DumbVector() { }

	/** @brief Copy constructor.
	 *
	 * @param pattern DumbVector to be copied.
	 */
	DumbVector(const DumbVector<T, ST>& pattern);

	/** @brief Element access.
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 * @return Reference to the specified element.
	 */
	T& operator[](unsigned int index)
	{
	    return m_data[index];
	}

	/** @brief Read-only element access.
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 * @return \c const reference to the specified element.
	 */
	const T& operator[](unsigned int index) const
	{
	    return m_data[index];
	}

	/** @brief Name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 *
	 * @note This function is declared but not defined as part of
	 * the DumbVector template.  It must be defined as a
	 * specialization for each instantiation of the template for
	 * which it or typeName() is used.
	 */
	inline static const char* staticTypeName();

	// Virtual functions of RObject:
	DumbVector<T, ST>* clone() const;
	const char* typeName() const;
    protected:
	/**
	 * Declared protected to ensure that DumbVector objects are
	 * allocated only using 'new'.
	 */
	~DumbVector()
	{
	    if (m_data != &m_singleton)
		MemoryBank::deallocate(m_data, size()*sizeof(T));
	}
    private:
	friend class boost::serialization::access;
	T* m_data;  // pointer to the vector's data block.

	// If there is only one element, it is stored here, internally
	// to the DumbVector object, rather than via a separate
	// allocation from CXXR::MemoryBank.  We put this last, so that it
	// will be adjacent to any trailing redzone.
	T m_singleton;

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	DumbVector& operator=(const DumbVector&);

	// If there is more than one element, this function is used to
	// allocate the required memory block from CXXR::MemoryBank :
	void allocData(size_t sz, bool initialize = false);

	template<class Archive>
	void load(Archive & ar, const unsigned int version) {
	    ar >> boost::serialization::base_object<VectorBase>(*this);
	    if (size()>1) { // Not a singleton vector
		allocData(size());
		for (unsigned int i=0;i<size();i++)
		    ar >> m_data[i];
	    } else {
		ar >> m_singleton;
		m_data=&m_singleton;
	    }
	}

	template<class Archive>
	void save(Archive & ar, const unsigned int version) const {
	    ar << boost::serialization::base_object<VectorBase>(*this);
	    if (size()>1) // Not a singleton vector
		for (unsigned int i=0;i<size();i++)
		    ar << m_data[i];
	    else
	        ar << m_singleton;
	}

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("DumbVector");
	    boost::serialization::split_member(ar, *this, version);
	}
    };

    template <typename T, SEXPTYPE ST>
    DumbVector<T, ST>::DumbVector(const DumbVector<T, ST>& pattern)
	: VectorBase(pattern), m_data(&m_singleton),
	  m_singleton(pattern.m_singleton)
    {
	size_t sz = size();
	if (sz > 1) {
	    allocData(sz);
	    memcpy(m_data, pattern.m_data, sizeof(T)*sz);
	}
    }

    template <typename T, SEXPTYPE ST>
    void DumbVector<T, ST>::allocData(size_t sz, bool initialize)
    {
	size_t bytes = sz*sizeof(T);
	// Check for integer overflow:
	if (bytes/sizeof(T) != sz)
	    Rf_error(_("Request to create impossibly large vector."));
	m_data = static_cast<T*>(MemoryBank::allocate(bytes));
	if (initialize) {
	    for (unsigned int i = 0; i < sz; ++i)
		m_data[i] = m_singleton;
	}
#if VALGRIND_LEVEL == 1
	// For VALGRIND_LEVEL > 1 this will already have been done:
	else VALGRIND_MAKE_MEM_UNDEFINED(m_data, bytes);
#endif
    }

    template <typename T, SEXPTYPE ST>
    DumbVector<T, ST>* DumbVector<T, ST>::clone() const
    {
	return expose(new DumbVector<T, ST>(*this));
    }

    template <typename T, SEXPTYPE ST>
    const char* DumbVector<T, ST>::typeName() const
    {
	return DumbVector<T, ST>::staticTypeName();
    }
}  // namespace CXXR

#endif  // DUMBVECTOR_HPP
