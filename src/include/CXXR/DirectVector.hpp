/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-11 Andrew R. Runnalls, subject to such other
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

/** @file DirectVector.hpp
 *
 * @brief Templated class CXXR::DirectVector.
 */

#ifndef DIRECTVECTOR_HPP
#define DIRECTVECTOR_HPP 1

#include "CXXR/Allocator.hpp"
#include "CXXR/VectorBase.h"

namespace CXXR {
    /** @brief Vector of objects directly incorporated in the vector.
     *
     * This is a templated class to represent an R data vector.
     *
     * @tparam T The type of the elements of the vector.  This may be
     *           a built-in data type (i.e. not a pointer or
     *           reference), or a class type with a default
     *           constructor, a copy constructor and an assignment
     *           operator.  The elements must not incorporate pointers
     *           or references to GCNode objects.
     *
     * @tparam ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class DirectVector : public VectorBase {
    private:
	typedef std::vector<T, Allocator<T> > Vector;
    public:
	typedef T element_type;
	typedef typename Vector::const_iterator const_iterator;

	/** @brief Create a vector.
         *
	 * If \a T is a class type, elements will be initialized with
	 * the default constructor.
	 *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 */
	explicit DirectVector(size_t sz)
	    : VectorBase(ST, sz), m_data(sz)
	{}

	/** @brief Create a vector.
         *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 *
	 * @param initializer Initial value to be assigned to each
	 *          element.
	 */
	explicit DirectVector(size_t sz,  const T& initializer)
	    : VectorBase(ST, sz), m_data(sz, initializer)
	{}

	/** @brief Element access.
	 *
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 *
	 * @return the specified element.
	 */
        T& operator[](unsigned int index)
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
	const T& operator[](unsigned int index) const
	{
	    return m_data[index];
	}

	/** @brief Iterator designating first element.
	 *
	 * @return An iterator designating the first element of the
	 * DirectVector.  Returns end() if the vector is empty.
	 */
	const_iterator begin() const
	{
	    return m_data.begin();
	}

	/** @brief One-past-the-end iterator.
	 *
	 * @return An iterator designating a position 'one past the
	 * end' of the DirectVector.
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
	 * the DirectVector template.  It must be defined as a
	 * specialization for each instantiation of the template for
	 * which it or typeName() is used.
	 */
	inline static const char* staticTypeName();

	// Virtual functions of RObject:
	DirectVector<T, ST>* clone() const;
	const char* typeName() const;
    protected:
	/**
	 * Declared protected to ensure that DirectVector objects are
	 * allocated only using 'new'.
	 */
	~DirectVector() {}
    private:
	Vector m_data;

	// Not implemented.  Declared to prevent
	// compiler-generated version:
	DirectVector& operator=(const DirectVector&);
    };

    template <typename T, SEXPTYPE ST>
    DirectVector<T, ST>* DirectVector<T, ST>::clone() const
    {
	return expose(new DirectVector<T, ST>(*this));
    }

    template <typename T, SEXPTYPE ST>
    const char* DirectVector<T, ST>::typeName() const
    {
	return DirectVector<T, ST>::staticTypeName();
    }
}  // namespace CXXR

#endif  // DIRECTVECTOR_HPP
