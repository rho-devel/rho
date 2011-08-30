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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2008
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

/** @file VectorBase.h
 * @brief Class CXXR::VectorBase and associated C interface.
 */

#ifndef VECTORBASE_H
#define VECTORBASE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include "CXXR/BSerializer.hpp"
#include "CXXR/SEXP_downcast.hpp"

typedef CXXR::RObject VECTOR_SEXPREC, *VECSEXP;

namespace CXXR {
    /** @brief Untemplated base class for R vectors.
     */
    class VectorBase : public RObject {
    public:
	/**
	 * @param stype The required ::SEXPTYPE.
	 * @param sz The required number of elements in the vector.
	 */
	VectorBase(SEXPTYPE stype, size_t sz)
	    : RObject(stype), m_truelength(sz), m_size(sz)
	{}

	/** @brief Copy constructor.
	 *
	 * @param pattern VectorBase to be copied.
	 */
	VectorBase(const VectorBase& pattern)
	    : RObject(pattern), m_truelength(pattern.m_truelength),
	      m_size(pattern.m_size)
	{}

	/** @default constructor for boost::serialization
	 *
	 */
	VectorBase() { }

	/** @brief Alter the size (number of elements) in the vector.
	 *
	 * @param new_size New size required.  Zero is permissible,
	 *          but (as presently implemented) the new size must
	 *          not be greater than the current size. 
	 */
	void resize(size_t new_size);

	/** @brief Number of elements in the vector.
	 *
	 * @return The number of elements in the vector.
	 */
	size_t size() const
	{
	    return m_size;
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "(vector type)";
	}

	// Make private in due course (or get rid altogether):
	R_len_t m_truelength;
    protected:
	~VectorBase() {}
    private:
	friend class boost::serialization::access;

	size_t m_size;

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("VectorBase");
	    ar & boost::serialization::base_object<RObject>(*this);
	    ar & m_truelength;
	    ar & m_size;
	}
    };
}  // namespace CXXR

extern "C" {

#endif /* __cplusplus */

/* Accessor functions */

/* Vector Access Functions */

/**
 * @param x Pointer to an CXXR::VectorBase .
 *
 * @return The length of \a x, or 0 if \a x is a null pointer.  (In 
 *         the case of certain hash tables, this means the 'capacity'
 *         of \a x , not all of which may be used.)
 */
#ifndef __cplusplus
int LENGTH(SEXP x);
#else
inline int LENGTH(SEXP x)
{
    using namespace CXXR;
    if (!x) return 0;
    VectorBase& vb = *SEXP_downcast<VectorBase*>(x);
    return vb.size();
}
#endif

/**
 * @param x Pointer to a CXXR::VectorBase .
 *
 * @return The 'true length' of \a x.  According to the R Internals
 *         document for R 2.4.1, this is only used for certain hash
 *         tables, and signifies the number of used slots in the
 *         table.
 */
#ifndef __cplusplus
int TRUELENGTH(SEXP x);
#else
inline int TRUELENGTH(SEXP x)
{
    using namespace CXXR;
    VectorBase& vb = *SEXP_downcast<VectorBase*>(x);
    return vb.m_truelength;
}
#endif

/**
 * Set length of vector.
 * @param x Pointer to a CXXR::VectorBase .
 * @param v The required new length.
 */
void SETLENGTH(SEXP x, int v);

/**
 * Set 'true length' of vector.
 * @param x Pointer to a CXXR::VectorBase .
 * @param v The required new 'true length'.
 */
#ifndef __cplusplus
void SET_TRUELENGTH(SEXP x, int v);
#else
inline void SET_TRUELENGTH(SEXP x, int v)
{
    using namespace CXXR;
    VectorBase& vb = *SEXP_downcast<VectorBase*>(x);
    vb.m_truelength = v;
}
#endif

/**
 * @brief Create a vector object.
 *
 *  Allocate a vector object.  This ensures only validity of
 *  ::SEXPTYPE values representing lists (as the elements must be
 *  initialized).  Initializing of other vector types is done in
 *  do_makevector(). 
 * @param stype The type of vector required.
 * @param length The length of the vector to be created.
 * @return Pointer to the created vector.
 */
SEXP Rf_allocVector(SEXPTYPE stype, R_len_t length);

#ifdef __cplusplus
}
#endif

#endif /* VECTORBASE_H */
