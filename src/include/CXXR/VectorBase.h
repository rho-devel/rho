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
 * @brief Class VectorBase and associated C interface.
 */

#ifndef VECTORBASE_H
#define VECTORBASE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

typedef CXXR::RObject VECTOR_SEXPREC, *VECSEXP;

namespace CXXR {
    /** @brief Untemplated base class for R vectors.
     */
    class VectorBase : public RObject {
    public:
	/**
	 * @param stype The required <tt>SEXPTYPE</tt>.
	 * @param sz The required number of elements in the vector.
	 */
	VectorBase(SEXPTYPE stype, size_t sz)
	    : RObject(stype), m_size(sz)
	{}

	/** @brief Alter the size (number of elements) in the vector.
	 * @param new_size New size required.  Zero is permissible,
	 *          but (as presently implemented) the new size must
	 *          not be greater than the current size. 
	 */
	void resize(size_t new_size);

	/**
	 * @return The number of elements in the vector.
	 */
	size_t size() const
	{
	    return m_size;
	}
    protected:
	~VectorBase() {}
    private:
	size_t m_size;
    };
}  // namespace CXXR

extern "C" {

#endif /* __cplusplus */

/* Accessor functions */

/* Vector Access Functions */

/**
 * @param x Pointer to an \c RObject .
 *
 * @return The length of \a x, or 0 if \a x is a null pointer, or is
 *         not a pointer to a vector object (VectorBase).  (In 
 *         the case of certain hash tables, this means the 'capacity'
 *         of \a x , not all of which may be used.)
 */
#ifndef __cplusplus
int LENGTH(SEXP x);
#else
inline int LENGTH(SEXP x)
{
    CXXR::VectorBase* vb = dynamic_cast<CXXR::VectorBase*>(x);
    return vb ? vb->size() : 0;
}
#endif

/**
 * @param x Pointer to a \c VectorBase .
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
    return reinterpret_cast<VECSEXP>(x)->u.vecsxp.truelength;
}
#endif

/**
 * Set length of vector.
 * @param x Pointer to a \c VectorBase .
 * @param v The required new length.
 */
void SETLENGTH(SEXP x, int v);

/**
 * Set 'true length' of vector.
 * @param x Pointer to a \c VectorBase .
 * @param v The required new 'true length'.
 */
#ifndef __cplusplus
void SET_TRUELENGTH(SEXP x, int v);
#else
inline void SET_TRUELENGTH(SEXP x, int v)
{
    reinterpret_cast<VECSEXP>(x)->u.vecsxp.truelength = v;
}
#endif

/**
 * @brief Create a vector object.
 *
 *  Allocate a vector object.  This ensures only validity of list-like
 *  SEXPTYPES (as the elements must be initialized).  Initializing of
 *  other vector types is done in do_makevector.
 * @param stype The type of vector required.
 * @param length The length of the vector to be created.
 * @return Pointer to the created vector.
 */
SEXP Rf_allocVector(SEXPTYPE stype, R_len_t length);

#ifdef __cplusplus
}
#endif

#endif /* VECTORBASE_H */
