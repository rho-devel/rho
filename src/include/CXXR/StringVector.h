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

/** @file StringVector.h
 * @brief Class CXXR::StringVector and associated C interface.
 *
 * (StringVector implementis STRSXP.)
 */

#ifndef STRINGVECTOR_H
#define STRINGVECTOR_H

#include "CXXR/String.h"
#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include "CXXR/EdgeVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* EdgeVector<String*, STRSXP>::staticTypeName()
    {
	return "character";
    }

    /** @brief Vector of strings.
     * @todo Replace the type parameter RObject* with something stricter.
     */
    class StringVector : public CXXR::EdgeVector<String*, STRSXP> {
    public:
	/** @brief Create a StringVector.
	 *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 */
	explicit StringVector(size_t sz)
	    : EdgeVector<String*, STRSXP>(sz,
					  const_cast<String*>(String::blank()))
	{}
    private:
	/**
	 * Declared private to ensure that StringVector objects are
	 * allocated only using 'new'.
	 */
	~StringVector() {}
    };
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is a vector of
     *         strings.
     */
#ifndef __cplusplus
    Rboolean Rf_isString(SEXP s);
#else
    inline Rboolean Rf_isString(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == STRSXP);
    }
#endif

/** @brief Set element of StringVector.
 * 
 * @param x Pointer to an \c StringVector .
 * @param i Index of the required element.  There is no bounds checking.
 * @param v Pointer to \c RObject representing the new value.
 */
#ifndef __cplusplus
void SET_STRING_ELT(SEXP x, int i, SEXP v);
#else
inline void SET_STRING_ELT(SEXP x, int i, SEXP v)
{
    CXXR::StringVector* sv
	= CXXR::SEXP_downcast<CXXR::StringVector*>(x);
    CXXR::String* s = CXXR::SEXP_downcast<CXXR::String*>(v);
    (*sv)[i] = s;
}
#endif

/**
 * @brief Examine element of a StringVector.
 * @param x Pointer to a StringVector.  An error is raised if \a x
 *          is not a pointer to a StringVector.
 * @param i Index of the required element.  There is no bounds checking.
 * @return Pointer to extracted \a i 'th element.
 */
#ifndef __cplusplus
SEXP STRING_ELT(SEXP x, int i);
#else
inline SEXP STRING_ELT(SEXP x, int i)
{
    return (*CXXR::SEXP_downcast<CXXR::StringVector*>(x))[i];
}
#endif

/**
 * @param x Pointer to a \c StringVector; an error is raised if \a x
 *          is not a pointer to a StringVector.
 * @return Pointer to the start of \a x 's data, interpreted (riskily)
 *         as an array of String*.
 * @deprecated This function puts the integrity of the write barrier
 * at the mercy of callers.  It is deliberately not made visible
 * to C code.
 */
#ifdef __cplusplus
inline CXXR::String** STRING_PTR(SEXP x)
{
    CXXR::StringVector* sv
	= CXXR::SEXP_downcast<CXXR::StringVector*>(x);
    return sv->dataPtr();
}
#endif

#ifdef __cplusplus
}
#endif

#endif /* STRINGVECTOR_H */
