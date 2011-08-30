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

/** @file StringVector.h
 * @brief Class CXXR::StringVector and associated C interface.
 *
 * (StringVector implements STRSXP.)
 */

#ifndef STRINGVECTOR_H
#define STRINGVECTOR_H

#include "CXXR/CachedString.h"
#include "CXXR/VectorBase.h"

#ifdef __cplusplus

#include <iostream>
#include <boost/serialization/serialization.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/export.hpp>
#include "CXXR/BSerializer.hpp"
#include "CXXR/HandleVector.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    // Template specialization:
    template <>
    inline const char* HandleVector<String, STRSXP>::staticTypeName()
    {
	return "character";
    }

    /** @brief Vector of strings.
     */
    class StringVector : public CXXR::HandleVector<String, STRSXP> {
    public:
	/** @brief Create a StringVector.
	 *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 */
	explicit StringVector(size_t sz)
	    : HandleVector<String, STRSXP>(sz, CachedString::blank())
	{}

	/** @brief Copy constructor.
	 *
	 * Copy the StringVector, using the RObject::Handle copying
	 * semantics.
	 *
	 * @param pattern StringVector to be copied.
	 */
	StringVector(const StringVector& pattern)
	    : HandleVector<String, STRSXP>(pattern)
	{}

	/** @brief Default constructor
	 *
	 * For Boost Serialization
	 */
	StringVector() { }

	// Virtual function of RObject:
	StringVector* clone() const;
    protected:
	/**
	 * Declared private to ensure that StringVector objects are
	 * allocated only using 'new'.
	 */
	~StringVector() {}
    private:
	friend class boost::serialization::access;

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("StringVector");
	    ar & boost::serialization::base_object<HandleVector<String, STRSXP> >(*this);
	}
    };

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void strdump(std::ostream& os, const StringVector& sv, size_t margin = 0);
}  // namespace CXXR

//typedef CXXR::HandleVector<String, STRSXP> hv;
BOOST_CLASS_EXPORT(CXXR::StringVector)
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

/** @brief Set element of CXXR::StringVector.
 * 
 * @param x Pointer to a CXXR::StringVector .
 * @param i Index of the required element.  There is no bounds checking.
 * @param v Pointer to CXXR::RObject representing the new value.
 */
void SET_STRING_ELT(SEXP x, int i, SEXP v);

/**
 * @brief Examine element of a CXXR::StringVector.
 * @param x Pointer to a CXXR::StringVector.  An error is raised if \a x
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

#ifdef __cplusplus
}
#endif

#endif /* STRINGVECTOR_H */
