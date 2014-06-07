/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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

/** @file S4Object.h
 *
 * @brief Class CXXR::S4Object and associated C interface.
 *
 * (S4Object implements S4SXP.)
 */

#ifndef S4OBJECT_H
#define S4OBJECT_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/nvp.hpp>

#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    /** @brief S4 object.
     *
     * This class is used to implement S4 classes that do not extend
     * objects of another R type, and corresponds to the ::SEXPTYPE
     * S4SXP.
     *
     * @note The 'R Internals' document says that S4SXP objects have a
     * tag field.  This is not currently implemented in CXXR.
     */
    class S4Object : public RObject {
    public:
	/** @brief Default constructor.
	 */
	S4Object()
	    : RObject(S4SXP)
	{
	    setS4Object(true);
	}

	/** @brief Copy constructor.
	 *
	 * @param pattern S4Object to be copied.
	 */
	S4Object(const S4Object& pattern)
	    : RObject(pattern)
	{}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "S4";
	}

	// Virtual functions of RObject:
	S4Object* clone() const;
	const char* typeName() const;
    private:
	friend class boost::serialization::access;

	// Declared private to ensure that S4Objects are allocated
	// only using 'new':
	~S4Object() {}

	// Not implemented.  Declared to prevent compiler-generated version:
        S4Object& operator=(const S4Object&);

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(RObject);
	}
    };
} // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::S4Object)

extern "C" {
#endif  /* __cplusplus */

    /** @brief Create an S4 object.
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocS4Object();

#ifdef __cplusplus
}
#endif

#endif /* S4OBJECT_H */
