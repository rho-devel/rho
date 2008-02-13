/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008  Andrew Runnalls
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

/** @file SEXP_downcast.hpp
 * @brief The templated function SEXP_downcast.
 */
#ifndef SEXP_DOWNCAST_HPP
#define SEXP_DOWNCAST_HPP 1

#include "R_ext/Error.h"
#include "CXXR/RObject.h"

#define USE_TYPE_CHECKING

namespace CXXR {
#ifndef USE_TYPE_CHECKING
    template <class Ptr>
    inline Ptr SEXP_downcast(SEXP s)
    {
	return static_cast<Ptr>(s);
    }
#else
    /** Down cast from RObject* to a pointer to a class derived from
     *  RObject.
     * @param Ptr Cast the pointer to type Ptr, where Ptr is a pointer
     *          or const pointer to RObject or a class derived from
     *          RObject.
     * @param s The pointer to be cast.
     * @return The cast pointer.
     */
    template <class Ptr>
    Ptr SEXP_downcast(SEXP s)
    {
	Ptr ans = dynamic_cast<Ptr>(s);
	if (!ans)
	    error("'%s' supplied where '%s' expected.",
		  s->typeName(), ans->staticTypeName());
	return ans;
    }
#endif
}  // namespace CXXR

#endif  // SEXP_DOWNCAST_HPP
