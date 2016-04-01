/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 * @brief The templated function rho::SEXP_downcast().
 */
#ifndef SEXP_DOWNCAST_HPP
#define SEXP_DOWNCAST_HPP 1

#include "rho/RObject.hpp"

namespace rho {
#ifdef __GNUC__
    __attribute__((noreturn))
#endif
    /** @brief Not for general use.
     *
     * (Used by SEXP_downcast() to report an erroneous cast.)
     */
    void SEXP_downcast_error(const char* given, const char* wanted);

    /** Down cast within the RObject class tree.
     *
     * @tparam PtrOut Cast the pointer to type \a PtrOut, where \a
     *          PtrOut is a pointer or const pointer to RObject or a
     *          class derived from RObject.
     *
     * @tparam PtrIn Cast the pointer from type \a PtrIn, where \a
     *          PtrIn is a pointer or const pointer to RObject or a
     *          class derived from RObject.  This type is usually
     *          inferred from the supplied parameter \a s.
     *
     * @param s The pointer to be cast.
     *
     * @param allow_null true iff \a s is permitted to be a null pointer.
     *
     * @return The cast pointer.
     */
#ifdef UNCHECKED_SEXP_DOWNCAST
    template <typename PtrOut, typename PtrIn>
    inline PtrOut SEXP_downcast(PtrIn s, bool allow_null = true)
    {
	if (!s && !allow_null) {
	    SEXP_downcast_error("NULL", PtrOut::staticTypeName());
	}
	return static_cast<PtrOut>(s);
    }
#else
    template <typename PtrOut, typename PtrIn>
    PtrOut SEXP_downcast(PtrIn s, bool allow_null = true)
    {
	PtrOut ans = nullptr;
	if (!s) {
	    if (allow_null)
		return nullptr;
	    else SEXP_downcast_error("NULL", ans->staticTypeName());
	}
	ans = dynamic_cast<PtrOut>(s);
	if (!ans)
	    SEXP_downcast_error(s->typeName(), ans->staticTypeName());
	return ans;
    }
#endif
}  // namespace rho

#endif  // SEXP_DOWNCAST_HPP
