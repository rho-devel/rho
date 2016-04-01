/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file StringVector.h
 * @brief Class rho::StringVector and associated C interface.
 *
 * (StringVector implements STRSXP.)
 */

#ifndef STRINGVECTOR_H
#define STRINGVECTOR_H

#include "Rinternals.h"

#ifdef __cplusplus

#include <iostream>

#include "rho/FixedVector.hpp"
#include "rho/RHandle.hpp"
#include "rho/SEXP_downcast.hpp"
#include "rho/String.hpp"

namespace rho {
    /** @brief Vector of strings.
     *
     * Note that the <tt>StringVector(size_type)</tt> constructor will
     * fill the constructed vector with blank strings rather than
     * with NULL.
     */
    typedef FixedVector<RHandle<String>, STRSXP> StringVector;

    /** @brief Create a StringVector containing a single std::string.
     *
     * This constructor constructs a StringVector containing a single
     * element, and initializes that element to represent a specified
     * string and encoding.
     *
     * @param str The required text of the single vector element.
     *
     * @param encoding The required encoding of the single vector
     *          element.  Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are
     *          permitted in this context (checked).
     */
    inline StringVector* asStringVector(const std::string& str,
					cetype_t encoding = CE_NATIVE)
    {
	GCStackRoot<String> cs(String::obtain(str, encoding));
	return StringVector::createScalar(cs);
    }

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void strdump(std::ostream& os, const StringVector& sv,
		 std::size_t margin = 0);
}  // namespace rho

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

/** @brief Set element of rho::StringVector.
 * 
 * @param x Non-null pointer to a rho::StringVector .
 *
 * @param i Index of the required element.  There is no bounds checking.
 *
 * @param v Non-null pointer to rho::String representing the new value.
 */
void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);

/**
 * @brief Examine element of a rho::StringVector.
 *
 * @param x Non-null pointer to a rho::StringVector.  An error is
 *          raised if \a x is not a pointer to a rho::StringVector.
 *
 * @param i Index of the required element.  There is no bounds
 *          checking.
 *
 * @return Pointer to extracted \a i 'th element.
 */
#ifndef __cplusplus
SEXP STRING_ELT(SEXP x, R_xlen_t i);
#else
inline SEXP STRING_ELT(SEXP x, R_xlen_t i)
{
    using namespace rho;
    if(TYPEOF(x) != STRSXP)
      Rf_error("%s() can only be applied to a '%s', not a '%s'",
               "STRING_ELT", "character vector", Rf_type2char(TYPEOF(x)));
    return (*SEXP_downcast<StringVector*>(x, false))[VectorBase::size_type(i)];
}
#endif

#ifdef COMPILING_RADIXSORT
SEXP* StringVectorDataPtr(SEXP string_vector);
#endif

#ifdef __cplusplus
}
#endif

#endif /* STRINGVECTOR_H */
