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

/** @file DotInternal.h
 *
 * @brief Table of functions invoked \e via <tt>.Internal()</tt>.
 */

#ifndef DOTINTERNAL_H
#define DOTINTERNAL_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/BuiltInFunction.h"
#include "CXXR/SEXP_downcast.hpp"

extern "C" {
#endif

    /** @brief Get function accessed via <tt>.Internal()</tt>.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return If \a x is associated with a function invoked in R \e
     * via <tt>.Internal()</tt>, then a pointer to the appropriate
     * CXXR::BuiltInFunction, otherwise a null pointer.
     */
#ifndef __cplusplus
    SEXP INTERNAL(SEXP x);
#else
    inline SEXP INTERNAL(SEXP x)
    {
	using namespace CXXR;
	const Symbol* sym = SEXP_downcast<Symbol*>(x);
	return BuiltInFunction::obtainInternal(sym);
    }
#endif

    /** @brief Associate a Symbol with a <tt>.Internal()</tt> function.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param v Pointer to the CXXR::BuiltInFunction (checked) to be
     * associated by this symbol.  A null pointer is permissible, and
     * signifies that any previous association of \a sym with a
     * function is to be removed from the table.
     */
    void SET_INTERNAL(SEXP x, SEXP v);

#ifdef __cplusplus
}
#endif

#endif /* DOTINTERNAL_H */
