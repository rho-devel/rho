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
#include "CXXR/GCRoot.h"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/StdFrame.hpp"
#include "CXXR/Symbol.h"

namespace CXXR {
    /** @brief Table of functions invoked \e via <tt>.Internal()</tt>.
     *
     * This class, all of whose members are static, defines a mapping
     * from Symbols to BuiltInFunction objects that are invoked \e via
     * R's <tt>.Internal()</tt> framework.
     */
    class DotInternalTable {
    public:
	/** @brief Get function accessed via <tt>.Internal()</tt>.
	 *
	 * @param sym Pointer to a Symbol.
	 *
	 * @return If \a x is associated with a function invoked in R \e
	 * via <tt>.Internal()</tt>, then a pointer to the appropriate
	 * CXXR::BuiltInFunction, otherwise a null pointer.
	 */
	static BuiltInFunction* get(const Symbol* sym)
	{
	    map::iterator it = s_table->find(sym);
	    if (it == s_table->end())
		return 0;
	    return (*it).second;
	}

	/** @brief Associate a Symbol with a <tt>.Internal()</tt> function.
	 *
	 * @param sym Pointer to a Symbol to be associated with a function.
	 *
	 * @param fun Pointer to the BuiltInFunction to be associated by
	 *          this symbol.  A null pointer is permissible, and
	 *          signifies that any previous association of \a sym with
	 *          a function is to be removed from the table.
	 */
	static void set(const Symbol* sym, BuiltInFunction* fun);
    private:
	typedef
	std::tr1::unordered_map<const Symbol*,
				GCRoot<BuiltInFunction>,
				std::tr1::hash<const Symbol*>,
				std::equal_to<const Symbol*>,
				CXXR::Allocator<std::pair<const Symbol* const,
							  GCRoot<BuiltInFunction> > >
	                        > map;

	static map* s_table;

	// Called from BuiltInFunction::initialize():
	static void initialize();

	friend class BuiltInFunction;
    };
}  // namespace CXXR

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
	return DotInternalTable::get(sym);
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
