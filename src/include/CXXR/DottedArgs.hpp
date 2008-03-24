/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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

/** @file DottedArgs.hpp
 * @brief Class CXXR::DottedArgs.
 */

#ifndef DOTTEDARGS_HPP
#define DOTTEDARGS_HPP

#include "CXXR/ConsCell.h"

namespace CXXR {
    /** @brief List of promises corresponding to an R ... argument
     * specification. 
     */
    class DottedArgs : public ConsCell {
    public:
	/**
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit DottedArgs(RObject* cr = 0, PairList* tl = 0, RObject* tg = 0)
	    : ConsCell(DOTSXP, cr, tl, tg)
	{}

	/** @brief Create a dotted argument list of a specified length.
	 *
	 * This constructor creates a DottedArgs list with a specified
	 * number of elements.  On creation, each element has null
	 * 'car' and 'tag'.
	 *
	 * @param sz Number of elements required in the list.  Must be
	 *           strictly positive; the constructor throws
	 *           std::out_of_range if \a sz is zero.
	 */
	explicit DottedArgs(size_t sz)
	    throw (std::bad_alloc, std::out_of_range)
	    : ConsCell(DOTSXP, sz)
	{}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "...";
	}

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	// Declared private to ensure that DottedArgs objects are
	// allocated only using 'new':
	~DottedArgs() {}

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	DottedArgs(const DottedArgs&);
	DottedArgs& operator=(const DottedArgs&);
    };
} // namespace CXXR

#endif /* DOTTEDARGS_HPP */
