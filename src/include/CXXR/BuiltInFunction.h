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
 *  Andrew Runnalls (C) 2007
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

/** @file BuiltInFunction.h
 * @brief Class CXXR::BuiltInFunction and associated C interface.
 *
 * CXXR::BuiltInFunction implements BUILTINSXP and SPECIALSXP.
 */

#ifndef BUILTINFUNCTION_H
#define BUILTINFUNCTION_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    /** @brief R function implemented within the interpreter.
     *
     * This class implements BUILTINSXP and SPECIALSXP, and is
     * basically an offset into a table of functions.
     *
     * @note This is an interim implementation: expect major changes
     * in the future.
     */
    class BuiltInFunction : public RObject {
    public:
	/**
	 * @param offset The required table offset.  (Not
	 * range-checked in any way.)
	 *
	 * @param evaluate true iff this is to be a BUILTINSXP;
	 *          otherwise it will be a SPECIALSXP.
	 */
	explicit BuiltInFunction(unsigned int offset, bool evaluate = true)
	    : RObject(evaluate ? BUILTINSXP : SPECIALSXP), m_offset(offset)
	{}

	/** @brief Get table offset.
	 *
	 * @return The offset into the table of functions.
	 */
	unsigned int offset() const
	{
	    return m_offset;
	}

	/** @brief The names by which this type is known in R.
	 *
	 * @return the names by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "(builtin or special)";
	}

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	unsigned int m_offset;
    };
}  // namespace CXXR

extern "C" {
#endif

    /** @brief Get offset of a CXXR::BuiltInFunction.
     *
     * @param x Pointer to a CXXR::BuiltInFunction.
     * @return The offset of this function within the function table.
     */
#ifndef __cplusplus
    int PRIMOFFSET(SEXP x);
#else
    inline int PRIMOFFSET(SEXP x)
    {
	CXXR::BuiltInFunction& bif
	    = *CXXR::SEXP_downcast<CXXR::BuiltInFunction*>(x);
	return bif.offset();
    }
#endif

    /** @brief Create a CXXR::BuiltInFunction object.
     *
     * @param offset The required table offset.  (Not
     * range-checked in any way.)
     *
     * @param evaluate true iff this is to be a BUILTINSXP;
     *          otherwise it will be a SPECIALSXP.
     *
     * @return Pointer to the created CXXR::BuiltInFunction object.
     */
#ifndef __cplusplus
    SEXP mkPRIMSXP(int offset, int eval);
#else
    inline SEXP mkPRIMSXP(int offset, int eval)
    {
	return new CXXR::BuiltInFunction(offset, eval);
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* BUILTINFUNCTION_H */
