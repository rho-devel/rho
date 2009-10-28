/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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

/** @file OrdinaryBuiltInFunction.h
 * @brief Class CXXR::OrdinaryBuiltInFunction.
 *
 * CXXR::OrdinaryBuiltInFunction BUILTINSXP.
 */

#ifndef ORDINARYBUILTINFUNCTION_H
#define ORDINARYBUILTINFUNCTION_H

#include "CXXR/BuiltInFunction.h"

namespace CXXR {
    /** @brief Built-in function taking unevaluated arguments.
     *
     * This class implements BUILTINSXP, and represents a built-in
     * function whose arguments are not evaluated before being passed
     * to the underlying C/C++ function.
     */
    class OrdinaryBuiltInFunction : public BuiltInFunction {
    public:
	/**
	 * @param offset The required table offset.  (Not
	 * range-checked in any way.)
	 */
	explicit OrdinaryBuiltInFunction(unsigned int offset)
	    : BuiltInFunction(offset, true)
	{}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "builtin";
	}

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	// Virtual function of FunctionBase:
	RObject* innerApply(Expression* call, Environment* env);
    };
}  // namespace CXXR

#endif /* ORDINARYBUILTINFUNCTION_H */
