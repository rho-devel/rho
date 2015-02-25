/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file UnaryFunction.cpp
 *
 * @brief Implementation of VectorOps::UnaryFunction and related functions.
 */

#include "CXXR/UnaryFunction.hpp"

#include "CXXR/Symbol.h"

using namespace CXXR;
using namespace VectorOps;

void CopyLayoutAttributes::copyAttributes(VectorBase* to,
					  const VectorBase* from)
{
    RObject* names = from->getAttribute(NamesSymbol);
    if (names)
	to->setAttribute(NamesSymbol, names);
    RObject* dim = from->getAttribute(DimSymbol);
    if (dim) {
	to->setAttribute(DimSymbol, dim);
	RObject* dimnames = from->getAttribute(DimNamesSymbol);
	if (dimnames)
	    to->setAttribute(DimNamesSymbol, dimnames);
    }
}
