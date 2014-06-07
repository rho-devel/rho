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

/** @file BinaryFunction.cpp
 *
 * @brief Implementation of VectorOps::BinaryFunction and related functions.
 */

#include "CXXR/BinaryFunction.hpp"

#include "CXXR/Symbol.h"

using namespace CXXR;
using namespace VectorOps;

// Implementation of operandsConformable() is in logic.cpp (for the
// time being).

void GeneralBinaryAttributeCopier::apply(VectorBase* vout,
					 const VectorBase* vl,
					 const VectorBase* vr)
{
    // Handle layout attributes:
    {
	RObject* dims = vl->getAttribute(DimSymbol);
	RObject* dimnames = 0;
	if (dims)
	    dimnames = vl->getAttribute(DimNamesSymbol);
	else
	    dims = vr->getAttribute(DimSymbol);
	if (!dimnames)
	    dimnames = vr->getAttribute(DimNamesSymbol);
	if (dims) {
	    vout->setAttribute(DimSymbol, dims);
	    if (dimnames)
		vout->setAttribute(DimNamesSymbol, dimnames);
	} else {
	    // Neither operand is an array:
	    if (vout->size() == vl->size())
		vout->setAttribute(NamesSymbol, vl->getAttribute(NamesSymbol));
	    else vout->setAttribute(NamesSymbol, vr->getAttribute(NamesSymbol));
	}
    }
    // Handle attributes related to time series:
    {
	RObject* tsp = vl->getAttribute(TspSymbol);
	RObject* klass = 0;
	if (tsp)
	    klass = vl->getAttribute(ClassSymbol);
	if (!tsp) {
	   tsp = vr->getAttribute(TspSymbol); 
	   if (tsp)
	       klass = vr->getAttribute(ClassSymbol);
	}
	if (tsp)
	    vout->setAttribute(TspSymbol, tsp);
	if (klass)
	    vout->setAttribute(ClassSymbol, klass);
    }
}
