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
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file BuiltInFunction.cpp
 *
 * Implementation of class CXXR::BuiltInFunction and associated
 * C interface.
 */

#include "CXXR/BuiltInFunction.h"

#include "CXXR/DotInternal.h"
#include "CXXR/Environment.h"
#include "CXXR/OrdinaryBuiltInFunction.hpp"
#include "CXXR/SpecialBuiltInFunction.hpp"
#include "CXXR/Symbol.h"

using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	const char* (*PRIMNAMEp)(SEXP x) = PRIMNAME;
	int (*PRIMOFFSETp)(SEXP x) = PRIMOFFSET;
	unsigned int (*PRIMVALp)(SEXP x) = PRIMVAL;
    }
}

// BuiltInFunction::s_function_table is defined in names.cpp

int BuiltInFunction::indexInTable(const char* name)
{
    for (int i = 0; s_function_table[i].name; ++i)
	if (strcmp(name, s_function_table[i].name) == 0)
	    return i;
    return -1;
}

void BuiltInFunction::initialize()
{
    for (int i = 0; s_function_table[i].name; ++i) {
	Symbol* sym = Symbol::obtain(s_function_table[i].name);
	BuiltInFunction* bif = BuiltInFunction::make(i);
	if ((s_function_table[i].flags%100)/10)
	    DotInternalTable::set(sym, bif);
	else
	    BaseEnvironment->frame()->obtainBinding(sym)->setValue(bif);
    }
}

BuiltInFunction* BuiltInFunction::make(unsigned int i)
{
    if (s_function_table[i].flags%10)
	return GCNode::expose(new OrdinaryBuiltInFunction(i));
    else return GCNode::expose(new SpecialBuiltInFunction(i));
}

// ***** C interface *****

SEXP mkPRIMSXP(int offset, int evaluate)
{
    using namespace CXXR;
    if (evaluate)
	return GCNode::expose(new OrdinaryBuiltInFunction(offset));
    else return GCNode::expose(new SpecialBuiltInFunction(offset));
}
