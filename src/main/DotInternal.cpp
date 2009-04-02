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

/** @file DotInternal.cpp
 *
 * @brief Table of functions invoked \e via <tt>.Internal()</tt>.
 */

#include "CXXR/DotInternal.h"

#include "CXXR/BuiltInFunction.h"
#include "CXXR/CachedString.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	SEXP (*INTERNALp)(SEXP x) = INTERNAL;
    }
}

DotInternalTable::map DotInternalTable::s_table;

void DotInternalTable::set(const Symbol* sym, BuiltInFunction* fun)
{
    if (!fun)
	s_table.erase(sym);
    else
	s_table[sym] = fun;
}


// ***** C interface *****

void SET_INTERNAL(SEXP x, SEXP v)
{
    const Symbol* sym = SEXP_downcast<Symbol*>(x);
    BuiltInFunction* fun = SEXP_downcast<BuiltInFunction*>(v);
    DotInternalTable::set(sym, fun);
}
