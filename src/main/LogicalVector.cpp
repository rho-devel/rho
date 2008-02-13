/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008 Andrew Runnalls.
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

/** @file LogicalVector.cpp
 *
 * Implementation of class LogicalVector and related functions.
 */

#include "CXXR/LogicalVector.h"

#include "CXXR/IntVector.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace {
    Rboolean (*isLogicalptr)(SEXP s) = Rf_isLogical;
}

int *LOGICAL(SEXP x)
{
#ifndef USE_TYPE_CHECKING_STRICT
    if (IntVector* ivec = dynamic_cast<IntVector*>(x))
	return &(*ivec)[0];
#endif
    return &(*CXXR::SEXP_downcast<LogicalVector*>(x))[0];
}
