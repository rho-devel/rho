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
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file IntVector.cpp
 *
 * Implementation of class IntVector and related functions.
 */

#include "CXXR/IntVector.h"

#include "CXXR/LogicalVector.h"

using namespace std;
using namespace CXXR;

int *INTEGER(SEXP x)
{
#ifndef USE_TYPE_CHECKING_STRICT
    if (LogicalVector* lvec = dynamic_cast<LogicalVector*>(x))
	return &(*lvec)[0];
#endif
    return &(*CXXR::SEXP_downcast<IntVector*>(x))[0];
}
