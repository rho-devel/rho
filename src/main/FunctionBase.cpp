/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file FunctionBase.cpp
 *
 * Class FunctionBase and associated C interface functions.
 */

#include "CXXR/FunctionBase.h"

#include <cstdarg>
#include "R_ext/Print.h"
#include "Rinternals.h"  // For Rf_PrintValue

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	int (*RTRACEptr)(SEXP x) = RTRACE;
	void (*SET_RTRACEptr)(SEXP x, int v) = SET_RTRACE;
    }
}

bool FunctionBase::s_tracing_enabled = true;

void FunctionBase::reportCall(const Expression* call)
{
    Rprintf("trace: ");
    Rf_PrintValue(const_cast<Expression*>(call));
}
