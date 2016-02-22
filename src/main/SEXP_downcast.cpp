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
 *  https://www.R-project.org/Licenses/
 */

/** @file SEXP_downcast.cpp
 *
 * Error reporting for SEXP_downcast.
 */

#include "CXXR/SEXP_downcast.hpp"

#include <cstdlib>
#include "localization.h"
#include "R_ext/Error.h"

using namespace std;
using namespace CXXR;

void CXXR::SEXP_downcast_error(const char* given, const char* wanted)
{
    Rf_error(_("'%s' supplied where '%s' expected."), given, wanted);
    abort();  // To avoid warning about noreturn function returning
}
