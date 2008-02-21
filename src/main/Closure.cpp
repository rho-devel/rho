/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007 Andrew Runnalls.
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

/** @file Closure.cpp
 *
 * At present, this file simply forces the generation of non-inlined
 * versions of inlined functions declared in Closure.h where these
 * are intended to be callable from C.  It is also used to check that
 * Closure.h is self-contained, i.e. #includes anything it needs, and
 * doesn't rely on anything having been previously #included in the
 * enclosing source file.
 */

#include "CXXR/Closure.h"

namespace CXXR {
    namespace ForceNonInline {
	SEXP (*BODYp)(SEXP x) = BODY;
	SEXP (*CLOENVp)(SEXP x) = CLOENV;
	SEXP (*FORMALSp)(SEXP x) = FORMALS;
	void (*SET_DEBUGp)(SEXP x, Rboolean v) = SET_DEBUG;
    }
}
