/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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

/** @file String.cpp
 *
 * Implementation of class CXXR::String and related functions.
 */

#include "CXXR/String.h"

#include "CXXR/UncachedString.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	int (*HASHVALUEptr)(SEXP x) = HASHVALUE;
	Rboolean (*IS_LATIN1ptr)(const SEXP x) = IS_LATIN1;
	Rboolean (*IS_UTF8ptr)(const SEXP x) = IS_UTF8;
	const char* (*R_CHARp)(SEXP x) = R_CHAR;
    }
}

GCRoot<const String> String::s_na(new UncachedString("NA"));
SEXP R_NaString = const_cast<String*>(String::NA());

// String::s_blank and R_BlankString are defined in SpecialSymbol.cpp
// to enforce initialization order.

// String::Comparator::operator()(const String&, const String&) is in
// sort.cpp

void String::checkEncoding(unsigned int encoding)
{
    if (encoding != 0 && encoding != UTF8_MASK && encoding != LATIN1_MASK)
        error("unknown encoding mask: %d", encoding);
}

// int hash() const is in envir.cpp (for the time being)
