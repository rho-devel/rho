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
 * Implementation of class String and related functions.
 */

#include "CXXR/String.h"

#include "CXXR/GCRoot.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	int (*HASHVALUEptr)(SEXP x) = HASHVALUE;
	Rboolean (*IS_LATIN1ptr)(const SEXP x) = IS_LATIN1;
	Rboolean (*IS_UTF8ptr)(const SEXP x) = IS_UTF8;
	const char* (*R_CHARp)(SEXP x) = R_CHAR;
	SEXP (*Rf_allocStringp)(R_len_t) = Rf_allocString;
	void (*SET_LATIN1ptr)(SEXP x) = SET_LATIN1;
	void (*SET_UTF8ptr)(SEXP x) = SET_UTF8;
	void (*UNSET_LATIN1ptr)(SEXP x) = UNSET_LATIN1;
	void (*UNSET_UTF8ptr)(SEXP x) = UNSET_UTF8;
    }
}

// String::Comparator::operator()(const String&, const String&) is in
// sort.cpp

String::String(size_t sz)
    : VectorBase(CHARSXP, sz), m_hash(-1),
      m_databytes(sz + 1), m_data(m_short_string)
{
    if (sz > s_short_strlen)
	m_data = reinterpret_cast<char*>(MemoryBank::allocate(m_databytes));
    // Insert trailing null byte:
    m_data[sz] = 0;
}

// int hash() const is in envir.cpp (for the time being)

const char* String::typeName() const
{
    return String::staticTypeName();
}
