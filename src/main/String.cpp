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

/** @file String.cpp
 *
 * Implementation of class String and related functions.
 */

#include "CXXR/String.h"

#include "CXXR/GCRoot.h"

using namespace std;
using namespace CXXR;

namespace {
    int (*hashvalueptr)(SEXP x) = HASHVALUE;
    Rboolean (*islatin1ptr)(const SEXP x) = IS_LATIN1;
    Rboolean (*isutf8ptr)(const SEXP x) = IS_UTF8;
    const char* (*R_CHARp)(SEXP x) = R_CHAR;
    SEXP (*Rf_allocStringp)(R_len_t) = Rf_allocString;
    void (*setlatin1ptr)(SEXP x) = SET_LATIN1;
    void (*setutf8ptr)(SEXP x) = SET_UTF8;
    void (*unsetlatin1ptr)(SEXP x) = UNSET_LATIN1;
    void (*unsetutf8ptr)(SEXP x) = UNSET_UTF8;
}

// String::Comparator::operator()(const String&, const String&) is in
// sort.cpp

String::String(size_t sz)
    : VectorBase(CHARSXP, sz), m_hash(-1),
      m_databytes(sz + 1), m_data(m_short_string)
{
    if (sz > s_short_strlen)
	m_data = reinterpret_cast<char*>(Heap::allocate(m_databytes));
    // Insert trailing null byte:
    m_data[sz] = 0;
}

// int hash() const is in envir.cpp (for the time being)

const char* String::typeName() const
{
    return String::staticTypeName();
}
