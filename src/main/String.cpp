/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-12 Andrew R. Runnalls, subject to such other
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

#include <algorithm>
#include "boost/lambda/lambda.hpp"
#include "R_ext/Error.h"
#include "CXXR/UncachedString.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	int (*HASHVALUEp)(SEXP x) = HASHVALUE;
	int (*ENC_KNOWNp)(const SEXP x) = ENC_KNOWN;
	int (*IS_BYTESp)(const SEXP x) = IS_BYTES;
	Rboolean (*IS_LATIN1p)(const SEXP x) = IS_LATIN1;
	Rboolean (*IS_UTF8p)(const SEXP x) = IS_UTF8;
	const char* (*R_CHARp)(SEXP x) = R_CHAR;
    }
}

GCRoot<String> String::s_na(expose(new UncachedString("NA", CE_NATIVE)));
SEXP R_NaString = String::NA();

// String::s_blank and R_BlankString are defined in Symbol.cpp to
// enforce initialization order.

String::String(size_t sz, cetype_t encoding)
    : VectorBase(CHARSXP, sz), m_c_str(0), m_hash(-1)
{
    switch(encoding) {
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
	m_encoding = encoding;
	break;
    default:
	error("character encoding %d not permitted here", encoding);
    }
}

// String::Comparator::operator()(const String*, const String*) is in
// sort.cpp

// int hash() const is in envir.cpp (for the time being)

namespace {
    // Used in GPBits2Encoding() and packGPBits():
    const unsigned int BYTES_MASK = 1<<1;
    const unsigned int LATIN1_MASK = 1<<2;
    const unsigned int UTF8_MASK = 1<<3;
}

cetype_t String::GPBits2Encoding(unsigned int gpbits)
{
    if ((gpbits & LATIN1_MASK) != 0)
	return CE_LATIN1;
    if ((gpbits & UTF8_MASK) != 0)
	return CE_UTF8;
    if ((gpbits & BYTES_MASK) != 0)
	return CE_BYTES;
    return CE_NATIVE;
}

unsigned int String::packGPBits() const
{
    unsigned int ans = VectorBase::packGPBits();
    switch (m_encoding) {
    case CE_UTF8:
	ans |= UTF8_MASK;
	break;
    case CE_LATIN1:
	ans |= LATIN1_MASK;
	break;
    case CE_BYTES:
	ans |= BYTES_MASK;
	break;
    default:
	break;
    }
    return ans;
}

bool CXXR::isASCII(const std::string& str)
{
    using namespace boost::lambda;
    std::string::const_iterator it
	= std::find_if(str.begin(), str.end(), _1 > 0x7F);
    return it == str.end();
}
