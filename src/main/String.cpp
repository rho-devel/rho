/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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
#include <boost/lambda/lambda.hpp>

#include "CXXR/errors.h"

using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	int (*ENC_KNOWNp)(const SEXP x) = ENC_KNOWN;
	int (*IS_ASCIIp)(const SEXP x) = IS_ASCII;
	int (*IS_BYTESp)(const SEXP x) = IS_BYTES;
	Rboolean (*IS_LATIN1p)(const SEXP x) = IS_LATIN1;
	Rboolean (*IS_UTF8p)(const SEXP x) = IS_UTF8;
	const char* (*R_CHARp)(SEXP x) = R_CHAR;
	SEXP (*mkCharp)(const char*) = Rf_mkChar;
	SEXP (*mkCharCEp)(const char*, cetype_t) = Rf_mkCharCE;
	SEXP (*mkCharLenp)(const char*, int) = Rf_mkCharLen;
    }
}
std::hash<std::string> String::Hasher::s_string_hasher;

static std::string* getNaString()
{
    static std::string* na_string = new std::string("NA");
    return na_string;
}

SEXP R_NaString = nullptr;
SEXP R_BlankString = nullptr;

// String::Comparator::operator()(const String*, const String*) is in
// sort.cpp

String::String(map::value_type* key_val_pr)
    : VectorBase(CHARSXP, key_val_pr ? key_val_pr->first.first.size() : 2),
      m_key_val_pr(key_val_pr), m_string(getNaString()), m_encoding(CE_NATIVE),
      m_symbol(nullptr)
{
    if (key_val_pr) {
	m_string = &key_val_pr->first.first;
	m_encoding = key_val_pr->first.second;
	switch(m_encoding) {
	case CE_NATIVE:
	case CE_UTF8:
	case CE_LATIN1:
	case CE_BYTES:
	    break;
	default:
	    Rf_error("character encoding %d not permitted here", m_encoding);
	}
    }
}

String::~String()
{
    // m_key_val_pr is null for serialization proxies.
    if (m_key_val_pr) {
	// Must copy the key, because some implementations may,
	// having deleted the cache entry pointed to by
	// m_key_val_pr, continue looking for other entries with
	// the given key.
	key k = m_key_val_pr->first;
	getCache()->erase(k);
    }
}

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

void String::initialize()
{
    R_NaString = NA();
    R_BlankString = blank();
}

String::map* String::getCache()
{
    static map* cache = new map();
    return cache;
}

bool CXXR::isASCII(const std::string& str)
{
    using namespace boost::lambda;
    // Beware of the iterator dereferencing to a *signed* char, hence
    // the bitwise test:
    std::string::const_iterator it
	= std::find_if(str.begin(), str.end(), _1 & 0x80);
    return it == str.end();
}

String* String::obtain(const std::string& str, cetype_t encoding)
{
    // This will be checked again when we actually construct the
    // String, but we precheck now so that we don't create an
    // invalid cache key:
    switch(encoding) {
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
	break;
    default:
        Rf_error("unknown encoding: %d", encoding);
    }
    bool ascii = CXXR::isASCII(str);
    if (ascii)
	encoding = CE_NATIVE;
    std::pair<map::iterator, bool> pr
	= getCache()->insert(map::value_type(key(str, encoding), nullptr));
    map::iterator it = pr.first;
    if (pr.second) {
	try {
	    map::value_type& val = *it;
	    val.second = new String(&val);
	    val.second->m_ascii = ascii;
	} catch (...) {
	    getCache()->erase(it);
	    throw;
	}
    }
    return (*it).second;
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

const char* String::typeName() const
{
    return String::staticTypeName();
}

// ***** C interface *****

SEXP Rf_mkCharLenCE(const char* text, int length, cetype_t encoding)
{
    switch(encoding) {
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
    case CE_SYMBOL:
    case CE_ANY:
	break;
    default:
	Rf_error(_("unknown encoding: %d"), encoding);
    }
    std::string str(text, length);
    return String::obtain(str, encoding);
}

// Needed for the instantiation in BOOST_CLASS_EXPORT_IMPLEMENT:
#include "CXXR/PairList.h"

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::String)
