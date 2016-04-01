/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file String.cpp
 *
 * Implementation of class rho::String and related functions.
 */

#include "rho/String.hpp"

#include <algorithm>
#include <boost/lambda/lambda.hpp>

#include "rho/errors.hpp"

using namespace rho;

namespace rho {
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

SEXP R_NaString = nullptr;
SEXP R_BlankString = nullptr;

// String::Comparator::operator()(const String*, const String*) is in
// sort.cpp

String::String(char* character_storage,
	       const std::string& text, cetype_t encoding, bool isAscii)
    : VectorBase(CHARSXP, text.size()),
      m_key_val_pr(nullptr),
      m_encoding(encoding),
      m_symbol(nullptr),
      m_ascii(isAscii)
{
    memcpy(character_storage, text.data(), text.size());
    character_storage[text.size()] = '\0';  // Null terminated.
    m_data = character_storage;
    assert(m_data);

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

String* String::create(const std::string& text, cetype_t encoding, bool isAscii)
{
    size_t size = sizeof(String) + text.size() + 1;
    void* storage = GCNode::operator new(size);
    char* character_storage = (char*)storage + sizeof(String);
    String* result = new(storage) String(character_storage, text, encoding,
					 isAscii);
    return result;
}

String* String::createNA()
{
    return String::create("NA", CE_NATIVE, true);
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
    // GCNode::~GCNode doesn't know about the string storage space in this
    // object, so account for it here.
    size_t bytes = size() + 1;
    MemoryBank::adjustBytesAllocated(-bytes);
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

bool rho::isASCII(const std::string& str)
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
    bool ascii = rho::isASCII(str);
    if (ascii)
	encoding = CE_NATIVE;
    std::pair<map::iterator, bool> pr
	= getCache()->insert(map::value_type(key(str, encoding), nullptr));
    map::iterator it = pr.first;
    if (pr.second) {
	try {
	    map::value_type& val = *it;
	    val.second = String::create(str, encoding, ascii);
	    val.second->m_key_val_pr = &*it;
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
    if (!text)
	text = "";

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
