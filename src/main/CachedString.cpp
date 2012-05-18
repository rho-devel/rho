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

/** @file CachedString.cpp
 *
 * Implementation of class CXXR::CachedString and related functions.
 */

#include "CXXR/CachedString.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	int (*IS_CACHEDp)(SEXP) = IS_CACHED;
	SEXP (*mkCharp)(const char*) = Rf_mkChar;
	SEXP (*mkCharCEp)(const char*, cetype_t) = Rf_mkCharCE;
	SEXP (*Rf_mkCharLenp)(const char*, int) = Rf_mkCharLen;
    }
}

tr1::hash<std::string> CachedString::Hasher::s_string_hasher;

CachedString::map* CachedString::s_cache = 0;
CachedString* CachedString::s_blank;
SEXP R_BlankString = 0;

CachedString::~CachedString()
{
    if (m_s11n_reloc)
	return;
    // During program exit, s_cache may already have been deleted:
    if (s_cache) {
	// Must copy the key, because some implementations may,
	// having deleted the cache entry pointed to by
	// m_key_val_pr, continue looking for other entries with
	// the given key.
	key k = m_key_val_pr->first;
	s_cache->erase(k);
    }
}

void CachedString::cleanup()
{
    // Clearing s_cache avoids valgrind 'possibly lost' reports on exit:
    s_cache->clear();
    s_cache = 0;
}

CachedString* CachedString::obtain(const std::string& str, cetype_t encoding)
{
    // This will be checked again when we actually construct the
    // CachedString, but we precheck now so that we don't create an
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
    if (CXXR::isASCII(str))
	encoding = CE_NATIVE;
    pair<map::iterator, bool> pr
	= s_cache->insert(map::value_type(key(str, encoding), 0));
    map::iterator it = pr.first;
    if (pr.second) {
	try {
	    map::value_type& val = *it;
	    val.second = expose(new CachedString(&val));
	} catch (...) {
	    s_cache->erase(it);
	    throw;
	}
    }
    return (*it).second;
}

void CachedString::initialize()
{
    static map the_map;
    s_cache = &the_map;
    static GCRoot<CachedString> blank(CachedString::obtain(""));
    s_blank = blank.get();
    R_BlankString = s_blank;
}

CachedString* CachedString::s11n_relocate() const 
{
    return m_s11n_reloc;
}

const char* CachedString::typeName() const
{
    return CachedString::staticTypeName();
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
    string str(text, length);
    return CachedString::obtain(str, encoding);
}
