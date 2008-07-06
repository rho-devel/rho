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

/** @file CachedString.cpp
 *
 * Implementation of class CXXR::CachedString and related functions.
 */

#include "CXXR/CachedString.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	SEXP (*mkCharp)(const char*) = Rf_mkChar;
	SEXP (*mkCharEncp)(const char*, int) = Rf_mkCharEnc;
    }
}

tr1::hash<std::string> CachedString::Hasher::s_string_hasher;

CachedString::map* CachedString::cache()
{
    static map the_cache;
    return &the_cache;
}

const CachedString* CachedString::obtain(const std::string& str,
					 unsigned int encoding)
{
    // This will be checked again when we actually construct the
    // CachedString, but we precheck now so that we don't create an
    // invalid cache key:
    if (encoding != 0 && encoding != UTF8_MASK && encoding != LATIN1_MASK)
        error("unknown encoding mask: %d", encoding);
    pair<map::iterator, bool> pr
	= cache()->insert(map::value_type(key(str, encoding), 0));
    map::iterator it = pr.first;
    if (pr.second) {
	try {
	    map::value_type& val = *it;
	    val.second = new CachedString(&val);
	    val.second->expose();
	} catch (...) {
	    cache()->erase(it);
	    throw;
	}
    }
    return (*it).second;
}
 
const char* CachedString::typeName() const
{
    return CachedString::staticTypeName();
}
