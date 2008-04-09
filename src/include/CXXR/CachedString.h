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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file CachedString.h
 * @brief Class CXXR::CachedString and associated C interface.
 */

#ifndef CACHEDSTRING_H
#define CACHEDSTRING_H

#include "CXXR/String.h"

#ifdef __cplusplus

#include <map>
#include <string>

#include "CXXR/Allocator.hpp"

namespace CXXR {
    /** @brief String object held in a cache.
     *
     * At any one time, at most one CachedString object with a
     * particular text and encoding may exist.
     */
    class CachedString : public String {
    public:
	/** @brief Get a pointer to a CachedString object.
	 *
	 * If no CachedString with the specified text and encoding
	 * currently exists, one will be created.  Otherwise a pointer
	 * to the existing CachedString will be returned.
	 *
	 * @param str The text of the required CachedString.
	 *          (Embedded null characters are permissible.)
	 *
	 * @param encoding The encoding of the required CachedString,
	 *          as  indicated by the LATIN1_MASK and UTF8_MASK
	 *          bits.  Zero signifies ASCII encoding, and at most
	 *          one of the MASK bits may be set (checked).
	 */
	static const CachedString* obtain(const std::string& str,
					  unsigned int encoding = 0);

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "char (cached)";
	}

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	// The first element of the key is the text, the second
	// element the encoding:
	typedef std::pair<std::string, unsigned int> key;

	// The cache is implemented as a mapping from keys to pointers
	// to CachedString objects.  Each CachedString simply contains an
	// iterator locating its text and encoding within the cache.
	// In the future this may be changed to a TR1 unordered_map.
	typedef std::map<key, CachedString*, std::less<key>,
			 Allocator<std::pair<const key, CachedString*> > > map;

	map::iterator m_it;

	explicit CachedString(map::iterator iter)
	    : String((*iter).first.first.size(), (*iter).first.second,
		     (*iter).first.first.c_str()),
	      m_it(iter)
	{}

	// Not implemented.  Declared to prevent
	// compiler-generated versions:
	CachedString(const CachedString&);
	CachedString& operator=(const CachedString&);

	// Declared private to ensure that CachedString objects are
	// allocated only using 'new'.
	~CachedString()
	{
	    cache()->erase(m_it);
	}

	// Return pointer to the cache:
	static map* cache();
    };
}  // namespace CXXR

extern "C" {

#endif /* __cplusplus */

    /** @brief Get a pointer to a cached ASCII string object.
     *
     * If no cached string with the specified text currently exists,
     * one will be created.  Otherwise a pointer to the existing
     * cached string will be returned.
     *
     * @param str The text of the required cached string.
     */
#ifndef __cplusplus
    SEXP Rf_mkChar(const char * str);
#else
    inline SEXP Rf_mkChar(const char * str)
    {
	return
	    const_cast<CXXR::CachedString*>(CXXR::CachedString::obtain(str));
    }
#endif
    
    /** @brief Get a pointer to a cached string object.
     *
     * If no cached string with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer
     * to the existing cached string will be returned.
     *
     * @param str The text of the required cached string.
     *
     * @param encoding The encoding of the required CachedString,
     *          as  indicated by the LATIN1_MASK and UTF8_MASK
     *          bits.  Zero signifies ASCII encoding, and at most
     *          one of the MASK bits may be set (checked).
     */
#ifndef __cplusplus
    SEXP Rf_mkCharEnc(const char * str, int encoding);
#else
    inline SEXP Rf_mkCharEnc(const char * str, int encoding)
    {
	return
	    const_cast<CXXR::CachedString*>(CXXR::CachedString::obtain(str,
								       encoding));
    }
#endif

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* CACHEDSTRING_H */
