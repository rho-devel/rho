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

#include <tr1/unordered_map>
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
	 * @param encoding The encoding of the required CachedString.
	 *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted
	 *          in this context (checked).
	 *
	 * @return Pointer to a CachedString representing the
	 *         specified text in the specified encoding.
	 */
	static const CachedString* obtain(const std::string& str,
					  cetype_t encoding = CE_NATIVE);

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
	typedef std::pair<std::string, cetype_t> key;

	// Hashing is based simply on the text of the key, not on its
	// encoding:
	class Hasher : public std::unary_function<key, std::size_t> {
	public:
	    std::size_t operator()(const key& k) const
	    {
		return s_string_hasher(k.first);
	    }
	private:
	    static std::tr1::hash<std::string> s_string_hasher;
	};

	// The cache is implemented as a mapping from keys to pointers
	// to CachedString objects.  Each CachedString simply contains
	// an pointer locating its entry within the cache.  Note that
	// we cannot use CXXR::Allocator here, because when creating a
	// new CachedString, the call to insert() might lead to a
	// garbage collection, which in turn might lead to a call to
	// erase() before the insert() was complete.  (Yes, I tried
	// this, and it took ages to debug!)
	typedef std::tr1::unordered_map<key, CachedString*, Hasher> map;

	map::value_type* m_key_val_pr;

	explicit CachedString(map::value_type* key_val_pr)
	    : String(key_val_pr->first.first.size(), key_val_pr->first.second,
		     key_val_pr->first.first.c_str()),
	      m_key_val_pr(key_val_pr)
	{}

	// Not implemented.  Declared to prevent
	// compiler-generated versions:
	CachedString(const CachedString&);
	CachedString& operator=(const CachedString&);

	// Declared private to ensure that CachedString objects are
	// allocated only using 'new'.
	~CachedString()
	{
	    // Must copy the key, because some implementations may,
	    // having deleted the cache entry pointed to by
	    // m_key_val_pr, continue looking for other entries with
	    // the given key.
	    key k = m_key_val_pr->first;
	    cache()->erase(k);
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
     *
     * @return Pointer to a string object representing the specified
     *         text.
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
     * @param encoding The encoding of the required CachedString.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to a string object representing the specified
     *         text in the specified encoding.
     */
#ifndef __cplusplus
    SEXP Rf_mkCharCE(const char * str, cetype_t encoding);
#else
    inline SEXP Rf_mkCharCE(const char * str, cetype_t encoding)
    {
	using namespace CXXR;
	return const_cast<CachedString*>(CachedString::obtain(str, encoding));
    }
#endif

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* CACHEDSTRING_H */
