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
#include "CXXR/SchwarzCounter.hpp"

namespace CXXR {
    /** @brief String object held in a cache.
     *
     * At any one time, at most one CachedString object with a
     * particular text and encoding may exist.
     */
    class CachedString : public String {
    public:
	/** @brief Blank string.
	 * @return <tt>const</tt> pointer to the string "".
	 */
	static CachedString* blank()
	{
	    return s_blank;
	}

	/** @brief Get a pointer to a CachedString object.
	 *
	 * If no CachedString with the specified text and encoding
	 * currently exists, one will be created, and a pointer to it
	 * returned.  Otherwise a pointer to the existing CachedString
	 * will be returned.
	 *
	 * @param str The text of the required CachedString.
	 *          (Embedded null characters are permissible.)
	 *
	 * @param encoding The encoding of the required CachedString.
	 *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted
	 *          in this context (checked).  Note that if \a str
	 *          contains no non-ASCII characters, then the
	 *          encoding is set to CE_NATIVE regardless of the
	 *          value of the \a encoding parameter.
	 *
	 * @return Pointer to a CachedString (preexisting or newly
	 * created) representing the specified text in the specified
	 * encoding.
	 */
	static CachedString* obtain(const std::string& str,
				    cetype_t encoding = CE_NATIVE);

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "char (cached)";
	}

	/** @brief Access encapsulated std::string.
	 *
	 * @return Reference to the encapsulated std::string.
	 */
	const std::string& stdstring() const
	{
	    return m_key_val_pr->first.first;
	}

	// Virtual function of GCNode:
	CachedString* s11n_relocate() const;

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	friend class boost::serialization::access;
	friend class SchwarzCounter<CachedString>;
	friend class Symbol;

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
	// a pointer locating its entry within the cache.
	typedef
	std::tr1::unordered_map<key, CachedString*, Hasher,
				std::equal_to<key>,
				CXXR::Allocator<std::pair<const key,
							  CachedString*> >
	                        > map;

	static map* s_cache;
	static CachedString* s_blank;

	map::value_type* m_key_val_pr;
	mutable Symbol* m_symbol;  // Pointer to the Symbol object identified
	  // by this CachedString, or a null pointer if none.
	std::string* m_s11n_string;  // Used only in temporary objects
	  // created during deserialisation.

	// This is used during (boost) deserialisation to construct a
	// bodged-up temporary object.  The subsequent call to
	// s11n_relocate() will then request that this be replaced by
	// a pukka object returned by CachedString::obtain().  (The
	// arguments to the String base-class constructor are
	// arbitrary, but will not be used during the lifetime of this
	// temporary object.)
	CachedString()
	    : String(0, CE_NATIVE), m_key_val_pr(0), m_s11n_string(0)
	{
	    m_s11n_string = new std::string;
	}

	explicit CachedString(map::value_type* key_val_pr)
	    : String(key_val_pr->first.first.size(), key_val_pr->first.second),
	    m_key_val_pr(key_val_pr), m_symbol(0), m_s11n_string(0)
	{
	    setCString(key_val_pr->first.first.c_str());
	}

	// Not implemented.  Declared to prevent
	// compiler-generated versions:
	CachedString(const CachedString&);
	CachedString& operator=(const CachedString&);

	// Declared private to ensure that CachedString objects are
	// allocated only using 'new'.
	~CachedString();

	static void cleanup();

	// Initialize the static data members:
	static void initialize();

	template<class Archive>
	void load(Archive & ar, const unsigned int version);

	template<class Archive>
	void save(Archive & ar, const unsigned int version) const;

	// Fields not serialised here are set up by the constructor:
	template <class Archive>
	void serialize(Archive& ar, const unsigned int version) {
	    BSerializer::Frame frame("CachedString");
	    boost::serialization::split_member(ar, *this, version);
	}
    };
}  // namespace CXXR

BOOST_CLASS_EXPORT(CXXR::CachedString)

namespace {
    CXXR::SchwarzCounter<CXXR::CachedString> cachedstring_schwarz_ctr;
}

// ***** Implementation of non-inlined templated members *****

template<class Archive>
void CXXR::CachedString::load(Archive& ar, const unsigned int version)
{
    // This will only ever be applied to a 'temporary' CachedString
    // created by the default constructor.
    ar & boost::serialization::base_object<String>(*this);
    ar >> *m_s11n_string;
}

template<class Archive>
void CXXR::CachedString::save(Archive& ar, const unsigned int version) const
{
    ar & boost::serialization::base_object<String>(*this);
    std::string str = stdstring();
    ar << str;
}

extern "C" {
#endif /* __cplusplus */

    /** @brief Is a String cached?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return a non-zero value iff \a x points to a CachedString.
     */
#ifndef __cplusplus
    int IS_CACHED(SEXP x);
#else
    inline int IS_CACHED(SEXP x)
    {
	// Use explicit namespace qualification to avoid ambiguities:
	const CXXR::String* str = CXXR::SEXP_downcast<const CXXR::String*>(x);
	return (dynamic_cast<const CXXR::CachedString*>(str) != 0);
    }
#endif

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
	return CXXR::CachedString::obtain(str);
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
	return CXXR::CachedString::obtain(str, encoding);
    }
#endif

    /** @brief Create a CXXR::UncachedString object for specified text
     * and encoding.
     *
     * @param text The text of the string to be created, possibly
     *          including embedded null characters.  The encoding is
     *          assumed to be CE_NATIVE.
     *
     * @param length The length of the string pointed to by \a text.
     *          Must be nonnegative.  The created string will comprise
     *          the text plus an appended null byte.
     *
     * @param encoding The encoding of the required CachedString.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to the created string.
     */
    SEXP Rf_mkCharLenCE(const char* text, int length, cetype_t encoding);

    /** @brief Create a CXXR::UncachedString object for specified text.
     *
     * @param text The text of the string to be created, possibly
     *          including embedded null characters.  The encoding is
     *          assumed to be CE_NATIVE.
     *
     * @param length The length of the string pointed to by \a text.
     *          Must be nonnegative.  The created string will comprise
     *          the text plus an appended null byte.
     *
     * @return Pointer to the created string.
     */
#ifndef __cplusplus
    SEXP Rf_mkCharLen(const char* text, int length);
#else
    inline SEXP Rf_mkCharLen(const char* text, int length)
    {
	return Rf_mkCharLenCE(text, length, CE_NATIVE);
    }
#endif

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* CACHEDSTRING_H */
