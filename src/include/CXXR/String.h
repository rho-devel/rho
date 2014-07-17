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

/** @file String.h
 * @brief Class CXXR::String and associated C interface.
 */

#ifndef CXXR_STRING_H
#define CXXR_STRING_H

#include "CXXR/GCRoot.h"
#include "CXXR/VectorBase.h"

typedef enum {
    CE_NATIVE = 0,
    CE_UTF8   = 1,
    CE_LATIN1 = 2,
    CE_BYTES  = 3,
    CE_SYMBOL = 5,
    CE_ANY    =99
} cetype_t;

#ifdef __cplusplus

#include <boost/serialization/nvp.hpp>
#include <unordered_map>
#include <string>

#include "CXXR/Allocator.hpp"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/SchwarzCounter.hpp"

namespace CXXR {
    /** @brief RObject representing a character string.
     *
     * At any one time, at most one String object with a particular
     * text and encoding may exist.
     *
     * @note When the method size() of VectorBase is applied to a
     * String, it returns the number of <tt>char</tt>s that the String
     * comprises.  If the string uses a multibyte encoding scheme,
     * this may be different from the number of Unicode characters
     * represented by the string.
     */
    class String : public VectorBase {
    public:
	/** @brief Comparison object for CXXR::String.
	 *
	 * STL-compatible comparison class for comparing CXXR::String
	 * objects.
	 */
	class Comparator {
	public:
	    /**
	     * @param na_last if true, the 'not available' string will
	     *          come after all other strings in the sort
	     *          ordering; if false, it will come before all
	     *          other strings.
	     */
	    explicit Comparator(bool na_last = true)
		: m_na_last(na_last)
	    {}

	    /** @brief Comparison operation.
	     *
	     * @param l non-null pointer to a String.
	     *
	     * @param r non-null pointer to a String.
	     *
	     * @return true iff \a l < \a r in the defined ordering.
	     */
	    bool operator()(const String* l, const String* r) const;
	private:
	    bool m_na_last;
	};

	/** @brief Read-only character access.
	 *
	 * @param index Index of required character (counting from
	 *          zero).  No bounds checking is applied.
	 *
	 * @return the specified character.
	 *
	 * @note For CXXR internal use only.
	 */
	char operator[](unsigned int index) const
	{
	    return m_string->c_str()[index];
	}

	/** @brief Blank string.
	 *
	 * @return <tt>const</tt> pointer to the string "".
	 */
	static String* blank()
	{
	    return s_blank;
	}

	/** @brief Access as a C-style string.
	 *
	 * @return Pointer to the text of the string represented as a
	 * C-style string.
	 */
	const char* c_str() const
	{
	    return m_string->c_str();
	}

	/** @brief Character encoding.
	 *
	 * @return the character encoding.  At present the only types
	 * of encoding are CE_NATIVE, CE_UTF8, CE_LATIN1 and CE_BYTES.
	 */
	cetype_t encoding() const
	{
	    return m_encoding;
	}

	/** @brief Extract encoding information from CR's \c gp bits
	 * field.
	 *   
	 * This function is used to extract the character encoding
	 * information contained in the <tt>sxpinfo_struct.gp</tt>
	 * field used in CR.  It should be used exclusively for
	 * deserialization.  Refer to the 'R Internals' document for
	 * details of this field.
	 *
	 * @param gpbits the \c gp bits field (within the
	 *          least significant 16 bits).
	 */
	static cetype_t GPBits2Encoding(unsigned int gpbits);

	/** @brief Is this Stringpure ASCII?
	 *
	 * @return true iff the String contains only ASCII characters.
	 */
	bool isASCII() const
	{
	    return m_ascii;
	}

	/** @brief Test if 'not available'.
	 *
	 * @return true iff this is the 'not available' string.
	 */
	bool isNA() const
	{
	    return this == s_na;
	}

	/** @brief 'Not available' string.
	 *
	 * Note that although the 'not available' string contains the
	 * text "NA", it is identified as the 'not available' string
	 * by its <em>address</em>, not by its content.  It is
	 * entirely in order to create another string with the text
	 * "NA", and that string will not be considered 'not
	 * available'.
	 *
	 * @return <tt>const</tt> pointer to the string representing
	 *         'not available'.
	 */
	static String* NA()
	{
	    return s_na;
	}

	/** @brief Get a pointer to a String object.
	 *
	 * If no String with the specified text and encoding currently
	 * exists, one will be created, and a pointer to it returned.
	 * Otherwise a pointer to the existing String will be
	 * returned.
	 *
	 * @param str The text of the required String. (Embedded null
	 *          characters are permissible.)
	 *
	 * @param encoding The encoding of the required String.
	 *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted
	 *          in this context (checked).  Note that if \a str
	 *          contains no non-ASCII characters, then the
	 *          encoding is set to CE_NATIVE regardless of the
	 *          value of the \a encoding parameter.
	 *
	 * @return Pointer to a String (preexisting or newly created)
	 * representing the specified text in the specified encoding.
	 */
	static String* obtain(const std::string& str,
			      cetype_t encoding = CE_NATIVE);

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "char";
	}

	/** @brief Access encapsulated std::string.
	 *
	 * @return Reference to the encapsulated std::string.
	 */
	const std::string& stdstring() const
	{
	    return *m_string;
	}

	// Virtual functions of RObject:
	unsigned int packGPBits() const;
	const char* typeName() const;
    private:
	friend class boost::serialization::access;
	friend class SchwarzCounter<String>;
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
	    static std::hash<std::string> s_string_hasher;
	};

	// The cache is implemented as a mapping from keys to pointers
	// to String objects.  Each String simply contains a pointer
	// locating its entry within the cache.
        typedef 
            std::unordered_map<key, String*, Hasher, std::equal_to<key>,
                               CXXR::Allocator<std::pair<const key,
                                                         String*> >
                               > map;

	static map* s_cache;
	static std::string* s_na_string;
	static String* s_na;
	static String* s_blank;

	map::value_type* m_key_val_pr;
	const std::string* m_string;
	cetype_t m_encoding;
	mutable Symbol* m_symbol;  // Pointer to the Symbol object identified
	  // by this String, or a null pointer if none.
	bool m_ascii;

	// A null value of key_val_pr is used to designate the NA string:
	explicit String(map::value_type* key_val_pr = 0);

	// Not implemented.  Declared to prevent
	// compiler-generated versions:
	String(const String&);
	String& operator=(const String&);

	// Declared private to ensure that String objects are
	// allocated only using 'new'.
	~String();

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
	    boost::serialization::split_member(ar, *this, version);
	}
    };

    /** @brief Is a std::string entirely ASCII?
     *
     * @param str The string to be examined.
     *
     * @return false if str contains at least one non-ASCII character,
     * otherwise true.  In particular the function returns true for an
     * empty string.
     */
    bool isASCII(const std::string& str);
}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::String)

namespace {
    CXXR::SchwarzCounter<CXXR::String> string_schwarz_ctr;
}
    
// ***** Implementation of non-inlined templated members *****

template<class Archive>
void CXXR::String::load(Archive& ar, const unsigned int version)
{
    // This will only ever be applied to a 'temporary' String
    // created by the default constructor.
    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(RObject);
    bool isna;
    ar >> BOOST_SERIALIZATION_NVP(isna);
    if (isna)
	S11nScope::defineRelocation(this, NA());
    else {
	std::string str;
	ar >> boost::serialization::make_nvp("string", str);
	ar >> BOOST_SERIALIZATION_NVP(m_encoding);
	S11nScope::defineRelocation(this, obtain(str, m_encoding));
    }
}

template<class Archive>
void CXXR::String::save(Archive& ar, const unsigned int version) const
{
    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(RObject);
    bool isna = (this == NA());
    ar << BOOST_SERIALIZATION_NVP(isna);
    if (!isna) {
	std::string str = stdstring();
	ar << boost::serialization::make_nvp("string", str);
	ar << BOOST_SERIALIZATION_NVP(m_encoding);
    }
}

extern "C" {

#endif /* __cplusplus */

    extern SEXP R_NaString;
    extern SEXP R_BlankString;

    /** @brief Is the encoding of a CXXR::String known?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return a non-zero value iff \a x is marked as having either
     * LATIN1 encoding or UTF8 encoding.
     */
#ifndef __cplusplus
    int ENC_KNOWN(SEXP x);
#else
    inline int ENC_KNOWN(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const CXXR::String& str = *CXXR::SEXP_downcast<const CXXR::String*>(x);
	cetype_t enc = str.encoding();
	return enc == CE_LATIN1 || enc == CE_UTF8;
    }
#endif

    /** @brief Is a CXXR::String pure ASCII?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return true iff \a x contains only ASCII characters..
     */
#ifndef __cplusplus
    int IS_ASCII(SEXP x);
#else
    inline int IS_ASCII(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const CXXR::String& str = *CXXR::SEXP_downcast<const CXXR::String*>(x);
	return Rboolean(str.isASCII());
    }
#endif

    /** @brief Does a CXXR::String have bytecode encoding?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return true iff \a x is marked as having BYTES encoding.
     */
#ifndef __cplusplus
    int IS_BYTES(SEXP x);
#else
    inline int IS_BYTES(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const CXXR::String& str = *CXXR::SEXP_downcast<const CXXR::String*>(x);
	return Rboolean(str.encoding() == CE_BYTES);
    }
#endif

    /** @brief Does a CXXR::String have LATIN1 encoding?
     *
     * @param x Pointer to a CXXR::String.
     *
     * @return true iff \a x is marked as having LATIN1 encoding.
     */
#ifndef __cplusplus
    Rboolean IS_LATIN1(SEXP x);
#else
    inline Rboolean IS_LATIN1(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const CXXR::String& str = *CXXR::SEXP_downcast<const CXXR::String*>(x);
	return Rboolean(str.encoding() == CE_LATIN1);
    }
#endif

    /** @brief Does a CXXR::String have UTF8 encoding?
     *
     * @param x Pointer to a CXXR::String (checked).
     *
     * @return true iff \a x is marked as having UTF8 encoding.
     */
#ifndef __cplusplus
    Rboolean IS_UTF8(SEXP x);
#else
    inline Rboolean IS_UTF8(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const CXXR::String& str = *CXXR::SEXP_downcast<const CXXR::String*>(x);
	return Rboolean(str.encoding() == CE_UTF8);
    }
#endif

    /** @brief Access the content of CXXR::String as a C-style string.
     *
     * @param x \c non-null pointer to a CXXR::String .
     *
     * @return \c const pointer to character 0 of \a x .
     */
#ifndef __cplusplus
    const char *R_CHAR(SEXP x);
#else
    inline const char *R_CHAR(SEXP x)
    {
	using namespace CXXR;
	return SEXP_downcast<String*>(x, false)->stdstring().c_str();
    }
#endif

    /** @brief Get a pointer to a CXXR::String object.
     *
     * CE_NATIVE encoding is assumed.  If no CXXR::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing CXXR::String will
     * be returned.
     *
     * @param str The null-terminated text of the required string.
     *
     * @return Pointer to a string object representing the specified
     *         text.
     */
#ifndef __cplusplus
    SEXP Rf_mkChar(const char* str);
#else
    inline SEXP Rf_mkChar(const char* str)
    {
	return CXXR::String::obtain(str);
    }
#endif
    
    /** @brief Get a pointer to a CXXR::String object.
     *
     * If no CXXR::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing CXXR::String will be returned.
     *
     * @param str The null-terminated text of the required cached string.
     *
     * @param encoding The encoding of the required String.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to a string object representing the specified
     *         text in the specified encoding.
     */
#ifndef __cplusplus
    SEXP Rf_mkCharCE(const char* str, cetype_t encoding);
#else
    inline SEXP Rf_mkCharCE(const char* str, cetype_t encoding)
    {
	return CXXR::String::obtain(str, encoding);
    }
#endif

    /** @brief Create a CXXR::String object for specified text and
     * encoding.
     *
     * If no CXXR::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing CXXR::String will be returned.
     *
     * @param text The text of the string to be created, possibly
     *          including embedded null characters.  The encoding is
     *          assumed to be CE_NATIVE.
     *
     * @param length The length of the string pointed to by \a text.
     *          Must be nonnegative.  The created string will comprise
     *          the text plus an appended null byte.
     *
     * @param encoding The encoding of the required String.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to the created string.
     */
    SEXP Rf_mkCharLenCE(const char* text, int length, cetype_t encoding);

    /** @brief Create a CXXR::String object for specified text.
     *
     * CE_NATIVE encoding is assumed.  If no CXXR::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing CXXR::String will
     * be returned.
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

    /** @brief Convert contents of a CXXR::String to UTF8.
     *
     * @param x Non-null pointer to a CXXR::String.
     *
     * @return The text of \a x rendered in UTF8 encoding.
     *
     * @note The result is held in memory allocated using R_alloc().
     * The calling code must arrange for this memory to be released in
     * due course.
     */
    const char* Rf_translateCharUTF8(SEXP x);

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* CXXR_STRING_H */
