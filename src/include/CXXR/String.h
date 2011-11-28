/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-11 Andrew R. Runnalls, subject to such other
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

#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    /** @brief Base class for RObject representing a character string.
     *
     * @note When the method size() of VectorBase is applied to a
     * String, it returns the number of <tt>char</tt>s that the String
     * comprises.  If the string uses a multibyte encoding scheme,
     * this may be different from the number of Unicode characters
     * represented by the string.
     */
    class String : public VectorBase {
    public:
	/* @brief Comparison object for CXXR::String.
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
	    return c_str()[index];
	}

	/** @brief Access encapsulated C-style string.
	 *
	 * @return Pointer to the encapsulated C-style (null
	 * terminated) string.
	 */
	const char* c_str() const
	{
	    return m_c_str;
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

	/** @brief Hash value.
	 *
	 * @return The hash value of this string.
	 *
	 * @note The current hashing algorithm (taken from CR) does
	 * not deal satisfactorily with strings containing embedded
	 * null characters: the hashing processes characters only up
	 * to the first null.  This function is mainly provided for
	 * backward compatibility with CR: CXXR's own hash tables are
	 * based on the value of the <em>pointer</em> to a
	 * CachedString.
	 */
	int hash() const;

	/** @brief Test if 'not available'.
	 *
	 * @return true iff this is the 'not available' string.
	 */
	bool isNA() const
	{
	    return this == s_na.get();
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

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "char";
	}

	// Virtual functions of RObject:
	unsigned int packGPBits() const;
    protected:
	/** @brief Create a string. 
	 *
	 * @param sz Number of <tt>char</tt>s in the string.  Zero is
	 *          permissible.  Note that if the string uses a
	 *          multibyte encoding scheme, this may be different
	 *          from the number of Unicode characters represented
	 *          by the string.
	 *
	 * @param encoding The encoding of the required CachedString.
	 *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted
	 *          in this context (checked).
	 */
	String(size_t sz, cetype_t encoding);

	/** @brief Mark the hash value as invalid.
	 *
	 * This should be called in the event that the text of the
	 * String may have been changed.
	 */
	void invalidateHash() const
	{
	    m_hash = -1;
	}

	/** @brief Designate the encapsulated C-style string.
	 *
	 * (Used by constructors of derived classes.)
	 *
	 * @param c_string Pointer to the C-style string which this
	 *          object encapsulates.
	 */
	void setCString(const char* c_string)
	{
	    m_c_str = c_string;
	}
    private:
	static GCRoot<String> s_na;

	const char* m_c_str;
	mutable int m_hash;  // negative signifies invalid
	cetype_t m_encoding;

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	String(const String&);
	String& operator=(const String&);
    };
}  // namespace CXXR

extern "C" {

#endif /* __cplusplus */

    extern SEXP R_NaString;
    extern SEXP R_BlankString;

    /**
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
	return SEXP_downcast<String*>(x, false)->c_str();
    }
#endif

    /**
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

    /**
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

    /**
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

    /** @brief Is a CXXR::String UTF8?
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

    /** @brief Hash value of CXXR::String.
     *
     * The hash value is obtained using String::hash().  See the
     * caveats in the description of that function.
     *
     * @param x Pointer to a CXXR::String (checked).
     *
     * @return The string's hash value.
     */
#ifndef __cplusplus
    int HASHVALUE(SEXP x);
#else
    inline int HASHVALUE(SEXP x)
    {
	const CXXR::String& str = *CXXR::SEXP_downcast<CXXR::String*>(x);
	return str.hash();
    }
#endif

    /** @brief Convert contents of a String to UTF8.
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
