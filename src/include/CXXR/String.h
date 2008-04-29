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

/** @file String.h
 * @brief Class CXXR::String and associated C interface.
 */

#ifndef CXXR_STRING_H
#define CXXR_STRING_H

#include "CXXR/GCRoot.h"
#include "CXXR/VectorBase.h"

#define LATIN1_MASK (1<<2)
#define UTF8_MASK   (1<<3)

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
	     * @param l const reference to a string.
	     * @param r const reference to a string.
	     * @return true iff \a l < \a r in the defined ordering.
	     */
	    bool operator()(const String& l, const String& r) const;
	private:
	    bool m_na_last;
	};

	/** @brief Read-only character access.
	 * @param index Index of required character (counting from
	 *          zero).  No bounds checking is applied.
	 * @return the specified character.
	 * @note For CXXR internal use only.
	 */
	char operator[](unsigned int index) const
	{
	    return m_c_str[index];
	}

	/** @brief Blank string.
	 * @return <tt>const</tt> pointer to the string "".
	 */
	static const String* blank()
	{
	    return s_blank;
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

	/** @brief Hash value.
	 *
	 * @return The hash value of this string.
	 *
	 * @note The current hashing algorithm (taken from CR) does
	 * not deal satisfactorily with strings containing embedded
	 * null characters: the hashing processes characters only up
	 * to the first null.
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
	static const String* NA()
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
    protected:
	/** @brief Create a string. 
	 *
	 * @param sz Number of <tt>char</tt>s in the string.  Zero is
	 *          permissible.  Note that if the string uses a
	 *          multibyte encoding scheme, this may be different
	 *          from the number of Unicode characters represented
	 *          by the string.
	 *
	 * @param encoding The intended encoding of the string, as
	 *          indicated by the LATIN1_MASK and UTF8_MASK bits.
	 *          Zero signifies ASCII encoding, and at most one of
	 *          the MASK bits may be set (checked).
	 *
	 * @param c_string Pointer to a representation of the string
	 *          as a C-style string (but possibly with embedded
	 *          null characters), with \a sz plus one bytes, the
	 *          last byte being a null byte.  (Because of the
	 *          possibility of embedded nulls the size of the
	 *          string is not checked.)  This string
	 *          representation must remain in existence for the
	 *          lifetime of the String object.  If a null pointer
	 *          is supplied here, a string pointer must be
	 *          supplied later in the construction of the derived
	 *          class object by calling setCString().
	 */
	String(size_t sz, unsigned int encoding,
	       const char* c_string = 0)
	    : VectorBase(CHARSXP, sz), m_c_str(c_string), m_hash(-1)
	{
	    if (encoding) checkEncoding(encoding);
	    m_gpbits = encoding;
	}

	/** @brief Supply pointer to the string representation.
	 *
	 * @param c_string Pointer to a representation of the string
	 *          as a C-style string (but possibly with embedded
	 *          null characters), with size() plus one bytes, the
	 *          last byte being a null byte.  (Because of the
	 *          possibility of embedded nulls the size of the
	 *          string is not checked.)  This string
	 *          representation must remain in existence for the
	 *          lifetime of the String object.
	 */
	void setCString(const char* c_string)
	{
	    m_c_str = c_string;
	}

	/** @brief Mark the hash value as invalid.
	 *
	 * This should be called in the event that the text of the
	 * String may have been changed.
	 */
	void invalidateHash() const
	{
	    m_hash = -1;
	}
    private:
	static GCRoot<const String> s_na;
	static GCRoot<const String> s_blank;

	const char* m_c_str;

	mutable int m_hash;  // negative signifies invalid

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	String(const String&);
	String& operator=(const String&);

	// Report error if encoding is invalid:
	static void checkEncoding(unsigned int encoding);
    };
}  // namespace CXXR

extern "C" {

#endif /* __cplusplus */

    extern SEXP R_NaString;
    extern SEXP R_BlankString;

    /**
     * @param x \c const pointer to a CXXR::String .
     * @return \c const pointer to character 0 of \a x .
     */
#ifndef __cplusplus
    const char *R_CHAR(SEXP x);
#else
    inline const char *R_CHAR(SEXP x)
    {
	return CXXR::SEXP_downcast<CXXR::String*>(x)->c_str();
    }
#endif

    /**
     * @param x Pointer to a CXXR::String.
     * @return true iff \a x is marked as having LATIN1 encoding.
     */
#ifndef __cplusplus
    Rboolean IS_LATIN1(SEXP x);
#else
    inline Rboolean IS_LATIN1(SEXP x)
    {
	return Rboolean(x->m_gpbits & LATIN1_MASK);
    }
#endif

    /**
     * @param x Pointer to a CXXR::String.
     * @return true iff \a x is marked as having UTF8 encoding.
     */
#ifndef __cplusplus
    Rboolean IS_UTF8(SEXP x);
#else
    inline Rboolean IS_UTF8(SEXP x)
    {
	return Rboolean(x->m_gpbits & UTF8_MASK);
    }
#endif

    /* Hashing Functions */

#ifndef __cplusplus
    int HASHVALUE(SEXP x);
#else
    inline int HASHVALUE(SEXP x)
    {
	const CXXR::String& str = *CXXR::SEXP_downcast<CXXR::String*>(x);
	return str.hash();
    }
#endif

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* CXXR_STRING_H */
