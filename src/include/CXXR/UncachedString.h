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

/** @file UncachedString.h
 * @brief Class CXXR::UncachedString and associated C interface.
 */

#ifndef UNCACHEDSTRING_H
#define UNCACHEDSTRING_H

#include "CXXR/String.h"

#ifdef __cplusplus

#include <string>

namespace CXXR {
    /** @brief String object not held in a cache.
     *
     * Unlike CachedString objects, at any one time more than one
     * UncachedString with the same text and encoding may exist, and
     * the text and encoding of an UncachedString may duplicate that
     * of a CachedString.  Moreover, the content and encoding of an
     * UncachedString may be modified after it has been constructed;
     * however, since the length may \e not be modified, this is of
     * limited usefulness.
     *
     * @deprecated The use of CachedString objects is to be preferred.
     */
    class UncachedString : public String {
    public:
	/** @brief Create an UncachedString object, leaving its contents
	 *         uninitialized.
	 *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 *
	 * @param encoding The intended encoding of the string, as
	 *          indicated by the LATIN1_MASK and UTF8_MASK bits.
	 *          Zero signifies ASCII encoding, and at most one of
	 *          the MASK bits may be set (checked).
	 */
	explicit UncachedString(size_t sz, unsigned int encoding = 0)
	    : String(sz, encoding), m_databytes(sz + 1), m_data(m_short_string)
	{
	    allocData(sz);
	}

	/** @brief Create an UncachedString object from a std::string.
	 *
	 * @param str The std::string whose text is to be copied into
	 *          the constructed UncachedString.  (Embedded null
	 *          characters are permissible.)
	 *
	 * @param encoding The intended encoding of the string, as
	 *          indicated by the LATIN1_MASK and UTF8_MASK bits.
	 *          Zero signifies ASCII encoding, and at most one of
	 *          the MASK bits may be set (checked).
	 */
	explicit UncachedString(const std::string& str,
				unsigned int encoding = 0);

	/** @brief Character access.
	 * @param index Index of required character (counting from
	 *          zero).  No bounds checking is applied.
	 * @return Reference to the specified character.  This is
	 * intended for immediate use: in particular it should not be
	 * used after any other method has been applied to the
	 * UncachedString object, including in particular hash().
	 */
	char& operator[](unsigned int index)
	{
	    invalidateHash();
	    return m_data[index];
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "char (uncached)";
	}

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	// Max. strlen stored internally:
	static const size_t s_short_strlen = 7;

	size_t m_databytes;  // includes trailing null byte
	char* m_data;  // pointer to the string's data block.

	// If there are fewer than s_short_strlen+1 chars in the
	// string (including the trailing null), it is stored here,
	// internally to the UncachedString object, rather than via a separate
	// allocation from CXXR::MemoryBank.  We put this last, so that it
	// will be adjacent to any trailing redzone.
	char m_short_string[s_short_strlen + 1];

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	UncachedString(const UncachedString&);
	UncachedString& operator=(const UncachedString&);

	// Declared private to ensure that UncachedString objects are
	// allocated only using 'new'.
	~UncachedString()
	{
	    if (m_data != m_short_string)
		MemoryBank::deallocate(m_data, m_databytes);
	}

	// Initialise m_data, if necessary by allocating a data block
	// from MemoryBank:
	void allocData(size_t sz);
    };
}  // namespace CXXR

extern "C" {

#endif /* __cplusplus */

   /**
     * @param x pointer to a CXXR::String .
     * @return pointer to character 0 of \a x .
     * @note For R internal use only.  May be removed in future.
     */
#ifndef __cplusplus
    char *CHAR_RW(SEXP x);
#else
    inline char *CHAR_RW(SEXP x)
    {
	return &(*CXXR::SEXP_downcast<CXXR::UncachedString*>(x))[0];
    }
#endif

    /**
     * @brief Set LATIN1 encoding.
     * @param x Pointer to a CXXR::String.
     */
#ifndef __cplusplus
    void SET_LATIN1(SEXP x);
#else
    inline void SET_LATIN1(SEXP x)
    {
	CXXR::SEXP_downcast<CXXR::UncachedString*>(x);
	x->m_gpbits |= LATIN1_MASK;
    }
#endif

    /**
     * @brief Unset LATIN1 encoding.
     * @param x Pointer to a CXXR::String.
     */
#ifndef __cplusplus
    void UNSET_LATIN1(SEXP x);
#else
    inline void UNSET_LATIN1(SEXP x)
    {
	CXXR::SEXP_downcast<CXXR::UncachedString*>(x);
	x->m_gpbits &= ~LATIN1_MASK;
    }
#endif

    /**
     * @brief Set UTF8 encoding.
     * @param x Pointer to a CXXR::String.
     */
#ifndef __cplusplus
    void SET_UTF8(SEXP x);
#else
    inline void SET_UTF8(SEXP x)
    {
	CXXR::SEXP_downcast<CXXR::UncachedString*>(x);
	x->m_gpbits |= UTF8_MASK;
    }
#endif

    /**
     * @brief Unset UTF8 encoding.
     * @param x Pointer to a CXXR::String.
     */
#ifndef __cplusplus
    void UNSET_UTF8(SEXP x);
#else
    inline void UNSET_UTF8(SEXP x)
    {
	CXXR::SEXP_downcast<CXXR::UncachedString*>(x);
	x->m_gpbits &= ~UTF8_MASK;
    }
#endif

    /**
     * @brief Create a string object.
     *
     *  Allocate a string object.
     * @param length The length of the string to be created (excluding the
     *          trailing null byte).
     * @return Pointer to the created string.
     */
#ifndef __cplusplus
    SEXP Rf_allocString(R_len_t length);
#else
    inline SEXP Rf_allocString(R_len_t length)
    {
	return new CXXR::UncachedString(length);
    }
#endif

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* UNCACHEDSTRING_H */
