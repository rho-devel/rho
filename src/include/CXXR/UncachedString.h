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

/** @file UncachedString.h
 * @brief Class CXXR::UncachedString and associated C interface.
 */

#ifndef UNCACHEDSTRING_H
#define UNCACHEDSTRING_H

#include "CXXR/String.h"

#ifdef __cplusplus

#include <string>
#include <boost/serialization/access.hpp>
#include <boost/serialization/export.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/string.hpp>

#include "CXXR/BSerializer.hpp"

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
	 * @param encoding The encoding of the required CachedString.
	 *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted
	 *          in this context (checked).
	 */
	explicit UncachedString(size_t sz, cetype_t encoding = CE_NATIVE)
	    : String(sz, encoding), m_databytes(sz + 1),
	      m_data(m_short_string)
	{
	    allocData(sz);
	    setCString(m_data);
	}

	/** @brief Create an UncachedString object from a std::string.
	 *
	 * @param str The std::string whose text is to be copied into
	 *          the constructed UncachedString.  (Embedded null
	 *          characters are permissible.)
	 *
	 * @param encoding The encoding of the required CachedString.
	 *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted
	 *          in this context (checked).
	 */
	explicit UncachedString(const std::string& str,
				cetype_t encoding = CE_NATIVE);

	/** @brief Provide read-write access to the string.
	 *
	 * @return A pointer to the start of the string.  This is
	 * intended for immediate use: in particular it should not be
	 * used after any other method has been applied to the
	 * UncachedString object, including in particular hash().
	 */
	char* ptr()
	{
	    invalidateHash();
	    return m_data;
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "char (uncached)";
	}

	// Virtual function of GCNode:
	UncachedString* s11n_relocate() const;

	// Virtual function of RObject:
	const char* typeName() const;
    private:
	friend class boost::serialization::access;

	// Max. strlen stored internally:
	static const size_t s_short_strlen = 6;

	size_t m_databytes;  // includes trailing null byte
	char* m_data;  // pointer to the string's data block.

	GCEdge<UncachedString> m_s11n_reloc;  // used only during
	  // (de)serialisation to provide special handling for the NA
	  // string.

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

	template<class Archive>
	void load(Archive & ar, const unsigned int version);

	template<class Archive>
	void save(Archive & ar, const unsigned int version) const;

	// Fields not handled here are set up by the constructor.
	template <class Archive>
	void serialize(Archive& ar, const unsigned int version)
	{
	    BSerializer::Frame frame("UncachedString");
	    boost::serialization::split_member(ar, *this, version);
	}
    };
}  // namespace CXXR

BOOST_CLASS_EXPORT(CXXR::UncachedString)

namespace boost {
    namespace serialization {
	template<class Archive>
	void load_construct_data(Archive& ar, CXXR::UncachedString* t,
				 const unsigned int version)
	{
	    std::string str;
	    cetype_t encoding;
	    ar >> BOOST_SERIALIZATION_NVP(str)
	       >> BOOST_SERIALIZATION_NVP(encoding);
	    new (t) CXXR::UncachedString(str, encoding);
	}

	template<class Archive>
	void save_construct_data(Archive& ar, const CXXR::UncachedString* t,
				 const unsigned int version)
	{
	    // Note that the encoding gets serialised twice, once here
	    // and again in String::serialize().  This duplication,
	    // though annoying, is harmless.
	    std::string str(t->c_str(), t->size());
	    cetype_t encoding = t->encoding();
	    ar << BOOST_SERIALIZATION_NVP(str)
	       << BOOST_SERIALIZATION_NVP(encoding);
	}
    }  // namespace serialization
}  // namespace boost

// ***** Implementation of non-inlined templated members *****

// The issue here is to record whether or not a serialised
// UncachedString was the 'NA' string.

template<class Archive>
void CXXR::UncachedString::load(Archive& ar, const unsigned int version)
{
    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(String);
    bool isna;
    ar >> BOOST_SERIALIZATION_NVP(isna);
    if (isna)
	m_s11n_reloc = static_cast<UncachedString*>(NA());
}

template<class Archive>
void CXXR::UncachedString::save(Archive& ar, const unsigned int version) const
{
    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(String);
    bool isna = (this == NA());
    ar << BOOST_SERIALIZATION_NVP(isna);
}

extern "C" {
#endif /* __cplusplus */

    /** @brief Read-write character access.
     *
     * @param x pointer to a CXXR::UncachedString (checked).
     *
     * @return pointer to character 0 of \a x .
     *
     * @note For R internal use only.  May be removed in future.  See
     * the remarks on UncachedString::operator[]().
     */
#ifndef __cplusplus
    char *CHAR_RW(SEXP x);
#else
    inline char *CHAR_RW(SEXP x)
    {
	using namespace CXXR;
	return SEXP_downcast<UncachedString*>(x)->ptr();
    }
#endif

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* UNCACHEDSTRING_H */
