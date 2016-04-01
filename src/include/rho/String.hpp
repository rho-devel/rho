/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 * @brief Class rho::String and associated C interface.
 */

#ifndef RHO_STRING_H
#define RHO_STRING_H

#include "Rinternals.h"
#include "rho/Allocator.hpp"
#include "rho/GCRoot.hpp"
#include "rho/RHandle.hpp"
#include "rho/SEXP_downcast.hpp"
#include "rho/VectorBase.hpp"
#include <string>
#include <unordered_map>

extern "C" void Rf_InitNames();

namespace rho {
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
	/** @brief Comparison object for rho::String.
	 *
	 * STL-compatible comparison class for comparing rho::String
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

	String* clone() const override {
	    return const_cast<String*>(this);
	}

	/** @brief Read-only character access.
	 *
	 * @param index Index of required character (counting from
	 *          zero).  No bounds checking is applied.
	 *
	 * @return the specified character.
	 *
	 * @note For rho internal use only.
	 */
	char operator[](unsigned int index) const
	{
	    return m_data[index];
	}

	/** @brief Blank string.
	 *
	 * @return <tt>const</tt> pointer to the string "".
	 */
	static String* blank()
        {
          static GCRoot<String> blank = String::obtain("");
          return blank;
        }

	/** @brief Access as a C-style string.
	 *
	 * @return Pointer to the text of the string represented as a
	 * C-style string.
	 */
	const char* c_str() const
	{
            return m_data;
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
            return this == NA();
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
          static GCRoot<String> na(createNA());
          return na.get();
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
	 * @return The string's value as a std::string.
	 */
	std::string stdstring() const
	{
	    assert(m_data);
            return std::string(m_data, size());
	}

	/** @brief Test for equality with a null-terminated string.
	 *
	 * @return true iff the strings are the same.
         *
         * @note Always returns false for the NA string.
	 */
        bool operator==(const char* text) const {
          if (this == NA()) {
            return false;
          }
	  assert(m_data);
          return strcmp(m_data, text) == 0;
        }

	// Virtual functions of RObject:
	unsigned int packGPBits() const override;
	const char* typeName() const override;
    private:
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
                               rho::Allocator<std::pair<const key,
                                                         String*> >
                               > map;

	static map* getCache();

        map::value_type* m_key_val_pr;
	const char* m_data;
	cetype_t m_encoding;
	mutable Symbol* m_symbol;  // Pointer to the Symbol object identified
	  // by this String, or a null pointer if none.
	bool m_ascii;

        // Should only be called by String::create().
        String(char* character_storage,
               const std::string& text, cetype_t encoding, bool isAscii);
        static String* create(const std::string& text, cetype_t encoding,
                              bool isAscii);
        static String* createNA();

	String(const String&) = delete;
	String& operator=(const String&) = delete;

	// Declared private to ensure that String objects are
	// allocated only using 'new'.
	~String();

	// Initialize the static data members:
	static void initialize();
        friend void ::Rf_InitNames();
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


    // Designed for use with std::accumulate():
    unsigned int stringWidth(unsigned int minwidth, const String* string);

    // Designed for use with std::accumulate():
    unsigned int stringWidthQuote(unsigned int minwidth, const String* string);

    // Since Strings are uniqued, coping them is almost zero-cost.
    // So allow implicit copies for RHandle<String>.
    template<>
    inline RHandle<String>::RHandle(const RHandle<String>&) = default;
    template<>
    inline RHandle<String>& RHandle<String>::operator=(const RHandle<String>&)
      = default;

    // Template specializations:
    namespace ElementTraits {
	template <>
	struct NAFunc<RHandle<String> > {
	    static RHandle<String> makeNA();

	    inline const RHandle<String>& operator()() const
	    {
		static RHandle<String> na = makeNA();
		return na;
	    }
	};

	template <>
        inline bool IsNA<RHandle<String>>::operator()(const RHandle<String>& t)
	    const
	{
	    typedef RHandle<String> T;
	    return t == NA<T>();
	}
    }

    // Make the default handle for a String point to a blank string:
    template <>
    inline RHandle<String>::RHandle()
    {
	operator=(String::blank());
    }
}  // namespace rho

extern "C" {

    extern SEXP R_NaString;
    extern SEXP R_BlankString;

    /** @brief Is the encoding of a rho::String known?
     *
     * @param x Pointer to a rho::String.
     *
     * @return a non-zero value iff \a x is marked as having either
     * LATIN1 encoding or UTF8 encoding.
     */
    inline int ENC_KNOWN(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const rho::String& str = *rho::SEXP_downcast<const rho::String*>(x);
	cetype_t enc = str.encoding();
	return enc == CE_LATIN1 || enc == CE_UTF8;
    }

    /** @brief Is a rho::String pure ASCII?
     *
     * @param x Pointer to a rho::String.
     *
     * @return true iff \a x contains only ASCII characters..
     */
    inline int IS_ASCII(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const rho::String& str = *rho::SEXP_downcast<const rho::String*>(x);
	return Rboolean(str.isASCII());
    }

    /** @brief Does a rho::String have bytecode encoding?
     *
     * @param x Pointer to a rho::String.
     *
     * @return true iff \a x is marked as having BYTES encoding.
     */
    inline int IS_BYTES(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const rho::String& str = *rho::SEXP_downcast<const rho::String*>(x);
	return Rboolean(str.encoding() == CE_BYTES);
    }

    /** @brief Does a rho::String have LATIN1 encoding?
     *
     * @param x Pointer to a rho::String.
     *
     * @return true iff \a x is marked as having LATIN1 encoding.
     */
    inline Rboolean IS_LATIN1(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const rho::String& str = *rho::SEXP_downcast<const rho::String*>(x);
	return Rboolean(str.encoding() == CE_LATIN1);
    }

    /** @brief Does a rho::String have UTF8 encoding?
     *
     * @param x Pointer to a rho::String (checked).
     *
     * @return true iff \a x is marked as having UTF8 encoding.
     */
    inline Rboolean IS_UTF8(SEXP x)
    {
	// Use explicit namespace qualification to prevent ambiguities:
	const rho::String& str = *rho::SEXP_downcast<const rho::String*>(x);
	return Rboolean(str.encoding() == CE_UTF8);
    }

    /** @brief Access the content of rho::String as a C-style string.
     *
     * @param x \c non-null pointer to a rho::String .
     *
     * @return \c const pointer to character 0 of \a x .
     */
    inline const char *R_CHAR(SEXP x)
    {
	using namespace rho;
	return SEXP_downcast<String*>(x, false)->c_str();
    }

    /** @brief Get a pointer to a rho::String object.
     *
     * CE_NATIVE encoding is assumed.  If no rho::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing rho::String will
     * be returned.
     *
     * @param str The null-terminated text of the required string.
     *
     * @return Pointer to a string object representing the specified
     *         text.
     */
    inline SEXP Rf_mkChar(const char* str)
    {
	return rho::String::obtain(str);
    }
    
    /** @brief Get a pointer to a rho::String object.
     *
     * If no rho::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing rho::String will be returned.
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
    inline SEXP Rf_mkCharCE(const char* str, cetype_t encoding)
    {
	return rho::String::obtain(str, encoding);
    }

    /** @brief Create a rho::String object for specified text and
     * encoding.
     *
     * If no rho::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing rho::String will be returned.
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

    /** @brief Create a rho::String object for specified text.
     *
     * CE_NATIVE encoding is assumed.  If no rho::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing rho::String will
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
    inline SEXP Rf_mkCharLen(const char* text, int length)
    {
	return Rf_mkCharLenCE(text, length, CE_NATIVE);
    }

    /** @brief Convert contents of a rho::String to UTF8.
     *
     * @param x Non-null pointer to a rho::String.
     *
     * @return The text of \a x rendered in UTF8 encoding.
     *
     * @note The result is held in memory allocated using R_alloc().
     * The calling code must arrange for this memory to be released in
     * due course.
     */
    const char* Rf_translateCharUTF8(SEXP x);
}  // extern "C"

#endif /* RHO_STRING_H */
