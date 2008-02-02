/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007, 2008  Andrew Runnalls
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef FLAGWORD_HPP
#define FLAGWORD_HPP 1

namespace CXXR {
    /** Unsigned short interpreted as an array of booleans.
     *
     * This class performs a similar function, and is implemented in a
     * similar way, to <tt>std::bitset<16></tt>.  However, its bits
     * are contained as a public data member, and within \c RObject
     * the member \c m_gpbits is a reference to this data member.  In
     * due course the use of \c m_gpbits will probably be eliminated
     * from CXXR, and then FlagWord itself will be replaced by
     * <tt>bitset<16></tt>.
     */
    class FlagWord {
    public:
	/** Proxy object for a bit of a FlagWord.
	 *
	 * Objects of this class are used to allow the Boolean values
	 * within a FlagWord to be examined and modified using the
	 * same syntax as would be used for accessing an array of
	 * <tt>bool</tt>.  See Item 30 of Scott Meyers's 'More
	 * Effective C++' for a general discussion of proxy objects,
	 * but see the <a
	 * href="http://www.aristeia.com/BookErrata/mec++-errata_frames.html">errata</a>.
	 * (It may look complicated, but an optimising compiler should
	 * be able to distil an invocation of FlagWord::operator[]
	 * into very few instructions.)
	 */
	class BitProxy {
	public:
	    /** Copy value of proxied bit from another proxied bit.
	     * @param rhs Proxied bit whose value is to be copied.
	     * @return Reference to this BitProxy.
	     */
	    BitProxy& operator=(const BitProxy& rhs)
	    {
		return operator=(static_cast<const bool>(rhs));
	    }

	    /** Assign value of proxied bit.
	     * @param rhs The required new value of the proxied bit.
	     * @return Reference to this BitProxy.
	     */
	    BitProxy& operator=(bool rhs)
	    {
		if (rhs)
		    *m_flags |= m_mask;
		else
		    *m_flags &= ~m_mask;
		return *this;
	    }

	    /**
	     * @return Value of proxied bit.
	     */
	    operator const bool() const {return *m_flags & m_mask;}
	private:
	    unsigned short* m_flags;
	    unsigned short m_mask;

	    BitProxy(unsigned short* flags, unsigned short mask)
		: m_flags(flags), m_mask(mask)
	    {}

	    // Not implemented:
	    BitProxy(const BitProxy&);

	    friend class FlagWord;
	};

	/**
	 * @param flags an initial value for the FlagWord.
	 */
	explicit FlagWord(unsigned short flags = 0)
	    : m_flags(flags)
	{}

	/**
	 * @param i An index in the range 0 to 15 inclusive.  Index 0
	 * refers to the least significant bit.
	 *
	 * @return a BitProxy object that can be used to examine or
	 * modify the Boolean value represented by bit i of the FlagWord.
	 */
	BitProxy operator[](unsigned int i)
	{
	    return BitProxy(&m_flags, s_mask[i]);
	}

	/**
	 * @param i An index in the range 0 to 15 inclusive. Index 0
	 * refers to the least significant bit.
	 *
	 * @return the Boolean value represented by bit i of the
	 * FlagWord.  The return value is const to prevent its use as
	 * an lvalue.
	 */ 
	const bool operator[](unsigned int i) const
	{
	    return m_flags & s_mask[i];
	}

	/**
	 * @return the FlagWord as an unsigned short.
	 */
	const unsigned short asUshort() const {return m_flags;}

	// To become private in due course:
	unsigned short m_flags;
    private:
	static unsigned short s_mask[];
    };
}

#endif  // FLAGWORD_HPP
