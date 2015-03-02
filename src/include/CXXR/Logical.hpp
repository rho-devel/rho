/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file Logical.h
 * @brief Class CXXR::Logical.
 */

#ifndef CXXR_LOGICAL_H
#define CXXR_LOGICAL_H

#include "R_ext/Arith.h"
#include "CXXR/Complex.hpp"
#include "CXXR/ElementTraits.hpp"

namespace CXXR {
    /** @brief Object representing a scalar logical value.
     *
     * In R, logical values have three states: TRUE, FALSE and NA.
     * The Logical class represents such a value and provides the standard
     * logic operations extended to ternary logic.
     *
     * Note that the representation of this class is completely constrained by
     * the C API to R's internals.  Any changes are likely to break existing
     * user code.
     */
    class Logical {
    public:
	Logical() {}
	explicit Logical(int i) : m_value(i) {}
	/*implicit*/ Logical(bool b) : m_value(b ? 1 : 0) {}
	
	explicit operator int() const { return m_value; }
	explicit operator double() const { return isNA() ? NA_REAL : m_value; }
	explicit operator CXXR::Complex() const { return double(); }

	bool isTrue() const  { return m_value == TRUE; }
	bool isFalse() const { return m_value == FALSE; }
	bool isNA() const    { return m_value == NA_LOGICAL; }
	
	static Logical NA() { return Logical(NA_LOGICAL); }

 	/** @brief NA aware equality operator.
	 *
	 *  Returns NA if either or both operands are NA.  Otherwise returns
	 *  whether or not the two values are equal.
	 */ 
	Logical equals(Logical other) const {
	    return (isNA() || other.isNA()) ? NA() : identical(other);
	}

 	/** @brief NA oblivious equality operator.
	 *
	 *  Returns True iff the values are equal, or if they are both NA.
	 */ 
	bool identical(Logical other) const {
	    return m_value == other.m_value; }
	
	// NB: operator==() is intentionally not defined.
	// Use either 'equals' or 'identical' instead.

	Logical operator!() const;
	Logical operator||(Logical other) const;
	Logical operator&&(Logical other) const;
    private:
	// The value.  Allowed values are TRUE, FALSE and NA_LOGICAL.
	int m_value;

	template<> template<class Archive>
	friend void ElementTraits::Serialize<Logical>::operator()(
	    Archive& ar, Logical& item);
    };

    namespace ElementTraits
    {
	template<>
	inline bool IsNA<Logical>::operator()(const Logical& value) const {
	    return value.isNA();
	}

	template<>
	inline const Logical& NAFunc<Logical>::operator()() const {
	    // TODO(kmillar): change NAFunc to return by value instead.
	    static Logical NA = Logical::NA();
	    return NA;
	}
	
	template<> template<class Archive>
	inline void Serialize<Logical>::operator()(Archive& ar, Logical& item) {
	    Serialize<int>()(ar, item.m_value);
	}
    }  // namespace ElementTraits

    // Inline definitions of operators.
    inline Logical Logical::operator!() const {
	if (isNA()) {
	    return NA();
	}
	return Logical(1 - m_value);
    }

    inline Logical Logical::operator||(Logical other) const {
	if (isTrue() || other.isTrue()) {
	    return true;
	}
	if (isNA() || other.isNA()) {
	    return NA();
	}
	return false;
    }

    inline Logical Logical::operator&&(Logical other) const {
	if (isFalse() || other.isFalse()) {
	    return false;
	}
	if (isNA() || other.isNA()) {
	    return NA();
	}
	return true;
    }

}  // namespace CXXR

#endif  // CXXR_LOGICAL_H
