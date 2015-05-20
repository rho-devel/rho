/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
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

/** @file ElementTraits.hpp
 *
 * @brief Namespace encapsulating traits of R vector element types.
 */

#ifndef ELEMENTTRAITS_HPP
#define ELEMENTTRAITS_HPP 1

#include <boost/mpl/bool.hpp>
#include "CXXR/GCNode.hpp"

namespace CXXR {
    /** @brief Namespace encapsulating traits of R vector element types.
     *
     * This namespace is used to record characteristics of types
     * capable of being used as the elements of R data vectors, to
     * facilitate the writing of generic algorithms manipulating such
     * vectors.
     */
    namespace ElementTraits {
	/** @brief Do elements of this type require construction?
	 *
	 * Specializations will define \c MustConstruct to
	 * be false if element type \a T is known to have a trivial
	 * default constructor.
	 *
	 * In the default case, covered here, \c MustConstruct is
	 * defined to false, signifying that construction is
	 * required.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 *
	 * @note This metafunction is defined explicitly because the
	 * capabilities of boost::has_trivial_constructor are
	 * platform-dependent.
	 */
	template <typename T>
	struct MustConstruct : boost::mpl::true_
	{};

	/** @brief Does this type have a destructor?
	 *
	 * Specializations will define \c MustDestruct to be true if
	 * element type \a T has a nontrivial destructor.
	 *
	 * In the default case, covered here, \c MustDestruct is
	 * defined to false, signifying that no destructor call is
	 * required.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 *
	 * @note This metafunction is defined explicitly because the
	 * capabilities of boost::has_trivial_destructor are
	 * platform-dependent.
	 */
	template <typename T>
	struct MustDestruct : boost::mpl::true_
	{};

	template<typename T>
	struct Duplicate {
	    T operator()(const T& value) const {
		return value;
	    }
	};

	template<class T> auto duplicate_element(const T& value)
	    -> decltype(Duplicate<T>()(value))
	{
	    return Duplicate<T>()(value);
	}

	/** @brief Function object to generate 'not available' value.
	 *
	 * Normally this will be accessed via the NA() function
	 * template declared at CXXR namespace level.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */
	template <typename T>
	struct NAFunc {
	    /** @brief Value to be used if 'not available'.
	     *
	     * @return The value of type \a T to be used if the actual
	     * value is not available.  This for example is the value that
	     * will be used if an element of a vector of \a T is accessed
	     * in R using a NA index.
	     *
	     * @note For some types, e.g. Rbyte, the value returned is not
	     * distinct from ordinary values of the type.  See
	     * hasDistinctNA().
	     */
	    const T& operator()() const;
	};

	/** @brief Function object for testing 'not available' status.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */
	template <typename T> struct IsNA {
	    /** @brief Does a value represent a distinct 'not available'
	     *  status?
	     *
	     * @param t A value of type \a T .
	     *
	     * @return true iff \a t has a distinct value (or
	     * possibly, one of a set of distinct values) signifying
	     * that the actual value of this quantity is not
	     * available.
	     */
	    bool operator()(const T& t) const
	    {
		return t == NAFunc<T>()();
	    }
	};

	/** @brief Function object for testing 'not available' and 'not a
	 *    number status.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */
	template <typename T> struct IsNaOrNaN {
	    /** @brief Does a value represent a distinct NA or NaN status?
	     *
	     * @param t A value of type \a T .
	     *
	     * @return true iff \a t has a distinct value (or
	     * possibly, one of a set of distinct values) signifying
	     * that the actual value of this quantity is NA or NaN.
	     */
	    bool operator()(const T& t) const
	    {
		return IsNA<T>()(t);
	    }
	};

	/** @brief Does a type have a distinct 'not available' value?
	 *
	 * @return true iff the range of type \a T includes a distinct
	 * value to signify that the actual value of the quantity is
	 * not available.
	 */
	template <typename T>
	inline bool hasDistinctNA()
	{
	    return isNA(NAFunc<T>()());
	}
    }  // namespace ElementTraits

    /** @brief Does a value represent a distinct 'not available'
     *  status?
     *
     * This templated function is syntactic sugar for the
     * ElementTraits::IsNA() function objects.  It should not be
     * specialized; instead specialize ElementTraits::IsNA itself.
     *
     * @tparam T A type capable of being used as the element type
     *           of an R data vector. 
     *
     * @param t A value of type \a T .
     *
     * @return true iff \a t has a distinct value (or possibly,
     * one of a set of distinct values) signifying that the actual
     * value of this quantity is not available.
     */
    template <typename T> bool isNA(const T& t)
    {
	return ElementTraits::IsNA<T>()(t);
    }

    /** @brief Does a value represent a 'not available' or 'not a number'
     *  status?
     *
     * This templated function is syntactic sugar for the
     * ElementTraits::IsNaOrNaN() function objects.  It should not be
     * specialized; instead specialize ElementTraits::IsNaOrNaN itself.
     *
     * @tparam T A type capable of being used as the element type
     *           of an R data vector. 
     *
     * @param t A value of type \a T .
     *
     * @return true iff \a t has a distinct value (or possibly,
     * one of a set of distinct values) signifying that the actual
     * value of this quantity is not available or NaN.
     */
    template <typename T> bool isNaOrNaN(const T& t)
    {
	return ElementTraits::IsNA<T>()(t);
    }

    /** @brief Value to be used if 'not available'.
     *
     * This templated function is syntactic sugar for the
     * ElementTraits::NAFunc() function objects.  It should not be
     * specialized; instead specialize ElementTraits::NAFunc itself.
     *
     * @tparam T A type capable of being used as the element type
     *           of an R data vector. 
     *
     * @return The value of type \a T to be used if the actual value
     * is not available.  This for example is the value that will be
     * used if an element of a vector of \a T is accessed in R using a
     * NA index.
     *
     * @note For some types, e.g. Rbyte, the value returned is not
     * distinct from ordinary values of the type.  See
     * hasDistinctNA().
     */
    template <typename T> const T& NA()
    {
	return ElementTraits::NAFunc<T>()();
    }
}  // namespace CXXR;

#endif  // ELEMENTTRAITS_HPP
