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
	/** @brief Information about the data payload.
	 *
	 * In some element types, including all the standard R atomic
	 * data types, the 'value' of a vector element is held
	 * directly in a data item of the element type \a T , and in
	 * that case a special value within the range of type \a T may
	 * be used to signify that an item of data is 'not available'.
	 *
	 * However, CXXR also allows the possibility that a vector
	 * element type \a T can be a class type whose objects contain
	 * a value of some underlying type, representing the data
	 * 'payload', along with a separate flag (typically a bool)
	 * indicating whether or not the data is 'not available'.
	 *
	 * This class provides facilities to allow generic programs to
	 * handle both these cases straightforwardly.  As defined
	 * here, the class deals with the first case described above;
	 * specializations of the Data template can be used to address
	 * the second case.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */
	template <typename T>
	struct Data {
	    /** @brief Type of the data payload held in this element
	     * type.
	     */
	    typedef T type;

	    /** @brief Access the data payload.
	     *
	     * @param t Reference to an object of the element type.
	     *
	     * @return reference to the data payload contained within
	     * \a t .
	     */
	    static const type& get(const T& t)
	    {
		return t;
	    }
	};  // struct Data

	/** @brief Access the data payload of an R vector element.
	 *
	 * This templated function is syntactic sugar for the
	 * Data::get() function.  It should not be specialized:
	 * instead specialize ElementTraits::Data itself.
	 *
	 * @tparam T type used as an element in the CXXR
	 *           implementation of an R vector type.
	 *
	 * @param t Reference to an object of type \a T .
	 *
	 * @return reference to the data payload contained within \a t .
	 */
	template <typename T>
	inline const typename ElementTraits::Data<T>::type&
	data(const T& t)
	{
	    return Data<T>::get(t);
	}

	/** @brief Function object for detaching referents.
	 *
	 * For element types for which \c HasReferents is true, this
	 * struct will be specialized into a function object which
	 * will detach the referents of a particular element \a t .
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */
	template <typename T>
	struct DetachReferents : std::unary_function<T, void> {
	    /** @brief Detach the referents of a \a T.
	     *
	     * @param t Reference to the \a T object whose referents
	     *          are to be detached.
	     */
	    void operator()(T& t) const
	    {}
	};

	/** @brief Do elements of this type refer to GCNode objects?
	 *
	 * Specializations will define \c HasReferents to
	 * be true if objects of element type \a T may incorporate
	 * references or (more likely) pointers to GCNode objects.
	 * Such types will also specialize the VisitReferents and
	 * DetachReferents function object types.
	 *
	 * In the default case, covered here, \c HasReferents is
	 * defined to false, signifying that no special handling
	 * regarding garbage collection is required.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */
	template <typename T>
	struct HasReferents : boost::mpl::false_
	{};

	/** @brief Do elements of this type require construction?
	 *
	 * Specializations will define \c MustConstruct to
	 * be true if element type \a T has a nontrivial default
	 * constructor.
	 *
	 * In the default case, covered here, \c MustConstruct is
	 * defined to false, signifying that no construction is
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
	struct MustConstruct : boost::mpl::false_
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
	struct MustDestruct : boost::mpl::false_
	{};

	/** @brief Function object for serialization/deserialization.
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */	 
	template <typename T>
	struct Serialize {
	    /** @brief Serialize/deserialize an element's payload.
	     *
	     * @tparam Archive boost::serialization archive type.
	     *           Serialization or deserialization will take
	     *           place according to whether this is an output
	     *           or an input archive type.
	     *
	     * @param ar Archive to be used.
	     *
	     * @param item Object whose data payload is to be
	     *          serialized/deserialized.
	     */
	    template <class Archive>
	    void operator()(Archive& ar, T& item)
	    {
		typename ElementTraits::Data<T>::type payload
		    = ElementTraits::data(item); 
		ar & boost::serialization::make_nvp("item", payload);
		item = payload;
	    }
	};
	    
	/** @brief Function object for visiting referents.
	 *
	 * For element types for which \c HasReferents::TruthType is
	 * True, this struct will be specialized into a function
	 * object which will conduct a visitor \a v to the referents
	 * of a particular element \a t .
	 *
	 * @tparam T A type capable of being used as the element type
	 *           of an R data vector. 
	 */
	template <typename T>
	struct VisitReferents : std::unary_function<T, void> {
	    /** @brief Constructor
	     *
	     * @param v Non-null pointer to the visitor object to be
	     *          applied by the function object constructed.
	     */
	    VisitReferents(GCNode::const_visitor* v)
	    {}

	    /** @brief Apply the function.
	     *
	     * @param t Object to which the function is to be applied.
	     */
	    void operator()(const T& t) const
	    {}
	};

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
