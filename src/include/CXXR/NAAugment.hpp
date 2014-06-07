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

/** @file NAAugment.hpp
 *
 * @brief Class template NAAugment<T> and associated element traits.
 */

#ifndef NAAUGMENT_HPP
#define NAAUGMENT_HPP 1

#include "CXXR/ElementTraits.hpp"

namespace CXXR {
    /** @brief Class template to augment a type with 'not available'.
     *
     * This class template is used to combine a value of type \a T
     * with an indication of whether or not the actual value is 'not
     * available' (NA).
     *
     * @tparam T an arbitrary data type, intended to serve as an
     *           element type in R data vectors.  (Presumably this
     *           type will not already have the capability of
     *           representing 'not available' status, e.g. by a
     *           specially designated value within its range.)
     */
    template <typename T> class NAAugment {
    public:
	/** @brief Default constructor.
	 *
	 * The constructed object will be flagged as NA.
	 *
	 * For this constructor to be used, \a T must have a default
	 * constructor (possibly trivial).
	 */
	NAAugment()
	    : m_na(true)
	{}

	/** @brief Construct from compatible value.
	 *
	 * @tparam U a type assignable to \a T.
	 *
	 * @param value The value to be given to the constructed
	 *          object.
	 */
	template <typename U>
	explicit NAAugment(const U& value)
	    : m_value(value), m_na(false)
	{}

	/** @brief Assignment from compatible value.
	 *
	 * @tparam U a type assignable to \a T.
	 *
	 * @param value The value to be given to the constructed
	 *          object.
	 */
	template <typename U>
	NAAugment<T>& operator=(const U& value)
	{
	    m_value = value;
	    m_na = false;
	    return *this;
	}

	operator T&()
	{
	    return m_value;
	}

	operator const T&() const
	{
	    return m_value;
	}

	/** @brief Is the value 'not available'?
	 *
	 * @return true iff this object's value is flagged as 'not
	 * available'.
	 */
	bool isNA() const
	{
	    return m_na;
	}

	/** @brief Exemplar object representing 'not available'.
	 *
	 * @return const reference to an instance of this type in
	 * which the value is flagged as 'not available'.
	 */
	static const NAAugment<T>& NA()
	{
	    static NAAugment<T> na;
	    return na;
	}

	/** @brief boost::serialization function.
	 *
	 * @tparam Archive archive class compatible with
	 *           boost::serialization.  Serialization or
	 *           deserialization will be performed according to
	 *           whether this is an output or an input archive.
	 *
	 * @param ar The archive to be read/written.
	 *
	 * @param file_version Ignored.
	 */
	template <class Archive>
	void serialize(Archive& ar, const unsigned int file_version);

	/** @brief Data value.
	 *
	 * @return The data value embedded within the object.
	 *
	 * @note This function (like the corresponding implicit
	 * conversion operator) ignores the 'not available' status of
	 * the object.  Perhaps there would be a case for it throwing
	 * an exception or reporting an error if the value is 'not
	 * available'.
	 */
	T& value()
	{
	    return m_value;
	}

	/** @brief Data value (const variant).
	 *
	 * @return The data value embedded within the object.
	 *
	 * @note This function (like the corresponding implicit
	 * conversion operator) ignores the 'not available' status of
	 * the object.  Perhaps there would be a case for it throwing
	 * an exception or reporting an error if the value is 'not
	 * available'.
	 */
	const T& value() const
	{
	    return m_value;
	}
    private:
	T m_value;
	bool m_na;
    };

    // ***** Implementation of non-inlined templated members *****

    template <class T>
    template <class Archive>
    void CXXR::NAAugment<T>::serialize(Archive& ar,
				       const unsigned int file_version)
    {
	ar & BOOST_SERIALIZATION_NVP(m_value);
	ar & BOOST_SERIALIZATION_NVP(m_na);
    }

    // ***** Template specializations *****
    namespace ElementTraits {
	template <class T>
	struct Data<NAAugment<T> > {
	    typedef T type;

	    static const type& get(const NAAugment<T>& t)
	    {
		return t.value();
	    }
	};

	template <class T>
	struct MustConstruct<NAAugment<T> > : boost::mpl::true_ 
	{};

	template <class T>
	struct MustDestruct<NAAugment<T> >  : boost::mpl::true_
	{};

	template <class T>
	struct NAFunc<NAAugment<T> > {
	    const NAAugment<T>& operator()() const
	    {
		return NAAugment<T>::NA();
	    }
	};

	template <class T>
	struct IsNA<NAAugment<T> > {
	    bool operator()(const NAAugment<T>& t) const
	    {
		return t.isNA();
	    }
	};
    }  // namespace ElementTraits
}  // namespace CXXR

#endif  // NAAUGMENT_HPP
