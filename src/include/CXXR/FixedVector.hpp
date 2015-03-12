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

/** @file FixedVector.hpp
 *
 * @brief Class template CXXR::FixedVector.
 */

#ifndef FIXEDVECTOR_HPP
#define FIXEDVECTOR_HPP 1

#include <boost/serialization/nvp.hpp>

#include "CXXR/VectorBase.h"
#include "CXXR/MemoryBank.hpp"

namespace CXXR {

namespace {
    template<class iterator>
    void visitElements(iterator start, iterator end, ...) {};

    template<class iterator>
    typename std::enable_if<
	std::is_base_of<GCEdgeBase, typename std::iterator_traits<iterator>::value_type>::value>::type
    visitElements(iterator start, iterator end, GCNode::const_visitor* v)
    {
	std::for_each(start, end, [=](GCNode* n) { if(n) (*v)(n); });
    }
}

    /** @brief R data vector primarily intended for fixed-size use.
     *
     * This is a general-purpose class template to represent an R data
     * vector, and is intended particularly for the case where the
     * size of the vector is fixed when it is constructed.
     *
     * Having said that, the template \e does implement decreaseSizeInPlace(),
     * primarily to service CR code's occasional use of SETLENGTH().
     *
     * CXXR implements all of CR's built-in vector types using this
     * template.
     *
     * @tparam T The type of the elements of the vector.
     *
     * @tparam ST The required ::SEXPTYPE of the vector.
     *
     * @tparam Initializer (optional).  Class of function object
     *           defining a static member function
     *           <code>initialize(RObject*)</code>. (Any return value
     *           is discarded.)  When a FixedVector object is
     *           constructed, this member function is applied to it.
     *           This can be used, for example, to apply an R class
     *           attribute etc.  The default is to do nothing.
     */
    // (Default binding of Initializer already defined in VectorBase.h)
    template <typename T, SEXPTYPE ST,
	      typename Initializer /* = RObject::DoNothing */>
    class FixedVector : public VectorBase {
    public:
	typedef T value_type;
	typedef T* iterator;
	typedef const T* const_iterator;

	/** @brief Create a vector, leaving its contents
	 *         uninitialized (for POD types) or default
	 *         constructed.
	 *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 */
	static FixedVector* create(size_type sz);

	/** @brief Create a vector from a range.
	 * 
	 * @tparam An iterator type, at least a forward iterator.
	 *
	 * @param from Iterator designating the start of the range
	 *          from which the FixedVector is to be constructed.
	 *
	 * @param to Iterator designating 'one past the end' of the
	 *          range from which the FixedVector is to be
	 *          constructed.
	 */
	template<typename FwdIter>
	static FixedVector* create(FwdIter from, FwdIter to) {
	    FixedVector* result = create(std::distance(from, to));
	    std::copy(from, to, result->begin());
	    return result;
	}

	/** @brief Create a vector from an initializer list.
	 *
	 * @param An initializer list containing the values to store in the
	 *          FixedVector.
	 */
	static FixedVector* create(std::initializer_list<T> items) {
	    return create(items.begin(), items.end());
	}

	/** @brief Create a vector containing a single value.
	 *
	 * @param value The value to store in the vector.
	 */
	template<class U>
	static FixedVector* createScalar(const U& value) {
	    FixedVector* result = create(1);
	    (*result)[0] = value;
	    return result;
	}

	/** @brief Element access.
	 *
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 *
	 * @return Reference to the specified element.
	 */
	T& operator[](size_type index)
	{
	    return m_data[index];
	}

	/** @brief Read-only element access.
	 *
	 * @param index Index of required element (counting from
	 *          zero).  No bounds checking is applied.
	 *
	 * @return \c const reference to the specified element.
	 */
	const T& operator[](size_type index) const
	{
	    return m_data[index];
	}

	/** @brief Iterator designating first element.
	 *
	 * @return An iterator designating the first element of the
	 * vector.  Returns end() if the vector is empty.
	 */
	iterator begin()
	{
	    return m_data;
	}

	/** @brief Const iterator designating first element.
	 *
	 * @return A const_iterator designating the first element of
	 * the vector.  Returns end() if the vector is empty.
	 */
	const_iterator begin() const
	{
	    return m_data;
	}

	/** @brief One-past-the-end iterator.
	 *
	 * @return An iterator designating a position 'one past the
	 * end' of the vector.
	 */
	iterator end()
	{
	    return begin() + size();
	}

	/** @brief One-past-the-end const_iterator.
	 *
	 * @return A const_iterator designating a position 'one past
	 * the end' of the vector.
	 */
	const_iterator end() const
	{
	    return begin() + size();
	}

	/** @brief Name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 *
	 * @note This function is declared but not defined as part of
	 * the FixedVector template.  It must be defined as a
	 * specialization for each instantiation of the template for
	 * which it or typeName() is used.
	 */
	static const char* staticTypeName();

	// Virtual functions of VectorBase:
	void decreaseSizeInPlace(size_type new_size) override;

	// Virtual functions of RObject:
	FixedVector<T, ST, Initializer>* clone() const override;
	const char* typeName() const override;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const override;
    protected:
	/**
	 * Declared protected to ensure that FixedVector objects are
	 * allocated only using 'new'.
	 */
	~FixedVector()
	{
	    destructElementsIfNeeded();

	    // GCNode::~GCNode doesn't know about the string storage space in
	    // this object, so account for it here.
	    size_t bytes = (size() - 1) * sizeof(T);
	    MemoryBank::adjustBytesAllocated(-bytes);
	}

	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	friend class boost::serialization::access;

	T* m_data;  // pointer to the vector's data block.

	alignas(T) char m_first_element_storage[sizeof(T)];

	// Trivial constructor.  Only used for boost serialization.
	FixedVector() : FixedVector(0) { }

	/** @brief Create a vector, leaving its contents
	 *         uninitialized (for POD types) or default
	 *         constructed.
	 *
	 * @param sz Number of elements required.  Zero is
	 *          permissible.
	 */
	FixedVector(size_type sz)
	    : VectorBase(ST, sz),
	      m_data(reinterpret_cast<T*>(m_first_element_storage))
	{
	    constructElementsIfNeeded();
	    Initializer::initialize(this);
	}

	/** @brief Copy constructor.
	 *
	 * @param pattern FixedVector to be copied.
	 */
	FixedVector(const FixedVector<T, ST, Initializer>& pattern);

	FixedVector& operator=(const FixedVector&) = delete;

	static void* allocate(size_type size);

	static void constructElements(iterator from, iterator to);
	static void constructElementsIfNeeded(iterator from, iterator to)
	{
	    // This is essential for e.g. RHandles, otherwise they
	    // may contain junk pointers.
	    if (ElementTraits::MustConstruct<T>::value) // compile-time constant
		constructElements(from, to);
	}
	void constructElementsIfNeeded() {
	    constructElementsIfNeeded(begin(), end());
	}

	void destructElementsIfNeeded(iterator from, iterator to)
	{
	    if (ElementTraits::MustDestruct<T>::value)  // compile-time constant
		destructElements(from, to);
	}
	void destructElementsIfNeeded() {
	    destructElementsIfNeeded(begin(), end());
	}
	void destructElements(iterator from, iterator to);

	// Helper function for detachReferents():
	void detachElements();

	template<class Archive>
	void load(Archive & ar, const unsigned int version); 
	template<class Archive>
	void loadValues(Archive & ar, const unsigned int version); 

	template<class Archive>
	void save(Archive & ar, const unsigned int version) const;

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version)
	{
	    boost::serialization::split_member(ar, *this, version);
	}
    };

    // VectorTypeFor<T>::type is the type of vector that can hold elements of
    // type T.
    template<class T>
    struct VectorTypeFor {
    };

}  // namespace CXXR

// ***** Implementation of non-inlined members *****

#include <algorithm>
#include "localization.h"
#include "R_ext/Error.h"

template <typename T, SEXPTYPE ST, typename Initr>
CXXR::FixedVector<T, ST, Initr>::FixedVector(
    const FixedVector<T, ST, Initr>& pattern)
    : VectorBase(pattern),
      m_data(reinterpret_cast<T*>(m_first_element_storage))
{
    constructElementsIfNeeded();
    std::copy(pattern.begin(), pattern.end(), begin());
    Initr::initialize(this);
}

template <typename T, SEXPTYPE ST, typename Initr>
void* CXXR::FixedVector<T, ST, Initr>::allocate(size_type sz)
{
    size_type blocksize = sz*sizeof(T);
    // Check for integer overflow:
    if (blocksize/sizeof(T) != sz)
	Rf_error(_("request to create impossibly large vector."));

    size_type headersize = sizeof(FixedVector);

    try {
	return GCNode::operator new(blocksize + headersize - sizeof(T));
    } catch (std::bad_alloc) {
	tooBig(blocksize);
	return nullptr;
    }
}

template <typename T, SEXPTYPE ST, typename Initr>
CXXR::FixedVector<T, ST, Initr>*
CXXR::FixedVector<T, ST, Initr>::create(size_type sz)
{
    void* storage = allocate(sz);
    return new(storage) FixedVector(sz);
}

template <typename T, SEXPTYPE ST, typename Initr>
CXXR::FixedVector<T, ST, Initr>* CXXR::FixedVector<T, ST, Initr>::clone() const
{
    void* storage = allocate(size());
    return new(storage) FixedVector(*this);
}

template <typename T, SEXPTYPE ST, typename Initr>
void CXXR::FixedVector<T, ST, Initr>::constructElements(iterator from,
							iterator to)
{
    for (iterator p = from; p != to; ++p)
	new (p) T;
}

template <typename T, SEXPTYPE ST, typename Initr>
void CXXR::FixedVector<T, ST, Initr>::destructElements(iterator from,
						       iterator to)
{
    for (iterator p = from; p != to; ++p)
	p->~T();
}

template <typename T, SEXPTYPE ST, typename Initr>
void CXXR::FixedVector<T, ST, Initr>::detachElements()
{
    std::fill(begin(), end(), T());
}

template <typename T, SEXPTYPE ST, typename Initr>
void CXXR::FixedVector<T, ST, Initr>::detachReferents()
{
    if (std::is_base_of<GCEdgeBase, T>::value) // known at compile time.
	detachElements();
    VectorBase::detachReferents();
}

template <typename T, SEXPTYPE ST, typename Initr>
template<class Archive>
void CXXR::FixedVector<T, ST, Initr>::load(Archive & ar,
					   const unsigned int version)
{
    size_t length;
    ar >> BOOST_SERIALIZATION_NVP(length);

    FixedVector* result = FixedVector::create(length);
    result->loadValues(ar, version);
    S11nScope::defineRelocation(this, result);
}

template <typename T, SEXPTYPE ST, typename Initr>
template<class Archive>
void CXXR::FixedVector<T, ST, Initr>::loadValues(Archive & ar,
						 const unsigned int version)
{
    ar >> BOOST_SERIALIZATION_BASE_OBJECT_NVP(VectorBase);

    size_type numnas;
    ar >> BOOST_SERIALIZATION_NVP(numnas);
    std::vector<size_type> na_indices;
    // Fill in NAs:
    {
	unsigned int idx = 0;
	for (size_type i = 0; i < numnas; ++i) {
	    unsigned int ii;
	    ar >> BOOST_SERIALIZATION_NVP(ii);
	    idx += ii;
	    m_data[idx] = NA<T>();
	    na_indices.push_back(idx);
	}
	na_indices.push_back(size());
    }

    // Fill in non-NA values:
    {
	size_type i = 0;
	for (std::vector<size_type>::const_iterator it = na_indices.begin();
	     it != na_indices.end(); ++it) {
	    size_type stop = *it;
	    while (i != stop) {
		ElementTraits::Serialize<T>()(ar, m_data[i]);
		++i;
	    }
	    ++i;  // Skip NA slot
	}
    }
}

// A FixedVector is serialized by first recording the number of NAs
// (if any) and the indices of the NAs (with all but the first
// expressed as an increment from the previous one), followed by the
// payloads of the non-NA values.
template <typename T, SEXPTYPE ST, typename Initr>
template<class Archive>
void CXXR::FixedVector<T, ST, Initr>::save(Archive & ar,
					   const unsigned int version) const
{
    size_t length = size();
    ar << BOOST_SERIALIZATION_NVP(length);

    ar << BOOST_SERIALIZATION_BASE_OBJECT_NVP(VectorBase);

    std::vector<size_type> na_indices;

    // Collect indices of NAs (if any):
    for (size_type i = 0; i < size(); ++i)
	if (isNA(m_data[i]))
	    na_indices.push_back(i);

    // Record first differences of NA indices:
    {
	size_type numnas = na_indices.size();
	ar << BOOST_SERIALIZATION_NVP(numnas);
	size_type last_idx = 0;
	for (std::vector<size_type>::const_iterator it = na_indices.begin();
	     it != na_indices.end(); ++it) {
	    size_type idx = *it;
	    size_type ii = idx - last_idx;  // ii = "index increment"
	    ar << BOOST_SERIALIZATION_NVP(ii);
	    last_idx = idx;
	}
    }

    // Record payloads of non-NAs:
    for (unsigned int i = 0; i < size(); ++i)
	if (!isNA(m_data[i]))
	    ElementTraits::Serialize<T>()(ar, m_data[i]);
};

template <typename T, SEXPTYPE ST, typename Initr>
void CXXR::FixedVector<T, ST, Initr>::decreaseSizeInPlace(size_type new_size)
{
    if (new_size > size()) {
	Rf_error("Increasing vector length in place not allowed.");
    }
    size_t bytes = (size() - new_size) * sizeof(T);
    MemoryBank::adjustBytesAllocated(-bytes);

    destructElementsIfNeeded(begin() + new_size, end());
    adjustSize(new_size);
}

template <typename T, SEXPTYPE ST, typename Initr>
const char* CXXR::FixedVector<T, ST, Initr>::typeName() const
{
    return FixedVector<T, ST, Initr>::staticTypeName();
}

template <typename T, SEXPTYPE ST, typename Initr>
void CXXR::FixedVector<T, ST, Initr>::visitReferents(const_visitor* v) const
{
    visitElements(begin(), end(), v);
    VectorBase::visitReferents(v);
}

#endif  // FIXEDVECTOR_HPP
