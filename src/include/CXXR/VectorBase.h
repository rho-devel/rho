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

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2008
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

/** @file VectorBase.h
 * @brief Class CXXR::VectorBase and associated C interface.
 */

#ifndef VECTORBASE_H
#define VECTORBASE_H

#include "CXXR/RObject.h"

/* types for length of vectors etc */
typedef int R_len_t;
typedef ptrdiff_t R_xlen_t;

#define R_LEN_T_MAX INT_MAX

#ifdef __cplusplus

#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/nvp.hpp>

#include "CXXR/ElementTraits.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    class String;
    template <typename, SEXPTYPE,
	      typename Initializer = RObject::DoNothing> class FixedVector;
    typedef FixedVector<int, INTSXP> IntVector;
    typedef FixedVector<RHandle<>, VECSXP> ListVector;
    typedef FixedVector<RHandle<String>, STRSXP> StringVector;

    /** @brief Untemplated base class for R vectors.
     */
    class VectorBase : public RObject {
    public:
	typedef std::size_t size_type;

	/**
	 * @param stype The required ::SEXPTYPE.
	 * @param sz The required number of elements in the vector.
	 */
	VectorBase(SEXPTYPE stype, size_type sz)
	    : RObject(stype), m_xtruelength(sz), m_size(sz)
	{}

	/** @brief Copy constructor.
	 *
	 * @param pattern VectorBase to be copied.
	 */
	VectorBase(const VectorBase& pattern)
	    : RObject(pattern), m_xtruelength(pattern.m_xtruelength),
	      m_size(pattern.m_size)
	{}

	/** @brief Names associated with the rows, columns or other
	 *  dimensions of an R matrix or array.
	 *
	 * @return A null pointer signifies that there are no names
	 * associated with any of the dimensions of \a *this ; a null
	 * pointer will always be returned if \a *this is not an R
	 * matrix or array.  Otherwise the return value will be a
	 * pointer to ListVector with as many elements as \a *this has
	 * dimensions.  Each element of the ListVector is either a
	 * null pointer, signifying that there are no names associated
	 * with the corresponding dimension, or a pointer to a
	 * StringVector with as many elements as the size of the array
	 * along the corresponding dimension, giving the names of the
	 * 'slices' along that dimension.  For example the zeroth
	 * element of the ListVector, if non-null, will give the row
	 * names, and the following element will give the column
	 * names.
	 */
	const ListVector* dimensionNames() const;

	/** @brief Names associated with a particular dimension of an
	 *  R matrix or array.
	 *
	 * @param d Dimension number (counting from 1) for which
	 *          dimension names are required.  Must be non-zero (not
	 *          checked).
	 *
	 * @return A null pointer if no names are associated with
	 * dimension \a d of \a *this , if \a *this does not have as
	 * many as \a d dimensions, or if \a *this is not an R matrix
	 * or array.  Otherwise a pointer to a StringVector with as
	 * many elements as the size of \a *this along the
	 * corresponding dimension, giving the names of the 'slices'
	 * along that dimension.
	 */
	const StringVector* dimensionNames(unsigned int d) const;

	/** @brief Dimensions of R matrix or array.
	 *
	 * @return A null pointer if \a *this is not an R matrix or
	 * array.  Otherwise a pointer to an IntVector with one or
	 * more elements, the product of the elements being equal to
	 * the number of elements in \a *this .  The number of
	 * elements in the return value is the dimensionality of the
	 * array (e.g. 2 for a matrix), and each element gives the
	 * size of the array along the respective dimension.
	 */
	const IntVector* dimensions() const;

	/** @brief Names of vector elements.
	 *
	 * @return either a null pointer (if the elements do not have
	 * names), or a pointer to a StringVector with the same number
	 * of elements as \a *this .
	 */
	const StringVector* names() const;

	/** @brief Create an extended or shrunken copy of an R vector.
	 *
	 * @tparam V A type inheriting from VectorBase.
	 *
	 * @param pattern Non-null pointer to the vector to be copied.
	 *
	 * @param new_size Required size of the copy, which may be
	 *          smaller than, equal to or larger than the current
	 *          size.  Zero is permissible.
	 *
	 * @return Pointer to the copied vector.  If \a new_size is
	 * smaller than the size of \a pattern , supernumerary
	 * elements at the end of \a pattern are not included in the
	 * copy.  If \a new_size is greater than the size of \a
	 * pattern, extra elements are appended to the result and set
	 * to the NA value of \a V::value_type .  If \a pattern has a
	 * <tt>names</tt> attribute, then the result is given a
	 * <tt>names</tt> attribute obtained by recursively applying
	 * this resize() function to the names of \a pattern .  Other
	 * attributes are copied across by calling
	 * copyAttributesOnResize().
	 */
	template <class V>
	static V* resize(const V* pattern, size_type new_size);

	/** @brief Adjust attributes for a resized vector.
	 *
	 * When a vector is resized (either by VectorBase::resize() or
	 * VectorBase::setSize() ), this function is used to determine
	 * the attributes of the resized vector.  'dim' and 'dimnames'
	 * attributes are discarded, and any 'names' attribute is
	 * itself resized.  Other attributes are carried across
	 * unchanged.
	 *
	 * @param attributes Pointer, possibly null, to the attribute
	 *          list of the original vector.
	 *
	 * @param new_size Size of the resized vector.
	 *
	 * @return attribute list (possibly null) for the resized
	 * vector.
	 */
	static PairList* resizeAttributes(const PairList* attributes,
					  size_type new_size);

	/** @brief Associate names with the rows, columns or other
	 *  dimensions of an R matrix or array.
	 *
	 * @param names If this is a null pointer, any names currently
	 *          associated with the dimensions of \a *this will be
	 *          removed.  Otherwise \a names must be a pointer to
	 *          ListVector with as many elements as \a *this has
	 *          dimensions.  Each element of the ListVector must
	 *          be either a null pointer, signifying that no names
	 *          are to be associated with the corresponding
	 *          dimension, or a pointer to a StringVector with as
	 *          many elements as the size of the array along the
	 *          corresponding dimension, giving the names to be
	 *          given to the 'slices' along that dimension.  For
	 *          example the zeroth element of the ListVector, if
	 *          non-null, will give the row names, and the
	 *          following element will give the column names.  \a
	 *          *this will assume ownership of this ListVector
	 *          (rather than duplicating it), so the calling code
	 *          must not subsequently modify it.
	 */
	void setDimensionNames(ListVector* names);

	/** @brief Associate names with a particular dimension of an
	 *  R matrix or array.
	 *
	 * @param d Dimension number (counting from 1) with which
	 *          dimension names are to be associated.  Must not be
	 *          greater than the * number of dimensions of \a
	 *          *this (checked).
	 *
	 * @param names If this is a null pointer, any names currently
	 *          associated with dimension \a d of \a *this are
	 *          removed.  Otherwise \a names must be a pointer to
	 *          a StringVector with as many elements as the size
	 *          of \a *this along the corresponding dimension,
	 *          giving the names to be given to the 'slices' along
	 *          that dimension.  \a *this will assume ownership of
	 *          this StringVector (rather than duplicating it), so
	 *          the calling code must not subsequently modify it.
	 */
	void setDimensionNames(unsigned int d, StringVector* names);

	/** @brief Define the dimensions of R matrix or array.
	 *
	 * As a side-effect, this function will remove any dimension names.
	 *
	 * @param dims If this is a null pointer, any existing dimensions
	 *          associated will be removed, and \a *this will
	 *          cease to be a R matrix/array.  Otherwise \a dims
	 *          must be a pointer to an IntVector with one or more
	 *          elements, the product of the elements being equal
	 *          to the number of elements in \a *this . The number
	 *          of elements in \a dims is the required
	 *          dimensionality of the array (e.g. 2 for a matrix),
	 *          and each element gives the required size of the
	 *          array along the respective dimension.  \a *this
	 *          will assume ownership of this IntVector (rather
	 *          than duplicating it), so the calling code must not
	 *          subsequently modify it.
	 */
	void setDimensions(IntVector* dims);

	/** @brief Associate names with the elements of a VectorBase.
	 *
	 * @param names Either a null pointer, in which case any
	 * existing names will be removed, or a pointer to a
	 * StringVector with the same number of elements as \a *this .
	 * \a *this will assume ownership of this StringVector (rather
	 * than duplicating it), so the calling code must not
	 * subsequently modify it.
	 */
	void setNames(StringVector* names);

	/** @brief Adjust the number of elements in the vector.
	 *
	 * The default implementation is simply to raise an error.
	 *
	 * @param new_size New size required.  Zero is permissible.
	 *          If the size is increased, the extra elements will
	 *          be initialized with <tt>NA<T>()</tt>, where \a T
	 *          is the element type.
	 */
	virtual void setSize(size_type new_size);

	/** @brief Number of elements in the vector.
	 *
	 * @return The number of elements in the vector.
	 */
	size_type size() const
	{
	    return m_size;
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "(vector type)";
	}

	// Virtual function of RObject, redeclared for covariant
	// return type:
	VectorBase* clone() const
	{
	    return 0;
	}

	// Make private in due course (or get rid altogether):
	R_xlen_t m_xtruelength;
    protected:
	~VectorBase() {}

	/** @brief Adjust the number of elements in the vector.
	 *
	 * Used by derived classes to modify the recorded size of the
	 * vector, and to adjust its attributes accordingly.
	 *
	 * @param new_size New size required.
	 */
	void adjustSize(size_type new_size)
	{
	    m_size = new_size;
	    setAttributes(resizeAttributes(attributes(), new_size));
	}

	/** @brief Raise error on attempt to allocate overlarge vector.
	 *
	 * @param bytes Size of data block for which allocation failed.
	 */
	static void tooBig(std::size_t bytes);
    private:
	friend class boost::serialization::access;

	size_type m_size;

	// m_size will always be passed in by the constructor, and so
	// is not serialised.
	template<class Archive>
	void serialize(Archive & ar, const unsigned int version)
	{
	    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(RObject);
	    ar & BOOST_SERIALIZATION_NVP(m_xtruelength);
	}
    };

    template <class V>
    V* VectorBase::resize(const V* pattern, size_type new_size)
    {
	GCStackRoot<V> ans(CXXR_NEW(V(new_size)));
	size_type copysz = std::min(pattern->size(), new_size);
	typename V::const_iterator patb = pattern->begin();
	typename V::iterator ansit
	    = std::copy(patb, patb + copysz, ans->begin());
	std::fill(ansit, ans->end(), NA<typename V::value_type>());
	ans->setAttributes(resizeAttributes(pattern->attributes(), new_size));
	ans->setS4Object(pattern->isS4Object());
	return ans;
    }
}  // namespace CXXR

extern "C" {

#endif /* __cplusplus */

    /* Accessor functions */

    /* Vector Access Functions */

    /**
     * @param x Pointer to an CXXR::VectorBase .
     *
     * @return The length of \a x, or 0 if \a x is a null pointer.  (In 
     *         the case of certain hash tables, this means the 'capacity'
     *         of \a x , not all of which may be used.)
     */
#ifndef __cplusplus
    R_xlen_t XLENGTH(SEXP x);
#else
    inline R_xlen_t XLENGTH(SEXP x)
    {
	using namespace CXXR;
	if (!x)
	    return 0;
	VectorBase& vb = *SEXP_downcast<VectorBase*>(x);
	return vb.size();
    }
#endif

    /**
     * @param x Pointer to a CXXR::VectorBase .
     *
     * @return The 'true length' of \a x.  According to the R Internals
     *         document for R 2.4.1, this is only used for certain hash
     *         tables, and signifies the number of used slots in the
     *         table.
     *
     * @deprecated May be withdrawn in the future.
     */
#ifndef __cplusplus
    R_xlen_t XTRUELENGTH(SEXP x);
#else
    inline R_xlen_t XTRUELENGTH(SEXP x)
    {
	using namespace CXXR;
	VectorBase& vb = *SEXP_downcast<VectorBase*>(x);
	return vb.m_xtruelength;
    }
#endif

    /**
     * Set length of vector.
     *
     * @param x Pointer to a CXXR::VectorBase .
     *
     * @param v The required new length, which must not be greater than
     *          the current length.
     *
     * @deprecated May be withdrawn in future.  Currently used in
     * library/stats/src/isoreg.c , and possibly in packages.
     */
    void SETLENGTH(SEXP x, int v);

    /**
     * Set 'true length' of vector.
     *
     * @param x Pointer to a CXXR::VectorBase .
     *
     * @param v The required new 'true length'.
     *
     * @deprecated May be withdrawn in the future.
     */
#ifndef __cplusplus
    void SET_XTRUELENGTH(SEXP x, R_xlen_t v);
#else
    inline void SET_XTRUELENGTH(SEXP x, R_xlen_t v)
    {
	using namespace CXXR;
	VectorBase& vb = *SEXP_downcast<VectorBase*>(x);
	vb.m_xtruelength = v;
    }
#endif

    /**
     * @brief Create a vector object.
     *
     *  Allocate a vector object.  This ensures only validity of
     *  ::SEXPTYPE values representing lists (as the elements must be
     *  initialized).  Initializing of other vector types is done in
     *  do_makevector().
     *
     * @param stype The type of vector required.
     *
     * @param length The length of the vector to be created.
     *
     * @return Pointer to the created vector.
     */
    SEXP Rf_allocVector(SEXPTYPE stype, R_xlen_t length);

    /** @brief Is an RObject a vector?
     *
     * Vector in this context embraces R matrices and arrays.
     *
     * @param s Pointer to the RObject to be tested.  The pointer may be
     *          null, in which case the function returns FALSE.
     *
     * @return TRUE iff \a s points to a vector object.
     */
    Rboolean Rf_isVector(SEXP s);

#ifdef __cplusplus
}
#endif

#endif /* VECTORBASE_H */
