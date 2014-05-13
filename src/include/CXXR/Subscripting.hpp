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

/** @file Subscripting.hpp
 *
 * @brief Functionality to support subscript operations on R vectors,
 * matrices and arrays.
 */

#ifndef SUBSCRIPTING_HPP
#define SUBSCRIPTING_HPP 1

#include "CXXR/GCStackRoot.hpp"
#include "CXXR/IntVector.h"
#include "CXXR/ListVector.h"
#include "CXXR/PairList.h"
#include "CXXR/StringVector.h"
#include "CXXR/Symbol.h"

namespace CXXR {
   /** @brief Services to support R subscripting operations.
     *
     * This class, all of whose members are static, encapsulates
     * services supporting subscripting of R vector objects, including
     * R matrices and arrays.  Foremost among its member functions are
     * subset() and subassign(); the other member functions are
     * auxiliary to these two.
     *
     * @todo At present this class does not handle the special case
     * (described in sec. 3.4.2 of the R Language Definition) where
     * subscripting is being applied to an array, and there is a
     * single subscript which is a matrix with as many columns as the
     * array has dimensions, each row of the matrix being used to pick
     * out a single element of the array.
     *
     * @todo A matter for possible review is the fact that during
     * subsetting and subassignment operations, CXXR sometimes makes
     * deep copies of vectors of RHandle objects in cases where CR
     * gets away with simple pointer copies.
     *
     * @todo CR 3 uses vectors of real numbers to represent subscripts
     * where these would be too big to fit into an INTEGER.
     */
    class Subscripting {
    public:
	/** @brief Assign to selected elements of an R matrix or array.
	 *
	 * @tparam VL A type inheriting from VectorBase.
	 * 
	 * @tparam VR A type inheriting from VectorBase.
	 * 
	 * @param lhs Non-null pointer to a \a VL object, which is
	 *          the object to whose elements assignment is to be
	 *          made.  Where feasible, the return value will
	 *          point to \a lhs itself, modified appropriately
	 *          (which is why this parameter is not
	 *          <tt>const</tt>); otherwise the return value will
	 *          point to a modified copy of \a lhs.  (Copying
	 *          will occur if \a lhs aliases \a rhs .)
	 *
	 * @param subscripts Pointer, possibly null, to a list of
	 *          objects inheriting from RObject , with the same
	 *          number of elements as \a v has dimensions.  (\a
	 *          subscripts can be null only if \a v has zero
	 *          dimensions.)  The elements of the list represent
	 *          the subscripting to be applied for successive
	 *          dimensions of \a v .  An error will be raised if
	 *          the type or contents of any element is
	 *          inappropriate for subscripting from the dimension
	 *          in question.
	 *
	 * @param rhs Non-null pointer to the vector from which values
	 *          are to be taken.  Successive elements are assigned
	 *          to the locations within the result defined by the
	 *          canonical index vectors in \a indices .  If the
	 *          number of elements selected by \a subscripts is
	 *          greater than the size of \a rhs , the elements of
	 *          \a rhs are repeated in rotation.  It is an error
	 *          for \a rhs to have zero elements unless no
	 *          elements at all are selected by \a subscripts .  A
	 *          warning will be given if the number of elements
	 *          selected by \a subscripts is not equal to or a
	 *          multiple of the length of \a rhs .
	 *
	 * @result Pointer to the result of the assignment.  Its
	 * attributes (including dimensions) will be identical to
	 * those of \a lhs .
	 */
	template <class VL, class VR>
	static VL* arraySubassign(VL* lhs, const PairList* subscripts,
				  const VR* rhs)
	{
	    std::vector<Indices> indicesvec;
	    Subscripting::canonicalizeArraySubscripts(&indicesvec, lhs,
						      subscripts);
	    return arraySubassign(lhs, indicesvec, rhs);
	}

	/** @brief Extract a subset from an R matrix or array.
	 *
	 * @tparam V A type inheriting from VectorBase.
	 *
	 * @param v Non-null pointer to a \a V object, which is an R
	 *          matrix or array from which a subset (not
	 *          necessarily a proper subset) is to be extracted.
	 *
	 * @param subscripts Pointer, possibly null, to a list of
	 *          objects inheriting from RObject , with the same
	 *          number of elements as \a v has dimensions.  (\a
	 *          subscripts can be null only if \a v has zero
	 *          dimensions.)  The elements of the list represent
	 *          the subscripting to be applied for successive
	 *          dimensions of \a v .  An error will be raised if
	 *          the type or contents of any element is
	 *          inappropriate for subscripting from the dimension
	 *          in question.
	 *
	 * @param drop true iff any dimensions of unit extent are to
	 *          be removed from the result.
	 * 
	 * @return Pointer to a newly created object of type \a V ,
	 * containing the designated subset of \a v .
	 */
	template <class V>
	static V* arraySubset(const V* v, const PairList* subscripts,
			      bool drop)
	{
	    std::vector<Indices> indicesvec;
	    Subscripting::canonicalizeArraySubscripts(&indicesvec, v,
						      subscripts);
	    return arraySubset(v, indicesvec, drop);
	}

	/** @brief Obtain canonical index vector from an arbitrary
	 * subscript object.
	 *
	 * This function is used in implementing the function
	 * Rf_arraySubscript() declared in Rinternals.h .
	 *
	 * A canonical index vector is an IntVector in which each element
	 * is either NA or a strictly positive integer, or a RealVector in
	 * which each element is either NaN or a strictly positive
	 * integer.  Integer elements represent an index into a vector
	 * counting from one.  The RealVector form is used if and only
	 * if some index values are too large to be represented as an
	 * int.
	 *
	 * @param raw_indices Pointer, possibly null, to an RObject.
	 *          If the type and contents of this object are legal
	 *          for subscripting, canonicalization will be
	 *          performed accordingly; otherwise an error will be
	 *          raised.
	 *
	 * @param range_size The size of the vector or dimension into
	 *          which indexing is being performed.
	 *
	 * @param range_names Pointer, possibly null, to the vector of
	 *          names associated with the vector or dimension into
	 *          which indexing is being performed.  If present,
	 *          the size of this vector must be equal to \a
	 *          range_size .
	 *
	 * @return The first element of the returned value is a
	 * pointer to the canonicalized index vector.  The second
	 * element is the maximum index value within the canonicalized
	 * index vector, or zero if the vector is empty or consists
	 * entirely of NAs.
	 */
	static std::pair<VectorBase*, std::size_t>
	canonicalize(const RObject* raw_indices, std::size_t range_size,
		     const StringVector* range_names = 0);

	/** @brief Remove dimensions of unit extent.
	 *
	 * If \a v does not point to an R matrix or array, \a v is
	 * left unchanged and the function returns false.
	 *
	 * Otherwise, the function will examine \a v to determine if
	 * it has any dimensions with just one level, in which case
	 * those dimensions are removed, and the corresponding
	 * 'dimnames' (if any) are discarded.
	 *
	 * If, after dropping dimensions, only one dimension is left,
	 * then \a v is converted to a vector, with its 'names'
	 * attribute taken from the 'dimnames' (if any) of the
	 * surviving dimension.
	 *
	 * If all the original dimensions were of unit extent, then
	 * again \a v is converted to a vector (with a single
	 * element).  This vector is given a 'names' attribute only if
	 * just one of the original dimensions had associated
	 * 'dimnames', in which case the 'names' are taken from them.
	 *
	 * @param v Non-null pointer to the vector whose dimensions
	 *          are to be examined and if necessary modified.
	 *
	 * @return true if any dimensions were dropped, otherwise
	 * false.
	 */
	static bool dropDimensions(VectorBase* v);

	/** @brief Assign to selected elements of an R vector object.
	 *
	 * @tparam VL A type inheriting from VectorBase.
	 * 
	 * @tparam VR A type inheriting from VectorBase.
	 * 
	 * @param lhs Non-null pointer to a \a VL object, which is
	 *            the object to whose elements assignment is to be
	 *            made.  Where feasible, the return value will
	 *            point to \a lhs itself, modified appropriately
	 *            (which is why this parameter is not
	 *            <tt>const</tt>); otherwise the return value will
	 *            point to a modified copy of \a lhs.  (Copying
	 *            will occur for example if \a lhs aliases \a rhs .)
	 *
	 * @param subscripts Pointer, possibly null, to a list of
	 *          objects inheriting from RObject .  If \a lhs is an R
	 *          matrix or array, then it is permissible for the
	 *          list to have as many elements as \a lhs has
	 *          dimensions, and in that case array subassignment is
	 *          applied, as described for method arraySubassign().
	 *          If the foregoing condition does not apply, then
	 *          the list must have either zero or one elements,
	 *          and vector subassignment is applied, as described for
	 *          method vectorSubassign().
	 *
	 * @param rhs Non-null pointer to the vector from which values
	 *          are to be taken.  Successive elements are assigned
	 *          to the locations within the result defined by the
	 *          subscripts.  If the return vector is larger than
	 *          \a rhs , the elements of \a rhs are repeated in
	 *          rotation.  It is an error for \a rhs to have zero
	 *          elements unless \a subscripts selects no locations
	 *          at all.  A warning will be given if the number of
	 *          locations to be assigned to is not equal to or a
	 *          multiple of the length of \a rhs .
	 *
	 * @result Pointer to the result of the assignment.  Refer to
	 * the descriptions of vectorSubassign() and arraySubassign()
	 * for further details.
	 */
	template <class VL, class VR>
	static VL* subassign(VL* lhs, const PairList* subscripts,
			     const VR* rhs);

	/** @brief Extract a subset from an R vector, matrix or array.
	 *
	 * @tparam V A type inheriting from VectorBase.
	 *
	 * @param v Non-null pointer to a \a V object from which a
	 *          subset (not necessarily a proper subset) is to be
	 *          extracted.
	 *
	 * @param subscripts Pointer, possibly null, to a list of
	 *          objects inheriting from RObject .  If \a v is an R
	 *          matrix or array, then it is permissible for the
	 *          list to have as many elements as \a v has
	 *          dimensions, and in that case array subsetting is
	 *          applied, as described for method arraySubset().
	 *          If the foregoing condition does not apply, then
	 *          the list must have either zero or one elements,
	 *          and vector subsetting is applied, as described for
	 *          method vectorSubset().
	 *
	 * @param drop true iff any dimensions of unit extent are to
	 *          be removed from the result.  Ignored if vector
	 *          subsetting is used.
	 * 
	 * @return Pointer to a newly created object of type \a V ,
	 * containing the designated subset of \a v .
	 */
	template <class V>
	static V* subset(const V* v, const PairList* subscripts, bool drop);

	/** @brief Assign to selected elements of an R vector object.
	 *
	 * @tparam VL A type inheriting from VectorBase.
	 * 
	 * @tparam VR A type inheriting from VectorBase.
	 * 
	 * @param lhs Non-null pointer to a \a VL object, which is
	 *          the object to whose elements assignment is to be
	 *          made.  Where feasible, the return value will
	 *          point to \a lhs itself, modified appropriately
	 *          (which is why this parameter is not
	 *          <tt>const</tt>); otherwise the return value will
	 *          point to a modified copy of \a lhs.  (Copying
	 *          will occur if \a lhs aliases either \a rhs or
	 *          the first element of \a indices_pr , or if the
	 *          return vector needs to be larger than \a lhs .)
	 *
	 * @param subscripts Pointer, possibly null, to an RObject.
	 *          If the type and context of this object are legal
	 *          for subscripting, subassignment will be performed
	 *          accordingly; otherwise an error will be raised.
	 *          It is an error for the subscripts to give rise to
	 *          any NA index values of \a rhs contains more than
	 *          one element.
	 *
	 * @param rhs Non-null pointer to the vector from which values
	 *          are to be taken.  Successive elements are assigned
	 *          to the locations within the result defined by the
	 *          subscripts.  If the return vector is larger than
	 *          \a rhs , the elements of \a rhs are repeated in
	 *          rotation.  It is an error for \a rhs to have zero
	 *          elements, unless \a subscripts selects no
	 *          locations at all.  A warning will be given if the number
	 *          of locations to be assigned to is not equal to or
	 *          a multiple of the length of \a rhs .
	 *
	 * @result Pointer to the result of the assignment.  The size of the
	 * this vector will be whichever is larger of the the size of
	 * \a lhs or the minimum size implied by \a subscripts .  If the
	 * result is larger than \a lhs , then any supplementary
	 * elements not assigned to by the specified subscripts will be
	 * set to the NA value of VL::value_type .
	 */
	template <class VL, class VR>
	static VL* vectorSubassign(VL* lhs, const RObject* subscripts,
				   const VR* rhs)
	{
	    Indices indices;
	    indices.initialize(subscripts, lhs->size(), lhs->names());
	    return vectorSubassign(lhs, indices, rhs);
	}

	/** @brief Extract a subset of an R vector object.
	 *
	 * @tparam V A type inheriting from VectorBase.
	 *
	 * @param v Non-null pointer to a \a V object, which is the
	 *          object from which a subset (not necessarily a
	 *          proper subset) is to be extracted.
	 *
	 * @param subscripts Pointer, possibly null, to an RObject.  If
	 *          the type and contents of this object are legal for
	 *          subscripting, subsetting will be performed
	 *          accordingly; otherwise an error will be raised.
	 *
	 * @return Pointer to a newly created object of type \a V ,
	 * containing the designated subset of \a v .
	 */
	template <class V>
	static V* vectorSubset(const V* v, const RObject* subscripts)
	{
	    Indices indices;
	    indices.initialize(subscripts, v->size(), v->names());
	    return vectorSubset(v, indices);
	}
    private:
	/** @brief Canonical representation of a vector of indices.
	 *
	 * Zero values within an Indices vector correspond to NA (or
	 * NaN) indices within an R vector.
	 */
	class Indices : public std::vector<std::size_t> {
	public:
	    /** @brief Constructor.
	     *
	     * This constructs an empty Indices vector, which will
	     * probably need subsequently to be initialized by calling
	     * an initialize() method.
	     */
	    Indices() : m_max_index(0), m_min_lhssize(0)  {}

	    /** @brief Introduce new names when subassigning a vector.
	     *
	     * When subassigning into a vector using element names as
	     * indices, if an element name supplied as an index does
	     * not correspond to an existing element name, then the
	     * subassign operation adds a new element at the end of the
	     * target vector and gives it the supplied name.  This
	     * function performs the housekeeping necessary to give
	     * elements newly introduced by this process the
	     * appropriate names.
	     *
	     * @param v Target vector into which subassignment is
	     *          taking place.
	     */
	    void applyNewNames(VectorBase* v) const;

	    /** @brief Initialize an Indices object.
	     *
	     * @param raw_indices Pointer, possibly null, to an RObject.
	     *          If the type and contents of this object are legal
	     *          for subscripting, an Indices object will be
	     *          constructed accordingly; otherwise an error will be
	     *          raised.
	     *
	     * @param range_size The size of the vector or dimension into
	     *          which indexing is being performed.
	     *
	     * @param range_names Pointer, possibly null, to the vector of
	     *          names associated with the vector or dimension into
	     *          which indexing is being performed.  If present,
	     *          the size of this vector must be equal to \a
	     *          range_size .
	     */
	    void initialize(const RObject* raw_indices, std::size_t range_size,
			    const StringVector* range_names);

	    /** @brief Maximum index within the Indices vector.
	     *
	     * @return the maximum index value contained within this
	     * Indices vector object.  It will be zero if the vector
	     * is empty or consists entirely of zeroes (corresponding
	     * to NAs in the R index vector).
	     *
	     * If maximumIndex() exceeds the argument \a range_size
	     * supplied to the constructor, it means that an attempt
	     * is being made to read from or write to elements beyond
	     * the end of the range (which may or may not be
	     * allowable).
	     */
	    std::size_t maximumIndex() const
	    {
		return m_max_index;
	    }

	    /** @brief Minimum size of LHS after vector subassignment.
	     *
	     * This function is used during subassignment to vectors.
	     * If the vector being assigned to is not already at least
	     * as large as minimumLHSSize(), it needs to be increased
	     * in size accordingly.
	     *
	     * @return Minimum size of LHS after vector subassignment.
	     */
	    std::size_t minimumLHSSize() const
	    {
		return m_min_lhssize;
	    }
	private:
	    /* All the elements of this ListVector are
	     * NULL, except where the corresponding element of \a
	     * raw_indices was mapped into a supplementary vector element,
	     * in which case the ListVector element will be a String
	     * giving the name of that supplementary vector element.
	     */
	    GCRoot<ListVector> m_use_names;

	    std::size_t m_max_index;
	    std::size_t m_min_lhssize;

	    /** @brief Helper function to public initialize().
	     *
	     * Constructs Indices object from an IntVector.
	     *
	     * @param raw_indices Non-null pointer to an IntVector.  An
	     *          error is raised if this vector is not of one of
	     *          the two legal forms: (i) consisting entirely of
	     *          non-negative integers and/or NAs; or (ii) consisting
	     *          entirely of non-positive integers with no NAs.  In
	     *          case (i) the constructed vector is obtained by
	     *          omitting any zeroes from \a raw_indices , and
	     *          replacing any NAs in \a raw_indices by zeroes.  In case
	     *          (ii) the constructed vector is the sequence (possibly
	     *          empty) from 1 to \a range_size , omitting any
	     *          values which appear (negated) within \a
	     *          raw_indices ; zero indices are ignored.
	     *
	     * @param range_size The size of the vector or dimension into
	     *          which indexing is being performed.
	     */
	    void initialize(const IntVector* raw_indices,
			    std::size_t range_size);

	    /** @brief Helper function to public initialize().
	     *
	     * Constructs Indices object from a RealVector.
	     *
	     * @param raw_indices Non-null pointer to a RealVector.
	     *
	     *          This vector must be of one of the following
	     *          forms: (i) consisting entirely of non-negative
	     *          integral values and/or non-finite values (Inf,
	     *          -Inf, NaN); or (ii) consisting entirely of
	     *          non-positive integral values with no
	     *          non-finite values.  (Beware that at present no
	     *          error is raised if \a raw_indices contains
	     *          finite but non-integral values, but the effect
	     *          of this is undefined.)  In case (i) the
	     *          constructed vector is obtained by omitting any
	     *          zero values from \a raw_indices , and
	     *          replacing any non-finite values in \a
	     *          raw_indices by zeroes.  In case (ii) the
	     *          constructed vector is the sequence (possibly
	     *          empty) from 1.0 to \a range_size , omitting
	     *          any values which appear (negated) within \a
	     *          raw_indices ; zero indices are ignored.
	     *
	     * @param range_size The size of the vector or dimension into
	     *          which indexing is being performed.
	     */
	    void initialize(const RealVector* raw_indices,
			    std::size_t range_size);

	    /** @brief Helper function to public initialize().
	     *
	     * Constructs Indices object from a LogicalVector.
	     *
	     * In the normal case, where \a raw_indices is non-empty, the
	     * action of this function can be understood by imagining that
	     * internally the function constructs an <b>effective
	     * lvector</b> comprising logical values (including NA).  This
	     * effective lvector is constructed by starting with a copy of
	     * \a raw_indices.  If the resulting lvector is smaller than
	     * \a range_size , its elements are repeated in rotation to
	     * bring it up to \a range_size .
	     *
	     * The canonical index vector is then generated by working
	     * through the elements of the effective lvector in order.
	     * Any element that is zero is ignored. Any element that
	     * is NA results in a zero being appended to the Indices
	     * vector.  Any element of the lvector that has any other
	     * value results in that element's position within the
	     * lvector (counting from 1) being appended to the Indices
	     * vector.
	     *
	     * @param raw_indices Non-null pointer to a LogicalVector.  If
	     *          this is empty, the Indices vector will also
	     *          be empty.  Otherwise the behaviour is as described
	     *          above.
	     *
	     * @param range_size The size of the vector or dimension into
	     *          which indexing is being performed.
	     */
	    void initialize(const LogicalVector* raw_indices,
			    std::size_t range_size);

	    /** @brief helper function to public initialize().
	     *
	     * Constructs Indices object from a StringVector.
	     *
	     * @param raw_indices Non-null pointer to a StringVector.  Any
	     *          element of this which is blank or NA will be
	     *          mapped to zero in the resulting Indices
	     *          vector.  Any non-blank, non-NA element which
	     *          matches a name in \a range_names will be mapped to
	     *          the corresponding index.  (If there are duplicate
	     *          names in \a range_names it will be mapped to the
	     *          lowest corresponding index.)  Any non-blank,
	     *          non-NA element which does not match a name in \a
	     *          range_names will be considered to be associated
	     *          with a supplementary element of the range into
	     *          which indexing is being performed; this
	     *          supplementary element will be allocated the lowest
	     *          (positive) index number not already used.
	     *
	     * @param range_size The size of the vector or dimension into
	     *          which indexing is being performed.
	     *
	     * @param range_names Pointer, possibly null, to the vector of
	     *          names associated with the vector or dimension into
	     *          which indexing is being performed.  If present,
	     *          the size of this vector must be equal to \a
	     *          range_size .
	     */
	    void initialize(const StringVector* raw_indices,
			    std::size_t range_size,
			    const StringVector* range_names);
	};

	// Data structure used in subsetting arrays, containing
	// information relating to a particular dimension. 
	struct DimIndexer {
	    std::size_t indexnum;  // Position (counting from 0) of
	      // the index within this dimension currently being processed.
	    std::size_t stride;  // Number of elements (within the
	      // linear layout of the source array) separating
	      // consecutive elements along this dimension.
	};

	typedef std::vector<DimIndexer, Allocator<DimIndexer> > DimIndexerVector;

	// Not implemented.  Declared private to prevent the
	// inadvertent creation of Subscripting objects.
	Subscripting();

	/** @brief Assign to selected elements of an R matrix or array.
	 *
	 * @tparam VL A type inheriting from VectorBase.
	 * 
	 * @tparam VR A type inheriting from VectorBase.
	 * 
	 * @param lhs Non-null pointer to a \a VL object, which is
	 *          the object to whose elements assignment is to be
	 *          made.  Where feasible, the return value will
	 *          point to \a lhs itself, modified appropriately
	 *          (which is why this parameter is not
	 *          <tt>const</tt>); otherwise the return value will
	 *          point to a modified copy of \a lhs.  (Copying
	 *          will occur if \a lhs aliases \a rhs .)
	 *
	 * @param indicesvec Reference to a vector with as many
	 *          elements as \a v has dimensions.  Each element of
	 *          the vector is an Indices vector giving the index
	 *          values (counting from 1)  to be selected for the
	 *          corresponding dimension.  All indices must be in
	 *          range for the relevant dimension of \a v .
	 *          Zero (here signifying NA) is a permissible 
	 *          index value only if \a rhs has length one, in
	 *          which case the index is ignored.
	 *
	 * @param rhs Non-null pointer to the vector from which values
	 *          are to be taken.  Successive elements are assigned
	 *          to the locations within the result defined by the
	 *          canonical index vectors in \a indices .  If the
	 *          number of elements selected by \a indices is
	 *          greater than the size of \a rhs , the elements of
	 *          \a rhs are repeated in rotation.  It is an error
	 *          for \a rhs to have zero elements unless no
	 *          elements at all are selected by \a indices .  A
	 *          warning will be given if the number of elements
	 *          selected by \a indices is not equal to or a
	 *          multiple of the length of \a rhs .
	 *
	 * @result Pointer to the result of the assignment.  Its
	 * attributes (including dimensions) will be identical to
	 * those of \a lhs .
	 */
	template <class VL, class VR>
	static VL* arraySubassign(VL* lhs,
				  const std::vector<Indices>& indicesvec,
				  const VR* rhs);

	/** @brief Extract a subset from an R matrix or array.
	 *
	 * This function differs from the next one in that the \a
	 * indices parameter must contain canonical index vectors
	 * rather than arbitrary subscripting objects.
	 *
	 * @tparam V A type inheriting from VectorBase.
	 *
	 * @param v Non-null pointer to a \a V object, which is an R
	 *          matrix or array from which a subset (not
	 *          necessarily a proper subset) is to be extracted.
	 *
	 * @param indices Reference to a vector with as many elements
	 *          as \a v has dimensions.  Each element of the
	 *          vector is an Indices vector giving the index
	 *          values (counting from 1) to be selected for the
	 *          corresponding dimension.  Zero (here signifying
	 *          NA) is a permissible index value, in which 
	 *          case any corresponding elements of the output
	 *          array will have an NA value appropriate to type \a
	 *          V .  Otherwise, all indices must be in range for
	 *          the relevant dimension of \a v .
	 *
	 * @param drop true iff any dimensions of unit extent are to
	 *          be removed from the result.
	 * 
	 * @return Pointer to a newly created object of type \a V ,
	 * containing the designated subset of \a v .
	 */
	template <class V>
	static V* arraySubset(const V* v, const std::vector<Indices>& indicesvec,
			      bool drop);

	/** Canonicalize a list of subscript objects for indexing an R
	 *  matrix/array.
	 *
	 * @param indicesvec Non-null pointer to an empty vector.  The
	 *          function will convert this into a vector with the
	 *          same number of elements as \a subscripts , each
	 *          element of the vector containing the Indices
	 *          vector for the corresponding dimension.
	 * 
	 * @param v Non-null pointer to the VectorBase to which
	 *          subscripting is to be applied.
	 *
	 * @param subscripts Pointer, possibly null, to a list of
	 *          objects inheriting from RObject , with the same
	 *          number of elements as \a v has dimensions.  (\a
	 *          subscripts can be null only if \a v has zero
	 *          dimensions.)  The elements of the list represent
	 *          the subscripting to be applied for successive
	 *          dimensions of \a v .  An error will be raised if
	 *          the type or contents of any element is
	 *          inappropriate for subscripting from the dimension
	 *          in question.
	 */
	static void
	canonicalizeArraySubscripts(std::vector<Indices>* indicesvec,
				    const VectorBase* v,
				    const PairList* subscripts);

	// Non-templated auxiliary function for arraySubset(), used to
	// initialise the vector of DimIndexers.  The function returns
	// the required size of the output vector.
	static std::size_t
	createDimIndexers(DimIndexerVector* dimindexers,
			  const IntVector* source_dims,
			  const std::vector<Indices>& indicesvec);

	// Non-templated auxiliary function for arraySubset(), used to
	// set the attributes on the result.
	static void setArrayAttributes(VectorBase* subset,
				       const VectorBase* source,
			               const std::vector<Indices>& dimindexers,
				       bool drop);

	/** @brief Set the attributes on a vector subset.
	 *
	 * This function sets up the 'names' and 'srcref' attributes
	 * on an R vector obtained by subsetting, by applying the
	 * corresponding subsetting to the 'names' and 'srcref's
	 * attributes of the source object.  If the source object has
	 * no 'names' attribute, but is a one-dimensional array, then
	 * the dimension names ('row names'), if any, associated with
	 * that one dimension, are used instead.
	 *
	 * @param subset Non-null pointer to an R vector representing
	 *          a subset of \a source defined by \a indices .
	 *
	 * @param source Non-null pointer to the object of which \a
	 *          subset is a subset.
	 *
	 * @param indices Reference to the Indices object used to
	 *          form \a subset from \a source .
	 */
	static void setVectorAttributes(VectorBase* subset,
					const VectorBase* source,
					const Indices& indices);

	/** @brief Assign to selected elements of an R vector object.
	 *
	 * @tparam VL A type inheriting from VectorBase.
	 * 
	 * @tparam VR A type inheriting from VectorBase.
	 * 
	 * @param lhs Non-null pointer to a \a VL object, which is
	 *            the object to whose elements assignment is to be
	 *            made.  Where feasible, the return value will
	 *            point to \a lhs itself, modified appropriately
	 *            (which is why this parameter is not
	 *            <tt>const</tt>); otherwise the return value will
	 *            point to a modified copy of \a lhs.  (Copying
	 *            will occur if \a lhs aliases either \a rhs or
	 *            the first element of \a indices_pr , or if the
	 *            return vector needs to be larger than \a lhs .)
	 *
	 * @param indices Reference to an Indices object designating
	 *          the elements of \a lhs to be assigned to.  It is
	 *          an error for the Indices vector to 
	 *          contain any zeroes (here signifying an NA index)
	 *          if \a rhs contains more than one element.
	 *
	 * @param rhs Non-null pointer to the vector from which values
	 *          are to be taken.  Successive elements are assigned
	 *          to the locations within the result defined by the
	 *          canonical index vector in \a indices_pr .  If the
	 *          return vector is larger than \a rhs , the elements
	 *          of \a rhs are repeated in rotation.  It is an
	 *          error for \a rhs to have zero elements, unless \a
	 *          subscripts selects no locations at all.  A warning
	 *          will be given if the number of indices in the
	 *          canonical index vector is not equal to or a
	 *          multiple of the length of \a rhs .
	 *
	 * @result Pointer to the result of the assignment.  The size of the
	 * this vector will be whichever is larger of the the size of
	 * \a lhs or the second element of \a indices_pr .  If the
	 * result is larger than \a lhs , then any supplementary
	 * elements not assigned to by the specified indices will be
	 * set to the NA value of VL::value_type .
	 */
	template <class VL, class VR>
	static VL* vectorSubassign(VL* lhs,
				   const Indices& indices,
				   const VR* rhs);

	/** @brief Extract a subset of an R vector object.
	 *
	 * @tparam V A type inheriting from VectorBase.
	 *
	 * @param v Non-null pointer to a \a V object, which is the
	 *          object from which a subset (not necessarily a
	 *          proper subset) is to be extracted.
	 *
	 * @param indices Reference to an Indices object
	 *          designating the elements of \a v to be included as
	 *          successive elements of the output vector, which
	 *          will be the same size as \a indices .  Zero (here
	 *          signifying NA) is a permissible index value, in
	 *          which case the corresponding element of the output
	 *          vector will have an NA value appropriate to type
	 *          \a V .  If an index is out of range with respect
	 *          to \a v , in that case also the corresponding
	 *          element of the output vector will have an NA value
	 *          appropriate to type \a V .
	 *
	 * @return Pointer to a newly created object of type \a V ,
	 * containing the designated subset of \a v .
	 */
	template <class V>
	static V* vectorSubset(const V* v, const Indices& indices);
    };  // class Subscripting

    template <class VL, class VR>
    VL* Subscripting::arraySubassign(VL* lhs,
				     const std::vector<Indices>& indicesvec,
				     const VR* rhs)
    {
	typedef typename VL::value_type Lval;
	typedef typename VR::value_type Rval;
	const IntVector* vdims = lhs->dimensions();
	std::size_t ndims = vdims->size();
	DimIndexerVector dimindexer(ndims);
	std::size_t ni = createDimIndexers(&dimindexer, vdims, indicesvec);
	std::size_t rhs_size = rhs->size();
	if (rhs_size > 1) {
	    // TODO: Move NA-detection back into the canonicalisation
	    // process.
	    for (std::size_t d = 0; d < ndims; ++d) {
		const Indices& dimindices = indicesvec[d];
		std::size_t dsz = dimindices.size();
		for (std::size_t i = 0; i < dsz; ++i)
		    if (dimindices[i] == 0)
			Rf_error(_("NA subscripts are not allowed"
				   " in this context"));
	    }
	}
	GCStackRoot<VL> ans(lhs);
	// If necessary, make a copy to be sure we don't modify rhs.
	// (FIXME: ideally this should be a shallow copy for
	// HandleVectors.)
	const VectorBase* ansvb = static_cast<VectorBase*>(ans.get());
	if (ansvb == rhs)
	    ans = CXXR_NEW(VL(*ans.get()));
	// Dispose of 'no indices' case:
	if (ni == 0)
	    return ans;
	if (rhs_size == 0)
	    Rf_error(_("replacement has length zero"));
	if (ni%rhs_size != 0)
	    Rf_warning(_("number of items to replace is not"
			 " a multiple of replacement length"));
	// Copy elements across:
	for (std::size_t irhs = 0; irhs < ni; ++irhs) {
	    bool naindex = false;
	    std::size_t iout = 0;
	    for (std::size_t d = 0; d < ndims; ++d) {
		const DimIndexer& di = dimindexer[d];
		const Indices& dimindices = indicesvec[d];
		std::size_t index = dimindices[di.indexnum];
		if (index == 0) {
		    naindex = true;
		    break;
		}
		iout += (index - 1)*di.stride;
	    }
	    if (!naindex) {
		// Be careful not to create a temporary RHandle.
		Lval& lval = (*ans)[iout];
		const Rval& rval = (*rhs)[irhs % rhs_size];
		if (isNA(rval))
		    lval = NA<Lval>();
		else
		    lval = rval;
	    }
	    // Advance the index selection:
	    {
		std::size_t d = 0;
		bool done;
		do {
		    done = true;
		    DimIndexer& di = dimindexer[d];
		    if (++di.indexnum >= indicesvec[d].size()) {
			di.indexnum = 0;
			done = (++d >= ndims);
		    }
		} while (!done);
	    }
	}
	return ans;
    }


    template <class V>
    V* Subscripting::arraySubset(const V* v,
				 const std::vector<Indices>& indicesvec,
				 bool drop)
    {
	const IntVector* vdims = v->dimensions();
	std::size_t ndims = vdims->size();
	DimIndexerVector dimindexer(ndims);
	std::size_t resultsize = createDimIndexers(&dimindexer, vdims,
						   indicesvec);
	GCStackRoot<V> result(CXXR_NEW(V(resultsize)));
	// Copy elements across:
	{
	    // ***** FIXME *****  Currently needed because Handle's
	    // assignment operator takes a non-const RHS:
	    V* vnc = const_cast<V*>(v);
	    for (std::size_t iout = 0; iout < resultsize; ++iout) {
		bool naindex = false;
		std::size_t iin = 0;
		for (std::size_t d = 0; d < ndims; ++d) {
		    const DimIndexer& di = dimindexer[d];
		    const Indices& dimindices = indicesvec[d];
		    std::size_t index = dimindices[di.indexnum];
		    if (index == 0) {
			naindex = true;
			break;
		    }
		    iin += (index - 1)*di.stride;
		}
		(*result)[iout]
		    = (naindex ? NA<typename V::value_type>()
		       : (*vnc)[iin]);
		// Advance the index selection:
		{
		    std::size_t d = 0;
		    bool done;
		    do {
			done = true;
			DimIndexer& di = dimindexer[d];
			if (++di.indexnum >= indicesvec[d].size()) {
			    di.indexnum = 0;
			    done = (++d >= ndims);
			}
		    } while (!done);
		}
	    }
	}
	setArrayAttributes(result, v, indicesvec, drop);
	return result;
    }

    template <class VL, class VR>
    VL* Subscripting::subassign(VL* lhs, const PairList* subscripts,
				const VR* rhs)
    {
	std::size_t nsubs = listLength(subscripts);
	if (nsubs > 1)
	    return arraySubassign(lhs, subscripts, rhs);
	const IntVector* dims = lhs->dimensions();
	if (dims && dims->size() == nsubs)
	    return arraySubassign(lhs, subscripts, rhs);
	return vectorSubassign(lhs, (subscripts ? subscripts->car()
				   : Symbol::missingArgument()), rhs);
    }

    template <class V>
    V* Subscripting::subset(const V* v, const PairList* subscripts, bool drop)
    {
	std::size_t nsubs = listLength(subscripts);
	if (nsubs > 1)
	    return arraySubset(v, subscripts, drop);
	const IntVector* dims = v->dimensions();
	if (dims && dims->size() == nsubs)
	    return arraySubset(v, subscripts, drop);
	return vectorSubset(v, (subscripts ? subscripts->car()
				: Symbol::missingArgument()));
    }

    template <class VL, class VR>
    VL* Subscripting::vectorSubassign(VL* lhs,
				      const Indices& indices,
				      const VR* rhs)
    {
	typedef typename VL::value_type Lval;
	typedef typename VR::value_type Rval;
	std::size_t ni = indices.size();
	std::size_t rhs_size = rhs->size();
	if (rhs_size > 1) {
	    for (std::size_t i = 0; i < ni; ++i)
		if (indices[i] == 0)
		    Rf_error(_("NA subscripts are not allowed"
			       " in this context"));
	}
	GCStackRoot<VL> ans(lhs);
	std::size_t minsize = indices.minimumLHSSize();
	if (minsize > lhs->size())
	    ans = VectorBase::resize(lhs, minsize);
	// If necessary, make a copy to be sure we don't modify rhs or
	// indices.  (FIXME: ideally this should be a shallow copy for
	// HandleVectors.)
	const VectorBase* ansvb = static_cast<VectorBase*>(ans.get());
	if (ansvb == rhs)
	    ans = CXXR_NEW(VL(*ans.get()));
	// Dispose of 'no indices' case:
	if (ni == 0)
	    return ans;
	if (rhs_size == 0)
	    Rf_error(_("replacement has length zero"));
	if (ni%rhs_size != 0)
	    Rf_warning(_("number of items to replace is not"
			 " a multiple of replacement length"));
	for (std::size_t i = 0; i < ni; ++i) {
	    std::size_t index = indices[i];
	    if (!index == 0) {
		// Be careful not to create a temporary RHandle.
		Lval& lval = (*ans)[index - 1];
		const Rval& rval = (*rhs)[i % rhs_size];
		if (isNA(rval))
		    lval = NA<Lval>();
		else
		    lval = rval;
	    }
	}
	indices.applyNewNames(ans);
	return ans;
    }

    template <class V>
    V* Subscripting::vectorSubset(const V* v, const Indices& indices)
    {
	std::size_t ni = indices.size();
	GCStackRoot<V> ans(CXXR_NEW(V(ni)));
	std::size_t vsize = v->size();
	// ***** FIXME *****  Currently needed because Handle's
	// assignment operator takes a non-const RHS:
	V* vnc = const_cast<V*>(v);
	for (std::size_t i = 0; i < ni; ++i) {
	    std::size_t index = indices[i];
	    // Note that zero and negative indices ought not to occur.
	    if (index == 0 || index > vsize)
		(*ans)[i] = NA<typename V::value_type>();
	    else (*ans)[i] = (*vnc)[index - 1];
	}
	setVectorAttributes(ans, v, indices);
	return ans;
    }
}  // namespace CXXR;

#endif  // SUBSCRIPTING_HPP
