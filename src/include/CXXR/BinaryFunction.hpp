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

/** @file BinaryFunction.hpp
 *
 * @brief Class VectorOps::BinaryFunction and related functions.
 */

#ifndef BINARYFUNCTION_HPP
#define BINARYFUNCTION_HPP 1

#include "CXXR/UnaryFunction.hpp"
#include "CXXR/VectorBase.h"
#include "CXXR/errors.h"

namespace CXXR {
   /** @brief Services to support common operations on R vectors and arrays.
     *
     * This namespace encapsulates services supporting various
     * commonly occurring operations on R vector objects, including R
     * matrices and arrays.
     */
    namespace VectorOps {
	/** @brief Are binary operands consistent?
	 *
	 * This function checks the operands of a binary vector
	 * function to verify that they are compatible with each
	 * other, and raises an error if not.
	 *
	 * An error will be raised in the following circumstances:
	 * <ul>
	 * <li>Both operands are arrays, but with different
	 * dimensions.</li>
	 *
	 * <li>Both operands are time series, but they are not
	 * conformable (i.e. they do not have the same start time, end
	 * time and frequency).</li>
	 *
	 * <li>Just one operand is a time series, and the
	 * non-time-series operand is longer than the time-series
	 * operand.</li>
	 * </ul>
	 *
	 * @param vl Non-null pointer to the first operand.
	 *
	 * @param vr Non-null pointer to the second operand.
	 */
	void checkOperandsConformable(const VectorBase* vl,
				      const VectorBase* vr);

	/** @brief Control attribute copying for binary functions.
	 *
	 * VectorOps::applyBinaryOp takes as a template parameter an
	 * \a AttributeCopier class which determines which attributes
	 * are copied from the input vectors to the output
	 * vector. This class can be used as the value of the \a
	 * AttributeCopier parameter, and acts as follows:
	 * <ul>
	 * <li>If both operands are arrays (in which case they must
	 * have the same dimensions), the result will be an array with
	 * the same dimensions.  Dimension names are taken from the
	 * first operand if it has them, otherwise from the second
	 * operand, if it has them.</li>
	 *
	 * <li>If just one operand is array, the result will be an
	 * array with the same dimensions.  Dimension names are taken
	 * from the array operand, if it has them.</li>
	 *
	 * <li>If neither operand is an array, the 'names' attribute
	 * is taken from the first operand if it has them, otherwise
	 * from the second operand, if it has them.</li>
	 *
	 * <li>If both operands are time series (in which case they
	 * must have the same parameters, i.e. start time, end time
	 * and frequency), the result will be a time series with the
	 * same parameters.  The result will take its class attribute
	 * from the first operand if it has one, otherwise from the
	 * second operand, if it has one.</li>
	 *
	 * <li>If just one operand is a time series, the result will
	 * be a time series with the same parameters.  The result from
	 * take its class attribute from the time-series operand, if
	 * it has one.</li>
	 * </ul>
	 *
	 * Note that no class attribute is applied to the result
	 * unless at least one of the operands is a time series.
	 */
	class GeneralBinaryAttributeCopier {
	public:
	    /** @brief Copy attributes as described above.
	     *
	     * @param vout Non-null pointer to the vector to which
	     *          attributes are to be copied.
	     *
	     * @param vl Non-null pointer to the first operand.
	     *
	     * @param vr Non-null pointer to the second operand.
	     */
	    static void copyAttributes(VectorBase* vout,
				       const VectorBase* vl,
				       const VectorBase* vr)
	    {
		if (!vl->attributes() && !vr->attributes())
		    return;
		apply(vout, vl, vr);
	    }
	private:
	    // Deal with non-trivial cases:
	    static void apply(VectorBase* vout,
			      const VectorBase* vl,
			      const VectorBase* vr);
	};


	class BinaryArithmeticAttributeCopier {
	public:
	    static void copyAttributes(VectorBase* vout,
				       const VectorBase* vl,
				       const VectorBase* vr)
	    {
		if (!vl->attributes() && !vr->attributes())
		    return;

		/* Copy most attributes from longer argument. */
		if (vl->size() > vr->size())
		    Rf_copyMostAttrib(const_cast<VectorBase*>(vl), vout);
		else if (vl->size() < vr->size())
		    Rf_copyMostAttrib(const_cast<VectorBase*>(vr), vout);
		else {
		    Rf_copyMostAttrib(const_cast<VectorBase*>(vr), vout);
		    Rf_copyMostAttrib(const_cast<VectorBase*>(vl), vout);
		}
		
		/* Handle remaining attributes. */
		GeneralBinaryAttributeCopier::copyAttributes(vout, vl, vr);
		if (vl->isS4Object() || vr->isS4Object()) {
		    vout->setS4Object(true);
		}
	    }
	};


	/** @brief Apply a binary function to a pair of vectors.
	 *
	 * If either operand has size zero then the result will have
	 * size zero.  Otherwise, the result will have the same number
	 * of elements as the larger of the operands, and each element
	 * of the result is obtained by applying the binary function
	 * to the corresponding elements of the two operands (treating
	 * them for this purpose as vectors).  If the operands are of
	 * unequal length, the elements in the shorter operand are
	 * recycled as necessary; in this case a warning is raised if
	 * shorter operand has non-zero length but its length is not
	 * an exact submultiple of the length of the longer operand.
	 *
	 * @tparam Op A function object defining the operation to apply to
	 *           each pair of elements.
	 *
	 * @tparam AttributeCopier  A class which determines which attributes
	 * are copied from the input vectors to the output
	 * vector. This class must define a function
	 *  void copyAttributes(VectorBase* output,
	 *                      const VectorBase* lhs, const VectorBase* rhs)
	 * which sets the attributes on \a output when called.
	 *
	 * @tparam LhsType Class of vector forming the first operand.  It
	 *           must be possible implicitly to convert the
	 *           element data type of \a LhsType to the type of the first
	 *           argument of \a op.
	 *
	 * @tparam RhsType Class of vector forming the second operand.  It
	 *           must be possible implicitly to convert the
	 *           element data type of \a RhsType to the type of the second
	 *           argument of \a op.
	 *
	 * @tparam OutputType Class of vector returned by the function.  It
	 *           must be possible implicitly to convert the return value
	 *           of \a op to this type's element data type.
	 */
	template<typename Op, typename AttributeCopier,
		 typename LhsType, typename RhsType,
		 typename OutputType = VectorOpReturnType<Op, LhsType, RhsType>>
	OutputType* applyBinaryOperator(const Op& op,
					AttributeCopier attribute_copier,
					const LhsType* lhs,
					const RhsType* rhs)
	{
	    size_t lhs_size = lhs->size();
	    size_t rhs_size = rhs->size();
	    size_t size = std::max(lhs->size(), rhs->size());
	    if (lhs_size == 0 || rhs_size == 0) {
		/* S4-compatibility change: if lhs or rhs are zero length, then
		   the result is zero length. */
		size = 0;
	    }
	    OutputType* result = OutputType::create(size);
	    if (size == 1) {
		(*result)[0] = op((*lhs)[0], (*rhs)[0]);
	    } else if (lhs_size == 1) {
		// TODO: move these into a separate function so that the scalar
		//  case can be inlined.
		typename LhsType::value_type lhs_value = (*lhs)[0];
		for (size_t i = 0; i < size; i++) {
		    (*result)[i] = op(lhs_value, (*rhs)[i]);
		}
	    } else if (rhs_size == 1) {
		typename RhsType::value_type rhs_value = (*rhs)[0];
		for (size_t i = 0; i < size; i++) {
		    (*result)[i] = op((*lhs)[i], rhs_value);
		}
	    } else if (lhs_size == rhs_size) {
		for (size_t i = 0; i < size; i++) {
		    (*result)[i] = op((*lhs)[i], (*rhs)[i]);
		}
	    } else {
		// Full recycling rule.
		size_t lhs_i = 0, rhs_i = 0;
		for (size_t i = 0; i < size; i++)
		{
		    (*result)[i] = op((*lhs)[lhs_i], (*rhs)[rhs_i]);

		    lhs_i = lhs_i + 1 == lhs_size ? 0 : lhs_i + 1;
		    rhs_i = rhs_i + 1 == rhs_size ? 0 : rhs_i + 1;
		}

		if (lhs_i != 0 || rhs_i != 0) {
		    Rf_warning(_("longer object length is not"
				 " a multiple of shorter object length"));
		}
	    }

	    attribute_copier.copyAttributes(result, lhs, rhs);
	    return result;
	}

    }  // namespace VectorOps
}  // namespace CXXR
	
#endif  // BINARYFUNCTION_HPP
