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

/** @file UnaryFunction.hpp
 *
 * @brief Class VectorOps::UnaryFunction and related functions.
 */

#ifndef UNARYFUNCTION_HPP
#define UNARYFUNCTION_HPP 1

#include <algorithm>
#include "CXXR/FixedVector.hpp"
#include "CXXR/errors.h"

namespace CXXR {
   /** @brief Services to support common operations on R vectors and arrays.
     *
     * This namespace encapsulates services supporting various
     * commonly occurring operations on R vector objects, including R
     * matrices and arrays.
     */
    namespace VectorOps {
	/** @brief Control attribute copying for unary functions.
	 *
	 * VectorOps::applyUnaryOperator takes as an AttributeCopier object
	 * as an argument which determines which attributes
	 * are copied from the input vector to the output vector.
	 *
	 * This class is a possible value of the \a AttributeCopier
	 * parameter, and its behaviour is to copy all attributes
	 * across, along with the S4 object status.
	 */
	struct CopyAllAttributes {
	    /** @brief Copy all attributes and S4 object status.
	     *
	     * @param to Non-null pointer to the vector to which
	     *          attributes are to be copied.
	     *
	     * @param from Non-null pointer to the vector from which
	     *          attributes are to be copied.
	     */
	    static void copyAttributes(VectorBase* to, const VectorBase* from)
	    {
		to->copyAttributes(from, true);
	    }
	};

	/** @brief Control attribute copying for unary functions.
	 *
	 * VectorOps::applyUnaryOperator takes as an AttributeCopier object
	 * as an argument which determines which attributes
	 * are copied from the input vector to the output vector.
	 *
	 * This class is a possible value of the \a AttributeCopier
	 * parameter, and its behaviour is to copy the 'names', 'dim'
	 * and 'dimnames' attributes if present.
	 */
	struct CopyLayoutAttributes {
	    /** @brief Copy 'names', 'dim' and 'dimnames' attributes.
	     *
	     * @param to Non-null pointer to the vector to which
	     *          attributes are to be copied.
	     *
	     * @param from Non-null pointer to the vector from which
	     *          attributes are to be copied.
	     */
	    static void copyAttributes(VectorBase* to, const VectorBase* from);
	};


	/** @brief Control attribute copying for unary functions.
	 *
	 * VectorOps::applyUnaryOperator takes as an AttributeCopier object
	 * as an argument which determines which attributes
	 * are copied from the input vector to the output vector.
	 *
	 * This class can be used as the value of the \a AttributeCopier
	 * parameter, and its behaviour is to copy no attributes at all.
	 */
	struct CopyNoAttributes {
	    /** @brief Copy no attributes.
	     */
	    static void copyAttributes(VectorBase*, const VectorBase*)
	    {}
	};

	// The type that Op returns when called with elements from the InputType
	// vectors.
	template<typename Op, typename... InputType>
	using OpReturnType = 
	   typename std::result_of<Op(typename InputType::value_type...)>::type;

	// The type of vector that can hold the elements that Op returns when
	// called with elements from the InputType vectors.
	template<typename Op, typename... InputType>
	using VectorOpReturnType =
	    typename VectorTypeFor<OpReturnType<Op, InputType...>>::type;

	template<typename Op, typename AttributeCopier,
		 typename InputType,
		 typename OutputType = VectorOpReturnType<Op, InputType>>
	OutputType* applyUnaryOperator(const Op& op,
				       AttributeCopier attribute_copier,
				       const InputType* input)
	{
	    OutputType* result = OutputType::create(input->size());
	    std::transform(input->begin(), input->end(), result->begin(),
			   op);
	    attribute_copier.copyAttributes(result, input);
	    return result;
	}
    }  // namespace VectorOps
}  // namespace CXXR


#endif  // UNARYFUNCTION_HPP
