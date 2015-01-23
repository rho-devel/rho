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

/** @file BinaryFunction.hpp
 *
 * @brief Class VectorOps::BinaryFunction and related functions.
 */

#ifndef BINARYFUNCTION_HPP
#define BINARYFUNCTION_HPP 1

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
	 * VectorOps::BinaryFunction takes as a template parameter an
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

	/** @brief Monitor function application for binary functions.
	 *
	 * VectorOps::BinaryFunction takes as a template parameter a
	 * \a FunctorWrapper class template.  The apply() method of
	 * BinaryFunction creates an object from this template, and
	 * delegates to it the task of calling the binary function;
	 * the \a FunctorWrapper object can then monitor any special
	 * conditions that occur, and take appropriate action either
	 * immediately (for example by raising an error and/or
	 * modifying the result value) or when processing of the input
	 * vector is complete (typically by providing one or more
	 * warnings).
	 *
	 * This template is the default value of the \a FunctorWrapper
	 * template parameter, and its effect is to ensure that if
	 * either of the arguments to the binary function is NA, then
	 * the result is set to NA (without actually calling the
	 * binary function).  If different behaviour is required,
	 * another class can be created using this class as a model.
	 *
	 * @tparam first_argument_type Element type of the first
	 *           operand to the BinaryFunction.
	 *
	 * @tparam second_argument_type Element type of the second
	 *           operand to the BinaryFunction.
	 *
	 * @tparam result_type Element type of the result of the
	 *           BinaryFunction.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a binary function.
	 *           <tt>ElementTraits<first_argument_type>::Data::Type</tt>
	 *           must be convertible to the type of the first
	 *           argument of this function, and likewise
	 *           <tt>ElementTraits<second_argument_type>::Data::Type</tt>
	 *           must be convertible to the type of the second
	 *           argument of this function.  The result of the
	 *           function must be assignable to \a result_type .
	 */
	template <typename first_argument_type,
		  typename second_argument_type,
		  typename result_type,
		  typename Functor>
	class BinaryNAPropagator
	    : public std::binary_function<first_argument_type,
					  second_argument_type,
					  result_type> {
	public:
	    /** @brief Constructor.
	     *
	     * @param f Pointer to an object of type \a Functor
	     *          defining the binary function whose operation
	     *          this \a FunctorWrapper is to monitor.
	     */
	    BinaryNAPropagator(const Functor& f)
		: m_func(f)
	    {}
		
	    /** @brief Monitored invocation of \a f .
	     *
	     * The apply() method of an object instantiating the
	     * VectorOps::BinaryFunction template will call this
	     * function to generate a value for an element of the
	     * output vector from the values of the corresponding
	     * elements of the operands, using the functor \a f .
	     *
	     * @param left First argument to which \a f is to be applied.
	     *
	     * @param right Second argument to which \a f is to be applied.
	     *
	     * @return If either \a left or \a right is NA, then
	     * ElementTraits<result_type>::NA(), and in this case \a f is not
	     * actually called.  Otherwise, the result of applying \a
	     * f to the element data of \a left and \a right .
	     */
	    result_type operator()(const first_argument_type& left,
				   const second_argument_type& right) const
	    {
		return (isNA(left) || isNA(right)
			? NA<result_type>() 
			: result_type((m_func)(ElementTraits::data(left),
					       ElementTraits::data(right))));
	    }

	    /** @brief Raise warnings after processing a vector.
	     *
	     * The apply() method of an object instantiating the
	     * VectorOps::BinaryFunction template will call this
	     * function once all the elements of the input vector have
	     * been processed.  Typically this function will do
	     * nothing if no abnormalities have occurred during the
	     * lifetime of this \a FunctorWrapper object , otherwise
	     * it will raise one or more warnings.  (Note that the
	     * lifetime of a \a FunctorWrapper object corresponds to
	     * the processing of an input vector by the apply() method
	     * of BinaryFunction.)
	     *
	     * The default behaviour, implemented here, is to do
	     * nothing.
	     */
	    void warnings()
	    {}
	private:
	    Functor m_func;
	};

	/** @brief Monitor function application for binary functions.
	 *
	 * This class is a possible value of the \a FunctorWrapper
	 * template parameter of VectorOps::BinaryFunction, and its
	 * behaviour is to apply no special monitoring at all.
	 *
	 * @tparam first_argument_type Element type of the first
	 *           operand to the BinaryFunction.
	 *
	 * @tparam second_argument_type Element type of the second
	 *           operand to the BinaryFunction.
	 *
	 * @tparam result_type Element type of the result of the
	 *           BinaryFunction.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a binary function.
	 *           <tt>ElementTraits<first_argument_type>::Data::Type</tt>
	 *           must be convertible to the type of the first
	 *           argument of this function, and likewise
	 *           <tt>ElementTraits<second_argument_type>::Data::Type</tt>
	 *           must be convertible to the type of the second
	 *           argument of this function.  The result of the
	 *           function must be assignable to \a result_type .
	 */
	template <typename first_argument_type,
		  typename second_argument_type,
		  typename result_type,
		  typename Functor>
	class NullBinaryFunctorWrapper
	    : public std::binary_function<first_argument_type,
					  second_argument_type,
					  result_type> {
	public:
	    /** @brief Constructor.
	     *
	     * @param f Pointer to an object of type \a Functor
	     *          defining the binary function whose operation
	     *          this \a FunctorWrapper is to monitor.
	     */
	    NullBinaryFunctorWrapper(const Functor& f)
		: m_func(f)
	    {}
		
	    /** @brief Monitored invocation of \a f .
	     *
	     * @param left First argument to which \a f is to be applied.
	     *
	     * @param right Second argument to which \a f is to be applied.
	     *
	     * @return The result of applying \a f to the element data
	     * of \a left and \a right .
	     */
	    result_type operator()(const first_argument_type& left,
				   const second_argument_type& right) const
	    {
		return (m_func)(ElementTraits::data(left),
				ElementTraits::data(right));
	    }

	    /** @brief Raise warnings after processing a vector.
	     *
	     * The behaviour implemented here is to do nothing.
	     */
	    void warnings()
	    {}
	private:
	    Functor m_func;
	};

	/** @brief Class used to apply a binary function to vectors.
	 *
	 * An object of this class is used to create a vector from two
	 * others (the operands); the operands must be conformable as
	 * tested by checkOperandsConformable().
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
	 * @tparam Functor Function or function object type,
	 *           representing a binary function.
	 *
	 * @tparam AttributeCopier This class must define a static
	 *           member function with the signature
	 *           <tt>copyAttributes(VectorBase* vout, const
	 *           VectorBase* vl, const VectorBase* vr)</tt>.  (Its return
	 *           value, if any, is ignored.)  The apply() method
	 *           of BinaryFunction will invoke this member
	 *           function to copy attributes from the operands
	 *           <tt>vl</tt> and <tt>vr</tt> of the binary
	 *           function to its result <tt>vout</tt>.  See the
	 *           description of
	 *           VectorOps::GeneralBinaryAttributeCopier for a 
	 *           commonly used example (in fact, the default).
	 *
	 * @tparam FunctorWrapper Each invocation of apply() will
	 *           create an object of class
	 *           FunctorWrapper<Functor>, and delegate to it the
	 *           task of calling the binary function; the \a
	 *           FunctorWrapper objects can then monitor any
	 *           special conditions.  See the description of
	 *           VectorOps::BinaryNAPropagator for further
	 *           information.
	 */
	template <typename Functor,
                  class AttributeCopier = GeneralBinaryAttributeCopier,
		  template <typename, typename,
                            typename, typename> class FunctorWrapper
	          = BinaryNAPropagator>
	class BinaryFunction {
	public:
	    /** @brief Constructor.
	     *
	     * @param f Pointer to an object of type \a Functor
	     *          defining the binary function that this
	     *          BinaryFunction object will use to generate an
	     *          output vector from the operands.
	     */
	    BinaryFunction(const Functor& f = Functor())
		: m_f(f)
	    {}

	    /** @brief Apply a binary function to a vector.
	     *
	     * @tparam Vout Class of vector to be produced as a
	     *           result.  It must be possible to assign values
	     *           of the return type of \a f to the elements of
	     *           a vector of type \a Vout .
	     *
	     * @tparam Vl Class of vector forming the first operand.  It
	     *           must be possible implicitly to convert the
	     *           element data type of \a Vl to the type of the first
	     *           argument of \a f .
	     *
	     * @tparam Vr Class of vector forming the second operand.  It
	     *           must be possible implicitly to convert the
	     *           element data type of \a Vr to the type of the second
	     *           argument of \a f .
	     *
	     * @param vl Non-null pointer to the first operand.
	     *
	     * @param vr Non-null pointer to the second operand.
	     *
	     * @result Pointer to the result vector.
	     */
	    template <class Vout, class Vl, class Vr>
	    Vout* apply(const Vl* vl, const Vr* vr) const;
	private:
	    Functor m_f;

	    // This is called by apply() to calculate the elements of
	    // the result, with 'flag' set as follows: -1 if the first
	    // operand is shorter, 0 if the operands are of equal
	    // length, or +1 if the second operand is shorter.
	    template <int flag, class Vout, class Vl, class Vr>
	    void mapElements(Vout* vout, const Vl* vl, const Vr* vr) const;
	};

	/** @brief Create a BinaryFunction object from a functor.
	 *
	 * @tparam AttributeCopier The apply() method of the resulting
	 *           BinaryFunction object will create an object of
	 *           this class and use it to determine which
	 *           attributes are copied from the operands to the
	 *           output vector.  See the description of
	 *           VectorOps::GeneralBinaryAttributeCopier for a
	 *           commonly used example.
	 *
	 * @tparam FunctorWrapper The apply() method of the resulting
	 *           BinaryFunction object will create an object of
	 *           class FunctorWrapper<Functor>, and delegate to it
	 *           the task of calling \a f ; the \a FunctorWrapper
	 *           objects can then monitor any special conditions.
	 *           See the description of
	 *           VectorOps::BinaryNAPropagator for further
	 *           information.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a binary function.
	 *
	 * @param f Pointer to an object of type \a Functor defining
	 *          the binary function that the resulting
	 *          BinaryFunction object will use to generate an
	 *          output vector from the operands.
	 */
	template <class AttributeCopier,
		  template <typename, typename,
                            typename, typename> class FunctorWrapper,
		  typename Functor>
	static BinaryFunction<Functor, AttributeCopier, FunctorWrapper>
	makeBinaryFunction(Functor f)
	{
	    return BinaryFunction<Functor, AttributeCopier, FunctorWrapper>(f);
	}

	/** @brief Create a BinaryFunction object from a functor.
	 *
	 * This differs from the previous function only in that the
	 * FunctorWrapper of the BinaryFunction is automatically set
	 * to BinaryNAPropagator.
	 *
	 * @tparam AttributeCopier The apply() method of the resulting
	 *           BinaryFunction object will create an object of
	 *           this class and use it to determine which
	 *           attributes are copied from the operands to the
	 *           output vector.  See the description of
	 *           VectorOps::GeneralBinaryAttributeCopier for a
	 *           commonly used example.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a binary function.
	 *
	 * @param f Pointer to an object of type \a Functor defining
	 *          the binary function that the resulting
	 *          BinaryFunction object will use to generate an
	 *          output vector from the operands.
	 */
	template <class AttributeCopier, typename Functor>
	static BinaryFunction<Functor, AttributeCopier>
	makeBinaryFunction(Functor f)
	{
	    return BinaryFunction<Functor, AttributeCopier>(f);
	}
    }  // namespace VectorOps
}  // namespace CXXR


// ***** Implementations of non-inlined templated functions. *****

template <typename Functor, class AttributeCopier,
	  template <typename, typename,
		    typename, typename> class FunctorWrapper>
template <class Vout, class Vl, class Vr>
Vout* CXXR::VectorOps::BinaryFunction<Functor,
				      AttributeCopier,
		                      FunctorWrapper>::apply(const Vl* vl,
							     const Vr* vr) const
{
    checkOperandsConformable(vl, vr);
    std::size_t lsize = vl->size();
    std::size_t rsize = vr->size();
    GCStackRoot<Vout> ans;
    if (lsize == 0 || rsize == 0) {
	ans = Vout::create(0);
    } else if (lsize == rsize) {
	ans = Vout::create(lsize);
	mapElements<0>(ans.get(), vl, vr);
    } else if (lsize > rsize) {
	ans = Vout::create(lsize);
	mapElements<1>(ans.get(), vl, vr);
    } else {
	ans = Vout::create(rsize);
	mapElements<-1>(ans.get(), vl, vr);
    }
    AttributeCopier::copyAttributes(ans, vl, vr);
    return ans;
}

template <typename Functor, class AttributeCopier, 
	  template <typename, typename,
		    typename, typename> class FunctorWrapper>
template <int flag, class Vout, class Vl, class Vr>
void CXXR::VectorOps::BinaryFunction<Functor,
				     AttributeCopier,
				     FunctorWrapper>::mapElements(Vout* vout,
								  const Vl* vl,
								  const Vr* vr) const
{
    typedef typename Vl::value_type Lelt;
    typedef typename Vl::const_iterator Lit;
    typedef typename Vr::value_type Relt;
    typedef typename Vr::const_iterator Rit;
    typedef typename Vout::value_type Oelt;
    typedef typename Vout::iterator Oit;
    FunctorWrapper<Lelt, Relt, Oelt, Functor> fwrapper(m_f);
    Lit lit = vl->begin();
    Lit lend = vl->end();
    Rit rit = vr->begin();
    Rit rend = vr->end();
    Oit oend = vout->end();
    for (Oit oit = vout->begin(); oit != oend; ++oit) {
	*oit = fwrapper(*lit++, *rit++);
	if (flag < 0 && lit == lend)
	    lit = vl->begin();
	if (flag > 0 && rit == rend)
	    rit = vr->begin();
    }
    if ((flag < 0 && lit != vl->begin())
	|| (flag > 0 && rit != vr->begin()))
	Rf_warning(_("longer object length is not"
		     " a multiple of shorter object length"));
    fwrapper.warnings();
}
	
#endif  // BINARYFUNCTION_HPP
