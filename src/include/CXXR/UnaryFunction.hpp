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

/** @file UnaryFunction.hpp
 *
 * @brief Class VectorOps::UnaryFunction and related functions.
 */

#ifndef UNARYFUNCTION_HPP
#define UNARYFUNCTION_HPP 1

#include <algorithm>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
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
	/** @brief Control attribute copying for unary functions.
	 *
	 * VectorOps::UnaryFunction takes as a template parameter an
	 * \a AttributeCopier class which determines which attributes
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
	 * VectorOps::UnaryFunction takes as a template parameter an
	 * \a AttributeCopier class which determines which attributes
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
	 * VectorOps::UnaryFunction takes as a template parameter an
	 * \a AttributeCopier class which determines which attributes
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

	/** @brief Monitor function application for unary functions.
	 *
	 * VectorOps::UnaryFunction takes as a template parameter a \a
	 * FunctorWrapper class template.  The apply() method of
	 * UnaryFunction creates an object from this template, and
	 * delegates to it the task of calling the unary function; the
	 * \a FunctorWrapper object can then monitor any special
	 * conditions that occur, and take appropriate action either
	 * immediately (for example by raising an error and/or
	 * modifying the result value) or when processing of the input
	 * vector is complete (typically by providing one or more
	 * warnings).
	 *
	 * This template is the default value of the \a FunctorWrapper
	 * template parameter, and its effect is to ensure that if the
	 * argument to the unary function is NA, then the result is
	 * set to NA (without actually calling the unary function).
	 * If different behaviour is required, another class can be
	 * created using this class as a model.
	 *
	 * @tparam argument_type Element type of the input vector of
	 *           the UnaryFunction.
	 *
	 * @tparam result_type Element type of the result of the
	 *           UnaryFunction.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a unary function.
	 *           <tt>ElementTraits<argument_type>::Data::Type</tt>
	 *           must be convertible to the type of the argument
	 *           of this function.  The result of the function
	 *           must be assignable to \a result_type .
	 */
	template <typename argument_type,
		  typename result_type,
		  typename Functor>
	class UnaryNAPropagator
	    : public std::unary_function<argument_type, result_type> {
	public:
	    /** @brief Constructor.
	     *
	     * @param f Pointer to an object of type \a Functor
	     *          defining the unary function whose operation
	     *          this \a FunctorWrapper is to monitor.
	     */
	    UnaryNAPropagator(const Functor& f)
		: m_func(f)
	    {}
		
	    /** @brief Monitored invocation of \a f .
	     *
	     * The apply() method of an object instantiating the
	     * VectorOps::UnaryFunction template will call this
	     * function to generate a value for an element of the
	     * output vector from the value of the corresponding
	     * element of the input vector, using the functor \a f .
	     *
	     * @param in Element of the input vector to which \a f is
	     *          to be applied. 
	     *
	     * @return If \a in is NA, then
	     * ElementTraits<result_type>::NA(), and in this case \a f
	     * is not actually called.  Otherwise, the result of
	     * applying \a f to the element data of \a in .
	     */
	    result_type operator()(const argument_type& in) const
	    {
		return (isNA(in)
			? NA<result_type>() 
			: result_type((m_func)(ElementTraits::data(in))));
	    }

	    /** @brief Raise warnings after processing a vector.
	     *
	     * The apply() method of an object instantiating the
	     * VectorOps::UnaryFunction template will call this
	     * function once all the elements of the input vector have
	     * been processed.  Typically this function will do
	     * nothing if no abnormalities have occurred during the
	     * lifetime of this \a FunctorWrapper object , otherwise
	     * it will raise one or more warnings.  (Note that the
	     * lifetime of a \a FunctorWrapper object corresponds to
	     * the processing of an input vector by the apply() method
	     * of UnaryFunction.)
	     *
	     * The default behaviour, implemented here, is to do
	     * nothing.
	     */
	    void warnings()
	    {}
	private:
	    Functor m_func;
	};

	/** @brief Monitor function application for unary functions.
	 *
	 * VectorOps::UnaryFunction takes as a template parameter a \a
	 * FunctorWrapper class.  The apply() method of UnaryFunction
	 * creates an object of the \a FunctorWrapper class, and
	 * delegates to it the task of calling the unary function; the
	 * \a FunctorWrapper object can then monitor any abnormal
	 * conditions that occur, and take appropriate action either
	 * immediately (typically by raising an error) or when
	 * processing of the input vector is complete (typically by
	 * providing one or more warnings).
	 *
	 * This class is the default value of the \a FunctorWrapper
	 * template parameter, and its behaviour is to apply no
	 * monitoring at all.  If different behaviour is required,
	 * another class can be created using this class as a model.
	 *
	 * @tparam Functor a function object class inheriting from an
	 *           instantiation of the std::unary_function template
	 *           (or otherwise defining nested types \a
	 *           result_type and \a argument_type appropriately).
	 */
	template <class Functor>
	class NullUnaryFunctorWrapper
	    : public std::unary_function<typename Functor::argument_type,
					 typename Functor::result_type> {
	public:
	    // See para. 3 of ISO14882:2003 Sec. 14.6.2 for why these
	    // typedefs aren't inherited from std::unary_function:
	    typedef typename Functor::argument_type argument_type;
	    typedef typename Functor::result_type result_type;

	    /** @brief Constructor.
	     *
	     * @param f Pointer to an object of type \a Functor
	     *          defining the unary function whose operation
	     *          this \a FunctorWrapper is to monitor.
	     */
	    NullUnaryFunctorWrapper(const Functor& f)
		: m_func(f)
	    {}
		
	    /** @brief Monitored invocation of \a f .
	     *
	     * The apply() method of an object instantiating the
	     * VectorOps::UnaryFunction template will call this
	     * function to generate a value for the output vector from
	     * a value from the input vector using the functor \a f .
	     * This function will monitor the operation of \a f , and
	     * take appropriate action if abnormalities occur, for
	     * example by raising an error, modifying the return
	     * value, and/or recording the abnormality for later
	     * reporting by warnings().
	     *
	     * @param in Input value to which \a f is to be applied.
	     *
	     * @result The result of applying \a f to \a in , possibly
	     * modified if abnormalities occurred.
	     */
	    result_type operator()(const argument_type& in) const
	    {
		return (m_func)(in);
	    }

	    /** @brief Raise warnings after processing a vector.
	     *
	     * The apply() method of an object instantiating the
	     * VectorOps::UnaryFunction template will call this
	     * function once all the elements of the input vector have
	     * been processed.  Typically this function will do
	     * nothing if no abnormalities have occurred during the
	     * lifetime of this \a FunctorWrapper object , otherwise
	     * it will raise one or more warnings.  (Note that the
	     * lifetime of a \a FunctorWrapper object corresponds to
	     * the processing of an input vector by the apply() method
	     * of UnaryFunction.)
	     *
	     * The default behaviour, implemented here, is to do
	     * nothing.
	     */
	    void warnings()
	    {}
	private:
	    Functor m_func;
	};

	/** @brief Class used to transform a vector elementwise using
	 *         unary function.
	 *
	 * An object of this class is used to map one vector into
	 * new vector of equal size, with each element of the output
	 * vector being obtained from the corresponding element of the
	 * input vector by the application of a unary function.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a unary function.
	 *
	 * @tparam AttributeCopier This class must define a static
	 *           member function with the signature
	 *           <tt>copyAttributes(VectorBase* to, const
	 *           VectorBase* from)</tt>.  (Its return value, if
	 *           any, is ignored.)  The apply() method of
	 *           UnaryFunction will invoke this member function to
	 *           copy attributes from the unary function operand
	 *           <tt>from</tt> to its result <tt>to</tt>.  See the
	 *           description of VectorOps::CopyAllAttributes for
	 *           an example.
	 *
	 * @tparam FunctorWrapper Each invocation of apply() will
	 *           create an object of class
	 *           FunctorWrapper<Functor>, and delegate to it the
	 *           task of calling the unary function; the \a
	 *           FunctorWrapper objects can then monitor any
	 *           special conditions.  See the description of
	 *           VectorOps::UnaryNAPropagator for further
	 *           information.
	 */
	template <typename Functor, class AttributeCopier,
		  template <typename, typename, typename> class FunctorWrapper
                  = UnaryNAPropagator>
	class UnaryFunction {
	public:
	    /** @brief Constructor.
	     *
	     * @param f Pointer to an object of type \a Functor
	     *          defining the unary function that this
	     *          UnaryFunction object will use to generate an
	     *          output vector from an input vector.
	     */
	    UnaryFunction(const Functor& f = Functor())
		: m_f(f)
	    {}

	    /** @brief Apply a unary function to a vector.
	     *
	     * @tparam Vout Class of vector to be produced as a
	     *           result.  It must be possible to assign values
	     *           of the return type of \a f to the elements of
	     *           a vector of type \a Vout .
	     *
	     * @tparam Vin Class of vector to be taken as input.  It
	     *           must be possible implicitly to convert the
	     *           element data type of \a Vin to the input type
	     *           of \a f .
	     *
	     * @param v Non-null pointer to the input vector.
	     *
	     * @result Pointer to the result vector, a newly created
	     * vector of the same size as \a v .
	     */
	    template <class Vout, class Vin>
	    Vout* apply(const Vin* v) const;
	private:
	    Functor m_f;
	};

	/** @brief Create a UnaryFunction object from a functor.
	 *
	 * @tparam AttributeCopier The apply() method of the resulting
	 *           UnaryFunction object will create an object of
	 *           this class and use it to determine which
	 *           attributes are copied from the input vector to
	 *           the output vector.  See the description of
	 *           VectorOps::CopyAllAttributes for a commonly used
	 *           example.
	 *
	 * @tparam FunctorWrapper The apply() method of the resulting
	 *           UnaryFunction object will create an object of
	 *           class FunctorWrapper<Functor>, and delegate to it
	 *           the task of calling \a f ; the \a
	 *           FunctorWrapper objects can then monitor any
	 *           special conditions.  See the description of
	 *           VectorOps::BinaryNAPropagator for further
	 *           information.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a unary function.
	 *
	 * @param f Pointer to an object of type \a Functor defining
	 *          the unary function that the resulting
	 *          UnaryFunction object will use to generate an
	 *          output vector from the operands.
	 */
	template <class AttributeCopier,
		  template <typename,
                            typename, typename> class FunctorWrapper,
                  typename Functor>
	static UnaryFunction<Functor, AttributeCopier, FunctorWrapper>
	makeUnaryFunction(Functor f)
	{
	    return UnaryFunction<AttributeCopier, Functor, FunctorWrapper>(f);
	}

	/** @brief Create a UnaryFunction object from a functor.
	 *
	 * This differs from the previous function only in that the
	 * FunctorWrapper of the UnaryFunction is automatically set
	 * to UnaryNAPropagator.
	 *
	 * @tparam AttributeCopier The apply() method of the resulting
	 *           UnaryFunction object will create an object of
	 *           this class and use it to determine which
	 *           attributes are copied from the input vector to
	 *           the output vector.  See the description of
	 *           VectorOps::CopyAllAttributes for a commonly used
	 *           example.
	 *
	 * @tparam Functor Function or function object type,
	 *           representing a unary function.
	 *
	 * @param f Pointer to an object of type \a Functor defining
	 *          the unary function that the resulting
	 *          UnaryFunction object will use to generate an
	 *          output vector from the operands.
	 */
	template <class AttributeCopier, typename Functor>
	static UnaryFunction<Functor, AttributeCopier>
	makeUnaryFunction(Functor f)
	{
	    return UnaryFunction<Functor, AttributeCopier>(f);
	}
    }  // namespace VectorOps
}  // namespace CXXR


// ***** Implementations of non-inlined templated functions. *****

template <typename Functor, class AttributeCopier, 
	  template <typename, typename, typename> class FunctorWrapper>
template <class Vout, class Vin>
Vout* CXXR::VectorOps::UnaryFunction<Functor,
                                     AttributeCopier,
				     FunctorWrapper>::apply(const Vin* v) const
{
    using namespace boost::lambda;
    typedef typename Vin::value_type Inelt;
    typedef typename Vout::value_type Outelt;
    GCStackRoot<Vout> ans(Vout::create(v->size()));
    FunctorWrapper<Inelt, Outelt, Functor> fwrapper(m_f);
    std::transform(v->begin(), v->end(), ans->begin(),
		   [&](Inelt in) { return fwrapper(in); });
    fwrapper.warnings();
    AttributeCopier::copyAttributes(ans, v);
    return ans;
}

#endif  // UNARYFUNCTION_HPP
