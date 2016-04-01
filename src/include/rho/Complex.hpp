/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file Complex.hpp
 * @brief Class rho::Complex.
 */

#ifndef RHO_COMPLEX_VECTOR_HPP
#define RHO_COMPLEX_VECTOR_HPP

#include "rho/ElementTraits.hpp"
#include "rho/Logical.hpp"
#include "rho/RealVector.hpp"  // for isNA(double)
#include "R_ext/Complex.h"

namespace rho
{
    /** @brief rho's extension of CR's Rcomplex.
     *
     * This class is a wrapper around the C struct Rcomplex defined by
     * CR.
     *
     * @note Backwards compatibility requires that <tt>sizeof(Complex)
     * == sizeof(Rcomplex)</tt>.
     */
    struct Complex : public Rcomplex {
	/** @brief Default constructor.
	 *
	 * Leaves data fields uninitialised.
	 */
	Complex()
	{}

	/** @brief Primary constructor.
	 *
	 * @param rl Real part.
	 *
	 * @param im Imaginary part.
	 */
	Complex(double rl, double im = 0.0)
	{
	    r = rl;
	    i = im;
	}

	explicit Complex(Logical l) : Complex(static_cast<double>(l)) {}

	/** @brief Assignment from double.
	 *
	 * @param rhs Value to be assigned.
	 *
	 * @return Reference to this object.
	 */
	Complex& operator=(double rhs)
	{
	    r = rhs;
	    i = 0;
	    return *this;
	}
    };

    // Template specializations:
    namespace ElementTraits {
	template<>
	struct MustConstruct<Complex> : boost::mpl::false_ {};

	template<>
	struct MustDestruct<Complex> : boost::mpl::false_ {};

	template <>
	inline const Complex& NAFunc<Complex>::operator()() const
	{
	    static Complex na(NA_REAL, NA_REAL);
	    return na;
	}

	template <>
	inline bool IsNA<Complex>::operator()(const Complex& c) const
	{
	    return isNA(c.r) || isNA(c.i);
	}

	template <>
	inline bool IsNaOrNaN<Complex>::operator()(const Complex& c) const
	{
	    return isNaOrNaN(c.r) || isNaOrNaN(c.i);
	}
    }
}  // namespace rho

#endif  // RHO_COMPLEX_VECTOR_HPP
