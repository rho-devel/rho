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

/** @file Bailout.hpp
 *
 * @brief Class rho::Bailout.
 */

#ifndef BAILOUT_HPP
#define BAILOUT_HPP

#include "rho/RObject.hpp"

#include "rho/SEXP_downcast.hpp"

namespace rho {
    /** @brief Class used to implement indirect flow of control in R.
     *
     * Classes derived from this abstract base class are used in
     * implementing R functions such as 'return' and 'break' that
     * result in an indirect flow of control, with the intention that
     * in the most common cases the overhead of throwing and
     * propagating a C++ exception can be avoided.
     *
     * An R function such as 'return' is implemented so that it
     * creates an object of a class inheriting from Bailout.  The
     * basic idea is that this object is then passed as a return value
     * up the chain from called function to caller, until it reaches
     * the intended destination of the indirect flow of control.
     *
     * However, this passing up the call chain happens only if the
     * caller has indicated, by wrapping its call in a BailoutContext,
     * that it is able to propagate the Bailout object correctly.  If
     * that is not the case, then the called function will invoke the
     * throwException() method of the Bailout object, which - as the
     * name suggests - will complete the indirect flow of control by
     * throwing a C++ exception.
     */
    class Bailout : public RObject {
    public:
	/** @brief Default constructor.
	 */
	Bailout()
	    : RObject(BAILSXP)
	{}

	/** @brief Throw the corresponding C++ exception.
	 */
	virtual void throwException() = 0;
    private:
	// Not implemented.  Declared to prevent compiler-generated versions:
	Bailout(const Bailout&);
        Bailout& operator=(const Bailout&);
    };
} // namespace rho

#endif // BAILOUT_HPP
