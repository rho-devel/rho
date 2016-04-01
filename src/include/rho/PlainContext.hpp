/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file PlainContext.hpp
 *
 * @brief Class rho::PlainContext.
 */

#ifndef PLAINCONTEXT_HPP
#define PLAINCONTEXT_HPP 1

#include "rho/Evaluator_Context.hpp"

namespace rho {
    /** @brief Lightweight Context neutralizing BailoutContext.
     *
     * A Context of this type is typically created in circumstances
     * where the current innermost Context may be a BailoutContext,
     * and it is desired to neutralize the effect of that
     * BailoutContext, i.e. to indicate to called code that Bailout
     * objects \e cannot be handled, but without creating a
     * FunctionContext or a ClosureContext.
     */
    struct PlainContext : Evaluator::Context {
	PlainContext()
	{
	    setType(PLAIN);
	}
    };
}  // namespace rho

#endif  // PLAINCONTEXT_HPP
