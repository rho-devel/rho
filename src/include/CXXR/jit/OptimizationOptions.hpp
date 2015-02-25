/*
 *  R : A Computer Language for Statistical Data Analysis
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

#ifndef CXXR_JIT_OPTIMIZATION_OPTIONS_HPP
#define CXXR_JIT_OPTIMIZATION_OPTIONS_HPP

namespace CXXR {
namespace JIT {

struct OptimizationOptions {
    OptimizationOptions()
	: AssumeSaneControlFlowOperators(true),
	  AssumeSaneAssignmentOperators(true) { }
    
    /*
     * These options affect the semantics of R.
     */

    // May assume that the following operators have their usual meaning unless
    // they are shadowed by function arguments, local assignments, definitions
    // in the enclosing package or package imports.
    //  {, (, if, for, while, repeat, next, break, return, ||, &&
    bool AssumeSaneControlFlowOperators;

    // May assume that <- and = have their normal meanings unless shadowed.
    bool AssumeSaneAssignmentOperators;

    /* ------------------------------------------------------------------------
     * These options do not change the semantics of R, but merely optimize more
     * aggressively.
     */

    // TODO(kmillar): add optimization options here.

    // TODO(kmillar): add LLVM optimization options.
};

}  // namespace JIT
}  // namespace CXXR

#endif  // CXXR_JIT_OPTIMIZATION_OPTIONS_HPP
