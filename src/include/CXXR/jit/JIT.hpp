/*CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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

#ifndef CXXR_JIT_JIT_HPP
#define CXXR_JIT_JIT_HPP

#include <memory>

#include "CXXR/jit/FrameDescriptor.hpp"
namespace CXXR {

class Closure;
class CompilerContext;
class Environment;
class Frame;
class RObject;

namespace JIT {

class FrameDescriptor;

class JITCompiledExpression {
public:
    ~JITCompiledExpression();

    RObject* evalInEnvironment(Environment* env) const
    {
	return m_function(env);
    }

    Frame* createFrame() const;

    static JITCompiledExpression* compileFunctionBody(const Closure* function);

private:
    JITCompiledExpression() { }
    JITCompiledExpression(const Closure* closure);

    // The compiled function itself.
    typedef RObject* (*CompiledExpressionPointer)(Environment* env);
    CompiledExpressionPointer m_function;

    // The interpreter requires the frame descriptor to work with the frames
    // that the compiled code generates.
    std::unique_ptr<FrameDescriptor> m_frame_descriptor;

    JITCompiledExpression(const JITCompiledExpression&) = delete;
    JITCompiledExpression& operator=(const JITCompiledExpression&) = delete;
};

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_JIT_HPP
