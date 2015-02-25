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

#ifndef CXXR_JIT_COMPILED_EXPRESSION_HPP
#define CXXR_JIT_COMPILED_EXPRESSION_HPP

#include <memory>

#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"

namespace llvm {

class ExecutionEngine;
}

namespace CXXR {

class Closure;
class CompilerContext;
class Environment;
class Frame;
class RObject;

namespace JIT {

class FrameDescriptor;

class CompiledExpression : public GCNode {
public:
    ~CompiledExpression();

    RObject* evalInEnvironment(Environment* env) const
    {
	GCStackRoot<const GCNode> protect(this);
	return m_function(env);
    }

    Frame* createFrame() const;

    bool hasMatchingFrameLayout(const Environment* env) const;

    static CompiledExpression* compileFunctionBody(const Closure* function);

    void detachReferents() override;
    void visitReferents(const_visitor* v) const override;

private:
    CompiledExpression(const Closure* closure);

    // The compiled function itself.
    typedef RObject* (*CompiledExpressionPointer)(Environment* env);
    CompiledExpressionPointer m_function;

    // The interpreter requires the frame descriptor to work with the frames
    // that the compiled code generates.
    GCEdge<FrameDescriptor> m_frame_descriptor;

    // TODO(kmillar): we ought to have a single engine that is shared by many
    //   functions.
    std::unique_ptr<llvm::ExecutionEngine> m_engine;

    CompiledExpression(const CompiledExpression&) = delete;
    CompiledExpression& operator=(const CompiledExpression&) = delete;
};

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_COMPILED_EXPRESSION_HPP
