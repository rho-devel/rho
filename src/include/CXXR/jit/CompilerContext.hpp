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

#ifndef CXXR_JIT_COMPILER_CONTEXT_HPP
#define CXXR_JIT_COMPILER_CONTEXT_HPP

#include <stack>
#include <typeinfo>

#include "CXXR/Frame.hpp"
#include "CXXR/GCRoot.h"

namespace llvm {
    class BasicBlock;
    class Function;
    class LLVMContext;
    class Module;
    class PHINode;
    class Value;
} // namespace llvm

namespace CXXR {

    class Closure;
    class Environment;
    class Symbol;

namespace JIT {

class Compiler;
class FrameDescriptor;
class MCJITMemoryManager;
struct OptimizationOptions;

class CompilerContext {
public:
    CompilerContext(const Closure* closure,
		    llvm::Value* environment,
		    // The function to emit code into.
		    llvm::Function* function,
		    MCJITMemoryManager* memory_manager);

    ~CompilerContext();

    llvm::Function* getFunction() {
	return m_function;
    }

    llvm::Module* getModule();
    llvm::Module* getRuntimeModule();
    llvm::LLVMContext& getLLVMContext();
    MCJITMemoryManager* getMemoryManager() {
	return m_memory_manager;
    }

    // The closure that is currently being compiled.
    const Closure* getClosure() {
	return m_closure;
    }

    // The local environment of the closure.
    llvm::Value* getEnvironment() {
	return m_environment;
    }

    // The environment that encloses the closure's local environment.
    const Environment* getEnclosingEnvironment();

    // If the symbol's definition can be determined with certainty (under
    // the current optimization options), returns the function definition.
    // Otherwise returns nullptr.
    FunctionBase* staticallyResolveFunction(const Symbol* symbol);

    // Optimization options.
    const OptimizationOptions& getOptimizationOptions();

    // Returns true if all control flow operations can be statically resolved,
    // so that they can be inlined without requiring guards.
    bool canInlineControlFlow();

    // Flow-control related functions.
    llvm::BasicBlock* getBreakDestination();
    llvm::BasicBlock* getNextDestination();
    llvm::BasicBlock* getExceptionLandingPad();

    void pushLoopContext(llvm::BasicBlock* continue_block,
			 llvm::BasicBlock* loop_header,
			 Compiler* compiler);
    void popLoopContext();

    void pushExceptionHandlerContext(const std::type_info* type,
				     llvm::PHINode* handler,
				     Compiler* compiler);
    void popExceptionHandlerContext();

    // These variables are read-write and publicly accessible for use by the
    // compiler.
    GCRoot<FrameDescriptor> m_frame_descriptor;

private:
    const Closure* m_closure;
    llvm::Value* m_environment;
    llvm::Function* m_function;
    MCJITMemoryManager* m_memory_manager;

    std::stack<llvm::BasicBlock*> m_break_destinations;
    std::stack<llvm::BasicBlock*> m_next_destinations;

    std::stack<llvm::PHINode*> m_exception_handlers;
    std::stack<llvm::BasicBlock*> m_exception_landing_pads;

    static const std::set<const Symbol*>& controlFlowOperatorNames();
    static const std::set<const Symbol*>& assignmentOperatorNames();

    CompilerContext(const CompilerContext&) = delete;
    CompilerContext& operator=(const CompilerContext&) = delete;
};


class LoopScope {
public:
    LoopScope(CompilerContext* context,
	      llvm::BasicBlock* continue_block,
	      llvm::BasicBlock* loop_header,
	      Compiler* compiler)
    {
	m_context = context;
	context->pushLoopContext(continue_block, loop_header, compiler);
    }
    
    ~LoopScope()
    {
	m_context->popLoopContext();
    }
private:
    CompilerContext* m_context;
    
    LoopScope(const LoopScope&) = delete;
    LoopScope& operator=(const LoopScope&) = delete;
};

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_COMPILER_CONTEXT_HPP
