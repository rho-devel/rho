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

#include "llvm/Analysis/Verifier.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/LinkAllIR.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetOptions.h"

#define R_NO_REMAP
#include "CXXR/jit/CompiledExpression.hpp"

#include "CXXR/jit/CompiledFrame.hpp"
#include "CXXR/jit/Compiler.hpp"
#include "CXXR/jit/CompilerContext.hpp"
#include "CXXR/jit/Globals.hpp"
#include "CXXR/jit/MCJITMemoryManager.hpp"
#include "CXXR/jit/Runtime.hpp"
#include "CXXR/jit/TypeBuilder.hpp"

#include "CXXR/Closure.h"
#include "CXXR/Environment.h"
#include "CXXR/RObject.h"

using llvm::Module;
using llvm::Value;

namespace CXXR {
namespace JIT {

CompiledExpression*
CompiledExpression::compileFunctionBody(const Closure* closure)
{
    return new CompiledExpression(closure);
}

CompiledExpression::CompiledExpression(const Closure* closure)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    EnsureGlobalsInitialized();

    const RObject* body = closure->body();

    // Create a module to compile the code in.  MCJIT requires that each
    // separate invocation of the JIT compiler uses its own module.
    llvm::LLVMContext& context = llvm::getGlobalContext();
    Module* module = Runtime::createModule(context);

    llvm::TargetOptions options;
    // TODO(kmillar): set options dynamically.
    options.JITEmitDebugInfo = true;
    options.NoFramePointerElim = true;
    options.EnableFastISel = true;

    MCJITMemoryManager* memory_manager = new MCJITMemoryManager(module);
    m_engine.reset(llvm::EngineBuilder(module)
		   .setUseMCJIT(true)
		   .setMCJITMemoryManager(memory_manager)
                   // TODO(kmillar): work out why this causes problems.
                   // .setOptLevel(llvm::CodeGenOpt::None)
                   .setTargetOptions(options)
		   .create());
    assert(m_engine);

    // Create a function with signature RObject* (*f)(Environment* environment)
    llvm::Function* function = llvm::Function::Create(
	llvm::TypeBuilder<RObject*(Environment*), false>::get(context),
	llvm::Function::InternalLinkage,
	"anonymous_function", // TODO: give it a useful name
	module);
    Value* environment = &*(function->getArgumentList().begin());
    environment->setName("environment");

    // Setup the compiler and generate code.
    CompilerContext compiler_context(closure, environment, function,
				     memory_manager);
    Compiler compiler(&compiler_context);
    Value* return_value = compiler.emitEval(body);

    if (!return_value->hasName())
	return_value->setName("return_value");
    compiler.CreateRet(return_value);

    // The IR is now complete.  Compile to native code.
    // function->dump(); // So we can see what's going on while developing.
    llvm::verifyFunction(*function);

    // TODO: add optimization passes and re-verify.

    auto ptr = m_engine->getFunctionAddress(function->getName());
    assert(ptr && "JIT compilation failed");

    m_function = reinterpret_cast<CompiledExpressionPointer>(ptr);
    m_frame_descriptor = compiler_context.m_frame_descriptor;
}

CompiledExpression::~CompiledExpression()
{
    // m_engine->freeMachineCodeForFunction(m_function);
}

Frame* CompiledExpression::createFrame() const {
  return CXXR_NEW(CompiledFrame(m_frame_descriptor));
}

bool CompiledExpression::hasMatchingFrameLayout(const Environment* env) const
{
  const CompiledFrame* frame = dynamic_cast<const CompiledFrame*>(env->frame());
  if (!frame)
    return false;
  return frame->getDescriptor() == m_frame_descriptor.get();
}

} // namespace JIT
} // namespace CXXR
