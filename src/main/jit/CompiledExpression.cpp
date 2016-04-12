/*
 *  R : A Computer Language for Statistical Data Analysis
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
#include "rho/jit/llvm.hpp"

#define R_NO_REMAP
#include "rho/jit/CompiledExpression.hpp"

#include "rho/jit/CompiledFrame.hpp"
#include "rho/jit/Compiler.hpp"
#include "rho/jit/CompilerContext.hpp"
#include "rho/jit/Globals.hpp"
#include "rho/jit/MCJITMemoryManager.hpp"
#include "rho/jit/Runtime.hpp"
#include "rho/jit/TypeBuilder.hpp"

#include "rho/Closure.hpp"
#include "rho/Environment.hpp"
#include "rho/RObject.hpp"

using llvm::Module;
using llvm::Value;

namespace rho {
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
    std::unique_ptr<Module> module = Runtime::createModule(context);

    // Create a function with signature RObject* (*f)(Environment* environment)
    llvm::Function* function = llvm::Function::Create(
	llvm::TypeBuilder<RObject*(Environment*), false>::get(context),
	llvm::Function::ExternalLinkage,
	"anonymous_function", // TODO: give it a useful name
	module.get());
    Value* environment = &*(function->getArgumentList().begin());
    environment->setName("environment");

    // Setup the compiler and generate code.
    std::unique_ptr<MCJITMemoryManager> memory_manager(
        new MCJITMemoryManager(module.get()));
    CompilerContext compiler_context(closure, environment, function,
				     memory_manager.get());
    Compiler compiler(&compiler_context);
    Value* return_value = compiler.emitEval(body);

    if (!return_value->hasName())
	return_value->setName("return_value");
    compiler.CreateRet(return_value);

    // function->dump(); // So we can see what's going on while developing.
    llvm::verifyFunction(*function);

    // TODO: add optimization passes and re-verify.

    // The IR is now complete.  Compile to native code.
    module->setTargetTriple(llvm::sys::getProcessTriple());

    llvm::TargetOptions options;
    // TODO(kmillar): set options dynamically.
    options.JITEmitDebugInfo = true;
    options.NoFramePointerElim = true;
    options.EnableFastISel = true;

    m_engine.reset(
#if (LLVM_VERSION < 306)
                   llvm::EngineBuilder(module.release())
		   .setMCJITMemoryManager(memory_manager.release())
		   .setUseMCJIT(true)
#else
                   llvm::EngineBuilder(std::move(module))
		   .setMCJITMemoryManager(std::move(memory_manager))
#endif
		   .setOptLevel(llvm::CodeGenOpt::None)
                   .setTargetOptions(options)
                   .setMCPU(llvm::sys::getHostCPUName())
		   .create());
    assert(m_engine);

    m_engine->finalizeObject();
    auto ptr = m_engine->getFunctionAddress(function->getName());
    assert(ptr && "JIT compilation failed");

    m_function = reinterpret_cast<CompiledExpressionPointer>(ptr);
    m_frame_descriptor = compiler_context.m_frame_descriptor;
}

CompiledExpression::~CompiledExpression()
{
    // m_engine->freeMachineCodeForFunction(m_function);
}

void CompiledExpression::detachReferents() {
    m_frame_descriptor = nullptr;
    GCNode::detachReferents();
}

void CompiledExpression::visitReferents(const_visitor* v) const {
    (*v)(m_frame_descriptor);
    GCNode::visitReferents(v);
}

Frame* CompiledExpression::createFrame() const {
  return new CompiledFrame(m_frame_descriptor);
}

bool CompiledExpression::hasMatchingFrameLayout(const Environment* env) const
{
  const CompiledFrame* frame = dynamic_cast<const CompiledFrame*>(env->frame());
  if (!frame)
    return false;
  return frame->getDescriptor() == m_frame_descriptor.get();
}

} // namespace JIT
} // namespace rho
