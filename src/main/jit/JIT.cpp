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

#include "CXXR/jit/JIT.hpp"

#include "llvm/Analysis/Verifier.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/LinkAllIR.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/TargetSelect.h"

#include "CXXR/Closure.h"
#include "CXXR/Environment.h"
#include "CXXR/RObject.h"
#include "CXXR/jit/EmitIR.hpp"
#include "CXXR/jit/Globals.hpp"
#include "CXXR/jit/Runtime.hpp"
#include "CXXR/jit/TypeBuilder.hpp"

using llvm::BasicBlock;
using llvm::IRBuilder;
using llvm::Module;
using llvm::Value;

namespace CXXR {
namespace JIT {

JITCompiledExpression compileFunctionBody(const Closure* closure)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    EnsureGlobalsInitialized();

    const RObject* body = closure->body();
    // const Environment* enclosing_env = closure->environment();

    // Create a module to compile the code in.  MCJIT requires that each
    // separate invocation of the JIT compiler uses its own module.
    llvm::LLVMContext& context = llvm::getGlobalContext();
    Module* module = new Module("module", context);

    // TODO: cleanup objects when the closure is destroyed.
    engine = llvm::EngineBuilder(module).setUseMCJIT(true).create();
    if (!engine) {
	assert(engine);
	return nullptr;
    }
    Runtime::mergeInRuntimeModule(module);

    // Create a function with signature RObject* (*f)(Environment* environment)
    llvm::Function* function = llvm::Function::Create(
        llvm::TypeBuilder<RObject*(Environment*), false>::get(context),
	llvm::Function::InternalLinkage,
        "anonymous_function", // TODO: give it a useful name
        module);
    Value* environment = &*(function->getArgumentList().begin());
    environment->setName("environment");

    // Setup the entry block.
    BasicBlock* entry_block
        = BasicBlock::Create(context, "EntryBlock", function);
    IRBuilder<> builder(entry_block);

    // Generate code for the function body.
    Value* return_value = emitEval(body, environment, &builder);

    if (!return_value->hasName())
        return_value->setName("return_value");
    builder.CreateRet(return_value);

    // The IR is now complete.  Compile to native code.
    function->dump(); // So we can see what's going on while developing.
    llvm::verifyFunction(*function);

    // TODO: add optimization passes and re-verify.

    auto ptr = engine->getFunctionAddress(function->getName());
    assert(ptr && "JIT compilation failed");
    return reinterpret_cast<JITCompiledExpression>(ptr);
}

} // namespace JIT
} // namespace CXXR
