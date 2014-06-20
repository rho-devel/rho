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

#include "CXXR/jit/Runtime.hpp"

#include "CXXR/jit/Compiler.hpp"
#include "CXXR/jit/Globals.hpp"
#include "CXXR/jit/TypeBuilder.hpp"
#include "CXXR/RObject.h"
#include "Defn.h"
#include "Rinternals.h"

#undef _
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"
#include "llvm/Transforms/Utils/Cloning.h"

using llvm::Constant;
using llvm::ConstantExpr;
using llvm::ConstantInt;
using llvm::Function;
using llvm::FunctionType;
using llvm::IRBuilder;
using llvm::LLVMContext;
using llvm::Linker;
using llvm::Module;
using llvm::StructType;
using llvm::Type;
using llvm::TypeBuilder;
using llvm::Value;
using llvm::dyn_cast;

namespace CXXR {

namespace JIT {

llvm::Module* getModule(IRBuilder<>* builder)
{
    return builder->GetInsertBlock()->getParent()->getParent();
}

namespace Runtime {

static Module* getRuntimeModule(LLVMContext& context);

llvm::Function* getDeclaration(FunctionId function, llvm::Module* module)
{
    std::string name = getName(function);
    llvm::Function* resolved_function = module->getFunction(name);
    return resolved_function;
}

static llvm::Function* getDeclaration(FunctionId fun, IRBuilder<>* builder)
{
    return getDeclaration(fun, getModule(builder));
}

Value* emitEvaluate(Value* value, Value* environment, IRBuilder<>* builder)
{
    Function* f = getDeclaration(EVALUATE, builder);
    return builder->CreateCall2(f, value, environment);
}

Value* emitLookupSymbol(Value* value, Value* environment, IRBuilder<>* builder)
{
    Function* f = getDeclaration(LOOKUP_SYMBOL, builder);
    return builder->CreateCall2(f, value, environment);
}

Value* emitLookupSymbolInCompiledFrame(Value* value, Value* environment,
				       int position, IRBuilder<>* builder)
{
    Function* f = getDeclaration(LOOKUP_SYMBOL_IN_COMPILED_FRAME, builder);
    return builder->CreateCall3(f, value, environment,
				builder->getInt32(position));
}

Value* emitLookupFunction(Value* value, Value* environment,
			  IRBuilder<>* builder)
{
    Function* f = getDeclaration(LOOKUP_FUNCTION, builder);
    return builder->CreateCall2(f, value, environment);
}

Value* emitCallFunction(llvm::Value* function_base, llvm::Value* pairlist_args,
			llvm::Value* call, llvm::Value* environment,
			IRBuilder<>* builder)
{
    Function* f = getDeclaration(CALL_FUNCTION, builder);
    return builder->CreateCall4(f, function_base, pairlist_args, call,
				environment);
}

static Module* createRuntimeModule(LLVMContext& context)
{
    std::string module_filename = std::string(R_Home) + "/jit/RuntimeImpl.bc";

    llvm::SMDiagnostic err;
    Module* runtime_module = llvm::ParseIRFile(module_filename, err, context);
    if (!runtime_module) {
	// TODO(kmillar): better error handling
	printf("parse failed\n");
	exit(1);
    }

    return runtime_module;
}

static Module* getRuntimeModule(LLVMContext& context)
{
    // TODO(kmillar): need multiple modules if we have multiple contexts.
    static Module* runtime_module = createRuntimeModule(context);
    return runtime_module;
}

llvm::Module* createModule(llvm::LLVMContext& context)
{
    // LLVM has an annoying bug where linking removes the type names in the
    // runtime module, even with Linker::PreserveSource (LLVM bug 20068).
    // Because of this, initialize modules with an entire copy of the
    // runtime module, which eliminates the need for linking.
    // TODO(kmillar): improve this.
    return llvm::CloneModule(getRuntimeModule(context));
}

void linkInRuntimeModule(llvm::Module* module)
{
    // Nothing needed at present.  See comments in createModule().
}

StructType* getCxxrType(const std::string& name, LLVMContext& context)
{
    return getRuntimeModule(context)->getTypeByName("class.CXXR::" + name);
}

std::string getName(FunctionId function)
{
    switch (function) {
    case NOT_A_RUNTIME_FUNCTION:
	assert(0 && "Invalid FunctionId value passed.");
	return nullptr; // TODO: throw an exception.
    case EVALUATE:
	return "cxxr_runtime_evaluate";
    case LOOKUP_SYMBOL:
	return "cxxr_runtime_lookupSymbol";
    case LOOKUP_SYMBOL_IN_COMPILED_FRAME:
	return "cxxr_runtime_lookupSymbolInCompiledFrame";
    case LOOKUP_FUNCTION:
	return "cxxr_runtime_lookupFunction";
    case CALL_FUNCTION:
	return "cxxr_runtime_callFunction";
    };
}

static const FunctionId allFunctionIds[]
    = { EVALUATE, LOOKUP_SYMBOL, LOOKUP_SYMBOL_IN_COMPILED_FRAME,
	LOOKUP_FUNCTION, CALL_FUNCTION };

FunctionId getFunctionId(llvm::Function* function)
{
    // TODO: implement more efficiently.
    std::string name = function->getName();
    for (FunctionId id : allFunctionIds) {
	if (name == getName(id)) {
	    return id;
	}
    }
    return NOT_A_RUNTIME_FUNCTION;
}

} // namespace Runtime
} // namespace JIT
} // namespace CXXR
