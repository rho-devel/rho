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

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"
#include "llvm/Transforms/Utils/Cloning.h"

#define R_NO_REMAP
#include "CXXR/jit/Runtime.hpp"

#include "CXXR/jit/Compiler.hpp"
#include "CXXR/jit/Globals.hpp"
#include "CXXR/jit/TypeBuilder.hpp"
#include "CXXR/RObject.h"
#include "Defn.h"
#include "Rinternals.h"

using namespace llvm;

namespace CXXR {

namespace JIT {

static llvm::Module* getModule(IRBuilder<>* builder)
{
    return builder->GetInsertBlock()->getParent()->getParent();
}

namespace Runtime {

static Module* getRuntimeModule(LLVMContext& context);

static llvm::Function* getDeclaration(const std::string& name,
				      llvm::Module* module)
{
    llvm::Function* resolved_function = module->getFunction(name);
    assert(resolved_function != nullptr);
    return resolved_function;
}

llvm::Function* getDeclaration(FunctionId function, llvm::Module* module)
{
    return getDeclaration(getName(function), module);
}

static llvm::Function* getDeclaration(FunctionId fun, Compiler* compiler)
{
    return getDeclaration(fun, getModule(compiler));
}

static llvm::Function* getDeclaration(const std::string& name,
				      Compiler* compiler)
{
    return getDeclaration(name, getModule(compiler));
}


Value* emitEvaluate(Value* value, Value* environment, Compiler* compiler)
{
    Function* evaluate = getDeclaration(EVALUATE, compiler);
    return compiler->emitCallOrInvoke(evaluate, { value, environment });
}

Value* emitLookupSymbol(Value* value, Value* environment, Compiler* compiler)
{
    Function* lookup_symbol = getDeclaration(LOOKUP_SYMBOL, compiler);
    return compiler->emitCallOrInvoke(lookup_symbol, { value, environment });
}

Value* emitLookupSymbolInCompiledFrame(Value* value, Value* environment,
				       int position, Compiler* compiler)
{
    Function* lookup_symbol_in_compiled_frame
	= getDeclaration(LOOKUP_SYMBOL_IN_COMPILED_FRAME, compiler);
    return compiler->emitCallOrInvoke(
	lookup_symbol_in_compiled_frame,
	{ value, environment, compiler->getInt32(position) });
}

Value* emitLookupFunction(Value* value, Value* environment,
			  Compiler* compiler)
{
    Function* lookup_function = getDeclaration(LOOKUP_FUNCTION, compiler);
    return compiler->emitCallOrInvoke(lookup_function, { value, environment });
}

Value* emitCallFunction(llvm::Value* function_base, llvm::Value* pairlist_args,
			llvm::Value* call, llvm::Value* environment,
			Compiler* compiler)
{
    Function* call_function = getDeclaration(CALL_FUNCTION, compiler);
    return compiler->emitCallOrInvoke(
	call_function,
	{ function_base, pairlist_args, call, environment });
}

llvm::Value* emitBreak(llvm::Value* environment, Compiler* compiler) {
    Function* do_break = getDeclaration(DO_BREAK, compiler);
    compiler->emitCallOrInvoke(do_break, { environment });
    return compiler->CreateUnreachable();
}

llvm::Value* emitNext(llvm::Value* environment, Compiler* compiler) {
    Function* do_next = getDeclaration(DO_NEXT, compiler);
    compiler->emitCallOrInvoke(do_next, { environment });
    return compiler->CreateUnreachable();
}

Value* emitCoerceToTrueOrFalse(llvm::Value* value,
			       const Expression* call,
			       Compiler* compiler)
{
    Function* coerce = getDeclaration(COERCE_TO_TRUE_OR_FALSE, compiler);
    Value* callp = compiler->emitConstantPointer(call);
    return compiler->emitCallOrInvoke(coerce, { value, callp });
}

Type* exceptionInfoType(Compiler* compiler)
{
    return llvm::StructType::get(compiler->getType<void*>(),
				 compiler->getType<int32_t>(),
				 nullptr);
}

LandingPadInst* emitLandingPad(Compiler* compiler)
{
  Function* exception_personality_function
      = getDeclaration("__gxx_personality_v0", compiler);
  return compiler->CreateLandingPad(exceptionInfoType(compiler),
                                    exception_personality_function, 0);
}
 
Value* emitBeginCatch(Value* exception_reference,
		      Compiler* compiler)
{
    Function* cxa_begin_catch = getDeclaration("__cxa_begin_catch", compiler);
    // Never throws.
    return compiler->CreateCall(cxa_begin_catch, exception_reference);
}

void emitEndCatch(Compiler* compiler)
{
    Function* cxa_end_catch = getDeclaration("__cxa_end_catch", compiler);
    // Throws only if a destructor throws.  That should never happen, so it
    // gets ignored here.
    compiler->CreateCall(cxa_end_catch);
}

Value* emitLoopExceptionIsNext(Value* loop_exception, Compiler* compiler)
{
    Function* loop_exception_is_next = getDeclaration(
	"cxxr_runtime_loopExceptionIsNext", compiler);
    // Never throws.
    return compiler->CreateCall(loop_exception_is_next, loop_exception);
}

Value* emitGetReturnExceptionValue(Value* return_exception, Compiler* compiler)
{
    Function* get_return_exception_value = getDeclaration(
	"cxxr_runtime_getReturnExceptionValue", compiler);
     // Never throws.
     return compiler->CreateCall(get_return_exception_value, return_exception);
}

Value* emitIsAFunction(llvm::Value* object, Compiler* compiler)
{
    Function* is_a_function = getDeclaration(
	"cxxr_runtime_is_function", compiler);
     return compiler->emitCallOrInvoke(is_a_function, object);
}

void emitError(const char* error_msg, llvm::ArrayRef<llvm::Value*> extra_args,
	       Compiler* compiler)
{
    std::vector<llvm::Value*> args;
    args.push_back(compiler->emitConstantPointer(error_msg));
    args.insert(args.end(), extra_args.begin(), extra_args.end());

    Function* error = getDeclaration("Rf_error", compiler);
    compiler->emitCallOrInvoke(error, args);
    compiler->CreateUnreachable();
}

void emitWarning(const char* warning_msg,
		 llvm::ArrayRef<llvm::Value*> extra_args,
		 Compiler* compiler)
{
    std::vector<llvm::Value*> args;
    args.push_back(compiler->emitConstantPointer(warning_msg));
    args.insert(args.end(), extra_args.begin(), extra_args.end());

    Function* warning = getDeclaration("Rf_warning", compiler);
    compiler->emitCallOrInvoke(warning, args);
}

static void cleanupRuntimeModule(Module* module)
{
    // The Runtime module contains a lot of functions that have already been
    // compiled into the current process.  There's no point in recompiling those
    // functions, so replace them with their declarations.
    for (Function& function : *module) {
	if (llvm::sys::DynamicLibrary::SearchForAddressOfSymbol(
		function.getName()))
	{
	    function.deleteBody();
	}
    }
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
    cleanupRuntimeModule(runtime_module);

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
    StructType* type = getRuntimeModule(context)->getTypeByName(
	"class.CXXR::" + name);
    assert(type != nullptr);
    return type;
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
    case DO_BREAK:
	return "cxxr_runtime_do_break";
    case DO_NEXT:
	return "cxxr_runtime_do_next";
    case COERCE_TO_TRUE_OR_FALSE:
	return "cxxr_runtime_coerceToTrueOrFalse";
    };
}

static const FunctionId allFunctionIds[]
    = { EVALUATE, LOOKUP_SYMBOL, LOOKUP_SYMBOL_IN_COMPILED_FRAME,
	LOOKUP_FUNCTION, CALL_FUNCTION, DO_BREAK, DO_NEXT,
	COERCE_TO_TRUE_OR_FALSE };

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
