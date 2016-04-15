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
#include "rho/jit/Runtime.hpp"

#include "rho/jit/Compiler.hpp"
#include "rho/jit/Globals.hpp"
#include "rho/jit/TypeBuilder.hpp"
#include "rho/RObject.hpp"
#include "Defn.h"
#include "Rinternals.h"

#include "RuntimeImpl.cpp"

using namespace llvm;

namespace rho {

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
    if (!resolved_function) {
	// Copy the declaration from the runtime to the module.
	// TODO(kmillar): omitting the function body eliminates opportunities
	//   for inlining, which will be required to generate fast code.
	Function* runtime_function = getRuntimeModule(module->getContext())
	    ->getFunction(name);
	assert(runtime_function);
	resolved_function = Function::Create(runtime_function->getFunctionType(),
					     Function::ExternalLinkage,
					     runtime_function->getName(),
					     module);
	assert(resolved_function);
	resolved_function->setAttributes(runtime_function->getAttributes());
    }
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

llvm::Function* getDeclaration(const std::string& name, Compiler* compiler)
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

Value* emitAssignSymbolInCompiledFrame(Value* symbol, Value* environment,
				       int position, Value* value,
				       Compiler* compiler)
{
    Function* assign_symbol_in_compiled_frame
	= getDeclaration(ASSIGN_SYMBOL_IN_COMPILED_FRAME, compiler);
    return compiler->emitCallOrInvoke(
	assign_symbol_in_compiled_frame,
	{ symbol, environment, compiler->getInt32(position), value });
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

void emitSetVisibility(llvm::Value* visible, Compiler* compiler)
{
    Function* setVisibility = getDeclaration(SET_VISIBILITY, compiler);
    compiler->CreateCall(setVisibility, visible);
}

void emitIncrementNamed(llvm::Value* value, Compiler* compiler)
{
    Function* incrementNamed = getDeclaration(INCREMENT_NAMED, compiler);
    compiler->CreateCall(incrementNamed, value);
}

void emitMaybeCheckForUserInterrupt(Compiler* compiler)
{
    Function* maybe_check_for_interrupt
	= getDeclaration("rho_runtime_maybeCheckForUserInterrupts", compiler);
    compiler->emitCallOrInvoke(maybe_check_for_interrupt, {});
}

Type* exceptionInfoType(Compiler* compiler)
{
    return llvm::StructType::get(compiler->getType<void*>(),
				 compiler->getType<int32_t>(),
				 nullptr);
}

Function* getExceptionPersonalityFunction(Compiler* compiler) {
  return getDeclaration("__gxx_personality_v0", compiler);
}

LandingPadInst* emitLandingPad(Compiler* compiler)
{
  return compiler->CreateLandingPad(exceptionInfoType(compiler),
#if (LLVM <= 306)
                                    getExceptionPersonalityFunction(compiler),
#endif
      0);
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

void emitRethrow(Compiler* compiler)
{
    Function* cxa_rethrow = getDeclaration("__cxa_rethrow", compiler);
    compiler->CreateCall(cxa_rethrow);
    compiler->CreateUnreachable();
}

Value* emitLoopExceptionIsNext(Value* loop_exception, Compiler* compiler)
{
    Function* loop_exception_is_next = getDeclaration(
	"rho_runtime_loopExceptionIsNext", compiler);
    // Never throws.
    return compiler->CreateCall(loop_exception_is_next, loop_exception);
}

Value* emitGetReturnExceptionValue(Value* return_exception, Compiler* compiler)
{
    Function* get_return_exception_value = getDeclaration(
	"rho_runtime_getReturnExceptionValue", compiler);
     // Never throws.
     return compiler->CreateCall(get_return_exception_value, return_exception);
}

Value* emitIsAFunction(llvm::Value* object, Compiler* compiler)
{
    Function* is_a_function = getDeclaration(
	"rho_runtime_is_function", compiler);
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

static std::unique_ptr<Module> createRuntimeModule(LLVMContext& context)
{
    std::string module_filename = std::string(R_Home) + "/jit/RuntimeImpl.bc";

    llvm::SMDiagnostic err;
    std::unique_ptr<Module> runtime_module
        = llvm::parseIRFile(module_filename, err, context);
    if (!runtime_module) {
	// TODO(kmillar): better error handling
	printf("parse failed\n");
	exit(1);
    }
    cleanupRuntimeModule(runtime_module.get());

    return runtime_module;
}

static Module* getRuntimeModule(LLVMContext& context)
{
    // TODO(kmillar): need multiple modules if we have multiple contexts.
    static Module* runtime_module = createRuntimeModule(context).release();
    return runtime_module;
}

std::unique_ptr<llvm::Module> createModule(llvm::LLVMContext& context)
{
    return std::unique_ptr<Module>(new Module("anonymous_module", context));
}

void linkInRuntimeModule(llvm::Module* module)
{
    // Nothing needed.
}

StructType* getRhoType(const std::string& name, LLVMContext& context)
{
    StructType* type = getRuntimeModule(context)->getTypeByName(
	"class.rho::" + name);
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
	return "rho_runtime_evaluate";
    case LOOKUP_SYMBOL:
	return "rho_runtime_lookupSymbol";
    case LOOKUP_SYMBOL_IN_COMPILED_FRAME:
	return "rho_runtime_lookupSymbolInCompiledFrame";
    case ASSIGN_SYMBOL_IN_COMPILED_FRAME:
	return "rho_runtime_assignSymbolInCompiledFrame";
    case LOOKUP_FUNCTION:
	return "rho_runtime_lookupFunction";
    case CALL_FUNCTION:
	return "rho_runtime_callFunction";
    case DO_BREAK:
	return "rho_runtime_do_break";
    case DO_NEXT:
	return "rho_runtime_do_next";
    case COERCE_TO_TRUE_OR_FALSE:
	return "rho_runtime_coerceToTrueOrFalse";
    case SET_VISIBILITY:
	return "rho_runtime_setVisibility";
    case INCREMENT_NAMED:
	return "rho_runtime_incrementNamed";
    };
}

static const FunctionId allFunctionIds[]
    = { EVALUATE, LOOKUP_SYMBOL, LOOKUP_SYMBOL_IN_COMPILED_FRAME,
	ASSIGN_SYMBOL_IN_COMPILED_FRAME,
	LOOKUP_FUNCTION, CALL_FUNCTION, DO_BREAK, DO_NEXT,
	COERCE_TO_TRUE_OR_FALSE, SET_VISIBILITY, INCREMENT_NAMED };

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

namespace ForceCodeEmission {
#define FORCE_EMISSION(FUNCTION) auto FUNCTION ## _p = &FUNCTION;
    FORCE_EMISSION(rho_runtime_evaluate);
    FORCE_EMISSION(rho_runtime_lookupSymbol);
    FORCE_EMISSION(rho_runtime_lookupSymbolInCompiledFrame);
    FORCE_EMISSION(rho_runtime_lookupFunction);
    FORCE_EMISSION(rho_runtime_callFunction);
    FORCE_EMISSION(rho_runtime_do_break);
    FORCE_EMISSION(rho_runtime_do_next);
    FORCE_EMISSION(rho_runtime_loopExceptionIsNext);
    FORCE_EMISSION(rho_runtime_coerceToTrueOrFalse);
    FORCE_EMISSION(rho_runtime_is_function);
    FORCE_EMISSION(rho_runtime_setVisibility);
}

} // namespace Runtime
} // namespace JIT
} // namespace rho
