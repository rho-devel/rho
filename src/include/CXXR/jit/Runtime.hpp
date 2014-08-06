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

#ifndef CXXR_JIT_RUNTIME_HPP
#define CXXR_JIT_RUNTIME_HPP

#include <string>
#include "llvm/ADT/ArrayRef.h"

namespace llvm {
class Function;
class LLVMContext;
class Module;
class StructType;
class Value;
} // namespace llvm

namespace CXXR {

class Environment;
class RObject;
class Symbol;
class FunctionBase;
class Expression;
class PairList;

namespace JIT {

class Compiler;

namespace Runtime {

// This closely follows the way LLVM handles Intrinsics, except that there
// is no overloading.
// TODO(kmillar): do we need the FunctionId?
enum FunctionId {
    NOT_A_RUNTIME_FUNCTION = 0,
    EVALUATE,
    LOOKUP_SYMBOL,
    LOOKUP_SYMBOL_IN_COMPILED_FRAME,
    LOOKUP_FUNCTION,
    CALL_FUNCTION,
    DO_BREAK,
    DO_NEXT,
    COERCE_TO_TRUE_OR_FALSE,
    // When adding to this list, make sure to add to allFunctionIds[] in
    // Runtime.cpp.
};

std::string getName(FunctionId function);

llvm::Function* getDeclaration(FunctionId id, llvm::Module* module);

// returns NOT_A_RUNTIME_FUNCTION if function isn't a runtime function.
FunctionId getFunctionId(llvm::Function* function);

// Return a module that has been setup to work with the runtime.
llvm::Module* createModule(llvm::LLVMContext& context);

// Links any needed runtime functions and definitions into the module.
void linkInRuntimeModule(llvm::Module* module);

// Returns the LLVM type that the runtime uses for the given class name.
// Most code should use TypeBuilder instead of calling this directly.
llvm::StructType* getCxxrType(const std::string& name,
			      llvm::LLVMContext& context);
/**
 * These functions generate simple calls into the interpreter.  They make no
 * attempt to generate more efficient code.
 */
llvm::Value* emitEvaluate(llvm::Value* value, llvm::Value* environment,
			  Compiler* compiler);

llvm::Value* emitLookupSymbol(llvm::Value* symbol, llvm::Value* environment,
			      Compiler* compiler);

llvm::Value* emitLookupSymbolInCompiledFrame(llvm::Value* symbol,
					     llvm::Value* environment,
					     int position,
					     Compiler* compiler);

llvm::Value* emitLookupFunction(llvm::Value* symbol, llvm::Value* environment,
				Compiler* compiler);

llvm::Value* emitCallFunction(llvm::Value* function_base,
			      llvm::Value* pairlist_args, llvm::Value* call,
			      llvm::Value* environment,
			      Compiler* compiler);

llvm::Value* emitBreak(llvm::Value* environment, Compiler* compiler);
llvm::Value* emitNext(llvm::Value* environment, Compiler* compiler);

// Coerce value to either 0 or 1.
// Throws a runtime error if the coercion fails.
llvm::Value* emitCoerceToTrueOrFalse(llvm::Value* value,
				     const Expression* call,
				     Compiler* compiler);

// Exception handling code.
// These functions currently don't have FunctionIds assigned.
llvm::Value* emitBeginCatch(llvm::Value* exception_reference,
			    Compiler* compiler);
void emitEndCatch(Compiler* compiler);

llvm::Value* emitLoopExceptionIsNext(llvm::Value* loop_exception,
				     Compiler* compiler);
llvm::Value* emitGetReturnExceptionValue(llvm::Value* return_exception,
					 Compiler* compiler);

// Utility functions.
llvm::Value* emitIsAFunction(llvm::Value* robject, Compiler* compiler);

// Error handling.
void emitError(const char* error_msg, llvm::ArrayRef<llvm::Value*> args,
	       Compiler* compiler);
void emitWarning(const char* warning_msg,
		 llvm::ArrayRef<llvm::Value*> args,
		 Compiler* compiler);

} // namespace Runtime
} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_RUNTIME_HPP
