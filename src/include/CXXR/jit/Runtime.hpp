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

#include "llvm/IR/IRBuilder.h"

namespace llvm {
class Function;
class Module;
} // namespace llvm

namespace CXXR {

class Environment;
class RObject;
class Symbol;
class FunctionBase;
class Expression;
class PairList;

namespace JIT {

llvm::Module* getModule(llvm::IRBuilder<>* builder);

namespace Runtime {

    // This closely follows the way LLVM handles Intrinsics, except that there
    // is no overloading.
    enum FunctionId {
	NOT_A_RUNTIME_FUNCTION = 0,
	EVALUATE,
	LOOKUP_SYMBOL,
	LOOKUP_FUNCTION,
	CALL_FUNCTION,
	// When adding to this list, make sure to add to allFunctionIds[] in
	// Runtime.cpp.
	FIRST_FUNCTION = EVALUATE,
	LAST_FUNCTION = CALL_FUNCTION,
    };

    std::string getName(FunctionId function);

    llvm::Function* getDeclaration(FunctionId id,
				   llvm::Module* module);

    // returns NOT_A_RUNTIME_FUNCTION if function isn't a runtime function.
    FunctionId getFunctionId(llvm::Function* function);

    // Adds the Runtime functions into the module.
    void mergeInRuntimeModule(llvm::Module* module);

/**
 * These functions generate simple calls into the interpreter.  They make no
 * attempt to generate more efficient code.
 */
llvm::Value* emitEvaluate(llvm::Value* value, llvm::Value* environment,
                          llvm::IRBuilder<>* builder);

llvm::Value* emitLookupSymbol(llvm::Value* symbol, llvm::Value* environment,
                              llvm::IRBuilder<>* builder);
llvm::Value* emitLookupSymbol(const Symbol* symbol, llvm::Value* environment,
                              llvm::IRBuilder<>* builder);

llvm::Value* emitLookupFunction(llvm::Value* symbol,
                                llvm::Value* environment,
                                llvm::IRBuilder<>* builder);
llvm::Value* emitLookupFunction(const Symbol* symbol,
                                llvm::Value* environment,
                                llvm::IRBuilder<>* builder);

llvm::Value* emitCallFunction(llvm::Value* function_base,
                              llvm::Value* pairlist_args,
                              llvm::Value* call,
                              llvm::Value* environment,
                              llvm::IRBuilder<>* builder);
} // namespace Runtime
} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_RUNTIME_HPP
