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

#ifndef CXXR_JIT_COMPILER_HPP
#define CXXR_JIT_COMPILER_HPP

#include "CXXR/jit/CompilerContext.hpp"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/TypeBuilder.h"

namespace CXXR {

class BuiltInFunction;
class DottedArgs;
class Environment;
class Expression;
class FunctionBase;
class RObject;
class Symbol;

namespace JIT {

class Compiler : public llvm::IRBuilder<> {
public:
    explicit Compiler(CompilerContext* context);

    // Code generation functions.
    // These generate optimized code.
    llvm::Value* emitEval(const RObject* object);
    llvm::Value* emitSymbolEval(const Symbol* symbol);
    llvm::Value* emitExpressionEval(const Expression* object);
    llvm::Value* emitDotsEval(const DottedArgs* object);

    // Utility functions.
    template <class T>
    llvm::Constant* emitConstantPointer(const T* value);
    // Aliases for emitConstantPointer for improved code readability.
    llvm::Constant* emitSymbol(const Symbol* symbol);
    llvm::Constant* emitNullValue();

    llvm::Value* emitCallOrInvoke(llvm::Function* function,
				  llvm::ArrayRef<llvm::Value*> args);

private:
    CompilerContext* m_context;

    llvm::Value* emitFunctionLookup(const Symbol* symbol,
				    FunctionBase** likely_function);
    
    // Code to generate inlined functions.
    llvm::Value* emitInlineableBuiltinCall(const Expression* expression,
					   llvm::Value* resolved_function,
					   FunctionBase* likely_function);

    // Specific functions to inline.
    llvm::Value* emitInlinedParen(const Expression* expression);
    llvm::Value* emitInlinedBegin(const Expression* expression);
    llvm::Value* emitInlinedReturn(const Expression* expression);

    typedef llvm::Value* (Compiler::*EmitBuiltinFn)(const Expression*);
    static const std::vector<std::pair<FunctionBase*, EmitBuiltinFn>>&
	getInlineableBuiltins();
    static EmitBuiltinFn getInlinedBuiltInEmitter(BuiltInFunction* builtin);

    // Utility functions.
    llvm::Constant* emitConstantPointer(const void* value, llvm::Type* type);
    llvm::BasicBlock* createBasicBlock(const char* name,
				       llvm::BasicBlock* insert_before = 0);
};

template <class T>
llvm::Constant* Compiler::emitConstantPointer(const T* value)
{
    llvm::LLVMContext& context = llvm::getGlobalContext();
    return emitConstantPointer(reinterpret_cast<const void*>(value),
			       llvm::TypeBuilder<T*, false>::get(context));
}

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_COMPILER__HPP
