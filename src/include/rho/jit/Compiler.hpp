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

#ifndef RHO_JIT_COMPILER_HPP
#define RHO_JIT_COMPILER_HPP

#include "rho/jit/CompilerContext.hpp"
#include "rho/jit/llvm.hpp"
namespace rho {

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

    // Code generation.
    llvm::Value* emitEval(const RObject* object);

    // Utility functions.
    template <class T>
    llvm::Constant* emitConstantPointer(const T* value);
    llvm::Constant* emitSymbol(const Symbol* symbol);
    llvm::Constant* emitNullValue();
    llvm::Constant* emitInvisibleNullValue();

    template<class T>
    llvm::Value* emitUncheckedCast(llvm::Value* value);

    llvm::Value* emitCallOrInvoke(llvm::Function* function,
				  llvm::ArrayRef<llvm::Value*> args);

    template<class T>
    llvm::Type* getType();
private:
    CompilerContext* m_context;

    llvm::Value* emitFunctionLookup(const Symbol* symbol,
				    FunctionBase** likely_function);
    
    // Code generation functions.
    // These generate optimized code.
    llvm::Value* emitEvalInternal(const RObject* object);
    llvm::Value* emitSymbolEval(const Symbol* symbol);
    llvm::Value* emitExpressionEval(const Expression* object);
    llvm::Value* emitDotsEval(const DottedArgs* object);

    // Code to generate inlined functions.
    llvm::Value* emitInlineableBuiltinCall(const Expression* expression,
					   llvm::Value* resolved_function,
					   FunctionBase* likely_function);

    // Specific functions to inline.
    llvm::Value* emitInlinedAssign(const Expression* expression);
    llvm::Value* emitInlinedParen(const Expression* expression);
    llvm::Value* emitInlinedBegin(const Expression* expression);
    llvm::Value* emitInlinedReturn(const Expression* expression);
    llvm::Value* emitInlinedIf(const Expression* expression);
    llvm::Value* emitInlinedWhile(const Expression* expression);
    llvm::Value* emitInlinedRepeat(const Expression* expression);
    llvm::Value* emitInlinedBreak(const Expression* expression);
    llvm::Value* emitInlinedNext(const Expression* expression);

    typedef llvm::Value* (Compiler::*EmitBuiltinFn)(const Expression*);
    static const std::vector<std::pair<FunctionBase*, EmitBuiltinFn>>&
	getInlineableBuiltins();
    static EmitBuiltinFn getInlinedBuiltInEmitter(BuiltInFunction* builtin);

    // Exception handling.
    friend class CompilerContext;
    llvm::Value* getExceptionTypeId(const std::type_info* type);
    llvm::BasicBlock* emitLandingPad(llvm::PHINode* dispatch);
    llvm::PHINode* emitDispatchToExceptionHandler(const std::type_info* type,
    						  llvm::PHINode* handler,
    						  llvm::PHINode* fallthrough);
    llvm::PHINode* emitLoopExceptionHandler(llvm::BasicBlock* break_destination,
					    llvm::BasicBlock* next_destination);
    llvm::PHINode* emitReturnExceptionHandler();
    llvm::PHINode* emitRethrowException();

    // Utility functions.
    std::vector<llvm::Value*> castFunctionArguments(
	llvm::ArrayRef<llvm::Value*> args,
	llvm::Function* function);

    llvm::Constant* emitConstantPointer(const void* value, llvm::Type* type);

    void emitSetVisibility(bool visible);

    void emitErrorUnless(llvm::Value* condition,
			 const char* error_msg,
			 llvm::ArrayRef<llvm::Value*> extra_args = {});
    llvm::BasicBlock* createBasicBlock(const char* name,
				       llvm::BasicBlock* insert_before = nullptr);
    llvm::BasicBlock* createBranch(const char* name, const RObject* expression,
				   llvm::PHINode* merge_point,
				   llvm::BasicBlock* insert_before = nullptr);
    llvm::Value* createBackEdge(llvm::BasicBlock* destination);
};

template <class T>
llvm::Constant* Compiler::emitConstantPointer(const T* value)
{
    llvm::Type* type = llvm::TypeBuilder<T*, false>::get(getContext());
    return emitConstantPointer(reinterpret_cast<const void*>(value), type);
}

template<class T>
llvm::Value* Compiler::emitUncheckedCast(llvm::Value* value)
{
    llvm::Type* type = llvm::TypeBuilder<T, false>::get(getContext());
    return CreatePointerCast(value, type);
}

template<class T>
llvm::Type* Compiler::getType() {
    return llvm::TypeBuilder<T, false>::get(getContext());
}

} // namespace JIT
} // namespace rho

#endif // RHO_JIT_COMPILER__HPP
