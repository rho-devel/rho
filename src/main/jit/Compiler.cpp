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

#include "CXXR/jit/Compiler.hpp"

#include "llvm/IR/Function.h"

#include "CXXR/BuiltInFunction.h"
#include "CXXR/ByteCode.hpp"
#include "CXXR/Closure.h"
#include "CXXR/DottedArgs.hpp"
#include "CXXR/Environment.h"
#include "CXXR/Expression.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/RObject.h"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/Symbol.h"
#include "CXXR/jit/FrameDescriptor.hpp"
#include "CXXR/jit/Runtime.hpp"
#include "CXXR/jit/TypeBuilder.hpp"

using llvm::BasicBlock;
using llvm::TypeBuilder;
using llvm::Value;

namespace CXXR {
namespace JIT {

Compiler::Compiler(CompilerContext* context)
    : IRBuilder<>(context->getLLVMContext()), m_context(context)
{
    // Setup the entry block.
    BasicBlock* entry_block = createBasicBlock("EntryBlock");
    SetInsertPoint(entry_block);
}

llvm::Constant* Compiler::emitConstantPointer(const void* value,
					      llvm::Type* type)
{
    llvm::ConstantInt* pointer_as_integer = llvm::ConstantInt::get(
	TypeBuilder<intptr_t, false>::get(m_context->getLLVMContext()),
	reinterpret_cast<intptr_t>(value));
    return llvm::ConstantExpr::getIntToPtr(pointer_as_integer, type);
}

llvm::Constant* Compiler::emitSymbol(const Symbol* symbol)
{
    // TODO(kmillar): consider caching these.
    // TODO(kmillar): give the symbol a useful name in the IR.
    return emitConstantPointer(symbol);
}

llvm::Constant* Compiler::emitNullValue()
{
    RObject* null = nullptr;
    return emitConstantPointer(null);
}

llvm::Value* Compiler::emitCallOrInvoke(llvm::Function* function,
					llvm::ArrayRef<llvm::Value*> args)
{
    BasicBlock* exception_handler = m_context->getExceptionLandingPad();
    if (exception_handler) {
	BasicBlock *continue_block = createBasicBlock("cont");
	Value* result = CreateInvoke(function,
				     continue_block, exception_handler,
				     args);
	SetInsertPoint(continue_block);
	return result;
    } else {
	return CreateCall(function, args);
    }
}

Value* Compiler::emitEval(const RObject* object)
{
    // This has a non-trivial implementation for all the objects which have
    // non-default object->evaluate() implementations.
    if (!object) {
	return emitNullValue();
    }

    switch (object->sexptype()) {
    case SYMSXP: {
	return emitSymbolEval(SEXP_downcast<const Symbol*>(object));
    }
    case LANGSXP:
	return emitExpressionEval(SEXP_downcast<const Expression*>(object));
    case DOTSXP:
	return emitDotsEval(SEXP_downcast<const DottedArgs*>(object));
    case BCODESXP:
    {
	// Ignore the bytecode and compile the corresponding source.
	const ByteCode* bytecode = SEXP_downcast<const ByteCode*>(object);
	return emitEval(bytecode->source_expression());
    }
    case PROMSXP:
	assert(0 && "Unexpected eval of a promise in JIT compilation.");
	return nullptr;
    default:
	return emitConstantPointer(object);
    }
}

Value* Compiler::emitSymbolEval(const Symbol* symbol)
{
    assert(m_context->m_frame_descriptor != nullptr);
    // Optimize the lookup in the likely case that this is a regular symbol
    // found in the local environment.
    int location = m_context->m_frame_descriptor->getLocation(symbol);
    if (location != -1
	&& symbol != DotsSymbol
	&& !symbol->isDotDotSymbol()
	&& symbol != Symbol::missingArgument()) {
	// Lookup the symbol directly.
	return Runtime::emitLookupSymbolInCompiledFrame(
	    emitSymbol(symbol), m_context->getEnvironment(), location,
	    this);
    }
    // Otherwise fallback to the interpreter for now.
    return Runtime::emitLookupSymbol(emitSymbol(symbol),
				     m_context->getEnvironment(), this);
}

Value* Compiler::emitExpressionEval(const Expression* expression)
{
    RObject* function = expression->car();
    Value* resolved_function = nullptr;

    // Evaluate the function argument and get a prediction of its likely value.
    FunctionBase* likely_function = dynamic_cast<FunctionBase*>(function);
    if (likely_function) {
	// The first element of the expression is a literal function.
        // TODO(kmillar): no need for a guard in the emitted code in this case.
	resolved_function
	    = emitConstantPointer(SEXP_downcast<FunctionBase*>(function));
    } else if (Symbol* symbol = dynamic_cast<Symbol*>(function)) {
	// The first element is a symbol.  Look it up.
	resolved_function = emitFunctionLookup(symbol, &likely_function);
    } else {
	// The first element is a (function-valued) expression.
	resolved_function = emitEval(function);
	// TODO(kmillar): is a cast from RObject* to FunctionBase* needed?
    }

    if (likely_function) {
	Value* result = emitInlineableBuiltinCall(
	    expression, resolved_function, likely_function);
	if (result) {
	    return result;
	}
    }

    // The function wasn't inlined, so emit a call to the interpreter.
    return Runtime::emitCallFunction(
	resolved_function, emitConstantPointer(expression->tail()),
	emitConstantPointer(expression), m_context->getEnvironment(), this);
}

Value* Compiler::emitDotsEval(const DottedArgs* expression)
{
    // Call the interpreter.
    return Runtime::emitEvaluate(emitConstantPointer(expression),
				 m_context->getEnvironment(), this);
}

Value* Compiler::emitFunctionLookup(const Symbol* symbol,
				    FunctionBase** expected_result) {
    *expected_result = findFunction(symbol,
				    m_context->getClosure()->environment());
    llvm::Value* fn = emitSymbol(symbol);
    // TODO(kmillar): emit optimized code for this.
    return Runtime::emitLookupFunction(fn, m_context->getEnvironment(),
				       this);
}

BasicBlock* Compiler::createBasicBlock(const char* name,
				       llvm::BasicBlock* insert_before)
{
    return BasicBlock::Create(getContext(), name,
			      m_context->getFunction(),
			      insert_before);
}

BasicBlock* Compiler::createBranch(const char* name,
				   const RObject* expression,
				   llvm::PHINode* merge_point,
				   llvm::BasicBlock* insert_before)
{
    InsertPointGuard preserve_insert_point(*this);

    BasicBlock* block = createBasicBlock(name, insert_before);
    SetInsertPoint(block);

    Value* result = emitEval(expression); // This line changes depending on what we're doing.
    CreateBr(merge_point->getParent());
    merge_point->addIncoming(result, GetInsertBlock());

    return block;
}

/*
 * The rest of this file contains the code to emit inlined versions of special
 * functions, primarily those that implement flow control.
 */
const std::vector<std::pair<FunctionBase*, Compiler::EmitBuiltinFn>>&
Compiler::getInlineableBuiltins()
{
    static std::vector<std::pair<FunctionBase*, EmitBuiltinFn>>
	inlineable_builtins = {
	std::make_pair(BuiltInFunction::obtain("("),
		       &Compiler::emitInlinedParen),
	std::make_pair(BuiltInFunction::obtain("{"),
		       &Compiler::emitInlinedBegin),
	std::make_pair(BuiltInFunction::obtain("return"),
		       &Compiler::emitInlinedReturn),
	std::make_pair(BuiltInFunction::obtain("if"),
		       &Compiler::emitInlinedIf),
    };
    return inlineable_builtins;
}

Compiler::EmitBuiltinFn Compiler::getInlinedBuiltInEmitter(
    BuiltInFunction* builtin)
{
    // Only functions that are primitive can be inlined directly.  Functions
    // that require argument matching or create a new function context are not
    // inlined here.
    if (!builtin || builtin->viaDotInternal()) {
	return nullptr;
    }
    for (auto inlineable_builtin : getInlineableBuiltins()) {
	if (builtin == inlineable_builtin.first) {
	    return inlineable_builtin.second;
	}
    }
    return nullptr;
}

Value* Compiler::emitInlineableBuiltinCall(const Expression* expression,
					   Value* resolved_function,
					   FunctionBase* likely_function)
{
    BuiltInFunction* builtin = dynamic_cast<BuiltInFunction*>(likely_function);
    EmitBuiltinFn emit_builtin = getInlinedBuiltInEmitter(builtin);
    if (!emit_builtin) {
	return nullptr;
    }

    // TODO(kmillar): write an 'emitGuardedCode' function, and use that here.
    InsertPoint incoming_insert_point = saveIP();
    BasicBlock* inlined_builtin_block = createBasicBlock(builtin->name());
    BasicBlock* merge_block = createBasicBlock("continue");
    BasicBlock* fallback_block = createBasicBlock("fallback");

    // Emit inlined code.
    SetInsertPoint(inlined_builtin_block);

    llvm::Value* inlined_builtin_value = (this->*emit_builtin)(expression);
    if (!inlined_builtin_value) {
	// Code generation failed.
	return nullptr;
    }
    CreateBr(merge_block);

    // If the function isn't the one we expected, fall back to the interpreter.
    // TODO(kmillar): do OSR or similar on guard failure to improve fast
    //   codepath performance and reduce the time spent compiling unlikely
    //   codepaths.
    // TODO(kmillar): allow this check to be skipped at some optimization
    //   levels.
    SetInsertPoint(fallback_block);
    Value* fallback_value
	=  Runtime::emitCallFunction(resolved_function,
				     emitConstantPointer(expression->tail()),
				     emitConstantPointer(expression),
				     m_context->getEnvironment(), this);
    CreateBr(merge_block);

    // Check if (resolved_function == likely_function) and setup the control
    // flow.
    restoreIP(incoming_insert_point);

    Value* likely_fn_value = emitConstantPointer(
	static_cast<FunctionBase*>(likely_function));
    Value* is_expected_builtin = CreateICmpEQ(resolved_function,
					      likely_fn_value);
    CreateCondBr(is_expected_builtin,
		 inlined_builtin_block,
		 fallback_block); // TODO(kmillar): set branch weights
    
    // Setup the merge point.
    SetInsertPoint(merge_block);

    llvm::Type* robject_type = TypeBuilder<RObject*, false>::get(getContext());
    llvm::PHINode* result = CreatePHI(robject_type, 2);
    result->addIncoming(inlined_builtin_value, inlined_builtin_block);
    result->addIncoming(fallback_value, fallback_block);
    return result;
}

Value* Compiler::emitInlinedParen(const Expression* expression)
{
    // '(' has a single argument -- the expression to evaluate.
    if (listLength(expression) == 2) {
	return emitEval(CADR(const_cast<Expression*>(expression)));
    } else {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }
}

Value* Compiler::emitInlinedBegin(const Expression* expression)
{
    // 'begin' is the '{' builtin.  It evaluates each argument and returns the
    // result of the last one.
    llvm::Value* value = nullptr;
    for (const ConsCell& argument : *expression->tail()) {
	value = emitEval(argument.car());
    }
    return value ? value : emitNullValue();
}

Value* Compiler::emitInlinedReturn(const Expression* expression)
{
    // Both return() and return(expr) are legal.
    int length = listLength(expression);
    if (length == 1) {
	return CreateRetVoid();
    } else if (length == 2) {
	Value* return_value = emitEval(CADR(const_cast<Expression*>(expression)));
	return CreateRet(return_value);
    } else {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }
    // Note: 'return' isn't valid at top-level, but since only functions get
    // compiled, there's no need to check for that here.
}

Value* Compiler::emitInlinedIf(const Expression* expression)
{
    int length = listLength(expression);
    if (length != 3 && length != 4) {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }

    InsertPoint incoming_insert_point = saveIP();

    // Create the merge point.
    BasicBlock* continue_block = createBasicBlock("continue");
    SetInsertPoint(continue_block);
    llvm::Type* robject_type = TypeBuilder<RObject*, false>::get(getContext());
    llvm::PHINode* result_value = CreatePHI(robject_type, 2);

    // Create the if_true branch.
    BasicBlock* if_true_block = createBranch(
	"if_true",
	CADDR(const_cast<Expression*>(expression)),
	result_value, continue_block);

    // Generate code to handle the 'else' case.
    BasicBlock* if_false_block;
    if (length == 3) {
	// No 'else' branch.
	// Drop straight to the continue block and return R_NilValue.
	if_false_block = continue_block;
	result_value->addIncoming(emitConstantPointer((RObject*)nullptr),
				  incoming_insert_point.getBlock());

    } else {
	// Create the else branch.
	if_false_block = createBranch(
	    "if_false",
	    CADDDR(const_cast<Expression*>(expression)),
	    result_value, continue_block);
    }

    // Evaluate the condition and branch.
    restoreIP(incoming_insert_point);
    Value* r_condition = emitEval(CADR(const_cast<Expression*>(expression)));
    Value* boolean_condition = Runtime::emitCoerceToTrueOrFalse(
	r_condition, expression, this);
    CreateCondBr(boolean_condition, if_true_block, if_false_block);

    SetInsertPoint(continue_block);
    return result_value;
}

} // namespace JIT
} // namespace CXXR
