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

#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"

#define R_NO_REMAP
#include "CXXR/jit/Compiler.hpp"

#include "CXXR/jit/CompilationException.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"
#include "CXXR/jit/MCJITMemoryManager.hpp"
#include "CXXR/jit/Runtime.hpp"
#include "CXXR/jit/TypeBuilder.hpp"

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

using llvm::BasicBlock;
using llvm::PHINode;
using llvm::Value;

namespace CXXR {
namespace JIT {

Compiler::Compiler(CompilerContext* context)
    : IRBuilder<>(context->getLLVMContext()), m_context(context)
{
    // Check that the minimum requirements for compilation are satisfied.
    if (!context->canInlineControlFlow()) {
	throw CompilationException();
    }

    // Setup the entry block.
    BasicBlock* entry_block = createBasicBlock("EntryBlock");
    SetInsertPoint(entry_block);
}

llvm::Constant* Compiler::emitConstantPointer(const void* value,
					      llvm::Type* type)
{
    llvm::Constant* pointer_as_integer = llvm::ConstantInt::get(
	getType<intptr_t>(),
	reinterpret_cast<intptr_t>(value));
    return llvm::ConstantExpr::getIntToPtr(pointer_as_integer, type);
}

llvm::Constant* Compiler::emitSymbol(const Symbol* symbol)
{
    return m_context->getMemoryManager()->getSymbol(symbol);
}

llvm::Constant* Compiler::emitNullValue()
{
    llvm::PointerType* type = (llvm::PointerType*)getType<RObject*>();
    return llvm::ConstantPointerNull::get(type);
}

llvm::Constant* Compiler::emitInvisibleNullValue()
{
    emitSetVisibility(false);
    llvm::PointerType* type = (llvm::PointerType*)getType<RObject*>();
    return llvm::ConstantPointerNull::get(type);
}

void Compiler::emitSetVisibility(bool visible)
{
    Runtime::emitSetVisibility(getInt1(visible), this);
}

void Compiler::emitErrorUnless(Value* condition,
			       const char* error_msg,
			       llvm::ArrayRef<Value*> extra_args) {
    BasicBlock* error_block = createBasicBlock("error_msg");
    BasicBlock* continue_block = createBasicBlock("continue", error_block);

    llvm::Type* type = condition->getType();
    if (type != llvm::Type::getInt1Ty(getContext())) {
	condition = CreateIsNotNull(condition);
    }
    CreateCondBr(condition, continue_block, error_block);

    SetInsertPoint(error_block);
    Runtime::emitError(error_msg, extra_args, this);

    SetInsertPoint(continue_block);
}

std::vector<llvm::Value*> Compiler::castFunctionArguments(
    llvm::ArrayRef<llvm::Value*> args,
    llvm::Function* function)
{
    assert(function->isVarArg() || function->arg_size() == args.size());

    // Upcast any arguments that require it.
    std::vector<llvm::Value*> type_checked_args;
    int i = 0;
    for (const llvm::Argument& argument : function->getArgumentList()) {
	type_checked_args.push_back(
	    // TODO(kmillar): these really ought to be upcasts,
	    //   in which case we can statically check that the cast is valid.
	    CreatePointerCast(args[i], argument.getType()));
	++i;
    }
    return type_checked_args;
}

llvm::Value* Compiler::emitCallOrInvoke(llvm::Function* function,
					llvm::ArrayRef<llvm::Value*> args)
{
    std::vector<llvm::Value*> type_checked_args = castFunctionArguments(
	args, function);

    BasicBlock* exception_handler = m_context->getExceptionLandingPad();
    if (exception_handler) {
	BasicBlock *continue_block = createBasicBlock("cont");
	Value* result = CreateInvoke(function,
				     continue_block, exception_handler,
				     type_checked_args);
	SetInsertPoint(continue_block);
	return result;
    } else {
	return CreateCall(function, type_checked_args);
    }
}

Value* Compiler::emitEval(const RObject* object)
{
    emitSetVisibility(true);

    Value* result = emitEvalInternal(object);

    if (GetInsertBlock()->getTerminator()) {
	// Any subsequent code is dead.  Create a dummy basic block for it.
	SetInsertPoint(createBasicBlock("dead_code"));

	if (dynamic_cast<llvm::TerminatorInst*>(result)) {
	    // There is no current value either.  Use undef as a placeholder.
	    return llvm::UndefValue::get(getType<RObject*>());
	}
    }

    return result;
}

Value* Compiler::emitEvalInternal(const RObject* object)
{
    // This has a non-trivial implementation for all the objects which have
    // non-default object->evaluate() implementations.
    if (!object) {
	return emitNullValue();
    }

    switch (object->sexptype()) {
    case SYMSXP:
	return emitSymbolEval(SEXP_downcast<const Symbol*>(object));
    case LANGSXP:
	return emitExpressionEval(SEXP_downcast<const Expression*>(object));
    case DOTSXP:
	return emitDotsEval(SEXP_downcast<const DottedArgs*>(object));
    case BCODESXP:
    {
	// Ignore the bytecode and compile the corresponding source.
	const ByteCode* bytecode = SEXP_downcast<const ByteCode*>(object);
	return emitEvalInternal(bytecode->source_expression());
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

// This function is the JIT version of Expression::evaluate().
Value* Compiler::emitExpressionEval(const Expression* expression)
{
    RObject* function = expression->car();
    Value* resolved_function;

    // Evaluate the function argument and get a prediction of its likely value.
    FunctionBase* likely_function = dynamic_cast<FunctionBase*>(function);
    if (likely_function) {
	// The first element of the expression is a literal function.
	resolved_function = emitConstantPointer(likely_function);
    } else if (Symbol* symbol = dynamic_cast<Symbol*>(function)) {
	// The first element is a symbol.  Look it up.
	resolved_function = emitFunctionLookup(symbol, &likely_function);
	// Check that the lookup succeeded, unless the function was resolved
	// at compile time.
	if (!dynamic_cast<llvm::Constant*>(resolved_function)) {
	    emitErrorUnless(resolved_function,
			    _("could not find function \"%s\""),
			    emitConstantPointer(symbol->name()->c_str()));
	}
    } else {
	// The first element is a (function-valued) expression.  Fallback
	// to the interpreter for now.
	return Runtime::emitEvaluate(emitConstantPointer(expression),
				     m_context->getEnvironment(), this);
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
    // Resolve the function statically if possible.
    *expected_result = m_context->staticallyResolveFunction(symbol);
    if (*expected_result) {
	return emitConstantPointer(*expected_result);
    }

    // Otherwise do a dynamic function lookup.
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

    Value* result = emitEval(expression);
    CreateBr(merge_point->getParent());
    merge_point->addIncoming(result, GetInsertBlock());

    return block;
}

Value* Compiler::createBackEdge(llvm::BasicBlock* destination)
{
    Runtime::emitMaybeCheckForUserInterrupt(this);
    return CreateBr(destination);
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
	// std::make_pair(BuiltInFunction::obtain("<-"),
	// 	       &Compiler::emitInlinedAssign),
	std::make_pair(BuiltInFunction::obtain("("),
		       &Compiler::emitInlinedParen),
	std::make_pair(BuiltInFunction::obtain("{"),
		       &Compiler::emitInlinedBegin),
	std::make_pair(BuiltInFunction::obtain("return"),
		       &Compiler::emitInlinedReturn),
	std::make_pair(BuiltInFunction::obtain("if"),
		       &Compiler::emitInlinedIf),
	// std::make_pair(BuiltInFunction::obtain("for"),
	// 	       &Compiler::emitInlinedFor),
	std::make_pair(BuiltInFunction::obtain("while"),
		       &Compiler::emitInlinedWhile),
	std::make_pair(BuiltInFunction::obtain("repeat"),
		       &Compiler::emitInlinedRepeat),
	std::make_pair(BuiltInFunction::obtain("break"),
		       &Compiler::emitInlinedBreak),
	std::make_pair(BuiltInFunction::obtain("next"),
		       &Compiler::emitInlinedNext)
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

    if (dynamic_cast<llvm::Constant*>(resolved_function)) {
	// When the resolved function is a compile-time constant, everything is
	// simple.
	return (this->*emit_builtin)(expression);
    }

    InsertPoint incoming_block = saveIP();

    // TODO(kmillar): write an 'emitGuardedCode' function, and use that here.
    BasicBlock* inlined_builtin_block = createBasicBlock(builtin->name());
    BasicBlock* merge_block = createBasicBlock("continue");
    BasicBlock* fallback_block = createBasicBlock("fallback");

    // Setup the merge point.
    SetInsertPoint(merge_block);
    llvm::Type* robject_type = getType<RObject*>();
    llvm::PHINode* result = CreatePHI(robject_type, 2);

    // Emit inlined code.
    SetInsertPoint(inlined_builtin_block);

    llvm::Value* inlined_builtin_value = (this->*emit_builtin)(expression);
    if (!inlined_builtin_value) {
	// Code generation failed.
	inlined_builtin_block->eraseFromParent();
	merge_block->eraseFromParent();
	fallback_block->eraseFromParent();
	restoreIP(incoming_block);
	return nullptr;
    }
    if (GetInsertBlock()->getTerminator() == nullptr) {
	assert(inlined_builtin_value->getType() == robject_type);
	CreateBr(merge_block);
	result->addIncoming(inlined_builtin_value, GetInsertBlock());
    }

    // Code to check if the function is the predicted one.
    restoreIP(incoming_block);
    Value* likely_fn_value = m_context->getMemoryManager()
	->getBuiltIn(builtin);
    likely_fn_value = CreateBitCast(likely_fn_value,
				    llvm::TypeBuilder<FunctionBase*, false>::get(getContext()));
    Value* is_expected_builtin = CreateICmpEQ(resolved_function,
					      likely_fn_value);
    CreateCondBr(is_expected_builtin,
		 inlined_builtin_block,
		 fallback_block); // TODO(kmillar): set branch weights

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
    if (GetInsertBlock()->getTerminator() == nullptr) {
	CreateBr(merge_block);
	result->addIncoming(fallback_value, GetInsertBlock());
    }

    // Continuing code should be in the merged block.
    SetInsertPoint(merge_block);
    return result;
}

Value* Compiler::emitInlinedParen(const Expression* expression)
{
    // '(' has a single argument -- the expression to evaluate.
    // As a side-effect, it also enables result printing.
    if (listLength(expression) == 2) {
	Value* result = emitEval(CADR(const_cast<Expression*>(expression)));
	emitSetVisibility(true);
	return result;
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
    Value* return_value;

    int length = listLength(expression);
    if (length == 1) {
	return_value = emitNullValue();
    } else if (length == 2) {
	return_value = emitEval(CADR(const_cast<Expression*>(expression)));
    } else {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }
    return CreateRet(return_value);

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

    // Evaluate the condition and branch.
    Value* r_condition = emitEval(CADR(const_cast<Expression*>(expression)));
    // TODO(kmillar): r_condition needs GC protection.
    Value* boolean_condition = Runtime::emitCoerceToTrueOrFalse(
	r_condition, expression, this);
    InsertPoint branch_point = saveIP();

    // Create the merge point.
    BasicBlock* continue_block = createBasicBlock("continue");
    SetInsertPoint(continue_block);
    llvm::Type* robject_type = getType<RObject*>();
    llvm::PHINode* result_value = CreatePHI(robject_type, 2);

    // Create the if_true branch.
    BasicBlock* if_true_block = createBranch(
	"if_true",
	CADDR(const_cast<Expression*>(expression)),
	result_value, continue_block);

    // Generate code to handle the 'else' case.
    BasicBlock* if_false_block;
    if (length == 3) {
	// No 'else' branch.  Return an invisible NULL.
	if_false_block = createBasicBlock("if_false");
	SetInsertPoint(if_false_block);
	emitSetVisibility(false);
	CreateBr(continue_block);
	result_value->addIncoming(emitNullValue(),
				  GetInsertBlock());
    } else {
	// Create the else branch.
	if_false_block = createBranch(
	    "if_false",
	    CADDDR(const_cast<Expression*>(expression)),
	    result_value, continue_block);
    }

    restoreIP(branch_point);
    CreateCondBr(boolean_condition, if_true_block, if_false_block);

    SetInsertPoint(continue_block);
    return result_value;
}

Value* Compiler::emitInlinedRepeat(const Expression* expression)
{
    if (listLength(expression) != 2) {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }

    const RObject* body = expression->tail()->car();

    BasicBlock* loop_body = createBasicBlock("repeat_body");
    BasicBlock* continue_block = createBasicBlock("continue");

    CreateBr(loop_body);

    SetInsertPoint(loop_body);
    {
	// The loop scope ensures that 'break' and 'next' work correctly.
	LoopScope loop(m_context,
		       continue_block, loop_body,
		       this);
	emitEval(body);
    }
    createBackEdge(loop_body);

    SetInsertPoint(continue_block);

    return emitInvisibleNullValue();
}

Value* Compiler::emitInlinedWhile(const Expression* expression)
{
    if (listLength(expression) != 3) {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }

    const RObject* condition = expression->tail()->car();
    const RObject* body = expression->tail()->tail()->car(); 

    BasicBlock* loop_header = createBasicBlock("while_header");
    BasicBlock* loop_body = createBasicBlock("while_body");
    BasicBlock* continue_block = createBasicBlock("continue");

    CreateBr(loop_header);

    SetInsertPoint(loop_header);
    llvm::Value* evaluated_condition = emitEval(condition);
    llvm::Value* condition_as_bool = Runtime::emitCoerceToTrueOrFalse(
	evaluated_condition, expression, this);
    CreateCondBr(condition_as_bool, loop_body, continue_block);

    SetInsertPoint(loop_body);
    {
	LoopScope loop(m_context,
		       continue_block, loop_header,
		       this);
	emitEval(body);
    }
    createBackEdge(loop_header);

    SetInsertPoint(continue_block);
    return emitInvisibleNullValue();
}

Value* Compiler::emitInlinedBreak(const Expression* expression) {
    if (listLength(expression) != 1) {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }

    BasicBlock* dest = m_context->getBreakDestination();
    if (dest) {
	return CreateBr(dest);
    } else {
	// This is probably user error. Let the interpreter deal with it.
	return Runtime::emitBreak(m_context->getEnvironment(), this);
    }
}

Value* Compiler::emitInlinedNext(const Expression* expression) {
    if (listLength(expression) != 1) {
	// This is probably a syntax error.  Let the interpreter handle it.
	return nullptr;
    }

    BasicBlock* dest = m_context->getNextDestination();
    if (dest) {
	return createBackEdge(dest);
    } else {
	return Runtime::emitNext(m_context->getEnvironment(), this);
    }
}

BasicBlock* Compiler::emitLandingPad(PHINode* dispatch) {
    InsertPointGuard preserve_insert_point(*this);

    BasicBlock* block = createBasicBlock("landing_pad");
    SetInsertPoint(block);

    llvm::LandingPadInst* landing_pad = Runtime::emitLandingPad(this);
    // It's entirely possible that the landing pad only needs to handle some
    // exception types, so catching everything is overly general.  However
    // exception handling should be rare, catching and rethrowing exceptions that
    // aren't handled is perfectly valid, so this simplification makes sense.
    landing_pad->addClause(emitConstantPointer((void*)nullptr));  // Catches everything.

    CreateBr(dispatch->getParent());
    dispatch->addIncoming(landing_pad, block);
    return block;
}

Value* Compiler::getExceptionTypeId(const std::type_info* type) {
    llvm::Function* get_type_id
	= llvm::Intrinsic::getDeclaration(m_context->getModule(),
					  llvm::Intrinsic::eh_typeid_for);
    Value* exception_type_info = m_context->getMemoryManager()
	->addGlobal((char*)type, true,
		    std::string("type.info.") + type->name());

    return CreateCall(get_type_id, exception_type_info);
}

PHINode* Compiler::emitDispatchToExceptionHandler(const std::type_info* type,
						  PHINode* handler,
						  PHINode* fallthrough) {
    InsertPointGuard preserve_insert_point(*this);

    BasicBlock* block = createBasicBlock("dispatch");
    SetInsertPoint(block);

    PHINode* exception_info = CreatePHI(Runtime::exceptionInfoType(this), 1);
    Value* exception_type_id = CreateExtractValue(exception_info, 1);

    Value* handled_exception_type_id = getExceptionTypeId(type);
    Value* matches = CreateICmpEQ(exception_type_id, handled_exception_type_id);
    CreateCondBr(matches,
		 handler->getParent(),
		 fallthrough->getParent());
    
    handler->addIncoming(exception_info, block);
    fallthrough->addIncoming(exception_info, block);
    return exception_info;
}

PHINode* Compiler::emitLoopExceptionHandler(llvm::BasicBlock* break_destination,
					    llvm::BasicBlock* next_destination)
{
    assert(next_destination != nullptr);
    assert(break_destination != nullptr);
    InsertPointGuard preserve_insert_point(*this);

    BasicBlock* block = createBasicBlock("loop_exception_handler");
    SetInsertPoint(block);
    
    PHINode* exception_info = CreatePHI(Runtime::exceptionInfoType(this), 1);
    Value* exception_ref = CreateExtractValue(exception_info, 0);
    Value* exception = Runtime::emitBeginCatch(exception_ref, this);
    Value* isNext = Runtime::emitLoopExceptionIsNext(exception, this);
    Runtime::emitEndCatch(this);

    CreateCondBr(isNext, next_destination, break_destination);
    return exception_info;
}

PHINode* Compiler::emitReturnExceptionHandler()
{
    InsertPointGuard preserve_insert_point(*this);

    BasicBlock* block = createBasicBlock("return_exception_handler");
    SetInsertPoint(block);
    
    PHINode* exception_info = CreatePHI(Runtime::exceptionInfoType(this), 1);
    Value* exception_ref = CreateExtractValue(exception_info, 0);
    Value* exception = Runtime::emitBeginCatch(exception_ref, this);

    Value* return_value = Runtime::emitGetReturnExceptionValue(exception, this);
    Runtime::emitEndCatch(this);

    CreateRet(return_value);
    return exception_info;
}

PHINode* Compiler::emitRethrowException()
{
    InsertPointGuard preserve_insert_point(*this);

    BasicBlock* block = createBasicBlock("rethrow_exception");
    SetInsertPoint(block);
    
    PHINode* exception_info = CreatePHI(Runtime::exceptionInfoType(this), 1);
    CreateResume(exception_info);
    return exception_info;
}

} // namespace JIT
} // namespace CXXR
