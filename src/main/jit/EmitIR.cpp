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

#include "CXXR/jit/EmitIR.hpp"

#include "llvm/IR/Function.h"

#include "CXXR/DottedArgs.hpp"
#include "CXXR/Expression.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/RObject.h"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/Symbol.h"
#include "CXXR/jit/Runtime.hpp"
#include "CXXR/jit/TypeBuilder.hpp"

using llvm::BasicBlock;
using llvm::Value;

namespace CXXR {
namespace JIT {

Compiler::Compiler(CompilerContext context)
    : IRBuilder<>(context.getLLVMContext()), m_context(context)
{
    // Setup the entry block.
    BasicBlock* entry_block
	= BasicBlock::Create(getContext(), "EntryBlock", context.getFunction());
    SetInsertPoint(entry_block);
}

llvm::Constant* Compiler::emitConstantPointer(const void* value,
					      llvm::Type* type)
{
    llvm::ConstantInt* pointer_as_integer = llvm::ConstantInt::get(
	llvm::TypeBuilder<intptr_t, false>::get(getContext()),
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
	assert(0 && "Unexpected eval of bytecode in JIT compilation.");
	return nullptr;
    case PROMSXP:
	assert(0 && "Unexpected eval of a promise in JIT compilation.");
	return nullptr;
    default:
	return emitConstantPointer(object);
    }
}

Value* Compiler::emitSymbolEval(const Symbol* symbol)
{
    return Runtime::emitLookupSymbol(emitSymbol(symbol),
				     m_context.getEnvironment(), this);
}

Value* Compiler::emitExpressionEval(const Expression* expression)
{
    RObject* function = expression->car();
    Value* resolved_function = nullptr;

    if (FunctionBase::isA(function)) {
	resolved_function
	    = emitConstantPointer(SEXP_downcast<FunctionBase*>(function));
    } else {
	// Lookup the function.
	// TODO(kmillar): verify that we actually got a symbol here.
	llvm::Value* fn = emitSymbol(SEXP_downcast<Symbol*>(function));
	resolved_function
	    = Runtime::emitLookupFunction(fn, m_context.getEnvironment(), this);
    }

    // Call the function.
    return Runtime::emitCallFunction(
	resolved_function, emitConstantPointer(expression->tail()),
	emitConstantPointer(expression), m_context.getEnvironment(), this);
}

Value* Compiler::emitDotsEval(const DottedArgs* expression)
{
    // Call the interpreter.
    return Runtime::emitEvaluate(emitConstantPointer(expression),
				 m_context.getEnvironment(), this);
}

} // namespace JIT
} // namespace CXXR
