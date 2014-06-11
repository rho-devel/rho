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

#include "CXXR/RObject.h"
#include "CXXR/Expression.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/Symbol.h"
#include "CXXR/jit/Runtime.hpp"
#include "CXXR/jit/TypeBuilder.hpp"

using llvm::IRBuilder;
using llvm::Value;

namespace CXXR {
namespace JIT {

llvm::Constant* emitConstantPointer(const void* value, llvm::Type* type)
{
    llvm::LLVMContext& context = llvm::getGlobalContext();
    llvm::ConstantInt* pointer_as_integer = llvm::ConstantInt::get(
	llvm::TypeBuilder<intptr_t, false>::get(context),
	reinterpret_cast<intptr_t>(value));
    return llvm::ConstantExpr::getIntToPtr(pointer_as_integer, type);
}

llvm::Constant* emitSymbol(const Symbol* symbol)
{
    // TODO: consider caching these.
    // TODO: give the symbol a useful name in the IR.
    return emitConstantPointer(symbol);
}

llvm::Constant* emitNullValue()
{
    RObject* null = nullptr;
    return emitConstantPointer(null);
}

Value* emitEval(const RObject* object, Value* environment, IRBuilder<>* builder)
{
    // This has a non-trivial implementation for all the objects which have
    // non-default object->evaluate() implementations.
    if (!object) {
	return emitNullValue();
    }

    switch (object->sexptype()) {
    case SYMSXP: {
	const Symbol* symbol = SEXP_downcast<const Symbol*>(object);
	return Runtime::emitLookupSymbol(symbol, environment, builder);
    }
    case LANGSXP:
	return emitExpressionEval(SEXP_downcast<const Expression*>(object),
				  environment, builder);
    case DOTSXP:
	// Call the interpreter.
	return Runtime::emitEvaluate(emitConstantPointer(object), environment,
				     builder);
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

Value* emitExpressionEval(const Expression* expression, Value* environment,
			  IRBuilder<>* builder)
{
    RObject* function = expression->car();
    Value* resolved_function = nullptr;

    if (FunctionBase::isA(function)) {
	resolved_function
	    = emitConstantPointer(SEXP_downcast<FunctionBase*>(function));
    } else {
	// Lookup the function.
	resolved_function = Runtime::emitLookupFunction(
	    SEXP_downcast<Symbol*>(function), environment, builder);
    }

    return Runtime::emitCallFunction(
	resolved_function, emitConstantPointer(expression->tail()),
	emitConstantPointer(expression), environment, builder);
}

} // namespace JIT
} // namespace CXXR
