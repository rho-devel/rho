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

#ifndef CXXR_JIT_EMIT_IR_HPP
#define CXXR_JIT_EMIT_IR_HPP

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/TypeBuilder.h"

namespace CXXR {

class Environment;
class Expression;
class FunctionBase;
class RObject;
class Symbol;

namespace JIT {

llvm::Constant* emitConstantPointer(const void* value, llvm::Type* type);

template <class T>
llvm::Constant* emitConstantPointer(const T* value)
{
    llvm::LLVMContext& context = llvm::getGlobalContext();
    return emitConstantPointer(reinterpret_cast<const void*>(value),
			       llvm::TypeBuilder<T*, false>::get(context));
}

llvm::Constant* emitSymbol(const Symbol* symbol);

llvm::Constant* emitNullValue();

// This emits the code to do: eval(object, environment).
llvm::Value* emitEval(const RObject* object, llvm::Value* environment,
		      llvm::IRBuilder<>* builder);

llvm::Value* emitExpressionEval(const Expression* object,
				llvm::Value* environment,
				llvm::IRBuilder<>* builder);

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_EMIT_IR__HPP
