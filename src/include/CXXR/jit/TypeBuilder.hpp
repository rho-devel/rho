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

#ifndef CXXR_JIT_TYPE_BUILDER_HPP
#define CXXR_JIT_TYPE_BUILDER_HPP

#include "llvm/IR/TypeBuilder.h"

namespace CXXR {
class Bailout;
class BuiltInFunction;
class ByteCode;
class Closure;
class ConsCell;
class DottedArgs;
class Environment;
class Expression;
class ExternalPointer;
class FunctionBase;
class LoopBailout;
class PairList;
class Promise;
class RObject;
class ReturnBailout;
class S4Object;
class Symbol;
class VectorBase;
class WeakRef;
} // namespace CXXR

/**
 * This file adds the necessary information for LLVM's TypeBuilder to handle
 * CXXR types.
 */
namespace llvm {

#define DECLARE_TYPEBUILDER_FOR(CXXR_TYPE)                                     \
    template <bool xcompile>                                                   \
    class TypeBuilder<::CXXR::CXXR_TYPE, xcompile> {                           \
    public:                                                                    \
	static StructType* get(LLVMContext& context);                          \
    };

// TODO: handle vector types: IntegerVector etc.
DECLARE_TYPEBUILDER_FOR(Bailout);
DECLARE_TYPEBUILDER_FOR(BuiltInFunction);
DECLARE_TYPEBUILDER_FOR(ByteCode);
DECLARE_TYPEBUILDER_FOR(Closure);
DECLARE_TYPEBUILDER_FOR(ConsCell);
DECLARE_TYPEBUILDER_FOR(DottedArgs);
DECLARE_TYPEBUILDER_FOR(Environment);
DECLARE_TYPEBUILDER_FOR(Expression);
DECLARE_TYPEBUILDER_FOR(ExternalPointer);
DECLARE_TYPEBUILDER_FOR(FunctionBase);
DECLARE_TYPEBUILDER_FOR(LoopBailout);
DECLARE_TYPEBUILDER_FOR(PairList);
DECLARE_TYPEBUILDER_FOR(Promise);
DECLARE_TYPEBUILDER_FOR(RObject);
DECLARE_TYPEBUILDER_FOR(ReturnBailout);
DECLARE_TYPEBUILDER_FOR(S4Object);
DECLARE_TYPEBUILDER_FOR(Symbol);
DECLARE_TYPEBUILDER_FOR(VectorBase);
DECLARE_TYPEBUILDER_FOR(WeakRef);

#undef DECLARE_TYPEBUILDER_FOR
} // namespace llvm

#endif // CXXR_JIT_TYPE_BUILDER_HPP
