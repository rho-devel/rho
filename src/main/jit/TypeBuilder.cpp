/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

#define R_NO_REMAP

#include "CXXR/jit/TypeBuilder.hpp"
#include "CXXR/jit/Runtime.hpp"

using namespace CXXR;

namespace llvm {

#define DEFINE_TYPEBUILDER_FOR(CXXR_TYPE)                                      \
    template <>                                                                \
    StructType* TypeBuilder<CXXR_TYPE, false>::get(LLVMContext& context)       \
    {                                                                          \
	static StructType* result                                              \
	    = CXXR::JIT::Runtime::getCxxrType(#CXXR_TYPE, context);            \
	return result;                                                         \
    }

DEFINE_TYPEBUILDER_FOR(Bailout);
DEFINE_TYPEBUILDER_FOR(BuiltInFunction);
DEFINE_TYPEBUILDER_FOR(ByteCode);
DEFINE_TYPEBUILDER_FOR(Closure);
DEFINE_TYPEBUILDER_FOR(ConsCell);
DEFINE_TYPEBUILDER_FOR(DottedArgs);
DEFINE_TYPEBUILDER_FOR(Environment);
DEFINE_TYPEBUILDER_FOR(Expression);
DEFINE_TYPEBUILDER_FOR(ExternalPointer);
DEFINE_TYPEBUILDER_FOR(FunctionBase);
DEFINE_TYPEBUILDER_FOR(LoopBailout);
DEFINE_TYPEBUILDER_FOR(PairList);
DEFINE_TYPEBUILDER_FOR(Promise);
DEFINE_TYPEBUILDER_FOR(RObject);
DEFINE_TYPEBUILDER_FOR(ReturnBailout);
DEFINE_TYPEBUILDER_FOR(S4Object);
DEFINE_TYPEBUILDER_FOR(Symbol);
DEFINE_TYPEBUILDER_FOR(VectorBase);
DEFINE_TYPEBUILDER_FOR(WeakRef);

} // namespace llvm
