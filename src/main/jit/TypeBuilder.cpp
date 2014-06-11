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

#include "CXXR/jit/TypeBuilder.hpp"

using namespace CXXR;

namespace llvm {

#define DEFINE_TYPEBUILDER_FOR(CXXR_TYPE)                                    \
    template <>                                                              \
    StructType* TypeBuilder<CXXR_TYPE, false>::get(LLVMContext& context)     \
    {                                                                        \
        static StructType* result =                                          \
	    StructType::create(context, std::string("CXXR::") + #CXXR_TYPE); \
        return result;                                                       \
    }

DEFINE_TYPEBUILDER_FOR(Bailout);
DEFINE_TYPEBUILDER_FOR(BuiltinFunction);
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
