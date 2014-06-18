#include "CXXR/ArgList.hpp"
#include "CXXR/Environment.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/PairList.h"
#include "CXXR/RObject.h"
#include "CXXR/Symbol.h"

/*
 * This file contains functions that are available in the runtime module.
 * It gets compiled into LLVM bytecode as part of the compilation process, and
 * the JIT compiler loads the bytecode and makes it available to the functions
 * it builds.
 * Because it is available as LLVM IR at runtime, LLVM is able to inline these
 * directly into the code that it generates.
 *
 * clang -emit-llvm -c  -o foo.bc # emits bitcode
 * clang -emit-llvm -S  -o foo.ll # emits human readable IR
 */

using namespace CXXR;

extern "C" {

RObject* cxxr_runtime_evaluate(RObject* value, Environment* environment)
{
    return value->evaluate(environment);
}

RObject* cxxr_runtime_lookupSymbol(const Symbol* value,
				   Environment* environment)
{
    return const_cast<Symbol*>(value)->evaluate(environment);
}

FunctionBase* cxxr_runtime_lookupFunction(const Symbol* symbol,
					  Environment* environment)
{
    return findFunction(symbol, environment, true);
}

RObject* cxxr_runtime_callFunction(const FunctionBase* function,
				   const PairList* args, const Expression* call,
				   Environment* environment)
{
    ArgList arglist(args, ArgList::RAW);
    return function->apply(&arglist, environment, call);
}
}
