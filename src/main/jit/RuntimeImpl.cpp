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

#define R_NO_REMAP

#include "CXXR/ArgList.hpp"
#include "CXXR/Environment.h"
#include "CXXR/Expression.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/LoopBailout.hpp"
#include "CXXR/LoopException.hpp"
#include "CXXR/PairList.h"
#include "CXXR/RObject.h"
#include "CXXR/StackChecker.hpp"
#include "CXXR/Symbol.h"
#include "CXXR/jit/CompiledFrame.hpp"
#include "Defn.h"

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
    return Evaluator::evaluate(value, environment);
}

RObject* cxxr_runtime_lookupSymbol(const Symbol* value,
				   Environment* environment)
{
    return const_cast<Symbol*>(value)->evaluate(environment);
}

/*
 * Lookup a symbol in a CompiledFrame.
 * Note that this function doesn't handle the cases where the symbol is
 * ..., ..n, or missingArg().  cxxr_runtime_lookupSymbol should be used in
 * those cases.
 */
RObject* cxxr_runtime_lookupSymbolInCompiledFrame(const Symbol* value,
						  Environment* environment,
						  int position)
{
    assert(environment->frame() != nullptr);
    assert(value != DotsSymbol);
    assert(!value->isDotDotSymbol());
    assert(value != R_MissingArg);
    assert(position >= 0);

    JIT::CompiledFrame* frame
	// TODO(kmillar): when optimizing make this a static cast.
	= dynamic_cast<JIT::CompiledFrame*>(environment->frame());
    assert(frame != nullptr);

    Frame::Binding* binding = frame->binding(position);
    if (binding) {
	// NB: this logic handles the case where the binding is a promise that
	//   resolves to an unboundValue or missingArgument slightly differently
	//   than Symbol::evaluate() does.
	std::pair<RObject*, bool> pair = binding->forcedValue2();
	RObject* value = pair.first;
	if (value
	    && value != Symbol::missingArgument()
	    && value != Symbol::unboundValue())
	{
	    if (pair.second) {
		SET_NAMED(value, 2);
	    }
	    else if (NAMED(value) < 1) {
		SET_NAMED(value, 1);
	    }
	    return value;
	}
    }
    // Fallback to the interpreter.
    return cxxr_runtime_lookupSymbol(value, environment);
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
    IncrementStackDepthScope scope;

    ArgList arglist(args, ArgList::RAW);
    return function->apply(&arglist, environment, call);
}

void cxxr_runtime_do_break(Environment* environment) {
    if (!environment->loopActive())
	Rf_error(_("no loop to break from"));
    CXXR_NEW(LoopBailout(environment, false))->throwException();
}

void cxxr_runtime_do_next(Environment* environment) {
    if (!environment->loopActive())
	Rf_error(_("no loop to break from"));
    CXXR_NEW(LoopBailout(environment, true))->throwException();
}

bool cxxr_runtime_loopExceptionIsNext(void* exception) {
    LoopException* loop_exception = static_cast<LoopException*>(exception);
    return loop_exception->next();
}

// In src/main/eval.cpp
extern "C++"
Rboolean asLogicalNoNA(SEXP s, SEXP call);

bool cxxr_runtime_coerceToTrueOrFalse(RObject* object, Expression* call) {
    return asLogicalNoNA(object, call);
}

bool cxxr_runtime_is_function(RObject* object) {
    return Rf_isFunction(object);
}

void cxxr_runtime_setVisibility(bool visible) {
    Evaluator::enableResultPrinting(visible);
}

void cxxr_runtime_maybeCheckForUserInterrupts() {
    Evaluator::maybeCheckForUserInterrupts();
}

// From R's C API.
void Rf_error(const char*, ...) __attribute__((noreturn));
void Rf_warning(const char*, ...);

// To force the symbol to be emitted.
// TODO(kmillar): remove this once BuiltInFunction is used in this file.
void force_symbol_emission_built_in_function(BuiltInFunction*) { }
}

void force_exception_handling_symbol_emission() {
    try {
	Rf_warning("unused");
    } catch (...) {
	Rf_warning("also unused");
    }
}
