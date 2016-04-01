/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

#include "rho/ArgList.hpp"
#include "rho/Environment.hpp"
#include "rho/Evaluator.hpp"
#include "rho/Expression.hpp"
#include "rho/FunctionBase.hpp"
#include "rho/LoopBailout.hpp"
#include "rho/LoopException.hpp"
#include "rho/PairList.hpp"
#include "rho/RObject.hpp"
#include "rho/StackChecker.hpp"
#include "rho/Symbol.hpp"
#include "rho/jit/CompiledFrame.hpp"
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

using namespace rho;

extern "C" {

RObject* rho_runtime_evaluate(RObject* value, Environment* environment)
{
    return Evaluator::evaluate(value, environment);
}

RObject* rho_runtime_lookupSymbol(const Symbol* value,
				   Environment* environment)
{
    return const_cast<Symbol*>(value)->evaluate(environment);
}

/*
 * Lookup a symbol in a CompiledFrame.
 * Note that this function doesn't handle the cases where the symbol is
 * ..., ..n, or missingArg().  rho_runtime_lookupSymbol should be used in
 * those cases.
 */
RObject* rho_runtime_lookupSymbolInCompiledFrame(const Symbol* symbol,
						  Environment* environment,
						  int position)
{
    assert(environment->frame() != nullptr);
    assert(symbol != DotsSymbol);
    assert(!symbol->isDotDotSymbol());
    assert(symbol != R_MissingArg);
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
    return rho_runtime_lookupSymbol(symbol, environment);
}

/*
 * Assign to a symbol in a CompiledFrame.
 */
void rho_runtime_assignSymbolInCompiledFrame(const Symbol* symbol,
					      Environment* environment,
					      int position,
					      RObject* value)
{
    assert(environment->frame() != nullptr);
    assert(value != R_MissingArg);
    assert(position >= 0);

    JIT::CompiledFrame* frame
	// TODO(kmillar): when optimizing make this a static cast.
	= dynamic_cast<JIT::CompiledFrame*>(environment->frame());
    assert(frame != nullptr);

    Frame::Binding* binding = frame->obtainBinding(symbol, position);
    binding->assign(value);
}

FunctionBase* rho_runtime_lookupFunction(const Symbol* symbol,
					  Environment* environment)
{
    return findFunction(symbol, environment, true);
}

RObject* rho_runtime_callFunction(const FunctionBase* function,
				   const PairList* args, const Expression* call,
				   Environment* environment)
{
    IncrementStackDepthScope scope;

    ArgList arglist(args, ArgList::RAW);
    return call->evaluateFunctionCall(function, environment, &arglist);
}

void rho_runtime_do_break(Environment* environment) {
    if (!environment->loopActive())
	Rf_error(_("no loop to break from"));
    (new LoopBailout(environment, false))->throwException();
}

void rho_runtime_do_next(Environment* environment) {
    if (!environment->loopActive())
	Rf_error(_("no loop to break from"));
    (new LoopBailout(environment, true))->throwException();
}

bool rho_runtime_loopExceptionIsNext(void* exception) {
    LoopException* loop_exception = static_cast<LoopException*>(exception);
    return loop_exception->next();
}

// In src/main/eval.cpp
extern "C++"
Rboolean asLogicalNoNA(SEXP s, SEXP call);
extern "C++"
SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho);

bool rho_runtime_coerceToTrueOrFalse(RObject* object, Expression* call) {
    return asLogicalNoNA(object, call);
}

bool rho_runtime_is_function(RObject* object) {
    return Rf_isFunction(object);
}

void rho_runtime_setVisibility(bool visible) {
    Evaluator::enableResultPrinting(visible);
}

void rho_runtime_incrementNamed(RObject* object) {
    switch (NAMED(object)) {
    case 0: SET_NAMED(object, 1); break;
    case 1: SET_NAMED(object, 2); break;
    }
}

void rho_runtime_maybeCheckForUserInterrupts() {
    Evaluator::maybeCheckForUserInterrupts();
}

// From R's C API.
void Rf_error(const char*, ...) __attribute__((noreturn));
void Rf_warning(const char*, ...);

// To force the symbol to be emitted.
// TODO(kmillar): remove this once BuiltInFunction is used in this file.
void force_symbol_emission_built_in_function(BuiltInFunction*) { }

RObject* rho_runtime_applydefine(RObject* call, RObject* op, RObject* args,
                                  RObject* rho) {
  return applydefine(call, op, args, rho);
}
}

void force_exception_handling_symbol_emission() {
    try {
	Rf_warning("unused");
    } catch (...) {
	Rf_warning("also unused");
        throw;
    }
}
