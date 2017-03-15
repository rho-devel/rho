/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Expression.cpp
 *
 * @brief Class Expression and associated C interface.
 */
#define R_NO_REMAP

#include "rho/Expression.hpp"

#include <iostream>
#include <boost/preprocessor.hpp>

#include "R_ext/Error.h"
#include "localization.h"
#include "rho/ArgList.hpp"
#include "rho/ArgMatcher.hpp"
#include "rho/BuiltInFunction.hpp"
#include "rho/Closure.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/Environment.hpp"
#include "rho/Evaluator.hpp"
#include "rho/FunctionContext.hpp"
#include "rho/FunctionBase.hpp"
#include "rho/GCStackFrameBoundary.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/PlainContext.hpp"
#include "rho/ProtectStack.hpp"
#include "rho/RAllocStack.hpp"
#include "rho/StackChecker.hpp"
#include "rho/Symbol.hpp"

#undef match

using namespace std;
using namespace rho;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace rho {
    namespace ForceNonInline {
	SEXP (*lconsp)(SEXP cr, SEXP tl) = Rf_lcons;
   }
}

GCRoot<> R_CurrentExpr;

Expression::Expression(RObject* function, std::initializer_list<RObject*> args)
    : Expression(function)
{
    ConsCell* current = this;
    for (RObject* arg : args) {
	PairList* next = new PairList(arg);
	current->setTail(next);
	current = next;
    }
}

Expression* Expression::clone() const
{
    return new CachingExpression(*this);
}

FunctionBase* Expression::getFunction(Environment* env) const
{
    RObject* head = car();
    if (head->sexptype() == SYMSXP) {
	Symbol* symbol = static_cast<Symbol*>(head);
	FunctionBase* func = findFunction(symbol, env);
	if (!func)
	    Rf_error(_("could not find function \"%s\""),
		  symbol->name()->c_str());
	return func;
    } else {
	RObject* val = Evaluator::evaluate(head, env);
	if (!FunctionBase::isA(val))
	    Rf_error(_("attempt to apply non-function"));
	return static_cast<FunctionBase*>(val);
    }
}

RObject* Expression::evaluate(Environment* env)
{
    IncrementStackDepthScope scope;
    RAllocStack::Scope ras_scope;
    ProtectStack::Scope ps_scope;

    FunctionBase* function = getFunction(env);

    ArgList arglist(tail(), ArgList::RAW);
    return evaluateFunctionCall(function, env, arglist);
}

RObject* Expression::evaluateFunctionCall(const FunctionBase* func,
                                          Environment* env,
                                          const ArgList& raw_arglist) const
{
    func->maybeTrace(this);

    if (func->sexptype() == CLOSXP) {
      return invokeClosure(static_cast<const Closure*>(func), env,
                           raw_arglist, nullptr);
    }

    assert(func->sexptype() == BUILTINSXP || func->sexptype() == SPECIALSXP);
    return applyBuiltIn(static_cast<const BuiltInFunction*>(func), env,
                        raw_arglist);
}

RObject* Expression::applyBuiltIn(const BuiltInFunction* builtin,
                                  Environment* env, const ArgList& raw_arglist)
    const
{
    RObject* result;

    if (builtin->createsStackFrame()) {
	FunctionContext context(this, env, builtin);
	result = evaluateBuiltInCall(builtin, env, raw_arglist);
    } else {
	PlainContext context;
        result = evaluateBuiltInCall(builtin, env, raw_arglist);
    }

    if (builtin->printHandling() != BuiltInFunction::SOFT_ON) {
	Evaluator::enableResultPrinting(
            builtin->printHandling() != BuiltInFunction::FORCE_OFF);
    }
    return result;
}

static void prepareToInvokeBuiltIn(const BuiltInFunction* func)
{
    if (func->printHandling() == BuiltInFunction::SOFT_ON) {
	Evaluator::enableResultPrinting(true);
    }

#ifdef Win32
    // This is an inlined version of Rwin_fpreset (src/gnuwin/extra.c)
    // and resets the precision, rounding and exception modes of a
    // ix86 fpu.
    // It gets called prior to every builtin function, just in case a badly
    // behaved DLL has changed the fpu control word.
    __asm__ ( "fninit" );
#endif
}

RObject* Expression::evaluateFixedArityBuiltIn(const BuiltInFunction* func,
					       Environment* env,
					       const ArgList& arglist) const
{
    const PairList* tags = arglist.tags();
    prepareToInvokeBuiltIn(func);
    switch(func->arity()) {
    case 0:
        return func->invokeFixedArity(this, env, tags);
/*  This macro expands out to:
    case 1:
	return func->invokeFixedArity(this, env, tags, arglist.get(0));
    case 2:
	return func->invokeFixedArity(this, env, tags, arglist.get(0), arglist.get(1));
    ...
*/
#define ARGUMENT_LIST(Z, N, IGNORED) BOOST_PP_COMMA_IF(N) arglist.get(N)
#define CASE_STATEMENT(Z, N, IGNORED)              \
    case N:                                        \
      return func->invokeFixedArity(this, env, tags, BOOST_PP_REPEAT(N, ARGUMENT_LIST, 0));

	BOOST_PP_REPEAT_FROM_TO(1, 20, CASE_STATEMENT, 0);

 #undef ARGUMENT_LIST
#undef CASE_STATEMENT

    default:
	Rf_errorcall(const_cast<Expression*>(this),
		  _("too many arguments, sorry"));
    }
}

static inline RObject* evalIfNonNull(RObject* x, Environment* env) {
  return x ? x->evaluate(env) : x;
}

RObject* Expression::evalArgsAndEvaluateFixedArityBuiltIn(
    const BuiltInFunction* func, Environment* env, const ArgList& arglist) const
{
    const PairList* tags = arglist.tags();
    int arity = func->arity();
    if (arity == 0) {
      prepareToInvokeBuiltIn(func);
      return func->invokeFixedArity(this, env, tags);
    }
    auto arg_iterator = arglist.getArgs().begin();
/*  This macro expands out to:
    RObject* arg0 = evalIfNonNull(arg_iterator->car(), env);
    if (arity == 1) {
      prepareToInvokeBuiltIn(func);
      return func->invokeFixedArity(this, env, tags, arg0);
    }
    ++arg_iterator;
    RObject* arg1 = evalIfNonNull(arg_iterator->car(), env);
    if (arity == 2) {
      prepareToInvokeBuiltIn(func);
      return func->invokeFixedArity(this, env, tags, arg0, arg1);
    }
    ...
*/
#define ARGUMENT_LIST(Z, N, IGNORED) BOOST_PP_COMMA_IF(N) arg##N
#define CMD_SEQUENCE(Z, N, IGNORED)                            \
    RObject* arg##N = evalIfNonNull(arg_iterator->car(), env); \
    if (arity == N + 1) {                                      \
      prepareToInvokeBuiltIn(func);                            \
      return func->invokeFixedArity(this, env, tags,           \
                                    BOOST_PP_REPEAT(BOOST_PP_ADD(N, 1), ARGUMENT_LIST, 0)); \
    }                                                          \
    ++arg_iterator;

    BOOST_PP_REPEAT_FROM_TO(0, 20, CMD_SEQUENCE, 0);

    Rf_errorcall(const_cast<Expression*>(this),
                 _("too many arguments, sorry"));
}
#undef CMD_SEQUENCE
#undef ARGUMENT_LIST

RObject* Expression::evaluateBuiltInCall(
    const BuiltInFunction* func, Environment* env, const ArgList& arglist) const
{
    if (func->getCallingConvention()
        == BuiltInFunction::CallingConvention::PairList)
    {
      if (func->sexptype() == SPECIALSXP) {
        return evaluatePairListSpecialCall(func, env, arglist);
      } else {
        return evaluatePairListBuiltInCall(func, env, arglist);
      }
    } else {
        return evaluateDirectBuiltInCall(func, env, arglist);
    }
}

RObject* Expression::evaluateDirectBuiltInCall(
    const BuiltInFunction* func, Environment* env, const ArgList& arglist) const
{
    bool args_need_evaluation = arglist.status() != ArgList::EVALUATED;
    if (args_need_evaluation && arglist.has3Dots()) {
        ArgList expanded_args(arglist);
        expanded_args.evaluate(env);
        return evaluateDirectBuiltInCall(func, env, expanded_args);
    }

    // Check the number of arguments.
    int num_evaluated_args = arglist.size();
    func->checkNumArgs(num_evaluated_args, this);

    // Check that any naming requirements on the first arg are satisfied.
    const char* first_arg_name = func->getFirstArgName();
    if (first_arg_name)
	check1arg(first_arg_name);

    if (func->getCallingConvention()
        == BuiltInFunction::CallingConvention::FixedNative)
    {
        if (args_need_evaluation) {
            return evalArgsAndEvaluateFixedArityBuiltIn(func, env, arglist);
        } else {
            return evaluateFixedArityBuiltIn(func, env, arglist);
        }
    }

    // Create an array on stack to write arguments to.
    RObject** evaluated_arg_array = (RObject**)alloca(
        num_evaluated_args * sizeof(RObject*));

    // Copy the arguments to the stack, evaluating if necessary.
    arglist.evaluateToArray(env, num_evaluated_args, evaluated_arg_array);

    prepareToInvokeBuiltIn(func);
    return func->invoke(this, env, evaluated_arg_array, num_evaluated_args,
                        arglist.tags());
}

RObject* Expression::evaluatePairListSpecialCall(
    const BuiltInFunction* func, Environment* env, const ArgList& arglist) const
{
    // Check the number of arguments.
    int num_evaluated_args = arglist.size();
    func->checkNumArgs(num_evaluated_args, this);

    // Check that any naming requirements on the first arg are satisfied.
    const char* first_arg_name = func->getFirstArgName();
    if (first_arg_name) {
	check1arg(first_arg_name);
    }

    return func->invokeSpecial(this, env, arglist.list());
}

RObject* Expression::evaluatePairListBuiltInCall(
    const BuiltInFunction* func, Environment* env, const ArgList& arglist) const
{
    assert(func->sexptype() == BUILTINSXP);
    ArgList evaluated_args(arglist);
    if (arglist.status() != ArgList::EVALUATED)
    {
      evaluated_args.evaluate(env);
    }

    // Check the number of arguments.
    int num_evaluated_args = evaluated_args.size();
    func->checkNumArgs(num_evaluated_args, this);

    // Check that any naming requirements on the first arg are satisfied.
    const char* first_arg_name = func->getFirstArgName();
    if (first_arg_name) {
	check1arg(first_arg_name);
    }

    prepareToInvokeBuiltIn(func);
    return func->invoke(this, env, std::move(evaluated_args));
}

RObject* Expression::invokeClosure(const Closure* func,
                                   Environment* calling_env,
                                   const ArgList& arglist,
                                   const Frame* method_bindings) const
{
  return GCStackFrameBoundary::withStackFrameBoundary(
      [=]() { return invokeClosureImpl(func, calling_env, arglist,
                                       method_bindings); });
}

void Expression::matchArgsIntoEnvironment(const Closure* func,
					  Environment* calling_env,
					  const ArgList& arglist,
					  Environment* execution_env) const
{
    const ArgMatcher* matcher = func->matcher();
    ClosureContext context(this, calling_env, func, execution_env);
    matcher->match(execution_env, arglist);
}

RObject* Expression::invokeClosureImpl(const Closure* func,
                                       Environment* calling_env,
                                       const ArgList& parglist,
                                       const Frame* method_bindings) const
{
    // We can't modify parglist, as it's on the other side of a
    // GCStackFrameboundary, so make a copy instead.
    ArgList arglist(parglist);
    arglist.wrapInPromises(calling_env, this);

    Environment* execution_env = func->createExecutionEnv(arglist);
    matchArgsIntoEnvironment(func, calling_env, arglist, execution_env);

    // If this is a method call, merge in supplementary bindings and modify
    // calling_env.
    if (method_bindings) {
      importMethodBindings(method_bindings, execution_env->frame());
      calling_env = getMethodCallingEnv();
    }

    RObject* result;
    {
      // Evaluate the function.
      ClosureContext context(this, calling_env, func, execution_env);
      result = func->execute(execution_env);
    }

    Environment::monitorLeaks(result);
    execution_env->maybeDetachFrame();

    return result;
}

void Expression::importMethodBindings(const Frame* method_bindings,
                                      Frame* newframe)
{
  method_bindings->visitBindings([&](const Frame::Binding* binding) {
      const Symbol* sym = binding->symbol();
      if (!newframe->binding(sym)) {
        newframe->importBinding(binding);
      }
    });
}

Environment* Expression::getMethodCallingEnv() {
  FunctionContext* fctxt = FunctionContext::innermost();
  while (fctxt && fctxt->function()->sexptype() == SPECIALSXP)
      fctxt = FunctionContext::innermost(fctxt->nextOut());
  return (fctxt ? fctxt->callEnvironment() : Environment::global());
}

const char* Expression::typeName() const
{
    return staticTypeName();
}

CachingExpression* CachingExpression::clone() const
{
    return new CachingExpression(*this);
}

void CachingExpression::visitReferents(const_visitor* v) const
{
    Expression::visitReferents(v);
    if (m_cached_matching_info)
        (*v)(m_cached_matching_info);
}

void CachingExpression::detachReferents()
{
    m_cached_matching_info = nullptr;
    Expression::detachReferents();
}

void CachingExpression::matchArgsIntoEnvironment(const Closure* func,
                                          Environment* calling_env,
                                          const ArgList& arglist,
                                          Environment* execution_env) const
{
    const ArgMatcher* matcher = func->matcher();
    matcher->match(execution_env, arglist, &m_cached_matching_info);
}

// ***** C interface *****

SEXP Rf_currentExpression()
{
    return R_CurrentExpr;
}

SEXP Rf_lcons(SEXP cr, SEXP tl)
{
    GCStackRoot<> crr(cr);
    GCStackRoot<PairList> tlr(SEXP_downcast<PairList*>(tl));
    return new CachingExpression(crr, tlr);
}

void Rf_setCurrentExpression(SEXP e)
{
    R_CurrentExpr = e;
}
