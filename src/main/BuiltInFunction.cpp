/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
 *  http://www.r-project.org/Licenses/
 */

/** @file BuiltInFunction.cpp
 *
 * Implementation of class CXXR::BuiltInFunction and associated
 * C interface.
 */

#include "CXXR/BuiltInFunction.h"

#include <cstdarg>
#include "Internal.h"
#include "CXXR/ArgList.hpp"
#include "CXXR/FunctionContext.hpp"
#include "CXXR/PlainContext.hpp"
#include "CXXR/ProtectStack.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/RAllocStack.h"
#include "CXXR/Symbol.h"
#include "CXXR/errors.h"
#include "R_ext/Print.h"
#include "Defn.h"

using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	const char* (*PRIMNAMEp)(SEXP x) = PRIMNAME;
	int (*PRIMOFFSETp)(SEXP x) = PRIMOFFSET;
	unsigned int (*PRIMVALp)(SEXP x) = PRIMVAL;
    }
}

// s_function_table is in names.cpp

// BuiltInFunction::apply() creates a FunctionContext only if
// m_transparent is false.  This affects the location at which
// Rf_error() reports an error as having occurred, and also determines
// whether a function is reported within traceback().
//
// Since functions called via .Internal are not visible to the R user,
// it seems clear that such functions should be 'transparent'.  One
// approach would be to leave it at that, so that errors within
// internal functions would be attributed to the surrounding call of
// .Internal.  However, CXXR (currently at least) goes further than
// this, with a view to attributing an error arising within an
// internal function to the documented R function which it implements.
// To this end do_internal itself and various 'syntactical' functions
// such as do_begin are also flagged as transparent.
//
// The flip side of this is that if an error really does occur within
// one of the 'syntactical' functions (rather than within some inner
// but transparent scope), it may be desirable to report the error
// using Rf_errorcall() rather than Rf_error(), so that it can
// specifically be attributed to the 'syntactical' function.

BuiltInFunction::BuiltInFunction(unsigned int offset)
    : FunctionBase(s_function_table[offset].flags%10
		   ? BUILTINSXP : SPECIALSXP),
      m_offset(offset), m_function(s_function_table[offset].cfun),
      m_quick_function(s_function_table[offset].quick_function)
{
    unsigned int pmdigit = (s_function_table[offset].flags/100)%10;
    m_result_printing_mode = ResultPrintingMode(pmdigit);
    m_transparent = (viaDotInternal()
		     || m_function == do_begin
		     || m_function == do_break
		     || m_function == do_for
		     || m_function == do_if
		     || m_function == do_internal
		     || m_quick_function == do_paren
		     || m_function == do_repeat
		     || m_function == do_return
		     || m_function == do_while);
}

BuiltInFunction::~BuiltInFunction()
{
    assert(0 && "BuiltInFunction's destructor should never be called");
}

RObject* BuiltInFunction::apply(ArgList* arglist, Environment* env,
				const Expression* call) const
{
    RAllocStack::Scope ras_scope;
    ProtectStack::Scope ps_scope;
#ifndef NDEBUG
    size_t pps_size = ProtectStack::size();
#endif

#ifdef Win32
    // This is an inlined version of Rwin_fpreset (src/gnuwin/extra.c)
    // and resets the precision, rounding and exception modes of a
    // ix86 fpu.
    // It gets called prior to every builtin function, just in case a badly
    // behaved DLL has changed the fpu control word.
    __asm__ ( "fninit" );
#endif

    GCStackRoot<> ans;
    if (m_transparent) {
	PlainContext cntxt;
	ans = evaluateAndInvoke(env, arglist, call);
    } else {
	FunctionContext cntxt(const_cast<Expression*>(call), env, this);
	ans = evaluateAndInvoke(env, arglist, call);
    }
    if (m_result_printing_mode != SOFT_ON)
	Evaluator::enableResultPrinting(m_result_printing_mode != FORCE_OFF);
#ifndef NDEBUG
    if (pps_size != ProtectStack::size())
	REprintf("Warning: stack imbalance in '%s', %d then %d\n",
		 name(), pps_size, ProtectStack::size());
#endif
    return ans;
}

static bool hasDotArgs(const ArgList* arglist) {
    const PairList* list = arglist->list();
    if (!list)
	return false;
    for (const ConsCell& cell : *list) {
	if (cell.car() == R_DotsSymbol)
	    return true;
    }
    return false;
}

bool BuiltInFunction::argsNeedEvaluating(const ArgList* arglist) const {
    return sexptype() == BUILTINSXP && arglist->status() != ArgList::EVALUATED;
}

RObject* BuiltInFunction::evaluateAndInvoke(Environment* env, ArgList* arglist,
					    const Expression* call) const
{
    if (m_quick_function) {
	return quickEvaluateAndInvoke(env, arglist, call);
    }

    if (argsNeedEvaluating(arglist)) {
	arglist->evaluate(env);
    }

    Evaluator::enableResultPrinting(true);
    return m_function(const_cast<Expression*>(call),
		      const_cast<BuiltInFunction*>(this),
		      const_cast<PairList*>(arglist->list()),
		      env);
}

static void copyArgsToArray(const PairList* args, RObject** array) {
    int i = 0;
    for (const ConsCell& cell : *args) {
	array[i] = cell.car();
	i++;
    }
}

RObject* BuiltInFunction::quickEvaluateAndInvoke(
    Environment* env, ArgList* arglist, const Expression* call) const
{
    bool args_need_evaluating = argsNeedEvaluating(arglist);
    if (args_need_evaluating && hasDotArgs(arglist)) {
	// This is slow, but handles '...' correctly.
	arglist->evaluate(env);
	args_need_evaluating = false;
    }

    const PairList* args = arglist->list();
    if (!args) {
	Evaluator::enableResultPrinting(true);
	return m_quick_function(const_cast<Expression*>(call),
				this, env, nullptr, 0, nullptr);
    }

    int num_args = listLength(args);
    
    // Rather than creating a linked list of evaluated arguments, this
    // simply stores them in an on-stack array.
    RObject** evaluated_args = static_cast<RObject**>(
	alloca(num_args * sizeof(RObject*)));
    if (args_need_evaluating) {
	arglist->evaluateToArray(env, num_args, evaluated_args);
    } else {
	copyArgsToArray(args, evaluated_args);
    }

    // Since builtins don't do argument matching 'args' has the correct
    // tags.
    const PairList* tags = args;
    
    Evaluator::enableResultPrinting(true);
    return m_quick_function(const_cast<Expression*>(call),
			    this, env, evaluated_args, num_args, tags);
}

void BuiltInFunction::checkNumArgs(const PairList* args,
				   const Expression* call) const
{
    if (arity() >= 0) {
	checkNumArgs(listLength(args), call);
    }
}

void BuiltInFunction::badArgumentCountError(int nargs, const Expression* call)
    const
{
    if (viaDotInternal())
	Rf_error(_("%d arguments passed to .Internal(%s)"
		   " which requires %d"), nargs, name(), arity());
    else
	Rf_errorcall(const_cast<Expression*>(call),
		     _("%d arguments passed to '%s' which requires %d"),
		     nargs, name(), arity());
}

int BuiltInFunction::indexInTable(const char* name)
{
    for (int i = 0; s_function_table[i].name; ++i)
	if (strcmp(name, s_function_table[i].name) == 0)
	    return i;
    return -1;
}

// BuiltInFunction::createLookupTables() is in names.cpp

std::pair<BuiltInFunction::map*, BuiltInFunction::map*>
BuiltInFunction::getLookupTables()
{
    static std::pair<map*, map*> tables = createLookupTables();
    return tables;
}

BuiltInFunction::map* BuiltInFunction::getPrimitiveFunctionLookupTable()
{
    return getLookupTables().first;
}

BuiltInFunction::map* BuiltInFunction::getInternalFunctionLookupTable()
{
    return getLookupTables().second;
}


BuiltInFunction* BuiltInFunction::obtainPrimitive(const std::string& name)
{
    const Symbol* symbol = Symbol::obtain(name);
    auto location = getPrimitiveFunctionLookupTable()->find(symbol);
    if (location == getPrimitiveFunctionLookupTable()->end()) {
	Rf_warning(_("%s is not the name of a built-in or special function"),
		   name.c_str());
	return nullptr;
    }
    return location->second;
}

void BuiltInFunction::addPrimitivesToEnvironment(Environment* environment)
{
    for(const auto& entry: *getPrimitiveFunctionLookupTable()) {
	const Symbol* symbol = entry.first;
	BuiltInFunction* function = entry.second;
	environment->frame()->bind(symbol, function);
    }
}

BuiltInFunction* BuiltInFunction::obtainInternal(const Symbol* name)
{
    auto location = getInternalFunctionLookupTable()->find(name);
    if (location == getInternalFunctionLookupTable()->end()) {
	return nullptr;
    }
    return location->second;
}

const char* BuiltInFunction::typeName() const
{
    return sexptype() == SPECIALSXP ? "special" : "builtin";
}

std::pair<bool, RObject*>
BuiltInFunction::InternalGroupDispatch(const char* group, const Expression* call,
				       Environment* env,
				       int num_args, RObject** args,
				       const PairList* tags) const
{
    bool has_class = false;
    for (int i = 0; i < num_args; i++) {
	if (args[i] && args[i]->hasClass()) {
	    has_class = true;
	    break;
	}
    }
    if (has_class) {
	ArgList arglist(PairList::make(num_args, args, tags), ArgList::EVALUATED);
	RObject* result = nullptr;
	bool dispatched = Rf_DispatchGroup(group,
					   const_cast<Expression*>(call),
					   const_cast<BuiltInFunction*>(this),
					   const_cast<PairList*>(arglist.list()),
					   env, &result);
	return std::make_pair(dispatched, result);
    } else {
	return std::make_pair(false, nullptr);
    }
}

BuiltInFunction::TableEntry::TableEntry(const char* name_,
					CCODE cfun_,
					unsigned int variant_,
					unsigned int flags_,
					int arity_,
					PPinfo gram_)
    : name(name_), cfun(cfun_), quick_function(nullptr),
      variant(variant_), flags(flags_), arity(arity_), gram(gram_)
{}

BuiltInFunction::TableEntry::TableEntry(const char* name_,
					QuickInvokeFunction qfun_,
					unsigned int variant_,
					unsigned int flags_,
					int arity_,
					PPinfo gram_)
    : name(name_), cfun(nullptr), quick_function(qfun_),
      variant(variant_), flags(flags_), arity(arity_), gram(gram_)
{}


BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::BuiltInFunction)
