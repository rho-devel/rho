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

/** @file BuiltInFunction.cpp
 *
 * Implementation of class rho::BuiltInFunction and associated
 * C interface.
 */

#include "rho/BuiltInFunction.hpp"

#include <cstdarg>
#include "Internal.h"
#include "rho/ArgList.hpp"
#include "rho/FunctionContext.hpp"
#include "rho/PlainContext.hpp"
#include "rho/ProtectStack.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/RAllocStack.hpp"
#include "rho/Symbol.hpp"
#include "rho/errors.hpp"
#include "R_ext/Print.h"
#include "Defn.h"

using namespace rho;

namespace rho {
    namespace ForceNonInline {
	const char* (*PRIMNAMEp)(SEXP x) = PRIMNAME;
	int (*PRIMOFFSETp)(SEXP x) = PRIMOFFSET;
	unsigned int (*PRIMVALp)(SEXP x) = PRIMVAL;
    }
}

// BuiltInFunction::getFunctionTable() is in names.cpp

unsigned int BuiltInFunction::s_next_offset = 0;

// BuiltInFunction::apply() creates a FunctionContext only if
// m_transparent is false.  This affects the location at which
// Rf_error() reports an error as having occurred, and also determines
// whether a function is reported within traceback().
//
// Since functions called via .Internal are not visible to the R user,
// it seems clear that such functions should be 'transparent'.  One
// approach would be to leave it at that, so that errors within
// internal functions would be attributed to the surrounding call of
// .Internal.  However, rho (currently at least) goes further than
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

BuiltInFunction::BuiltInFunction(const char* name,
				 CCODE cfun,
				 unsigned int variant,
				 unsigned int flags,
				 int arity,
				 PPinfo ppinfo,
				 const char* first_arg_name,
				 DispatchType dispatch)
    : BuiltInFunction(name, variant, flags, arity, ppinfo,
		      first_arg_name, dispatch)
{
    m_function = cfun;
    m_quick_function = nullptr;
    m_fixed_arity_fn = nullptr;

    if (m_function == do_External
	|| m_function == do_Externalgr
	|| m_function == do_begin
	|| m_function == do_break
	|| m_function == do_dotcall
	|| m_function == do_for
	|| m_function == do_if
	|| m_function == do_internal
	|| m_function == do_repeat
	|| m_function == do_return
	|| m_function == do_while) {
	m_transparent = true;
    }
    if (m_function == do_set) {
	m_transparent = false;
    }
}

BuiltInFunction::BuiltInFunction(const char* name,
				 QuickInvokeFunction fun,
				 unsigned int variant,
				 unsigned int flags,
				 int arity,
				 PPinfo ppinfo,
				 const char* first_arg_name,
				 DispatchType dispatch)
    : BuiltInFunction(name, variant, flags, arity, ppinfo,
		      first_arg_name, dispatch)
{
    m_function = nullptr;
    m_quick_function = fun;
    m_fixed_arity_fn = nullptr;
}

BuiltInFunction::BuiltInFunction(const char* name,
				 FixedArityFnStorage cfun,
				 unsigned int variant,
				 unsigned int flags,
				 int arity,
				 PPinfo ppinfo,
				 const char* first_arg_name,
				 DispatchType dispatch)
    : BuiltInFunction(name, variant, flags, arity, ppinfo,
		      first_arg_name, dispatch)
{
    m_function = nullptr;
    m_quick_function = nullptr;
    m_fixed_arity_fn = cfun;

    if (m_fixed_arity_fn == reinterpret_cast<FixedArityFnStorage>(do_paren))
	m_transparent = true;
}

BuiltInFunction::BuiltInFunction(const char* name,
				 unsigned int variant,
				 unsigned int flags,
				 int arity,
				 PPinfo ppinfo,
				 const char* first_arg_name,
				 DispatchType dispatch)
    : FunctionBase(flags % 10 ? BUILTINSXP : SPECIALSXP),
      m_offset(s_next_offset++), m_name(name), m_variant(variant),
      m_via_dot_internal((flags%100)/10 == 1), m_arity(arity),
      m_first_arg_name(first_arg_name), m_dispatch_type(dispatch),
      m_gram(ppinfo)
{
    unsigned int pmdigit = (flags / 100)%10;
    m_result_printing_mode = ResultPrintingMode(pmdigit);
    m_transparent = (viaDotInternal()
		     || (m_name.length() > 2
		     	 && m_name.substr(m_name.length() - 2) == "<-"));
}

BuiltInFunction::~BuiltInFunction()
{
    assert(0 && "BuiltInFunction's destructor should never be called");
}

void BuiltInFunction::badArgumentCountError(int nargs, int arity,
					    const Expression* call) const
{
    if (viaDotInternal())
	Rf_error(
	    ngettext("%d argument passed to .Internal(%s) which requires %d",
		     "%d arguments passed to .Internal(%s) which requires %d",
		     nargs),
	    nargs, name(), arity);
    else
	Rf_errorcall(const_cast<Expression*>(call),
		      ngettext("%d argument passed to '%s' which requires %d",
			       "%d arguments passed to '%s' which requires %d",
			       nargs),
		     nargs, name(), arity);
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

BuiltInFunction* BuiltInFunction::obtainPrimitive(const Symbol* symbol)
{
    auto location = getPrimitiveFunctionLookupTable()->find(symbol);
    if (location == getPrimitiveFunctionLookupTable()->end()) {
	Rf_warning(_("%s is not the name of a built-in or special function"),
		   symbol->name()->c_str());
	return nullptr;
    }
    return location->second;
}

BuiltInFunction* BuiltInFunction::obtainPrimitive(const std::string& name) {
  return obtainPrimitive(Symbol::obtain(name));
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
BuiltInFunction::InternalDispatch(const Expression* call,
				  Environment* env,
				  ArgList* args) const
{
    assert(m_dispatch_type != DispatchType::NONE);
    assert(args->status() == ArgList::EVALUATED);

    size_t num_args = args->size();
    RObject** args_array = static_cast<RObject**>(
	alloca(num_args * sizeof(RObject*)));
    for (int i = 0; i < num_args; i++) {
	args_array[i] = args->get(i);
    }
    return InternalDispatch(call, env, num_args, args_array, args->list());
}

const char* BuiltInFunction::GetInternalGroupDispatchName() const
{
    switch(m_dispatch_type) {
    case DispatchType::GROUP_MATH:
	return "Math";
    case DispatchType::GROUP_OPS:
	return "Ops";
    case DispatchType::GROUP_COMPLEX:
	return "Complex";
    case DispatchType::GROUP_SUMMARY:
	return "Summary";
    default:
	// Ought to be unreachable.
	Rf_error("Attempting to do group dispatch without a group");
    }
};

std::pair<bool, RObject*>
BuiltInFunction::RealInternalGroupDispatch(
    const Expression* call, Environment* env,
    int num_args, RObject* const* evaluated_args, const PairList* tags) const
{
    PairList* pargs = PairList::make(num_args, evaluated_args);
    pargs->copyTagsFrom(tags);
    ArgList arglist(pargs, ArgList::EVALUATED);
    RObject* result = nullptr;
    bool dispatched = Rf_DispatchGroup(GetInternalGroupDispatchName(),
				       const_cast<Expression*>(call),
				       const_cast<BuiltInFunction*>(this),
				       const_cast<PairList*>(arglist.list()),
				       env, &result);
    return std::make_pair(dispatched, result);
}

std::pair<bool, RObject*>
BuiltInFunction::RealInternalDispatch(const Expression* call,
				      int num_args,
				      RObject* const* evaluated_args,
				      const PairList* tags,
				      Environment* env) const
{
    PairList* pargs = PairList::make(num_args, evaluated_args);
    pargs->copyTagsFrom(tags);
    ArgList arglist(pargs, ArgList::EVALUATED);
    RObject* result = nullptr;
    bool dispatched = Rf_DispatchOrEval(const_cast<Expression*>(call),
					const_cast<BuiltInFunction*>(this),
					const_cast<PairList*>(arglist.list()),
					env, &result, MissingArgHandling::Drop,
					1);
    return std::make_pair(dispatched, result);
}
