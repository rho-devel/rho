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

// BuiltInFunction::getFunctionTable() is in names.cpp

unsigned int BuiltInFunction::TableEntry::s_next_offset = 0;

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

BuiltInFunction::BuiltInFunction(const char* name,
				 CCODE cfun,
				 unsigned int variant,
				 unsigned int flags,
				 int arity,
				 PPinfo ppinfo,
				 unsigned int offset)
    : BuiltInFunction(name, variant, flags, arity, ppinfo, offset)
{
    m_function = cfun;
    m_quick_function = nullptr;

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
				 unsigned int offset)
    : BuiltInFunction(name, variant, flags, arity, ppinfo, offset)
{
    m_function = nullptr;
    m_quick_function = fun;

    if (m_quick_function == do_paren)
	m_transparent = true;
}

BuiltInFunction::BuiltInFunction(const char* name,
				 unsigned int variant,
				 unsigned int flags,
				 int arity,
				 PPinfo ppinfo,
				 unsigned int offset)
    : FunctionBase(flags % 10 ? BUILTINSXP : SPECIALSXP),
      m_offset(offset), m_name(name), m_variant(variant),
      m_via_dot_internal((flags%100)/10 == 1), m_arity(arity), m_gram(ppinfo)
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

void BuiltInFunction::badArgumentCountError(int nargs, const Expression* call)
    const
{
    if (viaDotInternal())
	Rf_error(
	    ngettext("%d argument passed to .Internal(%s) which requires %d",
		     "%d arguments passed to .Internal(%s) which requires %d",
		     nargs),
	    nargs, name(), arity());
    else
	Rf_errorcall(const_cast<Expression*>(call),
		      ngettext("%d argument passed to '%s' which requires %d",
			       "%d arguments passed to '%s' which requires %d",
			       nargs),
		     nargs, name(), arity());
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
BuiltInFunction::RealInternalGroupDispatch(
    const char* group, const Expression* call, Environment* env,
    int num_args, RObject* const* evaluated_args, const PairList* tags) const
{
    PairList* pargs = PairList::make(num_args, evaluated_args);
    pargs->copyTagsFrom(tags);
    ArgList arglist(pargs, ArgList::EVALUATED);
    RObject* result = nullptr;
    bool dispatched = Rf_DispatchGroup(group,
				       const_cast<Expression*>(call),
				       const_cast<BuiltInFunction*>(this),
				       const_cast<PairList*>(arglist.list()),
				       env, &result);
    return std::make_pair(dispatched, result);
}

std::pair<bool, RObject*>
BuiltInFunction::RealInternalDispatch(const Expression* call, const char* generic,
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
					generic,
					const_cast<PairList*>(arglist.list()),
					env, &result, MissingArgHandling::Drop,
					1);
    return std::make_pair(dispatched, result);
}
