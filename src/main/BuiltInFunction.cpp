/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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
#include "CXXR/DotInternal.h"
#include "CXXR/FunctionContext.hpp"
#include "CXXR/PlainContext.hpp"
#include "CXXR/ProtectStack.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/RAllocStack.h"
#include "CXXR/Symbol.h"
#include "CXXR/errors.h"
#include "R_ext/Print.h"

using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	const char* (*PRIMNAMEp)(SEXP x) = PRIMNAME;
	int (*PRIMOFFSETp)(SEXP x) = PRIMOFFSET;
	unsigned int (*PRIMVALp)(SEXP x) = PRIMVAL;
    }
}

BuiltInFunction::TableEntry* BuiltInFunction::s_function_table = 0;
BuiltInFunction::map* BuiltInFunction::s_cache = 0;

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
      m_offset(offset), m_function(s_function_table[offset].cfun)
{
    unsigned int pmdigit = (s_function_table[offset].flags/100)%10;
    m_result_printing_mode = ResultPrintingMode(pmdigit);
    m_transparent = (viaDotInternal()
		     || m_function == do_begin
		     || m_function == do_break
		     || m_function == do_for
		     || m_function == do_if
		     || m_function == do_internal
		     || m_function == do_paren
		     || m_function == do_repeat
		     || m_function == do_return
		     || m_function == do_while);
}

BuiltInFunction::~BuiltInFunction()
{
    if (m_offset < 0)
	return;
    // During program exit, s_cache may already have been deleted:
    if (s_cache)
	s_cache->erase(name());
}

RObject* BuiltInFunction::apply(ArgList* arglist, Environment* env,
				const Expression* call) const
{
    Evaluator::maybeCheckForUserInterrupts();
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

    Evaluator::enableResultPrinting(m_result_printing_mode != FORCE_OFF);
    GCStackRoot<> ans;
    if (m_transparent) {
	PlainContext cntxt;
	if (arglist->status() != ArgList::EVALUATED && sexptype() == BUILTINSXP)
	    arglist->evaluate(env);
	ans = invoke(env, arglist, call);
    } else {
	FunctionContext cntxt(const_cast<Expression*>(call), env, this);
	if (arglist->status() != ArgList::EVALUATED && sexptype() == BUILTINSXP)
	    arglist->evaluate(env);
	ans = invoke(env, arglist, call);
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

void BuiltInFunction::checkNumArgs(const PairList* args,
				   const Expression* call) const
{
    if (arity() >= 0) {
	size_t nargs = listLength(args);
	if (int(nargs) != arity()) {
	    if (viaDotInternal())
		Rf_error(_("%d arguments passed to .Internal(%s)"
			   " which requires %d"), nargs, name(), arity());
	    else
		Rf_errorcall(const_cast<Expression*>(call),
			     _("%d arguments passed to '%s' which requires %d"),
			     nargs, name(), arity());
	}
    }
}

void BuiltInFunction::cleanup()
{
    // Clearing s_cache avoids valgrind 'possibly lost' reports on exit:
    s_cache->clear();
    s_cache = 0;
}

int BuiltInFunction::indexInTable(const char* name)
{
    for (int i = 0; s_function_table[i].name; ++i)
	if (strcmp(name, s_function_table[i].name) == 0)
	    return i;
    return -1;
}

// BuiltInFunction::initialize() is in names.cpp

BuiltInFunction* BuiltInFunction::obtain(const std::string& name)
{
    auto location = s_cache->find(name);
    if (location == s_cache->end()) {
	Rf_warning(_("%s is not the name of a built-in or special function"),
		   name.c_str());
	return 0;
    }
    return location->second;
}

const char* BuiltInFunction::typeName() const
{
    return sexptype() == SPECIALSXP ? "special" : "builtin";
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::BuiltInFunction)
