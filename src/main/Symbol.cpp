/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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

/** @file Symbol.cpp
 *
 * @brief Implementation of class CXXR::Symbol and associated C
 * interface.
 */

#include "CXXR/Symbol.h"

#include "boost/regex.hpp"
#include "CXXR/CachedString.h"

using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	Rboolean (*DDVALp)(SEXP x) = DDVAL;
	SEXP (*INTERNALp)(SEXP x) = INTERNAL;
	Rboolean (*isSymbolptr)(SEXP s) = Rf_isSymbol;
	SEXP (*PRINTNAMEp)(SEXP x) = PRINTNAME;
	SEXP (*SYMVALUEp)(SEXP x) = SYMVALUE;
    }
}

GCRoot<const CachedString> CachedString::s_blank(CachedString::obtain(""));
SEXP R_BlankString = const_cast<CachedString*>(CachedString::blank());

GCRoot<Symbol> Symbol::s_missing_arg(new Symbol, true);
SEXP R_MissingArg = Symbol::missingArgument();

GCRoot<Symbol> Symbol::s_restart_token(new Symbol, true);
SEXP R_RestartToken = Symbol::restartToken();

GCRoot<Symbol> Symbol::s_unbound_value(new Symbol, true);
SEXP R_UnboundValue = Symbol::unboundValue();

namespace {
    boost::basic_regex<char> dd_regex("\\.\\.\\d+");
}

Symbol::Symbol()
    : RObject(SYMSXP), m_name(*CachedString::blank()),
      m_value(s_unbound_value), m_internalfunc(0), m_dd_symbol(false)
{
    freeze();
}

Symbol::Symbol(const CachedString& name, RObject* val,
	       const BuiltInFunction* internal_func, bool frozen)
    : RObject(SYMSXP), m_name(name), m_value(val),
      m_internalfunc(internal_func)
{
    m_dd_symbol = boost::regex_match(name.c_str(), dd_regex);
    if (frozen) freeze();
}

const char* Symbol::typeName() const
{
    return staticTypeName();
}

void Symbol::visitChildren(const_visitor* v) const
{
    RObject::visitChildren(v);
    m_name.conductVisitor(v);
    if (m_value) m_value->conductVisitor(v);
    if (m_internalfunc) m_internalfunc->conductVisitor(v);
}

// ***** C interface *****

SEXP Rf_mkSYMSXP(SEXP name, SEXP value)
{
    GCRoot<const CachedString>
	namert(SEXP_downcast<const CachedString*>(name));
    GCRoot<> valuert(value);
    Symbol* ans = new Symbol(*namert, valuert);
    ans->expose();
    return ans;
}

void SET_INTERNAL(SEXP x, SEXP v)
{
    Symbol& sym = *SEXP_downcast<Symbol*>(x);
    BuiltInFunction* fun = SEXP_downcast<BuiltInFunction*>(v);
    sym.setInternalFunction(fun);
}
