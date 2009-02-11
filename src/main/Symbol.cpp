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

#include <sstream>
#include "localization.h"
#include "boost/regex.hpp"
#include "R_ext/Error.h"
#include "CXXR/CachedString.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	Rboolean (*DDVALp)(SEXP x) = DDVAL;
	SEXP (*INTERNALp)(SEXP x) = INTERNAL;
	Rboolean (*isSymbolptr)(SEXP s) = Rf_isSymbol;
	SEXP (*PRINTNAMEp)(SEXP x) = PRINTNAME;
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

Symbol::map Symbol::s_table;

// As of gcc 4.3.2, gcc's std::tr1::regex didn't appear to be working.
// (Discovered 2009-01-16)  So we use boost:

namespace {
    boost::basic_regex<char> dd_regex("\\.\\.\\d+");
}

Symbol::Symbol(const CachedString* name, bool frozen)
    : RObject(SYMSXP), m_name(name), m_internalfunc(0)
{
    // boost::regex_match (libboost_regex1_36_0-1.36.0-9.5) doesn't
    // seem comfortable with empty strings, hence the size check.
    m_dd_symbol
	= name->size() > 2 && boost::regex_match(name->c_str(), dd_regex);
    if (frozen) freeze();
}

Symbol* Symbol::obtain(const CachedString* name)
{
    pair<map::iterator, bool> pr = s_table.insert(map::value_type(name, 0));
    map::iterator it = pr.first;
    map::value_type& val = *it;
    if (pr.second) {
	try {
	    val.second = new Symbol(name, false);
	    val.second->expose();
	} catch (...) {
	    s_table.erase(it);
	    throw;
	}
    }
    return val.second;
}

Symbol* Symbol::obtainDDSymbol(unsigned int n)
{
    if (n == 0)
	Rf_error(_("..0 is not a permitted symbol name"));
    ostringstream nameos;
    nameos << ".." << n;
    GCRoot<const CachedString> name(CachedString::obtain(nameos.str()));
    return obtain(name);
}

const char* Symbol::typeName() const
{
    return staticTypeName();
}

void Symbol::visitChildren(const_visitor* v) const
{
    RObject::visitChildren(v);
    m_name->conductVisitor(v);
    if (m_internalfunc) m_internalfunc->conductVisitor(v);
}

void Symbol::visitTable(const_visitor* v)
{
    for (map::iterator it = s_table.begin(); it != s_table.end(); ++it) {
	// Beware that a garbage collection may occur in
	// Symbol::obtain(), after a new entry has been placed in the
	// symbol table but not yet made to point to a Symbol.  In
	// that case we need to visit the table key (i.e. the symbol
	// name); otherwise we don't bother, because it will be
	// reached via the Symbol anyway.
	const Symbol* symbol = (*it).second;
        if (symbol) symbol->conductVisitor(v);
	else (*it).first->conductVisitor(v);
    }
}

// ***** C interface *****

void SET_INTERNAL(SEXP x, SEXP v)
{
    Symbol& sym = *SEXP_downcast<Symbol*>(x);
    BuiltInFunction* fun = SEXP_downcast<BuiltInFunction*>(v);
    sym.setInternalFunction(fun);
}
