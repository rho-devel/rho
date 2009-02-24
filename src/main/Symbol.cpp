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
    : RObject(SYMSXP), m_name(name)
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

Symbol* Symbol::obtain(const std::string& name)
{
    GCRoot<const CachedString> str(CachedString::obtain(name));
    return Symbol::obtain(str);
}

Symbol* Symbol::obtainDotDotSymbol(unsigned int n)
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

// Predefined Symbols:
namespace CXXR {
    const GCRoot<Symbol> Bracket2Symbol(Symbol::obtain("[["), true);
    const GCRoot<Symbol> BracketSymbol(Symbol::obtain("["), true);
    const GCRoot<Symbol> BraceSymbol(Symbol::obtain("{"), true);
    const GCRoot<Symbol> TmpvalSymbol(Symbol::obtain("*tmp*"), true);
    const GCRoot<Symbol> ClassSymbol(Symbol::obtain("class"), true);
    const GCRoot<Symbol> DimNamesSymbol(Symbol::obtain("dimnames"), true);
    const GCRoot<Symbol> DimSymbol(Symbol::obtain("dim"), true);
    const GCRoot<Symbol> DollarSymbol(Symbol::obtain("$"), true);
    const GCRoot<Symbol> DotsSymbol(Symbol::obtain("..."), true);
    const GCRoot<Symbol> DropSymbol(Symbol::obtain("drop"), true);
    const GCRoot<Symbol> ExactSymbol(Symbol::obtain("exact"), true);
    const GCRoot<Symbol> LevelsSymbol(Symbol::obtain("levels"), true);
    const GCRoot<Symbol> ModeSymbol(Symbol::obtain("mode"), true);
    const GCRoot<Symbol> NamesSymbol(Symbol::obtain("names"), true);
    const GCRoot<Symbol> NaRmSymbol(Symbol::obtain("na.rm"), true);
    const GCRoot<Symbol> RowNamesSymbol(Symbol::obtain("row.names"), true);
    const GCRoot<Symbol> SeedsSymbol(Symbol::obtain(".Random.seed"), true);
    const GCRoot<Symbol> LastvalueSymbol(Symbol::obtain(".Last.value"), true);
    const GCRoot<Symbol> TspSymbol(Symbol::obtain("tsp"), true);
    const GCRoot<Symbol> CommentSymbol(Symbol::obtain("comment"), true);
    const GCRoot<Symbol> SourceSymbol(Symbol::obtain("source"), true);
    const GCRoot<Symbol> DotEnvSymbol(Symbol::obtain(".Environment"), true);
    const GCRoot<Symbol> RecursiveSymbol(Symbol::obtain("recursive"), true);
    const GCRoot<Symbol> UseNamesSymbol(Symbol::obtain("use.names"), true);
    const GCRoot<Symbol> SrcfileSymbol(Symbol::obtain("srcfile"), true);
    const GCRoot<Symbol> SrcrefSymbol(Symbol::obtain("srcref"), true);
}

// ***** C interface *****

SEXP R_Bracket2Symbol = CXXR::Bracket2Symbol;
SEXP R_BracketSymbol = CXXR::BracketSymbol;
SEXP R_BraceSymbol = CXXR::BraceSymbol;
SEXP R_ClassSymbol = CXXR::ClassSymbol;
SEXP R_DimNamesSymbol = CXXR::DimNamesSymbol;
SEXP R_DimSymbol = CXXR::DimSymbol;
SEXP R_DollarSymbol = CXXR::DollarSymbol;
SEXP R_DotsSymbol = CXXR::DotsSymbol;
SEXP R_DropSymbol = CXXR::DropSymbol;
SEXP R_LevelsSymbol = CXXR::LevelsSymbol;
SEXP R_ModeSymbol = CXXR::ModeSymbol;
SEXP R_NamesSymbol = CXXR::NamesSymbol;
SEXP R_RowNamesSymbol = CXXR::RowNamesSymbol;
SEXP R_SeedsSymbol = CXXR::SeedsSymbol;
SEXP R_TspSymbol = CXXR::TspSymbol;

SEXP R_CommentSymbol = CXXR::CommentSymbol;
SEXP R_DotEnvSymbol = CXXR::DotEnvSymbol;
SEXP R_ExactSymbol = CXXR::ExactSymbol;
SEXP R_LastvalueSymbol = CXXR::LastvalueSymbol;
SEXP R_NaRmSymbol = CXXR::NaRmSymbol;
SEXP R_RecursiveSymbol = CXXR::RecursiveSymbol;
SEXP R_SourceSymbol = CXXR::SourceSymbol;
SEXP R_SrcfileSymbol = CXXR::SrcfileSymbol;
SEXP R_SrcrefSymbol = CXXR::SrcrefSymbol;
SEXP R_TmpvalSymbol = CXXR::TmpvalSymbol;
SEXP R_UseNamesSymbol = CXXR::UseNamesSymbol;

// Rf_install() is currently defined in main.cpp

