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
#include "CXXR/GCStackRoot.h"
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

Symbol::map* Symbol::s_table = 0;

GCRoot<Symbol>* Symbol::s_missing_arg;
SEXP R_MissingArg;

GCRoot<Symbol>* Symbol::s_restart_token;
SEXP R_RestartToken;

GCRoot<Symbol>* Symbol::s_unbound_value;
SEXP R_UnboundValue;

// As of gcc 4.3.2, gcc's std::tr1::regex didn't appear to be working.
// (Discovered 2009-01-16)  So we use boost:

namespace {
    boost::basic_regex<char> dd_regex("\\.\\.\\d+");
}

// ***** Class Symbol itself *****

Symbol::Symbol(const CachedString* the_name, bool frozen)
    : RObject(SYMSXP), m_name(the_name)
{
    // boost::regex_match (libboost_regex1_36_0-1.36.0-9.5) doesn't
    // seem comfortable with empty strings, hence the size check.
    m_dd_symbol
	= name()->size() > 2 && boost::regex_match(name()->c_str(), dd_regex);
    if (frozen) freeze();
}

// Because Symbols are permanently preserved against garbage
// collection (see class description) this is never actually invoked.
Symbol::~Symbol()
{
    if (m_name) s_table->erase(m_name);
}

void Symbol::cleanup()
{
    delete s_unbound_value;
    delete s_restart_token;
    delete s_missing_arg;
    delete s_table;
}

void Symbol::initialize()
{
    s_table = new map;
    s_missing_arg = new GCRoot<Symbol>(expose(new Symbol));
    R_MissingArg = Symbol::missingArgument();
    s_restart_token = new GCRoot<Symbol>(expose(new Symbol));
    R_RestartToken = Symbol::restartToken();
    s_unbound_value = new GCRoot<Symbol>(expose(new Symbol));
    R_UnboundValue = Symbol::unboundValue();
}

Symbol* Symbol::obtain(const CachedString* name)
{
    GCStackRoot<const CachedString> namert(name);
    pair<map::iterator, bool> pr
	= s_table->insert(map::value_type(name, GCRoot<Symbol>(0)));
    map::iterator it = pr.first;
    map::value_type& val = *it;
    if (pr.second) {
	try {
	    val.second = expose(new Symbol(name, false));
	} catch (...) {
	    s_table->erase(it);
	    throw;
	}
    }
    return val.second;
}

Symbol* Symbol::obtain(const std::string& name)
{
    GCStackRoot<const CachedString> str(CachedString::obtain(name));
    return Symbol::obtain(str);
}

Symbol* Symbol::obtainDotDotSymbol(unsigned int n)
{
    if (n == 0)
	Rf_error(_("..0 is not a permitted symbol name"));
    ostringstream nameos;
    nameos << ".." << n;
    GCStackRoot<const CachedString> name(CachedString::obtain(nameos.str()));
    return obtain(name);
}

const char* Symbol::typeName() const
{
    return staticTypeName();
}

void Symbol::visitReferents(const_visitor* v) const
{
    const GCNode* name = m_name;
    RObject::visitReferents(v);
    if (name) name->conductVisitor(v);
}

// Predefined Symbols:
namespace CXXR {
    Symbol* const Bracket2Symbol = Symbol::obtain("[[");
    Symbol* const BracketSymbol = Symbol::obtain("[");
    Symbol* const BraceSymbol = Symbol::obtain("{");
    Symbol* const TmpvalSymbol = Symbol::obtain("*tmp*");
    Symbol* const ClassSymbol = Symbol::obtain("class");
    Symbol* const DimNamesSymbol = Symbol::obtain("dimnames");
    Symbol* const DimSymbol = Symbol::obtain("dim");
    Symbol* const DollarSymbol = Symbol::obtain("$");
    Symbol* const DotsSymbol = Symbol::obtain("...");
    Symbol* const DropSymbol = Symbol::obtain("drop");
    Symbol* const ExactSymbol = Symbol::obtain("exact");
    Symbol* const LevelsSymbol = Symbol::obtain("levels");
    Symbol* const ModeSymbol = Symbol::obtain("mode");
    Symbol* const NamesSymbol = Symbol::obtain("names");
    Symbol* const NaRmSymbol = Symbol::obtain("na.rm");
    Symbol* const RowNamesSymbol = Symbol::obtain("row.names");
    Symbol* const SeedsSymbol = Symbol::obtain(".Random.seed");
    Symbol* const LastvalueSymbol = Symbol::obtain(".Last.value");
    Symbol* const TspSymbol = Symbol::obtain("tsp");
    Symbol* const CommentSymbol = Symbol::obtain("comment");
    Symbol* const SourceSymbol = Symbol::obtain("source");
    Symbol* const DotEnvSymbol = Symbol::obtain(".Environment");
    Symbol* const RecursiveSymbol = Symbol::obtain("recursive");
    Symbol* const UseNamesSymbol = Symbol::obtain("use.names");
    Symbol* const SrcfileSymbol = Symbol::obtain("srcfile");
    Symbol* const SrcrefSymbol = Symbol::obtain("srcref");
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

