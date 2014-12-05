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
#include "CXXR/Environment.h"
#include "CXXR/Evaluator.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/String.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	Rboolean (*DDVALp)(SEXP x) = DDVAL;
	SEXP (*Rf_installp)(const char *name) = Rf_install;
	Rboolean (*isSymbolp)(SEXP s) = Rf_isSymbol;
	SEXP (*PRINTNAMEp)(SEXP x) = PRINTNAME;
    }
}

Symbol::Table* Symbol::s_table = nullptr;

Symbol* Symbol::s_missing_arg;
SEXP R_MissingArg;

Symbol* Symbol::s_unbound_value;
SEXP R_UnboundValue;

// As of gcc 4.3.2, gcc's std::tr1::regex didn't appear to be working.
// (Discovered 2009-01-16)  So we use boost:

namespace {
    boost::basic_regex<char>* dd_regex;  // "\\.\\.(\\d+)"
}

// ***** Class Symbol itself *****

Symbol::Symbol(const String* the_name)
    : RObject(SYMSXP), m_name(the_name), m_dd_index(0)
{
    if (m_name) {
	if (m_name->size() == 0)
	    Rf_error(_("attempt to use zero-length variable name"));
	if (m_name->size() > maxLength())
	    Rf_error(_("variable names are limited to %d bytes"), maxLength());
    }
    // If this is a ..n symbol, extract the value of n.
    // boost::regex_match (libboost_regex1_36_0-1.36.0-9.5) doesn't
    // seem comfortable with empty strings, hence the size check.
    if (m_name && m_name->size() > 2) {
	string name(m_name->c_str());
	boost::smatch dd_match;
	if (boost::regex_match(name, dd_match, *dd_regex)) {
	    istringstream iss(dd_match[1]);
	    iss >> m_dd_index;
	}
    }
}

void Symbol::cleanup()
{
    // Clearing s_table avoids valgrind 'possibly lost' reports on exit:
    s_table->clear();
}

void Symbol::detachReferents()
{
    m_name.detach();
    RObject::detachReferents();
}

RObject* Symbol::evaluate(Environment* env)
{
    if (this == DotsSymbol)
	Rf_error(_("'...' used in an incorrect context"));
    GCStackRoot<> val;
    if (isDotDotSymbol())
	val = Rf_ddfindVar(this, env);
    else {
	Frame::Binding* bdg = env->findBinding(this);
	if (bdg)
	    val = bdg->unforcedValue();
	else if (this == missingArgument())
	    val = this;  // This reproduces CR behaviour
	else val = unboundValue();
    }
    if (!val)
	return nullptr;
    if (val == unboundValue())
	Rf_error(_("object '%s' not found"), name()->c_str());
    if (val == missingArgument() && !isDotDotSymbol()) {
	if (name())
	    Rf_error(_("argument \"%s\" is missing, with no default"),
		     name()->c_str());
	else Rf_error(_("argument is missing, with no default"));
    }
    if (val->sexptype() == PROMSXP) {
	val = Rf_eval(val, env);
	SET_NAMED(val, 2);
    }
    else if (NAMED(val) < 1)
	SET_NAMED(val, 1);
    return val;
}

void Symbol::initialize()
{
    static Table table;
    s_table = &table;
    static GCRoot<Symbol> missing_arg(expose(new Symbol));
    s_missing_arg = missing_arg.get();
    R_MissingArg = s_missing_arg;
    static GCRoot<Symbol> unbound_value(expose(new Symbol));
    s_unbound_value = unbound_value.get();
    R_UnboundValue = s_unbound_value;
    static boost::basic_regex<char> dd_rx("\\.\\.(\\d+)");
    dd_regex = &dd_rx;
}

Symbol* Symbol::make(const String* name)
{
    Symbol* ans = CXXR_NEW(Symbol(name));
    s_table->push_back(GCRoot<Symbol>(ans));
    name->m_symbol = ans;
    return ans;
}

Symbol* Symbol::obtain(const std::string& name)
{
    GCStackRoot<const String> str(String::obtain(name));
    return Symbol::obtain(str);
}

Symbol* Symbol::obtainDotDotSymbol(unsigned int n)
{
    if (n == 0)
	Rf_error(_("..0 is not a permitted symbol name"));
    ostringstream nameos;
    nameos << ".." << n;
    GCStackRoot<const String> name(String::obtain(nameos.str()));
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
    if (name)
	(*v)(name);
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::Symbol)

// Predefined Symbols:
namespace CXXR {
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    Symbol* const CXXR_NAME = Symbol::obtain(R_NAME);
#include "CXXR/PredefinedSymbols.h"
#undef PREDEFINED_SYMBOL
}

// ***** C interface *****

#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    SEXP C_NAME = CXXR::CXXR_NAME;
#include "CXXR/PredefinedSymbols.h"
#undef PREDEFINED_SYMBOL

// Rf_install() is currently defined in main.cpp

