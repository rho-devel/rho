/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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

/** @file SpecialSymbol.cpp
 *
 * @brief Implementation of class CXXR:SpecialSymbol and associated C
 * interface.
 */

#include "CXXR/SpecialSymbol.h"

#include "CXXR/CachedString.h"

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	Rboolean (*isSymbolptr)(SEXP s) = Rf_isSymbol;
	SEXP (*PRINTNAMEp)(SEXP x) = PRINTNAME;
    }
}

GCRoot<const String> String::s_blank(CachedString::obtain(""));
SEXP R_BlankString = const_cast<String*>(String::blank());

GCRoot<SpecialSymbol> SpecialSymbol::s_missing_arg(new SpecialSymbol);
SEXP R_MissingArg = SpecialSymbol::missingArgument();

GCRoot<SpecialSymbol> SpecialSymbol::s_restart_token(new SpecialSymbol);
SEXP R_RestartToken = SpecialSymbol::restartToken();

GCRoot<SpecialSymbol> SpecialSymbol::s_unbound_value(new SpecialSymbol);
SEXP R_UnboundValue = SpecialSymbol::unboundValue();

const char* SpecialSymbol::typeName() const
{
    return staticTypeName();
}

void SpecialSymbol::visitChildren(const_visitor* v) const
{
    RObject::visitChildren(v);
    m_name.conductVisitor(v);
}
