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
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file StringVector.cpp
 *
 * Implementation of class StringVector and related functions.
 */

#include "rho/StringVector.hpp"
#include "Defn.h"

#include <iostream>

using namespace rho;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace rho {
    namespace ForceNonInline {
	Rboolean (*isStringp)(SEXP s) = Rf_isString;
	SEXP (*STRING_ELTp)(const SEXP x, R_xlen_t i) = STRING_ELT;
    }

    RHandle<String> ElementTraits::NAFunc<RHandle<String>>::makeNA()
    {
	RHandle<String> na;
	na = String::NA();
	return na;
    }

    template<>
    const char* StringVector::staticTypeName() {
	return "character";
    }
}

namespace {
    void indent(std::ostream& os, std::size_t margin)
    {
	while (margin--)
	    os << ' ';
    }
}

void rho::strdump(std::ostream& os, const StringVector& sv, std::size_t margin)
{
    indent(os, margin);
    os << "character:\n";
    for (unsigned int i = 0; i < sv.size(); ++i) {
	indent(os, margin + 2);
	os << sv[i]->c_str() << '\n';
    }
}

// ***** C interface *****

void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_STRING_ELT", "character vector", Rf_type2char(TYPEOF(x)));
    StringVector* sv = SEXP_downcast<StringVector*>(x, false);

    if(TYPEOF(v) != CHARSXP)
       error("Value of SET_STRING_ELT() must be a 'CHARSXP' not a '%s'",
	     Rf_type2char(TYPEOF(v)));
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %lu/%lu in SET_STRING_ELT"),
	      i, XLENGTH(x));
    String* s = SEXP_downcast<String*>(v, false);
    (*sv)[i] = s;
}

extern "C"
SEXP* attribute_hidden StringVectorDataPtr(SEXP string_vector) {
    return reinterpret_cast<SEXP*>(
	&(*(SEXP_downcast<StringVector*>(string_vector)->begin())));
}
