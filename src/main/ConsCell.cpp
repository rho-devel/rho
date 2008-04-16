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

/** @file ConsCell.cpp
 *
 * @brief Class CXXR::ConsCell and associated C interface.
 */

#include "CXXR/ConsCell.h"

#include <iostream>
#include "CXXR/ByteCode.hpp"
#include "CXXR/DottedArgs.hpp"
#include "CXXR/Expression.h"
#include "CXXR/PairList.h"
#include "CXXR/StringVector.h"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*TAGp)(SEXP e) = TAG;
	SEXP (*allocSExpp)(SEXPTYPE t) = Rf_allocSExp;
   }
}

ConsCell::ConsCell(SEXPTYPE st, size_t sz) throw (bad_alloc, out_of_range)
    : RObject(st)
{
    // checkST(st);
    if (sz == 0)
	throw out_of_range(_("Cannot construct PairList of zero length."));
    try {
	while (--sz)
	    m_tail = new PairList(0, m_tail, 0);
    } catch (...) {
	if (m_tail) m_tail->expose();
	throw;
    }
}

void ConsCell::checkST(SEXPTYPE st)
{
    switch (st) {
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case BCODESXP:
	break;
    default:
	throw invalid_argument("Inappropriate SEXPTYPE for ConsCell.");
    }
}

void ConsCell::visitChildren(const_visitor* v) const
{
    const ConsCell* p = this;
    do {
	p->RObject::visitChildren(v);
	if (p->m_car) p->m_car->conductVisitor(v);
	if (p->m_tag) p->m_tag->conductVisitor(v);
	p = p->m_tail;
    } while (p && (*v)(p));
}

namespace {
    void indent(ostream& os, size_t margin)
    {
	while (margin--) os << ' ';
    }

    const char* sympname(const RObject* sym) {
	const SpecialSymbol* symb = dynamic_cast<const SpecialSymbol*>(sym);
	if (!symb) return "(SYMSXP is not a Symbol or SpecialSymbol)";
	return symb->name().c_str();
    }
}

void CXXR::ccdump(ostream& os, const ConsCell& cc, size_t margin)
{
    indent(os, margin);
    os << Rf_type2char(cc.sexptype()) << '\n';
    for (const ConsCell* p = &cc; p; p = p->tail()) {
	// Print tag:
	indent(os, margin);
	os << "- ";
	const RObject* tag = p->tag();
	if (!tag) os << "(No tag):\n";
	else if (tag->sexptype() != SYMSXP) os << "(Tag not a SYMSXP):\n";
	else os << sympname(tag) << ":\n";
	// Print car:
	const RObject* car = p->car();
	if (const ConsCell* ccinner = dynamic_cast<const ConsCell*>(car))
	    ccdump(os, *ccinner, margin + 2);
	else if (const StringVector* sv
		 = dynamic_cast<const StringVector*>(car))
	    strdump(os, *sv, margin + 2);
	else {
	    indent(os, margin + 2);
	    if (!car) os << "NILSXP\n";
	    else {
		SEXPTYPE st = car->sexptype();
		os << Rf_type2char(st);
		if (st == SYMSXP)
		    os << ": " << sympname(car);
		os << '\n';
	    }
	}
    }
}

// ***** C interface functions *****

void SET_TAG(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    ConsCell& cc = *SEXP_downcast<ConsCell*>(x);
    cc.setTag(y);
}

SEXP SETCAR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    ConsCell& cc = *SEXP_downcast<ConsCell*>(x);
    cc.setCar(y);
    return y;
}

SEXP Rf_allocSExp(SEXPTYPE t)
{
    switch (t) {
    case LISTSXP:
	return new PairList;
    case LANGSXP:
	return new Expression;
    case DOTSXP:
	return new DottedArgs;
    case BCODESXP:
	return new ByteCode;
    default:
	throw invalid_argument("Inappropriate SEXPTYPE for ConsCell.");
    }
}
