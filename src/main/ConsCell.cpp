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

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	RObject* (*car0p)(ConsCell*) = CXXR::car0;
	PairList* (*tail0p)(ConsCell*) = CXXR::tail0;
	SEXP (*CAARp)(SEXP e) = CAAR;
	SEXP (*CARp)(SEXP e) = CAR;
	SEXP (*TAGp)(SEXP e) = TAG;
	SEXP (*allocSExpp)(SEXPTYPE t) = Rf_allocSExp;
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
	throw std::invalid_argument("Inappropriate SEXPTYPE for ConsCell.");
    }
}

void ConsCell::detachReferents()
{
    m_car.detach();
    m_tag.detach();
    m_tail.detach();
    RObject::detachReferents();
}

void ConsCell::visitReferents(const_visitor* v) const
{
    const GCNode* car = m_car;
    const GCNode* tag = m_tag;
    const GCNode* tail = m_tail;
    RObject::visitReferents(v);
    if (tag)
	(*v)(tag);
    if (car)
	(*v)(car);
    if (tail)
	(*v)(tail);
}

namespace {
    void indent(std::ostream& os, std::size_t margin)
    {
	while (margin--)
	    os << ' ';
    }

    const char* sympname(const RObject* sym) {
	const Symbol* symb = dynamic_cast<const Symbol*>(sym);
	if (!symb)
	    return "(SYMSXP is not a Symbol)";
	return symb->name()->c_str();
    }
}

void CXXR::ccdump(std::ostream& os, const ConsCell& cc, std::size_t margin)
{
    indent(os, margin);
    os << Rf_type2char(cc.sexptype()) << '\n';
    for (const ConsCell* p = &cc; p; p = p->tail()) {
	// Print tag:
	indent(os, margin);
	os << "- ";
	const RObject* tag = p->tag();
	if (!tag)
	    os << "(No tag):\n";
	else if (tag->sexptype() != SYMSXP)
	    os << "(Tag not a SYMSXP):\n";
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
	    if (!car)
		os << "NILSXP\n";
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
    if (!x)
	Rf_error(_("bad value"));
    ConsCell& cc = *SEXP_downcast<ConsCell*>(x);
    cc.setTag(y);
}

SEXP SETCAR(SEXP x, SEXP y)
{
    if (!x)
	Rf_error(_("bad value"));
    ConsCell& cc = *SEXP_downcast<ConsCell*>(x);
    cc.setCar(y);
    return y;
}

SEXP Rf_allocSExp(SEXPTYPE t)
{
    SEXP ans;
    switch (t) {
    case LISTSXP:
	ans = new PairList;
	break;
    case LANGSXP:
	ans = new Expression;
	break;
    case DOTSXP:
	ans = new DottedArgs;
	break;
    default:
	throw std::invalid_argument("Inappropriate SEXPTYPE for ConsCell.");
    }
    return ans;
}
