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

/** @file PairList.cpp
 *
 * At present, this file simply forces the generation of non-inlined
 * versions of inlined functions declared in PairList.h where these
 * are intended to be callable from C.  It is also used to check that
 * PairList.h is self-contained, i.e. \#includes anything it needs,
 * and doesn't rely on anything having been previously \#included in
 * the enclosing source file.
 */

#include "CXXR/PairList.h"

#include <iostream>
#include "localization.h"
#include "CXXR/StringVector.h"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*CAARp)(SEXP e) = CAAR;
	SEXP (*CAD4Rp)(SEXP e) = CAD4R;
	SEXP (*CADDDRp)(SEXP e) = CADDDR;
	SEXP (*CADDRp)(SEXP e) = CADDR;
	SEXP (*CADRp)(SEXP e) = CADR;
	SEXP (*CARp)(SEXP e) = CAR;
	SEXP (*CDARp)(SEXP e) = CDAR;
	SEXP (*CDDRp)(SEXP e) = CDDR;
	SEXP (*CDRp)(SEXP e) = CDR;
	SEXP (*TAGp)(SEXP e) = TAG;
	SEXP (*allocListp)(unsigned int n) = Rf_allocList;
	SEXP (*allocSExpp)(SEXPTYPE t) = Rf_allocSExp;
	SEXP (*consp)(SEXP cr, SEXP tl) = Rf_cons;
	SEXP (*lconsp)(SEXP cr, SEXP tl) = Rf_lcons;
   }
}

PairList::PairList(SEXPTYPE st, size_t sz) throw (bad_alloc, out_of_range)
    : RObject(st)
{
    // checkST(st);
    if (sz == 0)
	throw out_of_range(_("Cannot construct PairList of zero length."));
    try {
	while (--sz)
	    m_tail = new PairList(st, 0, m_tail, 0);
    } catch (...) {
	if (m_tail) m_tail->expose();
	throw;
    }
}

void  PairList::checkST(SEXPTYPE st)
{
    switch (st) {
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case BCODESXP:
	break;
    default:
	throw invalid_argument("Inappropriate SEXPTYPE for PairList.");
    }
}

const char* PairList::typeName() const
{
    switch (sexptype()) {
    case LISTSXP:
	return "pairlist";
    case LANGSXP:
	return "language";
    case DOTSXP:
	return "...";
    case BCODESXP:
	return "bytecode";
    default:
	throw logic_error(_("PairList has illegal SEXPTYPE."));
    }
}

void PairList::visitChildren(const_visitor* v) const
{
    const PairList* p = this;
    do {
	p->RObject::visitChildren(v);
	if (p->m_car) p->m_car->conductVisitor(v);
	if (p->m_tag) p->m_tag->conductVisitor(v);
	p = p->m_tail;
    } while (p && (*v)(p));
}

void PairList::visitChildren(visitor* v)
{
    PairList* p = this;
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
	const RObject* pname = sym->u.symsxp.pname;
	if (!pname) return "(Symbol has no PRINTNAME)";
	const String* pstr = dynamic_cast<const String*>(pname);
	if (!pstr) return "(PRINTNAME not a String)";
	return pstr->c_str();
    }
}

void CXXR::pldump(ostream& os, const PairList& pl, size_t margin)
{
    indent(os, margin);
    os << Rf_type2char(pl.sexptype()) << '\n';
    for (const PairList* p = &pl; p; p = p->tail()) {
	// Print tag:
	indent(os, margin);
	os << "- ";
	const RObject* tag = p->tag();
	if (!tag) os << "(No tag):\n";
	else if (tag->sexptype() != SYMSXP) os << "(Tag not a SYMSXP):\n";
	else os << sympname(tag) << ":\n";
	// Print car:
	const RObject* car = p->car();
	if (const PairList* plinner = dynamic_cast<const PairList*>(car))
	    pldump(os, *plinner, margin + 2);
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
    PairList& pl = *SEXP_downcast<PairList*>(x);
    pl.setTag(y);
}

SEXP SETCAR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    PairList& pl = *SEXP_downcast<PairList*>(x);
    pl.setCar(y);
    return y;
}

SEXP SETCDR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    PairList& pl = *SEXP_downcast<PairList*>(x);
    PairList* tl = SEXP_downcast<PairList*>(y);
    pl.setTail(tl);
    return y;
}

SEXP SETCADR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    PairList* pl = SEXP_downcast<PairList*>(x);
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl->setCar(y);
    return y;
}

SEXP SETCADDR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    PairList* pl = SEXP_downcast<PairList*>(x);
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl->setCar(y);
    return y;
}

SEXP SETCADDDR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    PairList* pl = SEXP_downcast<PairList*>(x);
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl->setCar(y);
    return y;
}

SEXP SETCAD4R(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    PairList* pl = SEXP_downcast<PairList*>(x);
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl = pl->tail();
    if (!pl) Rf_error(_("bad value"));
    pl->setCar(y);
    return y;
}
