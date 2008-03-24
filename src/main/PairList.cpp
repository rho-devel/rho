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
 * @brief Class CXXR::PairList and associated C interface.
 */

#include "CXXR/PairList.h"

#include <iostream>
#include "localization.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*CAARp)(SEXP e) = CAAR;
	SEXP (*CARp)(SEXP e) = CAR;
	SEXP (*CAD4Rp)(SEXP e) = CAD4R;
	SEXP (*CADDDRp)(SEXP e) = CADDDR;
	SEXP (*CADDRp)(SEXP e) = CADDR;
	SEXP (*CADRp)(SEXP e) = CADR;
	SEXP (*CDARp)(SEXP e) = CDAR;
	SEXP (*CDDRp)(SEXP e) = CDDR;
	SEXP (*CDRp)(SEXP e) = CDR;
	SEXP (*allocListp)(unsigned int n) = Rf_allocList;
	SEXP (*consp)(SEXP cr, SEXP tl) = Rf_cons;
   }
}

const char* PairList::typeName() const
{
    return staticTypeName();
}

SEXP SETCDR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    ConsCell& cc = *SEXP_downcast<ConsCell*>(x);
    PairList* tl = SEXP_downcast<PairList*>(y);
    cc.setTail(tl);
    return y;
}

SEXP SETCADR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    ConsCell* cc = SEXP_downcast<ConsCell*>(x);
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc->setCar(y);
    return y;
}

SEXP SETCADDR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    ConsCell* cc = SEXP_downcast<ConsCell*>(x);
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc->setCar(y);
    return y;
}

SEXP SETCADDDR(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    ConsCell* cc = SEXP_downcast<ConsCell*>(x);
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc->setCar(y);
    return y;
}

SEXP SETCAD4R(SEXP x, SEXP y)
{
    if (!x) Rf_error(_("bad value"));
    ConsCell* cc = SEXP_downcast<ConsCell*>(x);
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc = cc->tail();
    if (!cc) Rf_error(_("bad value"));
    cc->setCar(y);
    return y;
}
