/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007 Andrew Runnalls.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
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

namespace CXXR {
    namespace ForceNonInline {
	SEXP (*TAGp)(SEXP e) = TAG;
	SEXP (*CAARp)(SEXP e) = CAAR;
	SEXP (*CAD4Rp)(SEXP e) = CAD4R;
	SEXP (*CADDDRp)(SEXP e) = CADDDR;
	SEXP (*CADDRp)(SEXP e) = CADDR;
	SEXP (*CADRp)(SEXP e) = CADR;
	SEXP (*CARp)(SEXP e) = CAR;
	SEXP (*CDARp)(SEXP e) = CDAR;
	SEXP (*CDDRp)(SEXP e) = CDDR;
	SEXP (*CDRp)(SEXP e) = CDR;
	int (*MISSINGp)(SEXP x) = MISSING;
	void (*SET_MISSINGp)(SEXP x, int v) = SET_MISSING;
    }
}
