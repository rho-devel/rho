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

/** @file RObject.cpp
 *
 * At present, this file simply forces the generation of non-inlined
 * versions of inlined functions declared in RObject.h where these are
 * intended to be callable from C.  It is also used to check that
 * RObject.h is self-contained, i.e. #includes anything it needs, and
 * doesn't rely on anything having been previously #included in the
 * enclosing source file.
 */

// Needed while visitChildren(visitor* v) is unimplemented.
#include <iostream>

#include "CXXR/RObject.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace {
    SEXP (*attribptr)(SEXP e) = ATTRIB;
    Rboolean (*isNullptr)(SEXP s) = Rf_isNull;
    Rboolean (*isSymbolptr)(SEXP s) = Rf_isSymbol;
    Rboolean (*isLogicalptr)(SEXP s) = Rf_isLogical;
    Rboolean (*isRealptr)(SEXP s) = Rf_isReal;
    Rboolean (*isComplexptr)(SEXP s) = Rf_isComplex;
    Rboolean (*isExpressionptr)(SEXP s) = Rf_isExpression;
    Rboolean (*isStringptr)(SEXP s) = Rf_isString;
    Rboolean (*isObjectptr)(SEXP s) = Rf_isObject;
    int (*levelsptr)(SEXP x) = LEVELS;
    int (*namedptr)(SEXP x) = NAMED;
    Rboolean (*objectptr)(SEXP e) = OBJECT;
    int (*setlevelsptr)(SEXP x, int v) = SETLEVELS;
    void (*setnamedptr)(SEXP x, int v) = SET_NAMED;
    void (*setobjectptr)(SEXP x, int v) = SET_OBJECT;
    void (*settraceptr)(SEXP x, int v) = SET_TRACE;
    void (*settypeofptr)(SEXP x, SEXPTYPE v) = SET_TYPEOF;
    int (*traceptr)(SEXP x) = TRACE;
    SEXPTYPE (*typeofptr)(SEXP e) = TYPEOF;
    void (*setS4objectptr)(SEXP x) = SET_S4_OBJECT;
    void (*unsetS4objectptr)(SEXP x) = UNSET_S4_OBJECT;
    Rboolean (*bindingislockedptr)(SEXP b) = BINDING_IS_LOCKED;
    Rboolean (*isactivebindingptr)(SEXP b) = IS_ACTIVE_BINDING;
    void (*lockbindingptr)(SEXP b) = LOCK_BINDING;
    void (*setactivebindingbitptr)(SEXP b) = SET_ACTIVE_BINDING_BIT;
    void (*unlockbindingptr)(SEXP b) = UNLOCK_BINDING;
}

RObject::~RObject()
{
    if (m_data) Heap::deallocate(m_data, m_databytes);
}

void RObject::visitChildren(const_visitor* v) const
{
    if (m_attrib) m_attrib->conductVisitor(v);
    switch (sexptype()) {
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	for (int i = 0; i < length(); i++) {
	    const GCNode* node = reinterpret_cast<SEXP*>(m_data)[i];
	    if (node) node->conductVisitor(v);
	}
	break;
    case ENVSXP:
	if (frame()) frame()->conductVisitor(v);
	if (enclosingEnvironment())
	    enclosingEnvironment()->conductVisitor(v);
	if (hashTable()) hashTable()->conductVisitor(v);
	break;
    case CLOSXP:
    case PROMSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case SYMSXP:
    case BCODESXP:
	if (tag()) tag()->conductVisitor(v);
	if (car()) car()->conductVisitor(v);
	if (cdr()) cdr()->conductVisitor(v);
	break;
    case EXTPTRSXP:
	if (cdr()) cdr()->conductVisitor(v);
	if (tag()) tag()->conductVisitor(v);
	break;
    default:
	break;
    }
}

void RObject::visitChildren(visitor* v)
{
    cerr << "RObject::visitChildren(visitor* v) not implemented yet.\n";
    abort();
}

