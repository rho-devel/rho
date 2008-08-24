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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file RObject.cpp
 *
 * Class RObject and associated C interface functions.
 */

#include "CXXR/RObject.h"

#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/PairList.h"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	Rboolean (*isNullptr)(SEXP s) = Rf_isNull;
	Rboolean (*isObjectptr)(SEXP s) = Rf_isObject;
	int (*LEVELSptr)(SEXP x) = LEVELS;
	int (*NAMEDptr)(SEXP x) = NAMED;
	Rboolean (*OBJECTptr)(SEXP e) = OBJECT;
	int (*SETLEVELSptr)(SEXP x, int v) = SETLEVELS;
	void (*SET_NAMEDptr)(SEXP x, int v) = SET_NAMED;
	SEXPTYPE (*TYPEOFptr)(SEXP e) = TYPEOF;
	void (*SET_S4_OBJECTptr)(SEXP x) = SET_S4_OBJECT;
	void (*UNSET_S4_OBJECTptr)(SEXP x) = UNSET_S4_OBJECT;
	Rboolean (*BINDING_IS_LOCKEDptr)(SEXP b) = BINDING_IS_LOCKED;
	Rboolean (*IS_ACTIVE_BINDINGptr)(SEXP b) = IS_ACTIVE_BINDING;
	void (*LOCK_BINDINGptr)(SEXP b) = LOCK_BINDING;
	void (*SET_ACTIVE_BINDING_BITptr)(SEXP b) = SET_ACTIVE_BINDING_BIT;
	void (*UNLOCK_BINDINGptr)(SEXP b) = UNLOCK_BINDING;
    }
}

namespace {
    // Used in {,un}packGPBits():
    const unsigned int S4_OBJECT_MASK = 1<<4;
    const unsigned int BINDING_LOCK_MASK = 1<<14;
    const unsigned int ACTIVE_BINDING_MASK = 1<<15;
}

RObject* RObject::getAttribute(const Symbol& name)
{
    for (PairList* node = m_attrib; node; node = node->tail())
	if (node->tag() == &name) return node->car();
    return 0;
}

const RObject* RObject::getAttribute(const Symbol& name) const
{
    for (PairList* node = m_attrib; node; node = node->tail())
	if (node->tag() == &name) return node->car();
    return 0;
}

unsigned int RObject::packGPBits() const
{
    unsigned int ans = 0;
    if (isS4Object()) ans |= S4_OBJECT_MASK;
    if (m_binding_locked) ans |= BINDING_LOCK_MASK;
    if (m_active_binding) ans |= ACTIVE_BINDING_MASK;
    return ans;
}

// This follows CR in adding new attributes at the end of the list,
// though it would be easier to add them at the beginning.
void RObject::setAttribute(Symbol* name, RObject* value)
{
    if (!name)
	Rf_error(_("attempt to set an attribute on NULL"));
    // Update m_has_class if necessary:
    if (name == R_ClassSymbol)
	m_has_class = (value != 0);
    // Find attribute:
    PairList* prev = 0;
    PairList* node = m_attrib;
    while (node && node->tag() != name) {
	prev = node;
	node = node->tail();
    }
    if (node) {  // Attribute already present
	// Update existing attribute:
	if (value) node->setCar(value);
	// Delete existing attribute:
	else if (prev) prev->setTail(node->tail());
	else m_attrib = node->tail();
    } else if (value) {  
	// Create new node:
	PairList* newnode = new PairList(value, 0, name);
	newnode->expose();
	if (prev) prev->setTail(newnode);
	else { // No preexisting attributes at all:
	    m_attrib = newnode;
	    devolveAge(m_attrib);
	}
    }
}

// This has complexity O(n^2) where n is the number of attributes, but
// we assume n is very small.    
void RObject::setAttributes(PairList* new_attributes)
{
    clearAttributes();
    while (new_attributes) {
	Symbol* name = SEXP_downcast<Symbol*>(new_attributes->tag());
	setAttribute(name, new_attributes->car());
	new_attributes = new_attributes->tail();
    }
}

const char* RObject::typeName() const
{
    return Rf_type2char(sexptype());
}

void RObject::unpackGPBits(unsigned int gpbits)
{
    // Be careful with precedence!
    m_S4_object = ((gpbits & S4_OBJECT_MASK) != 0);
    m_binding_locked = ((gpbits & BINDING_LOCK_MASK) != 0);
    m_active_binding = ((gpbits & ACTIVE_BINDING_MASK) != 0);
}

void RObject::visitChildren(const_visitor* v) const
{
    if (m_attrib) m_attrib->conductVisitor(v);
}

// ***** C interface *****

SEXP ATTRIB(SEXP x)
{
    return x ? x->attributes() : 0;
}

void SET_ATTRIB(SEXP x, SEXP v)
{
    GCRoot<PairList> pl(SEXP_downcast<PairList*>(v));
    x->setAttributes(pl);
}
