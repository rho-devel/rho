/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
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

/** @file ProtectStack.cpp
 *
 * Implementation of class CXXR::ProtectStack and associated C
 * interface.
 */

#include "CXXR/ProtectStack.h"

#include <algorithm>
#include <stdexcept>
#include "CXXR/Evaluator_Context.hpp"

using namespace std;
using namespace CXXR;

// Force generation of non-inline embodiments of functions in the C
// interface:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*protectp)(SEXP) = Rf_protect;
        void (*unprotectp)(int) = Rf_unprotect;
	void (*unprotect_ptrp)(RObject*) = Rf_unprotect_ptr;
	void (*ProtectWithIndexp)(SEXP, PROTECT_INDEX *) = R_ProtectWithIndex;
	void (*Reprotectp)(SEXP, PROTECT_INDEX) = R_Reprotect;
    }
}

vector<RObject*>* ProtectStack::s_pps;
#ifndef NDEBUG
ProtectStack::Scope* ProtectStack::s_innermost_scope = 0;
#endif

void ProtectStack::initialize()
{
    static vector<RObject*> pps;
    s_pps = &pps;
}

void ProtectStack::restoreSize(size_t new_size)
{
    if (new_size > s_pps->size())
	throw out_of_range("ProtectStack::ppsRestoreSize: requested size"
			   " greater than current size.");
    trim(new_size);
}

void ProtectStack::reprotect(RObject* node, unsigned int index)
{
    GCNode::maybeCheckExposed(node);
#ifndef NDEBUG
    if (index >= s_pps->size())
	throw out_of_range("ProtectStack::reprotect: index out of range.");
#endif
    GCNode::incRefCount(node);
    RObject* entry = (*s_pps)[index];
    GCNode::decRefCount(entry);
    (*s_pps)[index] = node;
}

void ProtectStack::trim(size_t new_size)
{
    while (s_pps->size() > new_size) {
	RObject* node = s_pps->back();
	GCNode::decRefCount(node);
	s_pps->pop_back();
    }
}

void ProtectStack::unprotect(unsigned int count)
{
#ifndef NDEBUG
    size_t sz = s_pps->size();
    if (count > sz)
	throw out_of_range("ProtectStack::unprotect: count greater"
			   " than current stack size.");
    if (s_innermost_scope && sz - count < s_innermost_scope->startSize())
	throw logic_error("ProtectStack::unprotect: too many unprotects"
			  " in this scope.");
#endif
    for (unsigned int i = 0; i < count; ++i) {
	RObject* node = s_pps->back();
	GCNode::decRefCount(node);
	s_pps->pop_back();
    }
}

void ProtectStack::unprotectPtr(RObject* node)
{
    GCNode::decRefCount(node);
    vector<RObject*>::reverse_iterator rit
	= find(s_pps->rbegin(), s_pps->rend(), node);
    if (rit == s_pps->rend())
	throw invalid_argument("ProtectStack::unprotectPtr:"
			       " pointer not found.");
#ifndef NDEBUG
    if (s_innermost_scope && s_pps->size() == s_innermost_scope->startSize())
	throw logic_error("ProtectStack::unprotect: too many unprotects"
			  " in this scope.");
#endif
    // See Josuttis p.267 for the need for -- :
    s_pps->erase(--(rit.base()));
}

void ProtectStack::visitRoots(GCNode::const_visitor* v)
{
    vector<RObject*>::iterator ppsend = s_pps->end();
    for (vector<RObject*>::iterator it = s_pps->begin();
	 it != ppsend; ++it) {
	RObject* n = *it;
	if (n)
	    (*v)(n);
    }
}

// ***** C interface *****

void Rf_ppsRestoreSize(size_t new_size)
{
    ProtectStack::restoreSize(new_size);
}

size_t Rf_ppsSize()
{
    return ProtectStack::size();
}
