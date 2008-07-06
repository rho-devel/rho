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

/** @file GCRoot.cpp
 *
 * Implementation of class GCRootBase.
 */

#include "CXXR/GCRoot.h"

#include <iostream>
#include <stdexcept>

using namespace std;
using namespace CXXR;

// Force generation of non-inline embodiments of functions in the C
// interface:
namespace CXXR {
    namespace ForceNonInline {
	void (*unprotect_ptrp)(RObject*) = Rf_unprotect_ptr;
	void (*ProtectWithIndexp)(SEXP, PROTECT_INDEX *) = R_ProtectWithIndex;
	void (*Reprotectp)(SEXP, PROTECT_INDEX) = R_Reprotect;
    }
}

vector<const GCNode*>* GCRootBase::s_roots;

#ifdef NDEBUG
vector<RObject*>* GCRootBase::s_pps;
#else
vector<pair<RObject*, RCNTXT*> >* GCRootBase::s_pps;
#endif

void GCRootBase::initialize()
{
    s_roots = new vector<const GCNode*>;
#ifdef NDEBUG
    s_pps = new vector<RObject*>;
#else
    s_pps = new vector<pair<RObject*, RCNTXT*> >;
#endif
}

void GCRootBase::ppsRestoreSize(size_t new_size)
{
    if (new_size > s_pps->size())
	throw out_of_range("GCRootBase::ppsRestoreSize: requested size"
			   " greater than current size.");
    s_pps->resize(new_size);
}

void GCRootBase::reprotect(RObject* node, unsigned int index)
{
    if (index >= s_pps->size())
	throw out_of_range("GCRootBase::reprotect: index out of range.");
#ifdef NDEBUG
    (*s_pps)[index] = node;
#else
    pair<RObject*, RCNTXT*>& pr = (*s_pps)[index];
    if (pr.second != R_GlobalContext)
	throw logic_error("GCRootBase::reprotect: not in same context"
			  " as the corresponding call of protect().");
    pr.first = node;
#endif
}

void GCRootBase::seq_error()
{
    cerr << "Fatal error:"
	    " GCRoots must be destroyed in reverse order of creation\n";
    abort();
}

void GCRootBase::unprotect(unsigned int count)
{
    size_t sz = s_pps->size();
    if (count > sz)
	throw out_of_range("GCRootBase::unprotect: count greater"
			   " than current stack size.");
#ifdef NDEBUG
    s_pps->resize(sz - count);
#else
    for (unsigned int i = 0; i < count; ++i) {
	const pair<RObject*, RCNTXT*>& pr = s_pps->back();
	if (pr.second != R_GlobalContext)
	    throw logic_error("GCRootBase::unprotect: not in same context"
			      " as the corresponding call of protect().");
	s_pps->pop_back();
    }
#endif
}

void GCRootBase::unprotectPtr(RObject* node)
{
#ifdef NDEBUG
    vector<RObject*>::reverse_iterator rit
	= find(s_pps->rbegin(), s_pps->rend(), node);
#else
    vector<pair<RObject*, RCNTXT*> >::reverse_iterator rit = s_pps->rbegin();
    while (rit != s_pps->rend() && (*rit).first != node)
	++rit;
#endif
    if (rit == s_pps->rend())
	throw invalid_argument("GCRootBase::unprotectPtr:"
			       " pointer not found.");
    // See Josuttis p.267 for the need for -- :
    s_pps->erase(--(rit.base()));
}

void GCRootBase::visitRoots(GCNode::const_visitor* v)
{
    for (vector<const GCNode*>::iterator it = s_roots->begin();
	 it != s_roots->end(); ++it) {
	const GCNode* n = *it;
	if (n) n->conductVisitor(v);
    }
#ifdef NDEBUG
    for (vector<RObject*>::iterator it = s_pps->begin();
	 it != s_pps->end(); ++it) {
	RObject* n = *it;
	if (n) n->conductVisitor(v);
    }
#else
    for (vector<pair<RObject*, RCNTXT*> >::iterator it = s_pps->begin();
	 it != s_pps->end(); ++it) {
	RObject* n = (*it).first;
	if (n) n->conductVisitor(v);
    }
#endif
}

// ***** C interface *****

void Rf_ppsRestoreSize(size_t new_size)
{
    GCRootBase::ppsRestoreSize(new_size);
}

size_t Rf_ppsSize()
{
    return GCRootBase::ppsSize();
}

SEXP Rf_protect(SEXP node)
{
    GCRootBase::protect(node);
    return node;
}

void Rf_unprotect(int count)
{
    GCRootBase::unprotect(count);
}
