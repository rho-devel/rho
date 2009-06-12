/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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

/** @file GCStackRoot.cpp
 *
 * Implementation of class GCStackRootBase.
 */

#include "CXXR/GCStackRoot.h"

#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include "RCNTXT.h"

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

GCStackRootBase* GCStackRootBase::s_roots = 0;

#ifdef NDEBUG
vector<RObject*>* GCStackRootBase::s_pps;
#else
vector<pair<RObject*, RCNTXT*> >* GCStackRootBase::s_pps;
#endif

void GCStackRootBase::initialize()
{
#ifdef NDEBUG
    s_pps = new vector<RObject*>;
#else
    s_pps = new vector<pair<RObject*, RCNTXT*> >;
#endif
}

void GCStackRootBase::ppsRestoreSize(size_t new_size)
{
    if (new_size > s_pps->size())
	throw out_of_range("GCStackRootBase::ppsRestoreSize: requested size"
			   " greater than current size.");
    while (s_pps->size() > new_size) {
#ifdef NDEBUG
	RObject* node = s_pps->back();
#else
	const pair<RObject*, RCNTXT*>& pr = s_pps->back();
	RObject* node = pr.first;
#endif
	if (node && node->decRefCount() == 0)
	    node->makeMoribund();
	s_pps->pop_back();
    }
}

#ifndef NDEBUG
unsigned int GCStackRootBase::protect(RObject* node)
{
    GCNode::maybeCheckExposed(node);
    unsigned int index = s_pps->size();
    if (node)
	node->incRefCount();
    s_pps->push_back(std::make_pair(node, R_GlobalContext));
    return index;
}
#endif

void GCStackRootBase::reprotect(RObject* node, unsigned int index)
{
    GCNode::maybeCheckExposed(node);
    if (index >= s_pps->size())
	throw out_of_range("GCStackRootBase::reprotect: index out of range.");
    if (node)
	node->incRefCount();
#ifdef NDEBUG
    RObject* entry = (*s_pps)[index];
    if (entry && entry->decRefCount() == 0)
	entry->makeMoribund();
    (*s_pps)[index] = node;
#else
    pair<RObject*, RCNTXT*>& pr = (*s_pps)[index];
    if (pr.second != R_GlobalContext)
	throw logic_error("GCStackRootBase::reprotect: not in same context"
			  " as the corresponding call of protect().");
    if (pr.first && pr.first->decRefCount() == 0)
	pr.first->makeMoribund();
    pr.first = node;
#endif
}

void GCStackRootBase::seq_error()
{
    cerr << "Fatal error:"
	    " GCStackRoots must be destroyed in reverse order of creation\n";
    abort();
}

void GCStackRootBase::unprotect(unsigned int count)
{
    size_t sz = s_pps->size();
    if (count > sz)
	throw out_of_range("GCStackRootBase::unprotect: count greater"
			   " than current stack size.");
    for (unsigned int i = 0; i < count; ++i) {
#ifdef NDEBUG
	RObject* node = s_pps->back();
#else
	const pair<RObject*, RCNTXT*>& pr = s_pps->back();
	RObject* node = pr.first;
	if (pr.second != R_GlobalContext)
	    throw logic_error("GCStackRootBase::unprotect: not in same context"
			      " as the corresponding call of protect().");
#endif
	if (node && node->decRefCount() == 0)
	    node->makeMoribund();
	s_pps->pop_back();
    }
}

void GCStackRootBase::unprotectPtr(RObject* node)
{
    if (node && node->decRefCount() == 0)
	node->makeMoribund();
#ifdef NDEBUG
    vector<RObject*>::reverse_iterator rit
	= find(s_pps->rbegin(), s_pps->rend(), node);
#else
    vector<pair<RObject*, RCNTXT*> >::reverse_iterator rit = s_pps->rbegin();
    while (rit != s_pps->rend() && (*rit).first != node)
	++rit;
#endif
    if (rit == s_pps->rend())
	throw invalid_argument("GCStackRootBase::unprotectPtr:"
			       " pointer not found.");
    // See Josuttis p.267 for the need for -- :
    s_pps->erase(--(rit.base()));
}

void GCStackRootBase::visitRoots(GCNode::const_visitor* v)
{
    GCStackRootBase* root = s_roots;
    while (root) {
	if (root->m_target) root->m_target->conductVisitor(v);
	root = root->m_next;
    }
#ifdef NDEBUG
    vector<RObject*>::iterator ppsend = s_pps->end();
    for (vector<RObject*>::iterator it = s_pps->begin();
	 it != ppsend; ++it) {
	RObject* n = *it;
	if (n) n->conductVisitor(v);
    }
#else
    vector<pair<RObject*, RCNTXT*> >::iterator ppsend = s_pps->end();
    for (vector<pair<RObject*, RCNTXT*> >::iterator it = s_pps->begin();
	 it != ppsend; ++it) {
	RObject* n = (*it).first;
	if (n) n->conductVisitor(v);
    }
#endif
}

// ***** C interface *****

void Rf_ppsRestoreSize(size_t new_size)
{
    GCStackRootBase::ppsRestoreSize(new_size);
}

size_t Rf_ppsSize()
{
    return GCStackRootBase::ppsSize();
}

SEXP Rf_protect(SEXP node)
{
    GCStackRootBase::protect(node);
    return node;
}

void Rf_unprotect(int count)
{
    GCStackRootBase::unprotect(count);
}
