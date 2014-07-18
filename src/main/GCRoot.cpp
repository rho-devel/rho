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

/** @file GCRoot.cpp
 *
 * Implementation of class GCRootBase.
 */

#include "CXXR/GCRoot.h"

#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <unordered_map>

using namespace std;
using namespace CXXR;

// Force generation of non-inline embodiments of functions in the C
// interface:
namespace CXXR {
    namespace ForceNonInline {
    }
}

GCRootBase::List* GCRootBase::s_roots;

GCRootBase::GCRootBase(const GCNode* node)
    : m_it(s_roots->insert(s_roots->end(), node))
{
    GCNode::maybeCheckExposed(node);
    GCNode::incRefCount(node);
}

void GCRootBase::initialize()
{
    static List roots;
    s_roots = &roots;
}

void GCRootBase::visitRoots(GCNode::const_visitor* v)
{
    List::iterator end = s_roots->end();
    for (List::iterator it = s_roots->begin(); it != end; ++it) {
	const GCNode* n = *it;
	if (n)
	    (*v)(n);
    }
}

// ***** C interface *****

// This is not a busy list, so we don't bother to use CXXR::Allocator:
unordered_map<const RObject*, GCRoot<> > precious;

void R_PreserveObject(SEXP object)
{
    precious[object] = GCRoot<>(object);
}

void  R_ReleaseObject(SEXP object)
{
    precious.erase(object);
}
