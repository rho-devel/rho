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

/** @file GCRoot.cpp
 *
 * Implementation of class GCRootBase.
 */

#include "CXXR/GCRoot.hpp"

#include <iostream>

using namespace std;
using namespace CXXR;

vector<GCNode*> GCRootBase::s_roots;

void GCRootBase::seq_error()
{
    cerr << "GCRoots must be destroyed in reverse order of creation\n";
    abort();
}

void GCRootBase::visitRoots(GCNode::const_visitor* v)
{
    for (vector<GCNode*>::iterator it = s_roots.begin();
	 it != s_roots.end(); ++it) {
	GCNode* n = *it;
	if (n) n->conductVisitor(v);
    }
}

void GCRootBase::visitRoots(GCNode::visitor* v)
{
    for (vector<GCNode*>::iterator it = s_roots.begin();
	 it != s_roots.end(); ++it) {
	GCNode* n = *it;
	if (n) n->conductVisitor(v);
    }
}
