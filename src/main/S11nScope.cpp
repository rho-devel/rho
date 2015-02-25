/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file S11nScope.cpp
 *
 * Implementation of class S11nScope.
 */

#include "CXXR/S11nScope.hpp"

#include <cstdlib>
#include <iostream>

#include "CXXR/GCNode.hpp"

using namespace CXXR;

S11nScope* S11nScope::s_innermost = nullptr;

S11nScope::S11nScope()
    : m_next(s_innermost)
{
    s_innermost = this;
}

S11nScope::~S11nScope()
{
    if (this != s_innermost)
	seqError();
    s_innermost = m_next;
}

void S11nScope::defineRelocation(GCNode* from, GCNode* to)
{
    s_innermost->m_relocations[from] = to;
}

GCNode* S11nScope::relocate(GCNode* from)
{
    std::map<GCNode*, GCNode*> map = s_innermost->m_relocations;
    std::map<GCNode*, GCNode*>::const_iterator it = map.find(from);
    if (it == map.end())
	return nullptr;
    return (*it).second;
}
    
void S11nScope::seqError()
{
    using namespace std;
    cerr << "Fatal error:"
	    " S11nScopes must be destroyed in reverse order of creation\n";
    abort();
}
