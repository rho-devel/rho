/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-12 Andrew R. Runnalls, subject to such other
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

/** @file GCEdge.cpp
 *
 * Implementation of class GCEdgeBase.
 */

#include "CXXR/GCEdge.hpp"

#include <cstdlib>
#include <iostream>
#include <vector>

#include "CXXR/Environment.h"
#include "CXXR/GCRoot.h"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

namespace {
    std::vector<GCRoot<GCNode> > s11n_temporaries;
}

void GCEdgeBase::detach()
{
    GCNode::decRefCount(m_target);
    m_target = 0;
}

void GCEdgeBase::preserveS11nTemporary(GCNode* target)
{
    s11n_temporaries.push_back(GCRoot<GCNode>(target));
}

GCEdgeBase::EdgeSerializationType GCEdgeBase::serializationType() const {
    if (!m_target) return OTHEREDGE;

    if (typeid(*m_target)==typeid(Symbol))
	return SYMBOLEDGE;

    return OTHEREDGE;
}
