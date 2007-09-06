/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include "CXXR/GCNode.hpp"

using namespace std;
using namespace CXXR;

const unsigned int GCNode::s_num_old_generations;
GCNode* GCNode::s_oldpeg[];
unsigned int GCNode::s_oldcount[];
#ifndef EXPEL_OLD_TO_NEW
GCNode* GCNode::s_old_to_new_peg[];
#endif
GCNode* GCNode::s_newpeg;
unsigned int GCNode::s_num_nodes;

GCNode::GCNode()
{
    link(s_newpeg->m_prev, this);
    link(this, s_newpeg);
    ++s_num_nodes;
}

GCNode::~GCNode()
{
    --s_num_nodes;
    link(m_prev, m_next);
}

void GCNode::initialize()
{
    if (!s_newpeg) {
	s_newpeg = new GCNode(0);
	for (unsigned int gen = 0; gen < s_num_old_generations; ++gen) {
	    s_oldpeg[gen] = new GCNode(0);
	    s_oldcount[gen] = 0;
#ifndef EXPEL_OLD_TO_NEW
	    s_old_to_new_peg[gen] = new GCNode(0);
#endif
	}
    }
}
