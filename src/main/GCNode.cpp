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

/* Comment formerly in memory.c:

   The Heap Structure.  Nodes for each generation are arranged in
   circular doubly-linked lists.  The double linking allows nodes to
   be removed in constant time; this is used by the collector to move
   reachable nodes out of free space and into the appropriate
   generation.  The circularity eliminates the need for end checks.
   In addition, each link is anchored at an artificial node called a
   peg, which simplifies pointer maintenance.  The circular
   doubly-linked arrangement is taken from Baker's in-place
   incremental collector design; see
   ftp://ftp.netcom.com/pub/hb/hbaker/NoMotionGC.html or the Jones and
   Lins GC book.  The linked lists are implemented by adding two
   pointer fields to the SEXPREC structure, which increases its size
   from 5 to 7 words. Other approaches are possible but don't seem
   worth pursuing for R.

   There are two options for dealing with old-to-new pointers.  The
   first option is to make sure they never occur by transferring all
   referenced younger objects to the generation of the referrer when a
   reference to a newer object is assigned to an older one.  This is
   enabled by defining EXPEL_OLD_TO_NEW.  The second alternative is to
   keep track of all nodes that may contain references to newer nodes
   and to "age" the nodes they refer to at the beginning of each
   collection.  This is the default.  The first option is simpler in
   some ways, but will create more floating garbage and add a bit to
   the execution time, though the difference is probably marginal on
   both counts.*/

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include "CXXR/GCNode.h"

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
    link(s_newpeg->gengc_prev_node, this);
    link(this, s_newpeg);
    ++s_num_nodes;
}

GCNode::~GCNode()
{
    --s_num_nodes;
    link(gengc_prev_node, gengc_next_node);
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
