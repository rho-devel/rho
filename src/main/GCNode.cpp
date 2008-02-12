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

#include <iostream>

using namespace std;
using namespace CXXR;

unsigned int GCNode::s_last_gen;
vector<const GCNode*> GCNode::s_genpeg;
vector<unsigned int> GCNode::s_gencount;
unsigned int GCNode::s_num_nodes;

GCNode::~GCNode()
{
    --s_num_nodes;
    --s_gencount[m_gcgen];
    link(m_prev, m_next);
}

bool GCNode::check()
{
    if (s_genpeg.size() == 0) {
	cerr << "GCNode::check() : class not initialised.\n";
	abort();
    }
    if (s_genpeg.size() != s_last_gen + 1
	|| s_genpeg.size() != s_gencount.size()) {
	cerr << "GCNode::check() : internal vectors inconsistently sized.\n";
	abort();
    }
    // Check each generation:
    {
	unsigned int numnodes = 0;
	for (unsigned int gen = 0; gen <= s_last_gen; ++gen) {
	    unsigned int gct = 0;
	    OldToNewChecker o2n(gen);
	    for (const GCNode* node = s_genpeg[gen]->next();
		 node != s_genpeg[gen]; node = node->next()) {
		++gct;
		if (node->isMarked()) {
		    cerr << "GCNode::check() : marked node found.\n";
		    abort();
		}
		if (node->m_gcgen != gen) {
		    cerr << "GCNode::check() : "
			"node has wrong generation for its list.\n";
		    abort();
		}
		node->visitChildren(&o2n);
	    }
	    if (gct != s_gencount[gen]) {
		cerr << "GCNode::check() : nodes in generation " << gen
		     << " wrongly counted.\n";
		abort();
	    }
	    numnodes += gct;
	}
	// s_num_nodes > numnodes is possible because of infant immunity.
	if (numnodes > s_num_nodes) {
	    cerr << "GCNode::check() :"
		"generation node totals inconsistent with grand total.\n";
	    abort();
	}
    }
    return true;
}

void GCNode::devolveAge(const GCNode* node)
{
    if (node) {
	Ager ager(m_gcgen);
	node->conductVisitor(&ager);
    }
}

void GCNode::expose() const
{
    if (!m_prev)
	{
	    link(s_genpeg[0]->m_prev, this);
	    link(this, s_genpeg[0]);
	    ++s_gencount[0];
	}
}

// GCNode::gc() is in memory.cpp (for the time being)

void GCNode::initialize(unsigned int num_old_generations)
{
    if (s_genpeg.size() == 0) {
	s_last_gen = num_old_generations;
	s_genpeg.resize(num_old_generations + 1);
	s_gencount.resize(num_old_generations + 1, 0);
	for (unsigned int gen = 0; gen <= s_last_gen; ++gen)
	    s_genpeg[gen] = new GCNode(0);
    }
}

bool GCNode::Ager::operator()(const GCNode* node)
{
    node->expose();
    if (node->m_gcgen >= m_mingen)
	return false;
    --s_gencount[node->m_gcgen];
    node->m_gcgen = m_mingen;
    s_genpeg[m_mingen]->splice(node);
    ++s_gencount[node->m_gcgen];
    return true;
}

bool GCNode::Marker::operator()(const GCNode* node)
{
    if (node->isMarked() || node->m_gcgen > m_maxgen)
	return false;
    node->mark();
    return true;
}

bool GCNode::OldToNewChecker::operator()(const GCNode* node)
{
    if (node->m_gcgen < m_mingen) {
	cerr << "GCNode: old to new reference found.\n";
	abort();
    }
    return false;
}
