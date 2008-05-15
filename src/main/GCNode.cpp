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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include "CXXR/GCNode.hpp"

#include <iostream>
#include "CXXR/GCManager.hpp"
#include "CXXR/GCRoot.h"

using namespace std;
using namespace CXXR;

unsigned int GCNode::SchwarzCtr::s_count = 0;
unsigned int GCNode::s_num_generations = 0;
const GCNode** GCNode::s_genpeg;
unsigned int* GCNode::s_gencount;
size_t GCNode::s_num_nodes;

GCNode::SchwarzCtr::SchwarzCtr()
{
    if (!s_count++) {
	GCNode::initialize();
	GCRootBase::initialize();
    }
}

GCNode::SchwarzCtr::~SchwarzCtr()
{
    if (!--s_count) {
	GCRootBase::cleanup();
	GCNode::cleanup();
    }
}

bool GCNode::check()
{
    if (s_num_generations == 0) {
	cerr << "GCNode::check() : class not initialised.\n";
	abort();
    }
    // Check each generation:
    {
	unsigned int numnodes = 0;
	for (unsigned int gen = 0; gen < s_num_generations; ++gen) {
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
		// Don't try visiting children of nodes in Generation
		// 0, because they may still be under construction:
		if (gen > 0)
		    node->visitChildren(&o2n);
	    }
	    if (gct != s_gencount[gen]) {
		cerr << "GCNode::check() : nodes in generation " << gen
		     << " wrongly counted.\n";
		abort();
	    }
	    numnodes += gct;
	}
	if (numnodes != s_num_nodes) {
	    cerr << "GCNode::check() :"
		"generation node totals inconsistent with grand total.\n";
	    abort();
	}
    }
    return true;
}

void GCNode::cleanup()
{
    delete [] s_gencount;
    delete [] s_genpeg;
}

void GCNode::devolveAge(const GCNode* node)
{
    if (node) {
	Ager ager(m_gcgen);
	node->conductVisitor(&ager);
    }
}

// GCNode::gc() is in memory.cpp (for the time being)

void GCNode::initialize()
{
    s_num_generations = GCManager::numGenerations();
    s_genpeg = new const GCNode*[s_num_generations];
    s_gencount = new unsigned int[s_num_generations];
    for (unsigned int gen = 0; gen < s_num_generations; ++gen) {
	s_genpeg[gen] = new GCNode(0);
	s_gencount[gen] = 0;
    }
}

size_t GCNode::slaughterInfants()
{
    size_t ans = 0;
    const GCNode* node = s_genpeg[0]->next();
    while (node != s_genpeg[0]) {
	const GCNode* next = node->next();
	delete node;
	++ans;
	node = next;
    }
    return ans;
}

bool GCNode::Ager::operator()(const GCNode* node)
{
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
