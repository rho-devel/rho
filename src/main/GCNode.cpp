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

#include <cstdlib>
#include <iostream>
#include "CXXR/GCManager.hpp"
#include "CXXR/GCRoot.h"

using namespace std;
using namespace CXXR;

unsigned int GCNode::SchwarzCtr::s_count = 0;
unsigned int GCNode::s_num_generations = 0;
const GCNode** GCNode::s_generation;
unsigned int* GCNode::s_next_gen;
unsigned int* GCNode::s_gencount;
size_t GCNode::s_num_nodes;
GCNode::AgedList* GCNode::s_aged_list;

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

GCNode::GCNode(int)
    : m_next(0), m_gcgen(0), m_marked(false), m_aged(false)
{
    ++s_gencount[0];
    ++s_num_nodes;
}

/*
void* GCNode::operator new(size_t bytes)
{
    void* ans = MemoryBank::allocate(bytes);
    cout << "Node of " << bytes << " bytes allocated at " << ans << endl;
    return ans;
}
*/

void GCNode::ageTo(unsigned int mingen) const
{
    if (m_gcgen < mingen) {
	--s_gencount[m_gcgen];
	m_gcgen = mingen;
	++s_gencount[m_gcgen];
	if (!m_aged) {
	    m_aged = true;
	    s_aged_list->push_back(this);
	}
    }
}

bool GCNode::check()
{
    if (s_num_generations == 0) {
	cerr << "GCNode::check() : class not initialised.\n";
	abort();
    }
    vector<unsigned int> genct(s_num_generations);
    unsigned int numnodes = 0;
    // Check each generation:
    for (unsigned int gen = 0; gen < s_num_generations; ++gen) {
	OldToNewChecker o2n(gen);
	for (const GCNode* node = s_generation[gen];
	     node != 0; node = node->next()) {
	    unsigned int ngen = node->m_gcgen;
	    ++genct[ngen];
	    if (node->isMarked()) {
		cerr << "GCNode::check() : marked node found.\n";
		abort();
	    }
	    if (ngen >= s_num_generations) {
		cerr << "GCNode::check() : "
		    "Illegal generation number.\n";
		abort();
	    }
	    if (ngen < gen) {
		cerr << "GCNode::check() : "
		    "node has wrong generation for its list.\n";
		abort();
	    }
	    // Don't try visiting children of nodes in Generation
	    // 0, because these nodes may still be under construction:
	    if (gen > 0 && !node->m_aged)
		node->visitChildren(&o2n);
	}
    }
    // Check generation counts:
    for (unsigned int gen = 0; gen < s_num_generations; ++gen) {
	if (genct[gen] != s_gencount[gen]) {
	    cerr << "GCNode::check() : nodes in generation " << gen
		 << " wrongly counted.\nCounted " << genct[gen]
		 << "; expected " << s_gencount[gen] << "\n";
	    abort();
	}
	numnodes += genct[gen];
    }
    // Check total number of nodes:
    if (numnodes != s_num_nodes) {
	cerr << "GCNode::check() :"
	    "generation node totals inconsistent with grand total.\n";
	abort();
    }
    return true;
}

void GCNode::cleanup()
{
    delete s_aged_list;
    delete [] s_gencount;
    delete [] s_next_gen;
    delete [] s_generation;
}

// GCNode::gc() is in memory.cpp (for the time being)

void GCNode::initialize()
{
    s_num_generations = GCManager::numGenerations();
    s_generation = new const GCNode*[s_num_generations];
    s_next_gen = new unsigned int[s_num_generations];
    s_gencount = new unsigned int[s_num_generations];
    s_aged_list = new vector<const GCNode*>;
    for (unsigned int gen = 0; gen < s_num_generations; ++gen) {
	s_generation[gen] = 0;
	s_next_gen[gen] = gen + 1;
	s_gencount[gen] = 0;
    }
    s_next_gen[0] = 0;
    s_next_gen[s_num_generations - 1] = s_num_generations - 1;
}

void GCNode::propagateAges()
{
    for (AgedList::const_iterator it = s_aged_list->begin();
	 it != s_aged_list->end(); ++it) {
	const GCNode* node = *it;
	// node may already have been visited by an Ager on an earlier
	// round of this loop, in which case m_aged will have been set
	// false and there's nothing more to do:
	if (node->m_aged) {
	    Ager ager(node->m_gcgen);
	    node->visitChildren(&ager);
	    node->m_aged = false;
	}
    }
    s_aged_list->clear();
}

// Structure used to marshal nodes awaiting transfer to a
// different generation.  Nodes are added to the end of the
// list to help maintain the generation lists as far as
// possible in reverse order of node allocation.
class GCNode::XferList {
public:
    XferList()
	: m_peg(0), m_last(&m_peg)
    {}

    void append(const GCNode* node)
    {
	node->m_next = 0;
	m_last->m_next = node;
	m_last = node;
    }

    // Export the transfer list by prepending it to the list
    // whose first element is pointed to by *listp.  Following
    // this operation, the transfer list will itself be empty.
    void prependTo(const GCNode** listp)
    {
	m_last->m_next = *listp;
	*listp = m_peg.m_next;
	m_peg.m_next = 0;
	m_last = &m_peg;
    }
private:
    const GCNode m_peg;  // Dummy first element of list
    const GCNode* m_last;  // Pointer to last element
};
		

size_t GCNode::slaughterInfants()
{
    vector<XferList*> xferlist(s_num_generations);
    for (unsigned int gen = 0; gen < s_num_generations; ++gen)
	xferlist[gen] = new XferList;
    size_t ans = 0;
    const GCNode* node = s_generation[0];
    while (node) {
	const GCNode* next = node->next();
	if (node->m_gcgen > 0)
	    xferlist[node->m_gcgen]->append(node);
	else {
	    delete node;
	    ++ans;
	}
	node = next;
    }
    // Now move nodes on each transfer list into the generation list itself:
    for (unsigned int gen = 0; gen < s_num_generations; ++gen) {
	xferlist[gen]->prependTo(&s_generation[gen]);
	delete xferlist[gen];
    }
    return ans;
}

// In implementing sweep(), as far as possible: (i) visit each node
// only once; (ii) deallocate nodes in the reverse order of
// allocation.

void GCNode::sweep(unsigned int max_generation)
{
    vector<XferList*> xferlist(s_num_generations);
    for (unsigned int gen = 0; gen < s_num_generations; ++gen)
	xferlist[gen] = new XferList;
    // Process generations:
    for (unsigned int gen = 0; gen <= max_generation; ++gen) {
	// Scan through generation:
	const GCNode* node = s_generation[gen];
	while (node) {
	    const GCNode* next = node->next();
	    unsigned int ngen = node->m_gcgen;
	    if (ngen <= max_generation && ngen != 0) {
		if (!node->isMarked()) {
		    delete node;
		    node = 0;
		} else {
		    // Advance generation:
		    --s_gencount[ngen];
		    node->m_gcgen = s_next_gen[ngen];
		    ++s_gencount[node->m_gcgen];
		}
	    }
	    if (node) {
		node->m_marked = false;
		xferlist[node->m_gcgen]->append(node);
	    }
	    node = next;
	}
	// Zap the list:
	s_generation[gen] = 0;
    }
    // Now move nodes on each transfer list into the generation list itself:
    for (unsigned int gen = 0; gen < s_num_generations; ++gen) {
	xferlist[gen]->prependTo(&s_generation[gen]);
	delete xferlist[gen];
    }
    // cerr << "s_gencount[0] = " << s_gencount[0] << endl;
}

bool GCNode::Ager::operator()(const GCNode* node)
{
    if (node->m_gcgen >= m_mingen)
	return false;
    --s_gencount[node->m_gcgen];
    node->m_gcgen = m_mingen;
    ++s_gencount[node->m_gcgen];
    node->m_aged = false;
    return true;
}

bool GCNode::Marker::operator()(const GCNode* node)
{
    /*
    if (node->m_gcgen == 0)
	cerr << "Warning: Marker encountered gen 0 node.\n";
    */
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
