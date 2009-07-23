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
#include "CXXR/GCStackRoot.h"

using namespace std;
using namespace CXXR;

unsigned int GCNode::s_num_generations = 0;
GCNode::List* GCNode::s_generation;
GCNode::List* GCNode::s_aged_list;
GCNode::List* GCNode::s_reachable;
GCNode::List* GCNode::s_lists[8];
unsigned int* GCNode::s_next_gen;
unsigned char GCNode::s_mark = 0;
unsigned int GCNode::s_num_nodes = 0;
unsigned int GCNode::s_under_construction = 0;
unsigned int GCNode::s_inhibitor_count = 0;

void* GCNode::operator new(size_t bytes)
{
    if (MemoryBank::bytesAllocated() > GCManager::triggerLevel()
	&& s_under_construction == 0 && s_inhibitor_count == 0)
	GCManager::gc(bytes);
    return MemoryBank::allocate(bytes);
}

void GCNode::abortIfNotExposed(const GCNode* node)
{
    if (node && !(node->m_bits & GENERATION)) {
	cerr << "Internal error: GCNode still under construction\n";
	abort();
    }
}

void GCNode::ageTo(unsigned int mingen) const
{
    if (generation() < mingen) {
	m_bits &= ~GENERATION;
	m_bits |= mingen;
	if (!(m_bits & AGED)) {
	    m_bits |= AGED;
	    s_aged_list->splice_back(this);
	}
    }
}

bool GCNode::check()
{
    if (s_num_generations == 0) {
	cerr << "GCNode::check() : class not initialised.\n";
	abort();
    }
    unsigned int numnodes = 0;
    // Check aged list:
    {
	List::const_iterator end = s_aged_list->end();
	for (List::const_iterator it = s_aged_list->begin();
	     it != end; ++it) {
	    const GCNode* node = *it;
	    ++numnodes;
	    if (node->generation() >= s_num_generations) {
		cerr << "GCNode::check() : "
		    "Illegal generation number.\n";
		abort();
	    }
	}
    }
    // Check generation lists:
    for (unsigned int gen = 1; gen < s_num_generations; ++gen) {
	OldToNewChecker o2n(gen);
	List::const_iterator end = s_generation[gen].end();
	for (List::const_iterator it = s_generation[gen].begin();
	     it != end; ++it) {
	    const GCNode* node = *it;
	    ++numnodes;
	    unsigned int ngen = node->generation();
	    if (ngen >= s_num_generations) {
		cerr << "GCNode::check() : "
		    "Illegal generation number.\n";
		abort();
	    }
	    if (ngen != gen) {
		cerr << "GCNode::check() : "
		    "node has wrong generation for its list.\n";
		abort();
	    }
	    if (gen > 1)
		node->visitReferents(&o2n);
	}
    }
    // Check total number of nodes:
    if (numnodes != s_num_nodes) {
	cerr << "GCNode::check() :"
	    "recorded number of nodes inconsistent with nodes found.\n";
	abort();
    }
    return true;
}

void GCNode::cleanup()
{
    GCManager::cleanup();
    GCStackRootBase::cleanup();
    GCRootBase::cleanup();
    delete [] s_next_gen;
    for (unsigned int gen = 1; gen < s_num_generations; ++gen) {
	s_reachable[gen].freeLinks();
	s_generation[gen].freeLinks();
    }
    delete [] s_reachable;
    s_aged_list->freeLinks();
    delete s_aged_list;
    delete [] s_generation;
}

void GCNode::gc(unsigned int num_old_gens_to_collect)
{
    // cout << "GCNode::gc(" << num_old_gens_to_collect << ")\n";
    // GCNode::check();
    // cout << "Precheck completed OK: " << s_num_nodes << " nodes\n";

    if (s_under_construction != 0) {
	cerr << "GCNode::gc() : GC must not be used"
	    " while a GCNode is under construction.\n";
	abort();
    }
    unsigned int max_generation = num_old_gens_to_collect + 1;
    propagateAges();
    mark(max_generation);
    sweep(max_generation);

    // cout << "Finishing garbage collection\n";
    // GCNode::check();
    // cout << "Postcheck completed OK: " << s_num_nodes << " nodes\n";
}

void GCNode::initialize()
{
    s_num_generations = GCManager::numGenerations();
    if (s_num_generations > 4) {
	cerr << "GCNode::initialize(): "
	    "current implementation can handle at most 4 generations.\n";
	abort();
    }
    s_generation = new List[s_num_generations];
    s_aged_list = new List;
    s_reachable = new List[s_num_generations];
    for (unsigned int i = 1; i < 4; ++i) {
	s_lists[i] = &s_generation[i];
	s_lists[i+4] = s_aged_list;
    }
    s_next_gen = new unsigned int[s_num_generations];
    for (unsigned int gen = 1; gen < s_num_generations; ++gen)
	s_next_gen[gen] = gen + 1;
    s_next_gen[s_num_generations - 1] = s_num_generations - 1;
    GCRootBase::initialize();
    GCStackRootBase::initialize();
    GCManager::initialize();
}

// GCNode::mark() is in memory.cpp (for the time being)

void GCNode::nodeCheck(const GCNode* node)
{
    if (node && (node->m_bits & ~(LIST|MARK))) abort();
}

void GCNode::propagateAges()
{
    while (!s_aged_list->empty()) {
	const GCNode* node = s_aged_list->front();
	node->m_bits &= ~AGED;
	node->list()->splice_back(node);
	Ager ager(node->generation());
	node->visitReferents(&ager);
    }
}

void GCNode::sweep(unsigned int max_generation)
{
    // Zap nodes in the collected generations that haven't been moved
    // to a reachable list (i.e. are unreachable):
    for (unsigned int gen = 1; gen <= max_generation; ++gen)
	s_generation[gen].clear();
    // Transfer the s_reachable lists to the relevant generation list:
    for (unsigned int gen = 1; gen < s_num_generations; ++gen)
	s_generation[gen].splice_back(&s_reachable[gen]);
}

bool GCNode::Ager::operator()(const GCNode* node)
{
    if (node->generation() >= m_mingen)
	return false;
    node->m_bits &= ~(AGED | GENERATION);
    node->m_bits |= m_mingen;
    node->list()->splice_back(node);
    return true;
}

bool GCNode::Marker::operator()(const GCNode* node)
{
    unsigned int gen = node->generation();
    if (node->isMarked() || gen > m_maxgen)
	return false;
    // Update mark and adjust generation:
    unsigned int next_gen = s_next_gen[gen];
    node->m_bits &= ~(MARK | GENERATION);
    node->m_bits |= (s_mark | next_gen);
    s_reachable[next_gen].splice_back(node);
    return true;
}

bool GCNode::OldToNewChecker::operator()(const GCNode* node)
{
    if (node->generation() < m_mingen) {
	cerr << "GCNode: old to new reference found.\n";
	abort();
    }
    return false;
}

