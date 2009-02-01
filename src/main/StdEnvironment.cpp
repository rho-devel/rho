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

/** @file StdEnvironment.cpp
 *
 *
 * @brief Implementation of class CXXR:StdEnvironment.
 */

// A StdEnvironment is implemented using two data structures.  First
// there is a PairList, each of whose elements represents a binding,
// and so maps a symbol (held as the tag) to a value (held as the
// 'car'), and also contains information about locking, active binding
// etc.  Secondly there is an unordered_map (i.e. hash table) which
// maps symbols to elements of the PairList.  Operations on the
// PairList are always done via the unordered_map.  When a symbol is
// erased from the enviroment, the continuity of the PairList will be
// broken, and in this event the PairList is marked as stale.  The
// private function refreshFrameList() is invoked when necessary to
// restring the PairList by iterating over the hash table.

#include "CXXR/StdEnvironment.hpp"

#include <cmath>
#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

GCRoot<Environment> Environment::s_empty_env(new StdEnvironment, true);
GCRoot<Environment> Environment::s_base_env(new StdEnvironment(s_empty_env),
					    true);
GCRoot<Environment> Environment::s_global_env(new StdEnvironment(s_base_env),
					      true);

// We want to be able to determine quickly if a symbol is *not*
// defined in an environment, so that we can carry on working up the
// chain of enclosing environments.  On average the number of tests
// needed to determine that a symbol is not present is 1 + 2L, where L
// is the load factor.  So we keep the load factor small:
namespace {
    const float maximum_load_factor = 0.5;
}

StdEnvironment::StdEnvironment(Environment* enclosing, size_t initial_capacity)
    : Environment(enclosing), m_framelist(0), m_frame_stale(false),
      m_map(ceil(initial_capacity/maximum_load_factor))
{
    m_map.max_load_factor(maximum_load_factor);
}

PairList* StdEnvironment::frameBinding(const Symbol* symbol)
{
    map::iterator it = m_map.find(symbol);
    if (it == m_map.end())
	return 0;
    return (*it).second;
}

const PairList* StdEnvironment::frameBinding(const Symbol* symbol) const
{
    map::const_iterator it = m_map.find(symbol);
    if (it == m_map.end())
	return 0;
    return (*it).second;
}

void StdEnvironment::clear()
{
    m_map.clear();
    m_framelist = 0;
    m_frame_stale = false;
}

bool StdEnvironment::frameErase(const Symbol* symbol)
{
    bool erased = m_map.erase(symbol);
    if (erased)
	m_frame_stale = true;
    return erased;
}

const PairList* StdEnvironment::frameList() const
{
    if (m_frame_stale)
	refreshFrameList();
    return m_framelist;
}

PairList* StdEnvironment::frameObtainBinding(const Symbol* symbol)
{
    PairList*& bdg = m_map[symbol];
    if (!bdg) {
	if (isLocked())
	    error(_("cannot add bindings to a locked environment"));
	// We delay setting the tail, in case the 'new' results in a
	// garbage collection which leads to the frame list being restrung:
	bdg = new PairList(0, 0, const_cast<Symbol*>(symbol));
	propagateAge(bdg);
	bdg->setTail(m_framelist);
	m_framelist = bdg;
    }
    return bdg;
}

void StdEnvironment::refreshFrameList() const
{
    m_framelist = 0;
    for (map::const_iterator it = m_map.begin(); it != m_map.end(); ++it) {
	PairList* pl = (*it).second;
	// Beware that pl may be null, if this function is invoked by
	// a garbage collection in the course of frameObtainBinding():
	if (pl) {
	    pl->setTail(m_framelist);
	    m_framelist = pl;
	}
    }
    m_frame_stale = false;
}

size_t StdEnvironment::size() const
{
    return m_map.size();
}

void StdEnvironment::visitChildren(const_visitor* v) const
{
    Environment::visitChildren(v);
    if (m_frame_stale)
	refreshFrameList();
    if (m_framelist) m_framelist->conductVisitor(v);
}
