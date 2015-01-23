/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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

/** @file NodeStack.cpp
 *
 * Implementation of class CXXR::NodeStack.
 */

#include "CXXR/NodeStack.hpp"

#include <algorithm>
#include <iostream>
#include <stdexcept>

using namespace CXXR;

void NodeStack::Scope::nestingError()
{
    std::cerr << "Fatal error:"
	         " NodeStack::Scope objects must be destroyed"
	         " in reverse order of creation\n";
    abort();
}

NodeStack::NodeStack(size_t initial_capacity)
#ifndef NDEBUG
    : m_protected_count(0), m_innermost_scope(0)
#else
    : m_protected_count(0)
#endif
{
    m_vector.reserve(initial_capacity);
}

void NodeStack::eraseTopmost(RObject* node)
{
#ifndef NDEBUG
    if (m_innermost_scope
	&& m_vector.size() == m_innermost_scope->startSize())
	throw std::logic_error("NodeStack::eraseTopmost(): "
			       "too many pops in this scope.");
#endif
    std::vector<RObject*>::reverse_iterator rit
	= find(m_vector.rbegin(), m_vector.rend(), node);
    if (rit == m_vector.rend())
	throw std::invalid_argument("NodeStack::unprotectPtr:"
				    " pointer not found.");
    // See Josuttis p.267 for the need for -1 :
    std::vector<RObject*>::iterator it = rit.base() - 1;
    if (it - m_vector.begin() < int(m_protected_count)) {
	GCNode::decRefCount(node);
	--m_protected_count;
    }
    m_vector.erase(it);
}

// Foll. is inlined under NDEBUG:
#ifndef NDEBUG
void NodeStack::pop(unsigned int count)
{
    size_t sz = m_vector.size();
    if (count > sz)
	throw std::out_of_range("NodeStack::pop(): count greater"
				" than current stack size.");
    if (m_innermost_scope && sz - count < m_innermost_scope->startSize())
	throw std::logic_error("NodeStack::unprotect: too many unprotects"
			       " in this scope.");
    resize(sz - count);
}
#endif

void NodeStack::protectAll()
{
    std::vector<RObject*>::iterator start
	= m_vector.begin() + std::ptrdiff_t(m_protected_count);
    std::vector<RObject*>::iterator end = m_vector.end();
    for (std::vector<RObject*>::iterator it = start; it != end; ++it)
	GCNode::incRefCount(*it);
    m_protected_count = m_vector.size();
}

// Foll. is inlined under NDEBUG:
#ifndef NDEBUG
void NodeStack::retarget(RObject* node, size_t index)
{
    if (index >= m_vector.size())
	throw std::out_of_range("NodeStack::retarget():"
				" index out of range.");
    if (index < m_protected_count)
	retarget_aux(m_vector[index], node);
    m_vector[index] = node;
}
#else
namespace CXXR {
namespace ForceNonInline {
void (NodeStack::*nodeStackPopP)(unsigned int) = &NodeStack::pop;
void (NodeStack::*nodeStackRetargetP)(RObject*, size_t) = &NodeStack::retarget;
}
}
#endif

void NodeStack::retarget_aux(RObject* oldnode, RObject* newnode)
{
    GCNode::incRefCount(newnode);
    GCNode::decRefCount(oldnode);
}

void NodeStack::resize_aux(size_t new_size)
{
    m_vector.resize(m_protected_count);
    while (m_vector.size() > new_size) {
	RObject* node = m_vector.back();
	GCNode::decRefCount(node);
	m_vector.pop_back();
    }
    m_protected_count = new_size;
}

void NodeStack::visitRoots(GCNode::const_visitor* v)
{
    std::vector<RObject*>::iterator vecend = m_vector.end();
    for (std::vector<RObject*>::iterator it = m_vector.begin();
	 it != vecend; ++it) {
	RObject* n = *it;
	if (n)
	    (*v)(n);
    }
}
