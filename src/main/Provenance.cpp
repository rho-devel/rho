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

/* This file incorporates material Copyright (C) Chris A. Silles 2009-12.
 */

/*
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

#include <sys/time.h>
#include <cstdio>
#include <ctime>
#include <set>
#include "CXXR/Provenance.hpp"
#include "CXXR/Parentage.hpp"

using namespace CXXR;

Provenance::Provenance(const Expression* exp, const Symbol* sym,
		       Parentage* par)
    : m_expression(0), m_symbol(sym), m_parentage(par), m_xenogenous(false)
{
    if (exp)
	m_expression = exp->clone();
    if (m_parentage) {
	m_parentage->incRefCount(); // Increment reference count
	m_parentpos=m_parentage->size();
    }
    gettimeofday(&m_timestamp, 0);
    announceBirth();
}

Provenance::Set* Provenance::ancestors(Set* open)
{
    Set* closed = new Set();
    while (!open->empty()) {
	const Provenance* n = *(open->begin());
	const Parentage* p = n->getParentage();
	if (p) {
	    for (unsigned int i = 0; i < p->size(); ++i) {
		const Provenance* s = (*p)[i];
		// If s isn't in closed set, put it in open
		if (closed->find(s) == closed->end())
		    open->insert(s);
	    }
	}
	open->erase(n);
	closed->insert(n);
    }
    return closed;
}

void Provenance::announceBirth()
{
    if (!m_parentage)
	return;
    for (unsigned int i = 0; i < m_parentpos; ++i)
	(*m_parentage)[i]->registerChild(this);
}

void Provenance::announceDeath()
{
    if (!m_parentage)
	return;
    // Firstly, tell all of our parents we're dying
    for (unsigned int i = 0; i < m_parentpos; i++)
	(*m_parentage)[i]->deregisterChild(this);
    // If this is the last Provenance referring to this Parentage
    // then we should destroy it.
    if (!m_parentage->decRefCount())
	delete m_parentage;
    m_parentage = 0;
}

Provenance::Set* Provenance::descendants(Set* open)
{
    Set* closed = new Set();
    while (!open->empty()) {
	const Provenance* n = *(open->begin());
	const Set& c = n->children();
	for (Set::iterator it = c.begin(); it != c.end(); ++it) {
	    const Provenance* s = *it;
	    // If s isn't in closed set, put it in open
	    if (closed->find(s) == closed->end())
		open->insert(s);
	}
	open->erase(n);
	closed->insert(n);
    }
    return closed;
}

void Provenance::detachReferents()
{
    m_expression.detach();
    m_symbol.detach();
    announceDeath(); // Do necessary house-keeping
}

const String* Provenance::getTime() const
{
    char buffer[32];
    struct tm* lt = localtime(&m_timestamp.tv_sec);
    size_t p = strftime(buffer, 32, "%x %X", lt);
    sprintf(&buffer[p], ".%ld", m_timestamp.tv_usec);
    return String::obtain(buffer);
}

Provenance::Set* Provenance::pedigree()
{
    Set *open = new Set(), *rc;
    open->insert(this);
    rc = ancestors(open);
    delete open;
    return rc;
}

StringVector* Provenance::setAsStringVector(const Set& s)
{
    GCStackRoot<StringVector> rc(CXXR_NEW(StringVector(s.size())));
    unsigned int i = 0;
    for (Set::const_iterator it = s.begin(); it != s.end(); ++it) {
	const Provenance* p = *it;
	(*rc)[i++] = const_cast<String*>(p->getSymbol()->name());
    }
    return rc;
}

void Provenance::setXenogenous(const RObject* value)
{
    m_value = value;
    m_xenogenous = true;
}

double Provenance::timestamp() const
{
    return m_timestamp.tv_sec + 1.0E-6*m_timestamp.tv_usec;
}

void Provenance::visitReferents(const_visitor* v) const
{
    if (m_expression)
	(*v)(m_expression);
    if (m_symbol)
	(*v)(m_symbol);
    if (m_value)
	(*v)(m_value);
    if (m_parentage) { // cas : manually conduct to parents
	for (unsigned int i = 0; i < m_parentage->size(); ++i) {
	    const GCNode* rent = (*m_parentage)[i];
	    (*v)(rent);
	}
    }
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::Provenance)
