/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) Chris A. Silles 2009-12.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
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
 *  https://www.R-project.org/Licenses/
 */

#include "CXXR/Provenance.hpp"

#include <sys/time.h>
#include <cstdio>
#include <ctime>
#include <set>

using namespace CXXR;

unsigned int Provenance::s_next_serial = 0;

Provenance::Provenance(const Symbol* sym, const CommandChronicle* chron)
    : m_serial(s_next_serial++), m_xenogenous(false)
{
    m_symbol = sym;
    m_chronicle = chron;
    m_num_parents = m_chronicle->bindingsRead().size();
    gettimeofday(&m_timestamp, nullptr);
    announceBirth();
}

Provenance::Set* Provenance::ancestors(const Set& roots)
{
    Set open = roots;
    Set* closed = new Set();
    while (!open.empty()) {
	const Provenance* n = *(open.begin());
	std::pair<CommandChronicle::ParentVector::const_iterator,
	          CommandChronicle::ParentVector::const_iterator>
	    pr = n->parents();
	for (CommandChronicle::ParentVector::const_iterator it = pr.first;
	     it != pr.second; ++it) {
	    const Provenance* prov = *it;
	    if (closed->count(prov) == 0)
		open.insert(prov);
	}
	open.erase(n);
	closed->insert(n);
    }
    return closed;
}

void Provenance::announceBirth()
{
    std::pair<CommandChronicle::ParentVector::const_iterator,
	      CommandChronicle::ParentVector::const_iterator>
        pr = parents();
    for (CommandChronicle::ParentVector::const_iterator it = pr.first;
	 it != pr.second; ++it)
	(*it)->registerChild(this);
}

void Provenance::announceDeath()
{
    // During a mark-sweep garbage collection, m_chronicle may have
    // been detached:
    if (!m_chronicle)
	return;
    std::pair<CommandChronicle::ParentVector::const_iterator,
	      CommandChronicle::ParentVector::const_iterator>
        pr = parents();
    for (CommandChronicle::ParentVector::const_iterator it = pr.first;
	 it != pr.second; ++it)
	(*it)->deregisterChild(this);
}

Provenance::Set* Provenance::descendants(const Set& roots)
{
    Set open = roots;
    Set* closed = new Set();
    while (!open.empty()) {
	const Provenance* n = *(open.begin());
	const Set& c = n->children();
	for (const Provenance* s : c) {
	    // If s isn't in closed set, put it in open
	    if (closed->find(s) == closed->end())
		open.insert(s);
	}
	open.erase(n);
	closed->insert(n);
    }
    return closed;
}

void Provenance::detachReferents()
{
    m_symbol.detach();
    announceDeath(); // Do necessary house-keeping
    m_chronicle.detach();
    m_value.detach();
}

const String* Provenance::getTime() const
{
    char buffer[32];
    struct tm* lt = localtime(&m_timestamp.tv_sec);
    size_t p = strftime(buffer, 32, "%x %X", lt);
    sprintf(&buffer[p], ".%ld", m_timestamp.tv_usec);
    return String::obtain(buffer);
}

std::pair<CommandChronicle::ParentVector::const_iterator,
	  CommandChronicle::ParentVector::const_iterator>
Provenance::parents() const
{
    CommandChronicle::ParentVector::const_iterator bgn
	= m_chronicle->bindingsRead().begin();
    return make_pair(bgn, bgn + std::ptrdiff_t(m_num_parents));
}

void Provenance::setXenogenous(const RObject* value)
{
    m_value = value;
    m_xenogenous = true;
}

double Provenance::timestamp() const
{
    return double(m_timestamp.tv_sec) + 1.0E-6*double(m_timestamp.tv_usec);
}

void Provenance::visitReferents(const_visitor* v) const
{
    if (m_symbol)
	(*v)(m_symbol);
    if (m_chronicle)
	(*v)(m_chronicle);
    if (m_value)
	(*v)(m_value);
}
