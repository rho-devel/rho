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

using namespace std;
using namespace CXXR;

Provenance::Provenance() { }

Provenance::Provenance(const Expression* exp, Symbol* sym, Parentage* par)
    : m_symbol(sym), m_parentage(par), m_xenogenous(false)
{
	m_expression=(exp)?exp->clone():NULL;
	if (m_parentage) {
		m_parentage->incRefCount(); // Increment reference count
		m_parentpos=m_parentage->size();
	}
	
	gettimeofday(&m_timestamp,NULL);
	m_children=new Set();

	announceBirth();
}

Provenance::~Provenance() {
	announceDeath(); // Necessary house-keeping
	delete m_children;
}

Provenance::Set* Provenance::ancestors(Set* open) {
	Set *closed;
	closed=new Set();

	while (!open->empty()) {
		Provenance* n=*(open->begin());
		Parentage* p=n->getParentage();
		if (p) {
			for (unsigned int i=0;i<p->size();i++) {
				Provenance* s=p->at(i);
				// If s isn't in closed set, put it in open
				if (closed->find(s)==closed->end())
					open->insert(s);
			}
		}
		open->erase(n);
		closed->insert(n);
	}
	return closed;
}

Provenance::Set* Provenance::descendants(Set* open) {
	Set *closed;
	closed=new Set();

	while (!open->empty()) {
		Provenance* n=*(open->begin());
		Set* c=n->children();
		for (Set::iterator it=c->begin();
		     it!=c->end();
		     ++it) {
			Provenance* s=(*it);
			// If s isn't in closed set, put it in open
			if (closed->find(s)==closed->end())
				open->insert(s);
		}
		open->erase(n);
		closed->insert(n);
	}
	return closed;
}

void Provenance::announceBirth() {
	if (!m_parentage)
		return;
	for (unsigned int i=0;i<m_parentpos;++i)
		m_parentage->at(i)->registerChild(this);
}

void Provenance::announceDeath() {
	if (!m_parentage) return;
	// Firstly, tell all of our parents we're dying
	for (unsigned int i=0;i<m_parentpos;i++)
		m_parentage->at(i)->deregisterChild(this);
	// If this is the last Provenance refering to this Parentage
	// then we should destroy it.
	if (!m_parentage->decRefCount()) {
		for (Parentage::iterator it=m_parentage->begin();
		     it!=m_parentage->end();
		     ++it)
			(*it).detach();
		delete m_parentage;
	}
	m_parentage=NULL;
}

Provenance::Set* Provenance::children() const {
	return m_children;
}

void Provenance::deregisterChild(Provenance* child) {
	m_children->erase(child);
}

Expression* Provenance::getCommand() const {
	return m_expression;
}

Symbol* Provenance::getSymbol() const {
	return m_symbol;
}

Parentage* Provenance::getParentage() const {
	return m_parentage;
}

const String* Provenance::getTime() const{
	struct tm *lt;
	char buffer[32];
	size_t p;

	lt=localtime(&m_timestamp.tv_sec);
	p=strftime(buffer,32,"%x %X",lt);
	sprintf(&buffer[p],".%ld",m_timestamp.tv_usec);
	return String::obtain(buffer);
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

void Provenance::detachReferents() {
	m_expression.detach();
	m_symbol.detach();
	announceDeath(); // Do necessary house-keeping
}

void Provenance::registerChild(Provenance* child) {
	m_children->insert(child);
}

GCStackRoot<StringVector> Provenance::setAsStringVector(Set* s) {
	GCStackRoot<StringVector> rc(expose(new StringVector(s->size())));
	unsigned int i=0;
	for (Set::iterator it=s->begin();
	     it!=s->end();
	     ++it) {
		Provenance* p=(*it);
		(*rc)[i++]=const_cast<String*>(p->getSymbol()->name());
	}
	return rc;
}

void Provenance::visitReferents(const_visitor* v) const {
	const GCNode* exp=m_expression;
	const GCNode* sym=m_symbol;
	const GCNode* value = m_value;
	if (exp)
	    (*v)(exp);
	if (sym)
	    (*v)(sym);
	if (value)
	    (*v)(value);
	if (m_parentage) { // cas : manually conduct to parents
		for (unsigned int i=0;
		     i<m_parentage->size();
		     i++) {
			const GCNode* rent=m_parentage->at(i);
			(*v)(rent);
		}
	}
}

Provenance::Set *Provenance::pedigree() {
	Set *open=new Set(), *rc;
	open->insert(this);
	rc=ancestors(open);
	delete open;
	return rc;
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::Provenance)
