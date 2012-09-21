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

#include <cstdio>

#include "CXXR/Parentage.hpp"
#include "CXXR/Provenance.hpp"

using namespace std;
using namespace CXXR;

// ***** Class Parentage::Protector *****

void Parentage::Protector::detachReferents() { }

Parentage* Parentage::Protector::parentage() {
    return p_parentage;
}

void Parentage::Protector::set(Parentage* p) {
    if (p_parentage) // We'll be discarding a reference to c.par.
	if (!p_parentage->decRefCount())
	    delete p_parentage;
    p_parentage=p;
    p_parentage->incRefCount();
}

void Parentage::Protector::visitReferents(const_visitor* v) const {
    for (Parentage::iterator it=p_parentage->begin();
	 it!=p_parentage->end();
	 ++it) {
	const GCNode* rent=*it;
	(*v)(rent);
    }
}

// ***** Class Parentage *****

// Display method, mostly for debugging purposes
GCStackRoot<StringVector> Parentage::asStringVector() {
    GCStackRoot<StringVector> rc(GCNode::expose(new StringVector(size())));
    for (unsigned int i=0;i<size();i++) {
	Provenance *p=at(i);
	(*rc)[i]=const_cast<String*>(p->getSymbol()->name());
    }
    return rc;
}

unsigned long Parentage::decRefCount() {
    return --p_refcount;
}

void Parentage::Display() const {
    std::cout << "Printing Parentage..size() = " << size() << '\n';
    for (unsigned int i=0;i<size();i++) {
	GCRoot<Provenance> p(at(i));
	Provenance* p2=p;
	std::cout << "Symbol Name : " << p->getSymbol()->name()->c_str()
	     << "Prov addr : " << p2 << endl;
    }
}

unsigned long Parentage::incRefCount() {
    return ++p_refcount;
}

void Parentage::pushProvenance(Provenance* prov) {
    GCEdge<Provenance> tmp(prov);
    push_back(tmp);
}
