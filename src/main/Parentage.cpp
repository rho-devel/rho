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
