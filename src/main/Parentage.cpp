#include "CXXR/Parentage.hpp"

using namespace std;
using namespace CXXR;

Parentage::Parentage() { }

// Display method, mostly for debugging purposes
void Parentage::Display() const {
	printf("Printing Parentage..size() = %d\n",size());
	for (unsigned int i=0;i<size();i++) {
		GCRoot<Provenance> p(at(i));
		printf("Symbol Name : %s\n",p->getSymbol()->name()->c_str());
	}
}

GCStackRoot<StringVector> Parentage::asStringVector() {
	GCStackRoot<StringVector> rc(expose(new StringVector(size())));
	for (unsigned int i=0;i<size();i++) {
		Provenance *p=at(i);
		(*rc)[i]=const_cast<CachedString*>(p->getSymbol()->name());
	}
	return rc;
}

void Parentage::pushProvenance(Provenance* prov) {
	GCEdge<Provenance> tmp(prov);
	push_back(tmp);
}

void Parentage::detachReferents() { 
	for (unsigned int i=0;i<size();i++) {
		at(i).detach();
	}
}

void Parentage::visitReferents(const_visitor* v) const {
	for (unsigned int i=0;i<size();i++) {
		GCNode* prov=at(i);
		if (prov) prov->conductVisitor(v);
	}
}
