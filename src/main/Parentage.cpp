#include "CXXR/Parentage.hpp"

using namespace std;
using namespace CXXR;

Parentage::Parentage() : std::vector<GCEdge<Provenance> >(2048,GCEdge<Provenance>(0)), index(0) { }

// Display method, mostly for debugging purposes
void Parentage::Display() const {
	printf("Printing Parentage..size() = %d\n",size());
	for (unsigned int i=0;i<size();i++) {
		GCRoot<Provenance> p(at(i));
		printf("Symbol Name : %s\n",p->getSymbol()->name()->c_str());
	}
}

GCStackRoot<StringVector> Parentage::asStringVector() {
	GCStackRoot<StringVector> rc(expose(new StringVector(index)));
	for (unsigned int i=0;i<size();i++) {
		Provenance *p=at(i);
		(*rc)[i]=const_cast<CachedString*>(p->getSymbol()->name());
	}
	return rc;
}

void Parentage::pushProvenance(Provenance* prov) {
	at(index++).retarget(this,prov);
}

std::vector<GCEdge<Provenance> >::size_type Parentage::size() const {
	return index;
}

void Parentage::visitReferents(const_visitor* v) const {
	for (unsigned int i=0;i<size();i++) {
		//cout<<"Parentage::at("<<i<<")..."<<endl;
		GCNode* prov=at(i);
		if (prov) prov->conductVisitor(v);
	}
}
