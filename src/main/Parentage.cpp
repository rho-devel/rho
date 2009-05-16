#include "CXXR/Parentage.hpp"

using namespace std;
using namespace CXXR;

Parentage::Parentage() : std::vector<GCEdge<Provenance> >(20,GCEdge<Provenance>(0)), index(0) {
	printf("Parentage object initialised\n");
}

// Display method, mostly for debugging purposes
void Parentage::Display() {
	printf("Printing Parentage..\n");
	for (int i=0;i<index;i++) {
		Provenance* p=at(i);
		printf("Symbol Name : %s\n",p->getSymbol()->name()->c_str());
	}
}

GCStackRoot<StringVector> Parentage::asStringVector() {
	GCStackRoot<StringVector> rc(expose(new StringVector(index)));
	for (int i=0;i<index;i++) {
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
	return;
}
