#include "CXXR/ProvenanceSet.hpp"

using namespace std;
using namespace CXXR;

void ProvenanceSet::detachReferents() {
	clear();
}

void ProvenanceSet::visitReferents(const_visitor* v) const {
	for (iterator it=begin();
	     it!=end();
	     ++it) {
		GCNode* prov=(*it);
		if (prov)
		    (*v)(prov);
	}
}
