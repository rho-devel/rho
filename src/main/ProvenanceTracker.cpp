#include "CXXR/ProvenanceTracker.hpp"
#include "CXXR/Parentage.hpp"
#include "CXXR/Provenance.hpp"
#include "CXXR/ProvenanceSet.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Frame.hpp"
#include "Defn.h"

using namespace std;
using namespace CXXR;

GCRoot<Parentage>* ProvenanceTracker::p_current;
GCRoot<ProvenanceSet>* ProvenanceTracker::p_seen;

Parentage* ProvenanceTracker::parentage() {
	return *p_current;
}

ProvenanceSet* ProvenanceTracker::seen() {
	return *p_seen;
}

void ProvenanceTracker::resetParentage() {
	(*p_current)=GCNode::expose(new Parentage);
	(*p_seen)=GCNode::expose(new ProvenanceSet);
}

void ProvenanceTracker::readMonitor(const Frame::Binding& bdg) { 
	Frame::Binding& b=const_cast<Frame::Binding&>(bdg);
	Provenance* p=const_cast<Provenance*>(b.getProvenance());
	// If 'p' has not been written to
	GCEdge<Provenance> needle(p);
	if (seen()->find(needle)==seen()->end())
		parentage()->pushProvenance(p);
	GCEdge<Provenance> tmp(p);
	seen()->insert(tmp);
}

void ProvenanceTracker::writeMonitor(const Frame::Binding &bind) {
        CXXR::Frame::Binding& bdg=const_cast<CXXR::Frame::Binding&>(bind);
	RObject* e=R_CurrentExpr;
        Expression* expr=static_cast<Expression*>(e);
        Symbol* sym=const_cast<Symbol*>(bind.symbol());
        Parentage* parentage=ProvenanceTracker::parentage();


        bdg.setProvenance(GCNode::expose(
                new Provenance(expr,sym,(parentage->size() ? parentage : 0))
        ));
	Provenance* prov=const_cast<Provenance*>(bdg.getProvenance());

	GCEdge<Provenance> tmp(prov);
	seen()->insert(tmp);
}

void ProvenanceTracker::cleanup() {
	delete p_current;
	delete p_seen;
}

void ProvenanceTracker::initialize() { 
	p_current=new GCRoot<Parentage>(GCNode::expose(new Parentage()));
	p_seen=new GCRoot<ProvenanceSet>(GCNode::expose(new ProvenanceSet()));
}
