#include "CXXR/ProvenanceTracker.hpp"
#include "CXXR/Parentage.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Frame.hpp"

using namespace std;
using namespace CXXR;

GCRoot<Parentage>* ProvenanceTracker::p_current;

Parentage* ProvenanceTracker::parentage() {
	return *p_current;
}

void ProvenanceTracker::resetParentage() {
	(*p_current)=GCNode::expose(new Parentage);
}

void ProvenanceTracker::readMonitor(const Frame::Binding& bdg) { 
	Frame::Binding& b=const_cast<Frame::Binding&>(bdg);
	Provenance* p=const_cast<Provenance*>(b.getProvenance());

	ProvenanceTracker::parentage()->pushProvenance(p);
}

void ProvenanceTracker::writeMonitor(const Frame::Binding &bind) {
        CXXR::Frame::Binding& bdg=const_cast<CXXR::Frame::Binding&>(bind);

        Expression* expr=static_cast<Expression*>(R_CurrentExpr);
        Symbol* sym=const_cast<Symbol*>(bind.symbol());
        Parentage* parentage=ProvenanceTracker::parentage();
        bdg.setProvenance(GCNode::expose(
                new Provenance(expr,sym,(parentage->size() ? parentage : 0))
        ));

	if (parentage->size())
		ProvenanceTracker::resetParentage();
}

void ProvenanceTracker::cleanup() {
	delete p_current;
}

void ProvenanceTracker::initialize() { 
	p_current=new GCRoot<Parentage>(GCNode::expose(new Parentage()));
}
