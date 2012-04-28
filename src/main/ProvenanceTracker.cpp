#include "CXXR/ProvenanceTracker.hpp"
#include "CXXR/Parentage.hpp"
#include "CXXR/Provenance.hpp"
#include "CXXR/ProvenanceSet.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Frame.hpp"
#include "Defn.h"

using namespace std;
using namespace CXXR;

RObject* ProvenanceTracker::e_current;
GCRoot<Parentage::Protector>* ProvenanceTracker::p_current;
GCRoot<ProvenanceSet>* ProvenanceTracker::p_seen;

Expression* ProvenanceTracker::expression() {
	RObject* exp=R_CurrentExpr;
	if (!e_current)
		return static_cast<Expression*>(exp);

	if (TYPEOF(e_current)==EXPRSXP) {
		ExpressionVector* ev=static_cast<ExpressionVector*>(e_current);
		RObject* o=(*ev)[0];
		Expression* e=static_cast<Expression*>(o);
		return e;
	}
	
	return static_cast<Expression*>(exp);
}

void ProvenanceTracker::resetExpression() {
	setExpression(NULL);
}

void ProvenanceTracker::setExpression(RObject* expr) {
	e_current=expr;
}

void ProvenanceTracker::initEnvs() {
    Frame::setReadMonitor(ProvenanceTracker::readMonitor);
    Frame::setWriteMonitor(ProvenanceTracker::writeMonitor);
    Frame* global_frame = Environment::global()->frame();
    global_frame->enableReadMonitoring(true);
    global_frame->enableWriteMonitoring(true);
}

Parentage* ProvenanceTracker::parentage() {
	return (*p_current)->parentage();
}

ProvenanceSet* ProvenanceTracker::seen() {
	return *p_seen;
}

void ProvenanceTracker::resetParentage() {
	(*p_seen)=GCNode::expose(new ProvenanceSet);
	(*p_current)->set(new Parentage());
	return;
}

/*
 * Promises need to be handled slightly differently.
 * As illustrated by the case of lazy-loading, promises may
 * result in a new binding being created.
 * This binding would ordinarily be placed on the 'seen' set,
 * and so not recorded in parentage.
 */
void ProvenanceTracker::forcedPromise(const Frame::Binding& bdg) {
	writeMonitor(bdg,false); // Set up the new Provenance
	                         // but don't add to seen set
}

void ProvenanceTracker::readMonitor(const Frame::Binding& bdg) { 
	Frame::Binding& b=const_cast<Frame::Binding&>(bdg);
#ifdef VERBOSEMONITOR
	cout<<"Read '"<<b.symbol()->name()->c_str()<<"'"<<endl;
#endif
	Provenance* p=const_cast<Provenance*>(b.getProvenance());
	// If 'p' has not been written to
	if (!p) return;
	GCEdge<Provenance> needle(p);
	if (seen()->find(needle)==seen()->end())
		parentage()->pushProvenance(p);
	seen()->insert(needle);
}

/* Default behaviour is that the new object has been seen */
void ProvenanceTracker::writeMonitor(const Frame::Binding &bind) {
	writeMonitor(bind, true);
}

/* Have control over whether or not the object gets added to the seen set */
void ProvenanceTracker::writeMonitor(const Frame::Binding &bind, bool beenSeen) {
        CXXR::Frame::Binding& bdg=const_cast<CXXR::Frame::Binding&>(bind);
#ifdef VERBOSEMONITOR
	cout<<"Write '"<<bdg.symbol()->name()->c_str()<<"'"<<endl;
#endif
	RObject* e=expression();
        Expression* expr=static_cast<Expression*>(e);
        Symbol* sym=const_cast<Symbol*>(bind.symbol());

        bdg.setProvenance(GCNode::expose(
                new Provenance(expr,sym,parentage())
        ));
	Provenance* prov=const_cast<Provenance*>(bdg.getProvenance());

	if (beenSeen) {
		GCEdge<Provenance> tmp(prov);
		seen()->insert(tmp);
	}
}

void ProvenanceTracker::cleanup() {
	delete p_current;
	delete p_seen;
}

void ProvenanceTracker::initialize() {
	e_current=NULL;
	p_current=new GCRoot<Parentage::Protector>(GCNode::expose(new Parentage::Protector()));
	(*p_current)->set(new Parentage());
	p_seen=new GCRoot<ProvenanceSet>(GCNode::expose(new ProvenanceSet()));
}
