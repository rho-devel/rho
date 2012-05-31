#include "CXXR/ProvenanceTracker.h"

#include "CXXR/Parentage.hpp"
#include "CXXR/Provenance.hpp"
#include "CXXR/ProvenanceSet.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Frame.hpp"
#include "Defn.h"

using namespace std;
using namespace CXXR;

const Expression* ProvenanceTracker::e_current;
GCRoot<Parentage::Protector>* ProvenanceTracker::p_current;
GCRoot<ProvenanceSet>* ProvenanceTracker::p_seen;
bool ProvenanceTracker::s_xenogenous = false;

const Expression* ProvenanceTracker::expression() {
    return e_current;
#if 0
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
#endif
}

void ProvenanceTracker::resetExpression() {
	setExpression(NULL);
}

void ProvenanceTracker::setExpression(const RObject* arg) {
    if (arg && arg->sexptype() == EXPRSXP) {
	const ExpressionVector* ev = static_cast<const ExpressionVector*>(arg);
	arg = (*ev)[0];
    }
    // 'arg' could be anything presented for evaluation on the command
    // line, so is not necessarily an Expression.  It could for
    // example be a bare Symbol, or a literal number.  For the moment
    // we treat these odd cases as null expressions, because no
    // bindings should result from evaluating them..  (Oh, but what
    // happens if Promises are forced in the search for a Symbol? -
    // FIXME)
    if (arg && arg->sexptype() != LANGSXP)
	arg = 0;
    e_current = static_cast<const Expression*>(arg);
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
	s_xenogenous = false;
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
        const Expression* expr = expression();
        Symbol* sym=const_cast<Symbol*>(bind.symbol());

        bdg.setProvenance(GCNode::expose(
                new Provenance(expr,sym,parentage())
        ));
	Provenance* prov=const_cast<Provenance*>(bdg.getProvenance());
	if (s_xenogenous)
	    prov->setXenogenous(bdg.rawValue());  // Maybe ought to clone value
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

// ***** C interface *****

void flagXenogenous()
{
    ProvenanceTracker::flagXenogenous();
}
