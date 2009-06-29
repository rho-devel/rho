#include <sys/time.h>
#include <ctime>
#include <set>
#include "CXXR/Provenance.hpp"
#include "CXXR/Parentage.hpp"

using namespace std;
using namespace CXXR;

Provenance::Provenance(Expression* exp, Symbol* sym, Parentage* par) {
	m_expression=(exp)?exp->clone():NULL;
	m_symbol=sym;
	m_parentage=par;
	m_parentpos=(m_parentage) ? m_parentage->size() : 0;
	gettimeofday(&m_timestamp,NULL);
	m_children=new Set();
	announceBirth();
}

Provenance::~Provenance() {
	delete m_children;
}

Provenance::Set* Provenance::ancestors(Set* open) {
	Set *closed;
	closed=new Set();

	while (!open->empty()) {
		Provenance* n=*(open->begin());
		Parentage* p=n->getParentage();
		if (p) {
			for (unsigned int i=0;i<p->size();i++) {
				Provenance* s=p->at(i);
				// If s isn't in closed set, put it in open
				if (closed->find(s)==closed->end())
					open->insert(s);
			}
		}
		open->erase(n);
		closed->insert(n);
	}
	return closed;
}

Provenance::Set* Provenance::descendants(Set* open) {
	Set *closed;
	closed=new Set();

	while (!open->empty()) {
		Provenance* n=*(open->begin());
		Set* c=n->children();
		for (Set::iterator it=c->begin();
		     it!=c->end();
		     ++it) {
			Provenance* s=(*it);
			// If s isn't in closed set, put it in open
			if (closed->find(s)==closed->end())
				open->insert(s);
		}
		open->erase(n);
		closed->insert(n);
	}
	return closed;
}

void Provenance::announceBirth() {
	if (!m_parentage)
		return;
	for (Parentage::iterator it=m_parentage->begin();
	     it!=m_parentage->end();
	     ++it)
		(*it)->registerChild(this);
}

void Provenance::announceDeath() {
	if (!m_parentage) return;
	for (unsigned int i=0;
	     i<m_parentpos;
	     ++i) {
		Provenance* p=(*m_parentage).at(i);
		p->deregisterChild(this);
	}
}

Provenance::Set* Provenance::children() const {
	return m_children;
}

void Provenance::deregisterChild(Provenance* child) {
	m_children->erase(child);
}

Expression* Provenance::getCommand() const {
	return m_expression;
}

Symbol* Provenance::getSymbol() const {
	return m_symbol;
}

Parentage* Provenance::getParentage() const {
	return m_parentage;
}

const CachedString* Provenance::getTime() const{
	struct tm *lt;
	char buffer[32];
	size_t p;

	lt=localtime(&m_timestamp.tv_sec);
	p=strftime(buffer,32,"%x %X",lt);
	sprintf(&buffer[p],".%ld",m_timestamp.tv_usec);
	return CachedString::obtain(buffer);
}

void Provenance::detachReferents() {
	m_expression.detach();
	m_symbol.detach();
	announceDeath();
	m_parentage.detach();
}

void Provenance::registerChild(Provenance* child) {
	m_children->insert(child);
}

GCStackRoot<StringVector> Provenance::setAsStringVector(Set* s) {
	GCStackRoot<StringVector> rc(expose(new StringVector(s->size())));
	unsigned int i=0;
	for (Set::iterator it=s->begin();
	     it!=s->end();
	     ++it) {
		Provenance* p=(*it);
		(*rc)[i++]=const_cast<CachedString*>(p->getSymbol()->name());
	}
	return rc;
}

void Provenance::visitReferents(const_visitor* v) const {
	const GCNode* exp=m_expression;
	const GCNode* sym=m_symbol;
	const GCNode* par=m_parentage;
	if (exp) exp->conductVisitor(v);
	if (sym) sym->conductVisitor(v);
	if (par) par->conductVisitor(v);
}

Provenance::Set *Provenance::pedigree() {
	Set *open=new Set(), *rc;
	open->insert(this);
	rc=ancestors(open);
	delete open;
	return rc;
}
