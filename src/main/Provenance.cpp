#include <sys/time.h>
#include <ctime>
#include <set>
#include "CXXR/Provenance.hpp"
#include "CXXR/Parentage.hpp"

using namespace std;
using namespace CXXR;

Provenance::Provenance(Expression* exp, Symbol* sym, Parentage* par) {
	if (!exp)
		m_expression=NULL;
	else {
		GCStackRoot<Expression> expCpy(exp->clone());
		if (expCpy!=NULL)
			m_expression=expCpy;
		else
			m_expression=NULL;
	}
	m_symbol=sym;
	m_parentage=par;
	gettimeofday(&m_timestamp,NULL);
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
	m_parentage.detach();
}

void Provenance::visitReferents(const_visitor* v) const {
	const GCNode* exp=m_expression;
	const GCNode* sym=m_symbol;
	const GCNode* par=m_parentage;
	if (exp) exp->conductVisitor(v);
	if (sym) sym->conductVisitor(v);
	if (par) par->conductVisitor(v);
}

Provenance::Set *Provenance::pedigree(void) {
	Set *open, *closed;
	open=new Set();
	closed=new Set();

	open->insert(this);
	
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
	delete open;

	return closed;
}
