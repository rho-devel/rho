#include <sys/time.h>
#include <ctime>
#include "CXXR/Provenance.hpp"
#include "CXXR/Parentage.hpp"

using namespace CXXR;

Provenance::Provenance(Expression* exp, Symbol* sym, Parentage* par) {
	if (!exp)
		m_expression.retarget(this,0);
	else {
		GCStackRoot<Expression> expCpy(exp->clone());
		if (expCpy!=NULL)
			m_expression.retarget(this,expCpy);
		else
			m_expression.retarget(this,0);
	}
	m_symbol.retarget(this,sym);
	m_parentage.retarget(this,par);
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

void Provenance::visitReferents(const_visitor* v) const {
	const GCNode* exp=m_expression;
	const GCNode* sym=m_symbol;
	const GCNode* par=m_parentage;
	if (exp) exp->conductVisitor(v);
	if (sym) sym->conductVisitor(v);
	if (par) par->conductVisitor(v);
}
