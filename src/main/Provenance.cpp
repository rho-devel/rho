#include <sys/time.h>
#include <ctime>
#include "CXXR/Provenance.hpp"

using namespace CXXR;

Provenance::Provenance(Expression* exp, Symbol* sym) {
	m_expression.retarget(this,expose(exp->clone()));
	m_symbol.retarget(this,sym);
	gettimeofday(&m_timestamp,NULL);
}

Expression* Provenance::getCommand() const {
	return m_expression;
}

Symbol* Provenance::getSymbol() const {
	return m_symbol;
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
	if (exp) exp->conductVisitor(v);
}
