#ifndef PROVENANCE_HPP
#define PROVENANCE_HPP

#ifdef __cplusplus

#include <sys/time.h>
#include <ctime>
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/RObject.h"
#include "CXXR/CachedString.h"
#include "CXXR/Expression.h"
#include "CXXR/Symbol.h"

namespace CXXR {
	class Provenance : public GCNode {
	public:
	Provenance(Expression*,Symbol*);
	Expression* getCommand() const;
	Symbol* getSymbol() const;
	const CachedString* getTime() const;
	void visitReferents(const_visitor*) const;

	private:
	struct timeval m_timestamp;
	GCEdge<Expression> m_expression;
	GCEdge<Symbol> m_symbol;
	};
}

#endif
#endif
