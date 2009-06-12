#ifndef PROVENANCE_HPP
#define PROVENANCE_HPP

#ifdef __cplusplus

#include <sys/time.h>
#include <ctime>
#include <set>
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/RObject.h"
#include "CXXR/CachedString.h"
#include "CXXR/Expression.h"
#include "CXXR/Symbol.h"

namespace CXXR {
	class Parentage;

	class Provenance : public GCNode {
	public:
		class CompTime {
		public:
			bool operator()(Provenance* lhs, Provenance* rhs) {
				return (lhs->m_timestamp.tv_sec==rhs->m_timestamp.tv_sec) ?
					(lhs->m_timestamp.tv_usec<rhs->m_timestamp.tv_usec) :
					(lhs->m_timestamp.tv_sec<rhs->m_timestamp.tv_sec);
			}
		};
		typedef std::set<Provenance*,Provenance::CompTime> Set;
		Provenance(Expression*,Symbol*,Parentage*);
		Expression* getCommand() const;
		Symbol* getSymbol() const;
		Parentage* getParentage() const;
		const CachedString* getTime() const;
		void detachReferents();
		void visitReferents(const_visitor*) const;
		Set* pedigree(void);

	private:
		struct timeval m_timestamp;
		GCEdge<Expression> m_expression;
		GCEdge<Symbol> m_symbol;
		GCEdge<Parentage> m_parentage;
	};
} // Namespace CXXR

#endif
#endif
