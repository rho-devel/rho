#ifndef PROVENANCE_HPP
#define PROVENANCE_HPP

#ifdef __cplusplus

#include <sys/time.h>
#include <ctime>
#include <set>
#include "CXXR/Expression.h"
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/GCStackRoot.h"
#include "CXXR/RObject.h"
#include "CXXR/StringVector.h"
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
		~Provenance();

		static Set* ancestors(Set*);
		static Set* descendants(Set*);
		static GCStackRoot<StringVector> setAsStringVector(Set*);

		Set* children() const;
		void detachReferents();
		Expression* getCommand() const;
		Symbol* getSymbol() const;
		Parentage* getParentage() const;
		const CachedString* getTime() const;
		Set* pedigree();
		void visitReferents(const_visitor*) const;
	private:
		struct timeval m_timestamp;
		unsigned int m_parentpos;
		Set* m_children;
		GCEdge<Expression> m_expression;
		GCEdge<Symbol> m_symbol;
		GCEdge<Parentage> m_parentage;

		void announceBirth();
		void announceDeath();
		void deregisterChild(Provenance*);
		void registerChild(Provenance*);
	};
} // Namespace CXXR

#endif
#endif
