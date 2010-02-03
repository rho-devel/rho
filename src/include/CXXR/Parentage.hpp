#ifndef PARENTAGE_HPP
#define PARENTAGE_HPP

#ifdef __cplusplus

#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/GCStackRoot.h"
#include "CXXR/StringVector.h"
#include "CXXR/RObject.h"
#include "CXXR/Provenance.hpp"

namespace CXXR {
	class Parentage : public std::vector<GCEdge<Provenance> > {
	public:

	class Protector : public GCNode {
	public:
		Protector() : p_parentage(0) {}
		void detachReferents();
		Parentage* parentage();
		void set(Parentage*);
		void visitReferents(const_visitor*) const;
	private:
		Parentage* p_parentage;
	};
		
	Parentage() : p_refcount(0) { };
	GCStackRoot<StringVector> asStringVector();
	unsigned long decRefCount();
	void Display() const;
	unsigned long incRefCount();
	void pushProvenance(Provenance*);
	
	private:
	unsigned long p_refcount;
	};
}


#endif // CPP
#endif // PARENTAGE_HPP
