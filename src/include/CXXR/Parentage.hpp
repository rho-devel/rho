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
	class Parentage :
		public GCNode, public std::vector<GCEdge<Provenance> > {
	public:
	Parentage();
	void Display() const;
	GCStackRoot<StringVector> asStringVector();
	void pushProvenance(Provenance*);
	
	// Virtual method of GCNode
	void detachReferents();
	void visitReferents(const_visitor*) const;
	};
}


#endif // CPP
#endif // PARENTAGE_HPP
