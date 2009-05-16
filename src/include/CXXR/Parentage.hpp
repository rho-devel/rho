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
	void Display();
	GCStackRoot<StringVector> asStringVector();
	void pushProvenance(Provenance*);
	size_type size() const;
	
	// Virtual method of GCNode
	void visitReferents(const_visitor*) const;
	
	int index;
	};
}


#endif // CPP
#endif // PARENTAGE_HPP
