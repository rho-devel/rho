#ifndef PROVENANCESET_HPP
#define PROVENANCESET_HPP

#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/Provenance.hpp"

namespace CXXR {
	class ProvenanceSet :
		public GCNode, public std::set<GCEdge<Provenance>,Provenance::CompTime> {
	public:
	
	//virtual methods of GCNode
	void detachReferents();
	void visitReferents(const_visitor*) const;
	};

}

#endif
