#ifndef PARENTAGE_HPP
#define PARENTAGE_HPP

#ifdef __cplusplus

#include <vector>
#include <boost/serialization/access.hpp>
#include <boost/serialization/split_member.hpp>
#include <boost/serialization/vector.hpp>
#include "CXXR/BSerializer.hpp"
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/StringVector.h"
#include "CXXR/RObject.h"

namespace CXXR {
	class Provenance;
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
	friend class boost::serialization::access;

	template<class Archive>
	void load(Archive & ar, const unsigned int version) {
	    unsigned int sz;
	    ar >> sz;
	    for (unsigned int i=0;i<sz;i++) {
		Provenance *p;
		ar >> p;
		GCNode::expose(p);
		pushProvenance(p);
	    }
	}

	template<class Archive>
	void save(Archive & ar, const unsigned int version) const {
	    unsigned int sz=size();
	    ar << sz;
	    for (unsigned int i=0;i<sz;i++) {
		Provenance *p=at(i);
		ar << p;
	    }
	}

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("Parentage");
	    boost::serialization::split_member(ar, *this, version);
	}

	unsigned long p_refcount;
	};
}


#endif // CPP
#endif // PARENTAGE_HPP
