#ifndef PARENTAGE_HPP
#define PARENTAGE_HPP 1

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

	unsigned long p_refcount;

	template<class Archive>
	void load(Archive& ar, const unsigned int version);

	template<class Archive>
	void save(Archive& ar, const unsigned int version) const;

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("Parentage");
	    boost::serialization::split_member(ar, *this, version);
	}
    };
}

// ***** Implementation of non-inlined templated members *****

template<class Archive>
void CXXR::Parentage::load(Archive& ar, const unsigned int version)
{
    size_t sz;
    ar >> sz;
    resize(sz);
    for (size_t i = 0; i < sz; ++i)
	ar >> (*this)[i];
}

template<class Archive>
void CXXR::Parentage::save(Archive& ar, const unsigned int version) const
{
    size_t sz = size();
    ar << sz;
    for (size_t i = 0; i < sz; ++i)
	ar << (*this)[i];
}

#endif // PARENTAGE_HPP
