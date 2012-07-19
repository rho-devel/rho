#ifndef PARENTAGE_HPP
#define PARENTAGE_HPP 1

#include <vector>
#include <boost/serialization/access.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/split_member.hpp>
#include <boost/serialization/vector.hpp>

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
	void serialize(Archive & ar, const unsigned int version) {
	    using namespace boost::serialization;
	    size_t sz = size();
	    ar & boost::serialization::make_nvp("size", sz);
	    resize(sz);
	    for (size_t i = 0; i < sz; ++i) {
		GCEdge<Provenance>& parent = (*this)[i];
		GCNPTR_SERIALIZE(ar, parent);
	    }
	}
    };
}

// Not needed, because never serialised via base class pointer:
//BOOST_CLASS_EXPORT(CXXR::Parentage)

#endif // PARENTAGE_HPP
