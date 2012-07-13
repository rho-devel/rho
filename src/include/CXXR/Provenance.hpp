#ifndef PROVENANCE_HPP
#define PROVENANCE_HPP

#include <sys/time.h>
#include <ctime>
#include <set>
#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/split_member.hpp>
#include "CXXR/BSerializer.hpp"
#include "CXXR/Expression.h"
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/Parentage.hpp"
#include "CXXR/RObject.h"
#include "CXXR/StringVector.h"
#include "CXXR/Symbol.h"

namespace CXXR {

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

	Provenance(); // sort of for boost::serialization
	Provenance(const Expression*,Symbol*,Parentage*);
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

	const RObject* getValue() const
	{
	    return m_value;
	}

	bool isXenogenous() const
	{
	    return m_xenogenous;
	}

	Set* pedigree();
	void setXenogenous(const RObject* value);
	double timestamp() const;
	void visitReferents(const_visitor*) const;
    private:
	friend class boost::serialization::access;
	// Do away with compiler-generated copy constructor
	Provenance(const Provenance&);
	struct timeval m_timestamp;
	unsigned int m_parentpos;
	Set* m_children;
	GCEdge<Expression> m_expression;
	GCEdge<Symbol> m_symbol;
	GCEdge<const RObject> m_value;
	Parentage* m_parentage;
	bool m_xenogenous;

	void announceBirth();
	void announceDeath();
	void deregisterChild(Provenance*);
	void registerChild(Provenance*);

	template <class Archive>
	void load(Archive& ar, const unsigned int version);
		
	template <class Archive>
	void save(Archive& ar, const unsigned int version) const;

	template <class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("Provenance");
		
	    boost::serialization::split_member(ar, *this, version);
	}
    };
} // Namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::Provenance)

// ***** Implementation of non-inlined templated members *****

template <class Archive>
void CXXR::Provenance::load(Archive& ar, const unsigned int version)
{
    ar >> BOOST_SERIALIZATION_BASE_OBJECT_NVP(GCNode);
    ar >> boost::serialization::make_nvp("sec", m_timestamp.tv_sec);
    ar >> boost::serialization::make_nvp("usec", m_timestamp.tv_usec);
    BSerializer::attrib("m_expression");
    ar >> BOOST_SERIALIZATION_NVP(m_expression);
    ar >> BOOST_SERIALIZATION_NVP(m_parentpos);
    BSerializer::attrib("m_symbol");
    ar >> BOOST_SERIALIZATION_NVP(m_symbol);
    ar >> BOOST_SERIALIZATION_NVP(m_value);
    BSerializer::attrib("m_parentage");
    ar >> BOOST_SERIALIZATION_NVP(m_parentage);
    ar >> BOOST_SERIALIZATION_NVP(m_xenogenous);
    m_children=new Set();

    m_parentage->incRefCount();
    announceBirth();
}
		
template <class Archive>
void CXXR::Provenance::save(Archive& ar, const unsigned int version) const
{
    ar << BOOST_SERIALIZATION_BASE_OBJECT_NVP(GCNode);
    ar << boost::serialization::make_nvp("sec", m_timestamp.tv_sec);
    ar << boost::serialization::make_nvp("usec", m_timestamp.tv_usec);
    BSerializer::attrib("m_expression");
    ar << BOOST_SERIALIZATION_NVP(m_expression);
    ar << BOOST_SERIALIZATION_NVP(m_parentpos);
    BSerializer::attrib("m_symbol");
    ar << BOOST_SERIALIZATION_NVP(m_symbol);
    ar << BOOST_SERIALIZATION_NVP(m_value);
    BSerializer::attrib("m_parentage");
    ar << BOOST_SERIALIZATION_NVP(m_parentage);
    ar << BOOST_SERIALIZATION_NVP(m_xenogenous);
}

#endif
