/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-12 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/* This file incorporates material Copyright (C) Chris A. Silles 2009-12.
 */

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef PROVENANCE_HPP
#define PROVENANCE_HPP

#include <sys/time.h>
#include <ctime>
#include <set>
#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/split_member.hpp>

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
	    bool operator()(const Provenance* lhs, const Provenance* rhs) {
		return (lhs->m_timestamp.tv_sec==rhs->m_timestamp.tv_sec) ?
		    (lhs->m_timestamp.tv_usec<rhs->m_timestamp.tv_usec) :
		    (lhs->m_timestamp.tv_sec<rhs->m_timestamp.tv_sec);
	    }
	};

	typedef std::set<const Provenance*, Provenance::CompTime> Set;

	// For boost::serialization:
	Provenance()
	{}

	Provenance(const Expression* exp, const Symbol* sym, Parentage* par);

	~Provenance()
	{
	    announceDeath(); // Necessary house-keeping
	}

	static StringVector* setAsStringVector(const Set& s);

	// CXXR FIXME: make this private in due course:
	static Set* ancestors(Set*);

	const Set& children() const
	{
	    return m_children;
	}

	void detachReferents();

	const Expression* getCommand() const
	{
	    return m_expression;
	}

	const Symbol* getSymbol() const
	{
	    return m_symbol;
	}

	const Parentage* getParentage() const
	{
	    return m_parentage;
	}

	const String* getTime() const;

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

	mutable Set m_children;
	struct timeval m_timestamp;
	unsigned int m_parentpos;
	GCEdge<const Expression> m_expression;
	GCEdge<const Symbol> m_symbol;
	GCEdge<const RObject> m_value;
	Parentage* m_parentage;
	bool m_xenogenous;

	static Set* descendants(Set*);

	// Do away with compiler-generated copy constructor
	Provenance(const Provenance&);

	void announceBirth();

	void announceDeath();

	void deregisterChild(const Provenance* child) const
	{
	    m_children.erase(child);
	}

	void registerChild(const Provenance* child) const
	{
	    m_children.insert(child);
	}

	template <class Archive>
	void load(Archive& ar, const unsigned int version);
		
	template <class Archive>
	void save(Archive& ar, const unsigned int version) const;

	template <class Archive>
	void serialize(Archive & ar, const unsigned int version) {
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
    ar >> BOOST_SERIALIZATION_NVP(m_parentpos);
    GCNPTR_SERIALIZE(ar, m_expression);
    GCNPTR_SERIALIZE(ar, m_symbol);
    GCNPTR_SERIALIZE(ar, m_value);
    ar >> BOOST_SERIALIZATION_NVP(m_parentage);
    ar >> BOOST_SERIALIZATION_NVP(m_xenogenous);

    m_parentage->incRefCount();
    announceBirth();
}
		
template <class Archive>
void CXXR::Provenance::save(Archive& ar, const unsigned int version) const
{
    ar << BOOST_SERIALIZATION_BASE_OBJECT_NVP(GCNode);
    ar << boost::serialization::make_nvp("sec", m_timestamp.tv_sec);
    ar << boost::serialization::make_nvp("usec", m_timestamp.tv_usec);
    ar << BOOST_SERIALIZATION_NVP(m_parentpos);
    GCNPTR_SERIALIZE(ar, m_expression);
    GCNPTR_SERIALIZE(ar, m_symbol);
    GCNPTR_SERIALIZE(ar, m_value);
    ar << BOOST_SERIALIZATION_NVP(m_parentage);
    ar << BOOST_SERIALIZATION_NVP(m_xenogenous);
}

#endif
