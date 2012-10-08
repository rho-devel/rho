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

    class Parentage : public std::vector<GCEdge<const Provenance> > {
    public:
	class Protector : public GCNode {
	public:
	    Protector()
		: m_parentage(0)
	    {}

	    Parentage* parentage()
	    {
		return m_parentage;
	    }

	    void set(Parentage* p);

	    // Virtual functions of GCNode:
	    void detachReferents();
	    void visitReferents(const_visitor* v) const;
	private:
	    Parentage* m_parentage;
	};
		
	Parentage()
	    : m_refcount(0)
	{}

	StringVector* asStringVector() const;

	void Display() const;

	void pushProvenance(const Provenance* prov);
    private:
	friend class boost::serialization::access;
	friend class Provenance;

	unsigned long m_refcount;

	unsigned long decRefCount()
	{
	    return --m_refcount;
	}

	unsigned long incRefCount()
	{
	    return ++m_refcount;
	}

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    using namespace boost::serialization;
	    size_t sz = size();
	    ar & boost::serialization::make_nvp("size", sz);
	    resize(sz);
	    for (size_t i = 0; i < sz; ++i) {
		GCEdge<const Provenance>& parent = (*this)[i];
		GCNPTR_SERIALIZE(ar, parent);
	    }
	}
    };
}

// Not needed, because never serialised via base class pointer:
//BOOST_CLASS_EXPORT(CXXR::Parentage)

#endif // PARENTAGE_HPP
