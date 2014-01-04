/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *
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

/** @file StdFrame.hpp
 *
 * @brief Class CXXR::StdFrame.
 */

#ifndef STDFRAME_HPP
#define STDFRAME_HPP

#include <tr1/unordered_map>
#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/split_member.hpp>
#include <boost/serialization/unordered_map.hpp>

#include "CXXR/Allocator.hpp"
#include "CXXR/Frame.hpp"

namespace CXXR {
    /** @brief General-purpose implementation of CXXR::Frame.
     */
    class StdFrame : public Frame {
    private:
	typedef
	std::tr1::unordered_map<const Symbol*, Binding,
				std::tr1::hash<const Symbol*>,
				std::equal_to<const Symbol*>,
				CXXR::Allocator<std::pair<const Symbol* const,
							  Binding> >
	                        > map;
    public:
	/**
	 * @param initial_capacity A hint to the implementation that
	 *          the constructed StdFrame should be
	 *          configured to have capacity for at least \a
	 *          initial_capacity Bindings.  This does not impose an
	 *          upper limit on the capacity of the StdFrame,
	 *          but some reconfiguration (and consequent time
	 *          penalty) may occur if it is exceeded.
	 */
	explicit StdFrame(std::size_t initial_capacity = 15);
	// Why 15?  Because if the implementation uses a prime number
	// hash table sizing policy, this will result in the
	// allocation of a hash table array comprising 31 buckets.  On
	// a 32-bit architecture, this will fit well into two 64-byte
	// cache lines.

	StdFrame(const StdFrame &source);
	
	// Virtual functions of Frame (qv):
#ifdef __GNUG__
	__attribute__((hot,fastcall))
#endif
	Binding* binding(const Symbol* symbol);

	const Binding* binding(const Symbol* symbol) const;
	BindingRange bindingRange() const;
	StdFrame* clone() const;
	void lockBindings();
	std::size_t size() const;
    private:
	friend class boost::serialization::access;

	map m_map;

	// Declared private to ensure that StdFrame objects are
	// created only using 'new':
	~StdFrame() {
	    clear();
	}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	StdFrame& operator=(const StdFrame&);

	template<class Archive>
	void load(Archive& ar, const unsigned int version);
	
	template<class Archive>
	void save(Archive& ar, const unsigned int version) const;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int version) {
	    boost::serialization::split_member(ar, *this, version);
	}

	// Virtual functions of Frame (qv):
	void v_clear();
	bool v_erase(const Symbol* symbol);
	Binding* v_obtainBinding(const Symbol* symbol);
    };

    // ***** Implementation of non-inlined templated members *****

    template<class Archive>
    void StdFrame::load(Archive& ar, const unsigned int version)
    {
	ar >> BOOST_SERIALIZATION_BASE_OBJECT_NVP(Frame);
	size_t numberOfBindings;
	ar >> BOOST_SERIALIZATION_NVP(numberOfBindings);
	for (size_t i = 0; i < numberOfBindings; ++i) {
	    GCStackRoot<Symbol> symbol;
	    GCNPTR_SERIALIZE(ar, symbol);
	    Binding* binding = obtainBinding(symbol);
	    ar >> boost::serialization::make_nvp("binding", *binding);
	}
    }
	
    template<class Archive>
    void StdFrame::save(Archive& ar, const unsigned int version) const
    {
	ar << BOOST_SERIALIZATION_BASE_OBJECT_NVP(Frame);
	size_t numberOfBindings = size();
	ar << BOOST_SERIALIZATION_NVP(numberOfBindings);
	for (map::const_iterator it = m_map.begin();
	     it != m_map.end(); ++it) {
	    const Symbol* symbol = (*it).first;
	    const Binding& binding = (*it).second;
	    GCNPTR_SERIALIZE(ar, symbol);
	    ar << BOOST_SERIALIZATION_NVP(binding);
	}
    }
}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::StdFrame)

#endif // STDFRAME_HPP
