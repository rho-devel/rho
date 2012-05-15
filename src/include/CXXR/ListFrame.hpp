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

/** @file ListFrame.hpp
 *
 * @brief Class CXXR::ListFrame.
 */

#ifndef LISTFRAME_HPP
#define LISTFRAME_HPP

#include <list>
#include <boost/serialization/export.hpp>

#include "CXXR/Allocator.hpp"
#include "CXXR/Frame.hpp"

namespace CXXR {
    /** @brief Lightweight implementation of CXXR::Frame.
     *
     * This implementation is intended for Frames containing only a
     * small number of Bindings.  Look-up time is linear in the number
     * of Bindings.
     */
    class ListFrame : public Frame {
    private:
	typedef
	std::list<Binding, CXXR::Allocator<Binding> > List;
    public:
	// Virtual functions of Frame (qv):
	PairList* asPairList() const;

#ifdef __GNUG__
	__attribute__((hot,fastcall))
#endif
	Binding* binding(const Symbol* symbol);

	const Binding* binding(const Symbol* symbol) const;
	void clear();
	ListFrame* clone() const;
	bool erase(const Symbol* symbol);
	void import(const Frame* frame);
	void lockBindings();
	std::size_t numBindings() const;
	Binding* obtainBinding(const Symbol* symbol);
	std::size_t size() const;
	void softMergeInto(Frame* target) const;
	std::vector<const Symbol*> symbols(bool include_dotsymbols) const;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Virtual function of GCNode:
	void detachReferents();
    private:
	friend class boost::serialization::access;

	List m_list;

	// Declared private to ensure that ListFrame objects are
	// created only using 'new':
	~ListFrame() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	ListFrame& operator=(const ListFrame&);

	template<class Archive>
	void load(Archive& ar, const unsigned int version)
	{
	    ar >> BOOST_SERIALIZATION_BASE_OBJECT_NVP(Frame);
	    size_t numberOfBindings;
	    ar >> BOOST_SERIALIZATION_NVP(numberOfBindings);
	    for (size_t i = 0; i < numberOfBindings; ++i) {
		const Symbol* symbol = loadSymbol(ar);
		m_list.push_back(Binding());
		Binding& binding = m_list.back();
		binding.initialize(this, symbol);
		statusChanged(symbol);
		ar >> BOOST_SERIALIZATION_NVP(binding);
	    }
	}

	template<class Archive>
	void save(Archive& ar, const unsigned int version) const
	{
	    ar << BOOST_SERIALIZATION_BASE_OBJECT_NVP(Frame);
	    size_t numberOfBindings = size();
	    ar << BOOST_SERIALIZATION_NVP(numberOfBindings);
	    for (List::const_iterator it = m_list.begin();
		 it != m_list.end(); ++it) {
		const Binding& binding = *it;
		saveSymbol(ar, binding.symbol());
		ar << BOOST_SERIALIZATION_NVP(binding);
	    }
	}

	template<class Archive>
	void serialize(Archive& ar, const unsigned int version) {
	    BSerializer::Frame frame("ListFrame");
	    boost::serialization::split_member(ar, *this, version);
	}
    };
}  // namespace CXXR

BOOST_CLASS_EXPORT(CXXR::ListFrame)

#endif // LISTFRAME_HPP
