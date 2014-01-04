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

/** @file ListFrame.hpp
 *
 * @brief Class CXXR::ListFrame.
 */

#ifndef LISTFRAME_HPP
#define LISTFRAME_HPP

#include <list>

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
	ListFrame() { }
	ListFrame(const ListFrame &pattern);

	// Virtual functions of Frame (qv):
#ifdef __GNUG__
	__attribute__((hot,fastcall))
#endif
	Binding* binding(const Symbol* symbol);

	const Binding* binding(const Symbol* symbol) const;
	BindingRange bindingRange() const;
	ListFrame* clone() const;
	void lockBindings();
	std::size_t size() const;
    private:
	friend class boost::serialization::access;

	List m_list;

	// Declared private to ensure that ListFrame objects are
	// created only using 'new':
	~ListFrame() {
	    clear();
	}

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
		GCStackRoot<Symbol> symbol;
		GCNPTR_SERIALIZE(ar, symbol);
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
		const Symbol* symbol = binding.symbol();
		GCNPTR_SERIALIZE(ar, symbol);
		ar << BOOST_SERIALIZATION_NVP(binding);
	    }
	}

	template<class Archive>
	void serialize(Archive& ar, const unsigned int version) {
	    boost::serialization::split_member(ar, *this, version);
	}

	// Virtual functions of Frame (qv):
	void v_clear();
	bool v_erase(const Symbol* symbol);
	Binding* v_obtainBinding(const Symbol* symbol);
    };
}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::ListFrame)

#endif // LISTFRAME_HPP
