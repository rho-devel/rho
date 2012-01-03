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

/** @file VectorFrame.hpp
 *
 * @brief Class CXXR::VectorFrame.
 */

#ifndef VECTORFRAME_HPP
#define VECTORFRAME_HPP

#include <vector>
#include "CXXR/Allocator.hpp"
#include "CXXR/Frame.hpp"

namespace CXXR {
    /** @brief Lightweight implementation of CXXR::Frame.
     *
     * This implementation is intended for Frames containing only a
     * small number of Bindings.  Look-up time is linear in the number
     * of Bindings, but construction, destruction and memory
     * allocation overheads are lower.
     */
    class VectorFrame : public Frame {
    private:
	typedef
	std::vector<Binding, CXXR::Allocator<Binding> > Vector;
    public:
	// Virtual functions of Frame (qv):
	PairList* asPairList() const;

#ifdef __GNUG__
	__attribute__((hot,fastcall))
#endif
	Binding* binding(const Symbol* symbol);

	const Binding* binding(const Symbol* symbol) const;
	void clear();
	VectorFrame* clone() const;
	bool erase(const Symbol* symbol);
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
	Vector m_vector;

	// Declared private to ensure that VectorFrame objects are
	// created only using 'new':
	~VectorFrame() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	VectorFrame& operator=(const VectorFrame&);
    };
}  // namespace CXXR

#endif // VECTORFRAME_HPP
