/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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

/** @file StdEnvironment.hpp
 * @brief Class CXXR::StdEnvironment.
 */

#ifndef STDENVIRONMENT_HPP
#define STDENVIRONMENT_HPP

#include <tr1/unordered_map>
#include "CXXR/Environment.h"

namespace CXXR {
    /** @brief General-purpose implementation of CXXR::Environment.
     */
    class StdEnvironment : public Environment {
    private:
	typedef std::tr1::unordered_map<const Symbol*, Binding> map;
    public:
	/**
	 * @param enclosing Pointer to the enclosing environment.
	 *
	 * @param initial_capacity A hint to the implementation that
	 *          the constructed StdEnvironment should be
	 *          configured to have capacity for at least \a
	 *          initial_capacity Bindings.  This does not impose an
	 *          upper limit on the capacity of the StdEnvironment,
	 *          but some reconfiguration (and consequent time
	 *          penalty) may occur if it is exceeded.
	 */
	explicit StdEnvironment(Environment* enclosing = 0,
				size_t initial_capacity = 11);
	// Why 11?  Because if the implementation uses a prime number
	// hash table sizing policy, this will result in the
	// allocation of a hash table array comprising 23 buckets.  On
	// a 32-bit architecture, this will fit well into three
	// 32-byte cache lines.

	// Virtual functions of Environment (qv):
	void clear();
	bool erase(const Symbol* symbol);
	PairList* frameList() const;
	void lockBindings();
	Binding* obtainBinding(const Symbol* symbol);
	size_t size() const;

	// Virtual function of GCNode:
	void visitChildren(const_visitor* v) const;
    private:
	map m_map;

	// Declared private to ensure that StdEnvironment objects are
	// created only using 'new':
	~StdEnvironment() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	StdEnvironment(const Environment&);
	StdEnvironment& operator=(const Environment&);

	// Virtual functions of Environment (qv):
	Binding* frameBinding(const Symbol* symbol);
	const Binding* frameBinding(const Symbol* symbol) const;
    };
}  // namespace CXXR

#endif // STDENVIRONMENT_HPP
