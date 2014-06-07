/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file StdFrame.cpp
 *
 *
 * @brief Implementation of class CXXR:StdFrame.
 */

#include "CXXR/StdFrame.hpp"

#include <cmath>
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

// We want to be able to determine quickly if a symbol is *not*
// defined in an frame, so that we can carry on working up the
// chain of enclosing frames.  On average the number of tests
// needed to determine that a symbol is not present is 1 + 2L, where L
// is the load factor.  So we keep the load factor small:
namespace {
    const double maximum_load_factor = 0.5;
}

StdFrame::StdFrame(size_t initial_capacity)
  : m_map(size_t(ceil(double(initial_capacity)/maximum_load_factor)))
{
    m_map.max_load_factor(maximum_load_factor);
}

Frame::Binding* StdFrame::binding(const Symbol* symbol)
{
    map::iterator it = m_map.find(symbol);
    if (it == m_map.end())
	return 0;
    return &(*it).second;
}

const Frame::Binding* StdFrame::binding(const Symbol* symbol) const
{
    map::const_iterator it = m_map.find(symbol);
    if (it == m_map.end())
	return 0;
    return &(*it).second;
}

Frame::BindingRange StdFrame::bindingRange() const
{
    boost::function<const Binding& (const map::value_type&)> f
	= boost::bind(&map::value_type::second, _1);
    return BindingRange(boost::make_transform_iterator(m_map.begin(), f),
			boost::make_transform_iterator(m_map.end(), f));
}

StdFrame* StdFrame::clone() const
{
    return expose(new StdFrame(*this));
}

void StdFrame::lockBindings()
{
    for (map::iterator it = m_map.begin(); it != m_map.end(); ++it)
	(*it).second.setLocking(true);
}

size_t StdFrame::size() const
{
    return m_map.size();
}

void StdFrame::v_clear()
{
    m_map.clear();
}

bool StdFrame::v_erase(const Symbol* symbol)
{
    return m_map.erase(symbol);
}

Frame::Binding* StdFrame::v_obtainBinding(const Symbol* symbol)
{
    return &m_map[symbol];
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::StdFrame)
