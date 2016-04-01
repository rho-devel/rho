/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
 */

/** @file StdFrame.cpp
 *
 *
 * @brief Implementation of class rho:StdFrame.
 */

#include "rho/StdFrame.hpp"

#include <cmath>
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include "localization.h"
#include "R_ext/Error.h"
#include "rho/GCStackRoot.hpp"
#include "rho/SEXP_downcast.hpp"
#include "rho/Symbol.hpp"

using namespace std;
using namespace rho;

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

StdFrame::StdFrame(const StdFrame &pattern)
{
    importBindings(&pattern);
    if (pattern.isLocked())
	lock(false);
}

Frame::Binding* StdFrame::v_binding(const Symbol* symbol)
{
    map::iterator it = m_map.find(symbol);
    if (it == m_map.end())
	return nullptr;
    return &(*it).second;
}

const Frame::Binding* StdFrame::v_binding(const Symbol* symbol) const
{
    map::const_iterator it = m_map.find(symbol);
    if (it == m_map.end())
	return nullptr;
    return &(*it).second;
}

void StdFrame::visitBindings(std::function<void(const Binding*)> f) const
{
    for (const auto& item : m_map) {
	const Binding* binding = &item.second;
	f(binding);
    }
}

StdFrame* StdFrame::clone() const
{
    return new StdFrame(*this);
}

void StdFrame::lockBindings()
{
    for (auto & elem : m_map) {
	Binding& binding = elem.second;
	binding.setLocking(true);
    }
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
