/*CXXR
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

#define R_NO_REMAP

#include "CXXR/jit/CompiledFrame.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"
#include "CXXR/StdFrame.hpp"

#include "boost/iterator/filter_iterator.hpp"
#include "boost/range/adaptor/map.hpp"
#include "boost/range/join.hpp"

namespace CXXR {
namespace JIT {

CompiledFrame::CompiledFrame(const FrameDescriptor* descriptor)
    : m_descriptor(descriptor)
{
    m_bindings = new Binding[descriptor->getNumberOfSymbols()];
    m_extension = nullptr;
}

CompiledFrame::CompiledFrame(const CompiledFrame& pattern)
{
    m_descriptor = pattern.m_descriptor;
    m_bindings = new Binding[m_descriptor->getNumberOfSymbols()];
    m_extension = nullptr;

    importBindings(&pattern);
    if (pattern.isLocked()) {
	lock(false);
    }
}

CompiledFrame::~CompiledFrame()
{
    delete[] m_bindings;
    if (m_extension) {
	delete m_extension;
    }
}

Frame::Binding* CompiledFrame::binding(const Symbol* symbol)
{
    int location = m_descriptor->getLocation(symbol);
    if (location != -1) {
	return binding(location);
    }
    if (m_extension != nullptr) {
	auto it = m_extension->find(symbol);
	if (it != m_extension->end()) {
	    return &(it->second);
	}
    }
    return nullptr;
}

Frame::BindingRange CompiledFrame::bindingRange() const
{
    auto firstBinding = m_bindings;
    auto lastBinding = m_bindings + m_descriptor->getNumberOfSymbols();
    BindingRange range = BindingRange(
	boost::make_filter_iterator(isSet, firstBinding, lastBinding),
	boost::make_filter_iterator(isSet, lastBinding, lastBinding));

    if (m_extension) {
	BindingRange extension = (*m_extension) | boost::adaptors::map_values;
	range = boost::join(range, extension);
    }
    return range;
}

CompiledFrame* CompiledFrame::clone() const
{
    return new CompiledFrame(*this);
}

std::size_t CompiledFrame::size() const
{
  std::size_t result = 0;
  for (int i = 0; i < m_descriptor->getNumberOfSymbols(); ++i) {
    if (isSet(m_bindings[i])) {
      result += 1;
    }
  }
  if (m_extension) {
    result += m_extension->size();
  }
  return result;
}

void CompiledFrame::lockBindings()
{
    int size = m_descriptor->getNumberOfSymbols();
    for (int i = 0; i < size; i++) {
	m_bindings[i].setLocking(true);
    }
    if (m_extension) {
	for (auto& item : *m_extension) {
	    item.second.setLocking(true);
	}
    }
}

void CompiledFrame::detachReferents()
{
    Frame::detachReferents();
    m_descriptor.detach();
}

void CompiledFrame::visitReferents(const_visitor* v) const
{
    Frame::visitReferents(v);
    m_descriptor->visitReferents(v);
}

void CompiledFrame::v_clear()
{
    int size = m_descriptor->getNumberOfSymbols();
    for (int i = 0; i < size; i++) {
	unsetBinding(m_bindings + i);
    }
    if (m_extension) {
	delete m_extension;
	m_extension = nullptr;
    }
}

bool CompiledFrame::v_erase(const Symbol* symbol)
{
    int location = m_descriptor->getLocation(symbol);
    if (location != -1) {
	Binding* binding = m_bindings + location;
	if (isSet(*binding)) {
	    unsetBinding(binding);
	    return true;
	}
	return false;
    }
    if (m_extension) {
	return m_extension->erase(symbol);
    }
    return false;
}

Frame::Binding* CompiledFrame::v_obtainBinding(const Symbol* symbol)
{
    int location = m_descriptor->getLocation(symbol);
    if (location != -1) {
	return m_bindings + location;
    }
    if (!m_extension) {
	m_extension = new std::map<const Symbol*, Binding>();
    }
    return &((*m_extension)[symbol]);
}

void CompiledFrame::unsetBinding(Binding* binding)
{
    if (!isSet(*binding)) {
	return;
    }
    // Destroy the binding and create a new uninitialized one in its place.
    binding->~Binding();
    new (binding) Binding();
}

} // namespace JIT
} // namespace CXXR
