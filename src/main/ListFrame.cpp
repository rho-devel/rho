/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file ListFrame.cpp
 *
 *
 * @brief Implementation of class CXXR:ListFrame.
 */

#include "CXXR/ListFrame.hpp"

#include <cmath>
#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

ListFrame::ListFrame(size_t size)
{
    m_bindings = new Binding[size];
    m_bindings_size = size;
    m_used_bindings_size = 0;
    m_overflow = nullptr;
}

ListFrame::ListFrame(const ListFrame &pattern)
    : ListFrame(pattern.size())
{
    importBindings(&pattern);
    if (pattern.isLocked())
	lock(false);
}

ListFrame::~ListFrame()
{
    delete[] m_bindings;
    if (m_overflow) {
	delete m_overflow;
    }
}

Frame::Binding* ListFrame::v_binding(const Symbol* symbol)
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	if (m_bindings[i].symbol() == symbol && isSet(m_bindings[i]))
	    return &m_bindings[i];
    }
    if (m_overflow) {
	auto location = m_overflow->find(symbol);
	if (location != m_overflow->end()) {
	    return &location->second;
	}
    }
    return nullptr;
}

const Frame::Binding* ListFrame::v_binding(const Symbol* symbol) const
{
    return const_cast<ListFrame*>(this)->v_binding(symbol);
}

void ListFrame::visitBindings(std::function<void(const Binding*)> f) const
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	if (isSet(m_bindings[i]))
	    f(&m_bindings[i]);
    }
    if (m_overflow) {
	for (const auto& entry : *m_overflow) {
	    f(&entry.second);
	}
    }
}

ListFrame* ListFrame::clone() const
{
    return new ListFrame(*this);
}

void ListFrame::lockBindings()
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	m_bindings[i].setLocking(true);
    }
    if (m_overflow) {
	for (auto& item : *m_overflow) {
	    item.second.setLocking(true);
	}
    }
}

std::size_t ListFrame::size() const
{
  std::size_t result = 0;
  for (int i = 0; i < m_used_bindings_size; ++i) {
    if (isSet(m_bindings[i])) {
      result++;
    }
  }
  if (m_overflow) {
    result += m_overflow->size();
  }
  return result;
}

void ListFrame::v_clear()
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	unsetBinding(m_bindings + i);
    }
    if (m_overflow) {
	delete m_overflow;
	m_overflow = nullptr;
    }
}

bool ListFrame::v_erase(const Symbol* symbol)
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	if (m_bindings[i].symbol() == symbol && isSet(m_bindings[i]))
	{
	    unsetBinding(&m_bindings[i]);
	    return true;
	}
    }

    if (m_overflow) {
	return m_overflow->erase(symbol);
    }
    return false;
}

Frame::Binding* ListFrame::v_obtainBinding(const Symbol* symbol)
{
    // If the binding exists, return that.
    Frame::Binding* binding = v_binding(symbol);
    if (binding) {
	return binding;
    }

    // Otherwise return the first unused space in the array if any.
    for (size_t i = 0; i < m_bindings_size; ++i) {
	if (!isSet(m_bindings[i])) {
	    // Found an unused spot.
	    m_used_bindings_size = std::max(m_used_bindings_size, i + 1);
	    return &m_bindings[i];
	}
    }

    // Otherwise go to the overflow.
    if (!m_overflow) {
	m_overflow = new std::map<const Symbol*, Binding>();
    }
    return &((*m_overflow)[symbol]);
}

void ListFrame::unsetBinding(Binding* binding)
{
    if (!isSet(*binding)) {
	return;
    }
    // Destroy the binding and create a new uninitialized one in its place.
    binding->~Binding();
    new (binding) Binding();
}
