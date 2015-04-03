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

ListFrame::ListFrame(const ListFrame &pattern)
{
    importBindings(&pattern);
    if (pattern.isLocked())
	lock(false);

}

Frame::Binding* ListFrame::v_binding(const Symbol* symbol)
{
    List::iterator end = m_list.end();
    List::iterator it = m_list.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it == end)
	return nullptr;
    return &(*it);
}

const Frame::Binding* ListFrame::v_binding(const Symbol* symbol) const
{
    List::const_iterator end = m_list.end();
    List::const_iterator it = m_list.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it == end)
	return nullptr;
    return &(*it);
}

void ListFrame::visitBindings(std::function<void(const Binding*)> f) const
{
    for (const Binding& binding : m_list) {
	f(&binding);
    }
}

ListFrame* ListFrame::clone() const
{
    return new ListFrame(*this);
}

void ListFrame::lockBindings()
{
    for (Binding& binding : m_list)
	binding.setLocking(true);
}

size_t ListFrame::size() const
{
    return m_list.size();
}

void ListFrame::v_clear()
{
    m_list.clear();
}

bool ListFrame::v_erase(const Symbol* symbol)
{
    List::iterator it = m_list.begin();
    while (it != m_list.end() && (*it).symbol() != symbol)
	++it;
    if (it == m_list.end())
	return false;
    m_list.erase(it);
    return true;
}

Frame::Binding* ListFrame::v_obtainBinding(const Symbol* symbol)
{
    List::iterator end = m_list.end();
    List::iterator it = m_list.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it != end)
	return &(*it);
    m_list.push_back(Binding());
    return &m_list.back();
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::ListFrame)
