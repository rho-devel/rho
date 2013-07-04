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

Frame::Binding* ListFrame::binding(const Symbol* symbol)
{
    List::iterator end = m_list.end();
    List::iterator it = m_list.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it == end)
	return 0;
    return &(*it);
}

const Frame::Binding* ListFrame::binding(const Symbol* symbol) const
{
    List::const_iterator end = m_list.end();
    List::const_iterator it = m_list.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it == end)
	return 0;
    return &(*it);
}

Frame::BindingRange ListFrame::bindingRange() const
{
    return BindingRange(m_list.begin(), m_list.end());
}

ListFrame* ListFrame::clone() const
{
    return expose(new ListFrame(*this));
}

void ListFrame::lockBindings()
{
    for (List::iterator it = m_list.begin(); it != m_list.end(); ++it)
	(*it).setLocking(true);
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
