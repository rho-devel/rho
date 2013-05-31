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

PairList* ListFrame::asPairList() const
{
    GCStackRoot<PairList> ans(0);
    for (List::const_iterator it = m_list.begin();
	 it != m_list.end(); ++it)
	ans = (*it).asPairList(ans);
    return ans;
}

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

void ListFrame::clear()
{
    statusChanged(0);
    m_list.clear();
}

ListFrame* ListFrame::clone() const
{
    return expose(new ListFrame(*this));
}

void ListFrame::detachReferents()
{
    m_list.clear();
    Frame::detachReferents();
}

bool ListFrame::erase(const Symbol* symbol)
{
    if (isLocked())
	Rf_error(_("cannot remove bindings from a locked frame"));
    List::iterator it = m_list.begin();
    while (it != m_list.end() && (*it).symbol() != symbol)
	++it;
    if (it == m_list.end())
	return false;
    m_list.erase(it);
    statusChanged(symbol);
    return true;
}

void ListFrame::import(const Frame* frame)
{
    cerr << "Not (yet) implemented.\n";
    abort();
}

void ListFrame::lockBindings()
{
    for (List::iterator it = m_list.begin(); it != m_list.end(); ++it)
	(*it).setLocking(true);
}

Frame::Binding* ListFrame::obtainBinding(const Symbol* symbol)
{
    List::iterator end = m_list.end();
    List::iterator it = m_list.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it != end)
	return &(*it);
    if (isLocked())
	Rf_error(_("cannot add bindings to a locked frame"));
    m_list.push_back(Binding());
    Binding* bdg = &m_list.back();
    bdg->initialize(this, symbol);
    statusChanged(symbol);
    return bdg;
}

size_t ListFrame::size() const
{
    return m_list.size();
}

void ListFrame::softMergeInto(Frame* target) const
{
    for (List::const_iterator it = m_list.begin();
	 it != m_list.end(); ++it) {
	const Binding& mybdg = *it;
	const Symbol* symbol = mybdg.symbol();
	if (!target->binding(symbol)) {
	    Binding* yourbdg = target->obtainBinding(symbol);
	    yourbdg->setValue(mybdg.value(), mybdg.origin());
	}
    }
}

void ListFrame::visitReferents(const_visitor* v) const
{
    Frame::visitReferents(v);
    for (List::const_iterator it = m_list.begin();
	 it != m_list.end(); ++it)
	(*it).visitReferents(v);
}

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::ListFrame)
