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

/** @file VectorFrame.cpp
 *
 *
 * @brief Implementation of class CXXR:VectorFrame.
 */

#include "CXXR/VectorFrame.hpp"

#include <cmath>
#include "localization.h"
#include "R_ext/Error.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

PairList* VectorFrame::asPairList() const
{
    GCStackRoot<PairList> ans(0);
    for (Vector::const_iterator it = m_vector.begin();
	 it != m_vector.end(); ++it)
	ans = (*it).asPairList(ans);
    return ans;
}

Frame::Binding* VectorFrame::binding(const Symbol* symbol)
{
    Vector::iterator end = m_vector.end();
    Vector::iterator it = m_vector.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it == end)
	return 0;
    return &(*it);
}

const Frame::Binding* VectorFrame::binding(const Symbol* symbol) const
{
    Vector::const_iterator end = m_vector.end();
    Vector::const_iterator it = m_vector.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it == end)
	return 0;
    return &(*it);
}

void VectorFrame::clear()
{
    statusChanged(0);
    m_vector.clear();
}

VectorFrame* VectorFrame::clone() const
{
    return expose(new VectorFrame(*this));
}

void VectorFrame::detachReferents()
{
    m_vector.clear();
    Frame::detachReferents();
}

bool VectorFrame::erase(const Symbol* symbol)
{
    if (isLocked())
	Rf_error(_("cannot remove bindings from a locked frame"));
    Vector::iterator it = m_vector.begin();
    while (it != m_vector.end() && (*it).symbol() != symbol)
	++it;
    if (it == m_vector.end())
	return false;
    m_vector.erase(it);
    statusChanged(symbol);
    return true;
}

void VectorFrame::lockBindings()
{
    for (Vector::iterator it = m_vector.begin(); it != m_vector.end(); ++it)
	(*it).setLocking(true);
}

size_t VectorFrame::numBindings() const
{
    return m_vector.size();
}

Frame::Binding* VectorFrame::obtainBinding(const Symbol* symbol)
{
    Vector::iterator end = m_vector.end();
    Vector::iterator it = m_vector.begin();
    while (it != end && (*it).symbol() != symbol)
	++it;
    if (it != end)
	return &(*it);
    if (isLocked())
	Rf_error(_("cannot add bindings to a locked frame"));
    if (m_vector.empty())
	// 192 bytes (3 cache lines) on 32-bit architecture:
	m_vector.reserve(12);
    m_vector.push_back(Binding());
    Binding* bdg = &m_vector.back();
    bdg->initialize(this, symbol);
    statusChanged(symbol);
    return bdg;
}

size_t VectorFrame::size() const
{
    return m_vector.size();
}

void VectorFrame::softMergeInto(Frame* target) const
{
    for (Vector::const_iterator it = m_vector.begin();
	 it != m_vector.end(); ++it) {
	const Binding& mybdg = *it;
	const Symbol* symbol = mybdg.symbol();
	if (!target->binding(symbol)) {
	    Binding* yourbdg = target->obtainBinding(symbol);
	    yourbdg->setValue(mybdg.value(), mybdg.origin());
	}
    }
}

vector<const Symbol*> VectorFrame::symbols(bool include_dotsymbols) const
{
    vector<const Symbol*> ans;
    for (Vector::const_iterator it = m_vector.begin();
	 it != m_vector.end(); ++it) {
	const Symbol* symbol = (*it).symbol();
	if (include_dotsymbols || !isDotSymbol(symbol))
	    ans.push_back(symbol);
    }
    return ans;
}

void VectorFrame::visitReferents(const_visitor* v) const
{
    Frame::visitReferents(v);
    for (Vector::const_iterator it = m_vector.begin();
	 it != m_vector.end(); ++it)
	(*it).visitReferents(v);
}
