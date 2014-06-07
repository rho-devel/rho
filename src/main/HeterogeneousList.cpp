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

/** @file HeterogeneousList.cpp
 *
 * Implementation of class HeterogeneousListBase.
 */

#include "CXXR/HeterogeneousList.hpp"

using namespace std;
using namespace CXXR;

void HeterogeneousListBase::clear()
{
    while (!empty())
	delete m_peg->m_next;
}

void HeterogeneousListBase::freeLinks()
{
    while (!empty())
	m_peg->m_next->freeLink();
}

void HeterogeneousListBase::splice_links(Link* pos, Link* from, Link* to)
{
    if (from != to) {
	Link* last = to->m_prev;
	link(from->m_prev, to);
	link(pos->m_prev, from);
	link(last, pos);
    }
}
