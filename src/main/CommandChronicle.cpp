/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) Chris A. Silles 2009-12.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
 */

/** @file CommandChronicle.cpp
 *
 * Implementation of class CommandChronicle.
 */

#include "rho/CommandChronicle.hpp"

#include "rho/Provenance.hpp"

using namespace rho;

void CommandChronicle::detachReferents()
{
    m_reads.clear();
    m_command.detach();
}

void CommandChronicle::readBinding(const Provenance* bdgprov)
{
    std::pair<std::set<unsigned int>::iterator, bool> pr
	= m_seen.insert(bdgprov->serialNumber());
    if (pr.second) {
	GCEdge<const Provenance> parent;
	parent = bdgprov;
	m_reads.push_back(parent);
    }
}

void CommandChronicle::visitReferents(const_visitor* v) const
{
    for (const GCNode* parent : m_reads) {
	(*v)(parent);
    }
    if (m_command)
	(*v)(m_command);
}

void CommandChronicle::writeBinding(const Provenance* bdgprov)
{
    m_seen.insert(bdgprov->serialNumber());
}
