/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) Chris A. Silles 2009-12.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

#include "CXXR/ProvenanceTracker.h"

#include "CXXR/CommandChronicle.hpp"
#include "CXXR/Environment.h"

using namespace CXXR;

#ifdef PROVENANCE_TRACKING

ProvenanceTracker::CommandScope* ProvenanceTracker::s_scope = 0;


// ***** Class ProvenanceTracker::CommandScope *****

// CXXR FIXME: Maybe ought to duplicate 'command', either here or in
// CommandChronicle constructor.
ProvenanceTracker::CommandScope::CommandScope(const RObject* command)
    : m_xenogenetic(false)
{
    if (!ProvenanceTracker::s_scope) {
	m_chronicle = new CommandChronicle(command);
	ProvenanceTracker::s_scope = this;
    }
}

ProvenanceTracker::CommandScope::~CommandScope()
{
    if (s_scope == this) {
	m_chronicle->close();
	ProvenanceTracker::s_scope = 0;
    }
}

void ProvenanceTracker::CommandScope::monitorRead(const Frame::Binding& bdg)
{ 
    const Provenance* prov = bdg.provenance();
    if (prov)
	m_chronicle->readBinding(prov);
}

void ProvenanceTracker::CommandScope::monitorWrite(const Frame::Binding &bdg)
{
    const Symbol* sym = bdg.symbol();
    GCStackRoot<Provenance> prov(new Provenance(sym, m_chronicle));
    if (m_xenogenetic)
	prov->setXenogenous(bdg.rawValue());  // Maybe ought to clone value
    CXXR::Frame::Binding& ncbdg = const_cast<CXXR::Frame::Binding&>(bdg);
    ncbdg.setProvenance(prov);
    m_chronicle->writeBinding(prov);
}


// ***** Class ProvenanceTracker *****

void ProvenanceTracker::flagXenogenesis()
{
    if (s_scope)
	s_scope->flagXenogenesis();
}

void ProvenanceTracker::monitorRead(const Frame::Binding& bdg)
{
    if (s_scope)
	s_scope->monitorRead(bdg);
}

void ProvenanceTracker::monitorWrite(const Frame::Binding& bdg)
{
    if (s_scope)
	s_scope->monitorWrite(bdg);
}

void ProvenanceTracker::setMonitors()
{
    Frame::setReadMonitor(ProvenanceTracker::monitorRead);
    Frame::setWriteMonitor(ProvenanceTracker::monitorWrite);
    Frame* global_frame = Environment::global()->frame();
    global_frame->enableReadMonitoring(true);
    global_frame->enableWriteMonitoring(true);
}

#endif  // PROVENANCE_TRACKING

// ***** C interface *****

void flagXenogenesis()
{
    ProvenanceTracker::flagXenogenesis();
}
