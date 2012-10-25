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

/* This file incorporates material Copyright (C) Chris A. Silles 2009-12.
 */

/*
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

#include "CXXR/Provenance.hpp"
#include "CXXR/ProvenanceSet.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Frame.hpp"
#include "Defn.h"

using namespace CXXR;

GCRoot<CommandChronicle> ProvenanceTracker::s_chronicle;
bool ProvenanceTracker::s_xenogenous = false;

void ProvenanceTracker::setExpression(const RObject* arg) {
    if (arg) {
	s_chronicle = CXXR_NEW(CommandChronicle(arg));
	s_xenogenous = false;
    } else if (s_chronicle) {
	s_chronicle->close();
	s_chronicle = 0;
    }
}

void ProvenanceTracker::initEnvs()
{
    Frame::setReadMonitor(ProvenanceTracker::readMonitor);
    Frame::setWriteMonitor(ProvenanceTracker::writeMonitor);
    Frame* global_frame = Environment::global()->frame();
    global_frame->enableReadMonitoring(true);
    global_frame->enableWriteMonitoring(true);
}

void ProvenanceTracker::readMonitor(const Frame::Binding& bdg)
{ 
#ifdef VERBOSEMONITOR
    cout << "Read '" << bdg.symbol()->name()->c_str() << "'" <<endl;
#endif
    const Provenance* p = bdg.provenance();
    if (p)
	s_chronicle->readBinding(p);
}

void ProvenanceTracker::writeMonitor(const Frame::Binding &bind)
{
#ifdef VERBOSEMONITOR
    cout << "Write '" << bind.symbol()->name()->c_str() << "'" <<endl;
#endif
    const Symbol* sym = bind.symbol();
    GCStackRoot<Provenance> prov(CXXR_NEW(Provenance(sym, s_chronicle)));
    if (s_xenogenous)
	prov->setXenogenous(bind.rawValue());  // Maybe ought to clone value
    CXXR::Frame::Binding& bdg = const_cast<CXXR::Frame::Binding&>(bind);
    bdg.setProvenance(prov);
    s_chronicle->writeBinding(prov);
}

void flagXenogenous()
{
    ProvenanceTracker::flagXenogenous();
}
