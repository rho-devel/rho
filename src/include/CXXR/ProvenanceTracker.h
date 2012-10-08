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

#ifndef PROVENANCETRACKER_H
#define PROVENANCETRACKER_H

#ifdef __cplusplus

#include "CXXR/SchwarzCounter.hpp"
#include "CXXR/GCEdge.hpp"
#include "CXXR/Parentage.hpp"
#include "CXXR/Provenance.hpp"
#include "CXXR/ProvenanceSet.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Frame.hpp"

namespace CXXR {
    class ProvenanceTracker {
    public:
	static Parentage* parentage()
	{
	    return (*p_current)->parentage();
	}

	static void resetParentage();

	static const Expression* expression();

	static void setExpression(const RObject* arg);

	static void flagXenogenous()
	{
	    s_xenogenous = true;
	}

	static void forcedPromise(const Frame::Binding&);

	static void readMonitor(const Frame::Binding& bdg);

	static void writeMonitor(const Frame::Binding& bind)
	{
	    writeMonitor(bind, true);
	}

	static void writeMonitor(const Frame::Binding&,bool);

	static void initEnvs();
    private:
	static GCRoot<Parentage::Protector>* p_current;
	static GCRoot<ProvenanceSet>* p_seen;
	static const Expression* e_current;
	static bool s_xenogenous;

	ProvenanceTracker();

	static ProvenanceSet* seen()
	{
	    return *p_seen;
	}

	// Required for SchwarzCounter
	static void cleanup();
	static void initialize();
	friend class SchwarzCounter<ProvenanceTracker>;
    };
} // namespace CXXR

namespace {
	CXXR::SchwarzCounter<CXXR::ProvenanceTracker> provtrack_schwarz_ctr;
}

extern "C" {
#endif // __cplusplus

    void flagXenogenous();

#ifdef __cplusplus
}  // extern "C"
#endif

#endif // PROVENANCETRACKER_H
