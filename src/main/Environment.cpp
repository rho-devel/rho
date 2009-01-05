/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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

/** @file Environment.cpp
 *
 *
 * @brief Implementation of class CXXR:Environment and associated C
 * interface.
 */

#include "CXXR/Environment.h"

using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*ENCLOSp)(SEXP x) = ENCLOS;
	Rboolean (*ENV_DEBUGp)(SEXP x) = ENV_DEBUG;
	SEXP (*HASHTABp)(SEXP x) = HASHTAB;
	Rboolean (*isEnvironmentptr)(SEXP s) = Rf_isEnvironment;
	SEXP (*FRAMEp)(SEXP x) = FRAME;
	void (*SET_ENCLOSp)(SEXP x, SEXP v) = SET_ENCLOS;
	void (*SET_ENV_DEBUGp)(SEXP x, Rboolean v) = SET_ENV_DEBUG;
	void (*SET_FRAMEp)(SEXP x, SEXP v) = SET_FRAME;
	void (*SET_HASHTABp)(SEXP x, SEXP v) = SET_HASHTAB;
    }
}

namespace {
    // Used in {,un}packGPBits():
    const unsigned int FRAME_LOCK_MASK = 1<<14;
    const unsigned int GLOBAL_FRAME_MASK = 1<<15;
}

GCRoot<Environment> Environment::s_empty_env(new Environment, true);
SEXP R_EmptyEnv = const_cast<Environment*>(Environment::emptyEnvironment());

GCRoot<Environment> Environment::s_base_env(new Environment(s_empty_env),
					    true);
SEXP R_BaseEnv = Environment::base();

GCRoot<Environment> Environment::s_global_env(new Environment(s_base_env),
					      true);
SEXP R_GlobalEnv = Environment::global();

unsigned int Environment::packGPBits() const
{
    unsigned int ans = RObject::packGPBits();
    if (m_locked) ans |= FRAME_LOCK_MASK;
    if (m_globally_cached) ans |= GLOBAL_FRAME_MASK;
    return ans;
}

const char* Environment::typeName() const
{
    return staticTypeName();
}

void Environment::unpackGPBits(unsigned int gpbits)
{
    RObject::unpackGPBits(gpbits);
    // Be careful with precedence!
    m_locked = ((gpbits & FRAME_LOCK_MASK) != 0);
    m_globally_cached = ((gpbits & GLOBAL_FRAME_MASK) != 0);
}

void Environment::visitChildren(const_visitor* v) const
{
    RObject::visitChildren(v);
    if (m_enclosing) m_enclosing->conductVisitor(v);
    if (m_frame) m_frame->conductVisitor(v);
    if (m_hashtable) m_hashtable->conductVisitor(v);
}
