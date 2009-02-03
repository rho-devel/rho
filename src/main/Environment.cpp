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

#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*ENCLOSp)(SEXP x) = ENCLOS;
	Rboolean (*ENV_DEBUGp)(SEXP x) = ENV_DEBUG;
	Rboolean (*isEnvironmentptr)(SEXP s) = Rf_isEnvironment;
	SEXP (*FRAMEp)(SEXP x) = FRAME;
	void (*SET_ENCLOSp)(SEXP x, SEXP v) = SET_ENCLOS;
	void (*SET_ENV_DEBUGp)(SEXP x, Rboolean v) = SET_ENV_DEBUG;
    }
}

namespace {
    // Used in {,un}packGPBits():
    const unsigned int FRAME_LOCK_MASK = 1<<14;
    const unsigned int GLOBAL_FRAME_MASK = 1<<15;
}

// Environment::s_base_env etc. are defined in StdEnvironment.cpp

SEXP R_BaseEnv = Environment::base();

SEXP R_EmptyEnv = const_cast<Environment*>(Environment::emptyEnvironment());

SEXP R_GlobalEnv = Environment::global();


// ***** Class Environment::Binding *****

PairList* Environment::Binding::asPairList(PairList* tail) const
{
    PairList* ans
	= new PairList(m_value, tail, const_cast<Symbol*>(symbol()));
    SET_MISSING(ans, missing());
    if (isActive()) SET_ACTIVE_BINDING_BIT(ans);
    if (isLocked()) LOCK_BINDING(ans);
    ans->expose();
    return ans;
}

// Environment::Binding::assign() is defined in envir.cpp (for the time being).
	
void Environment::Binding::fromPairList(PairList* pl)
{
    const RObject* tag = pl->tag();
    if (tag && tag != m_symbol)
	Rf_error(_("internal error in %s"),
		 "Environment::Binding::fromPairList()");
    if (pl->m_active_binding)
	setFunction(SEXP_downcast<FunctionBase*>(pl->car()));
    else setValue(pl->car());
    setMissing(pl->m_missing);
    setLocking(pl->m_binding_locked);
}
    
void Environment::Binding::initialize(const Environment* env,
				      const Symbol* sym)
{
    if (m_environment)
	Rf_error(_("internal error: binding already initialized"));
    if (!env || !sym)
	Rf_error(_("internal error in %s"),
		 "Environment::Binding::initialize()");
    m_environment = env;
    m_symbol = sym;
    env->propagateAge(sym);
}

void Environment::Binding::setFunction(FunctionBase* function)
{
    // See if binding already has a non-null value:
    if (m_value) {
	if (!isActive())
	    Rf_error(_("symbol already has a regular binding"));
	if (isLocked())
	    Rf_error(_("cannot change active binding if binding is locked"));
    }
    m_value = function;
    m_environment->propagateAge(m_value);
    m_active = true;
    m_environment->monitorWrite(*this);
}

void Environment::Binding::setMissing(short int missingval)
{
    if (isLocked())
	Rf_error(_("cannot change missing status of a locked binding"));
    m_missing = missingval;
}

void Environment::Binding::setValue(RObject* new_value)
{
    if (isLocked())
	Rf_error(_("cannot change value of locked binding for '%s'"),
		 symbol()->name()->c_str());
    if (isActive())
	Rf_error(_("internal error: use %s for active bindings"),
		 "setFunction()");
    m_value = new_value;
    m_environment->propagateAge(m_value);
    m_environment->monitorWrite(*this);
}

// Environment::Binding::value() is defined in envir.cpp (for the time being).
	
void Environment::Binding::visitChildren(const_visitor* v) const
{
    // We assume the visitor has just from m_environment, so we don't
    // visit that.
    if (m_symbol) m_symbol->conductVisitor(v);
    if (m_value) m_value->conductVisitor(v);
}


// ***** Class Environment itself *****

Environment::Binding*
Environment::binding(const Symbol* symbol, bool recursive)
{
    if (recursive) abort();
    return frameBinding(symbol);
}

const Environment::Binding*
Environment::binding(const Symbol* symbol, bool recursive) const
{
    if (recursive) abort();
    return frameBinding(symbol);
}

unsigned int Environment::packGPBits() const
{
    unsigned int ans = RObject::packGPBits();
    if (m_locked) ans |= FRAME_LOCK_MASK;
    // if (m_globally_cached) ans |= GLOBAL_FRAME_MASK;
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
    // m_globally_cached = ((gpbits & GLOBAL_FRAME_MASK) != 0);
}

void Environment::visitChildren(const_visitor* v) const
{
    RObject::visitChildren(v);
    if (m_enclosing) m_enclosing->conductVisitor(v);
}

namespace CXXR {
    void envReadPairList(Environment* env, PairList* bindings)
    {
	for (PairList* pl = bindings; pl != 0; pl = pl->tail()) {
	    RObject* tag = pl->tag();
	    Symbol* symbol = dynamic_cast<Symbol*>(tag);
	    if (!symbol) Rf_error(_("list used to set environment bindings"
				    " must have symbols as tags throughout"));
	    Environment::Binding* bdg = env->obtainBinding(symbol);
	    bdg->fromPairList(pl);
	}
    }
}
