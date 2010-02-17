/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
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

#include <cstdlib>
#include "R_ext/Error.h"
#include "localization.h"
#include "CXXR/FunctionBase.h"
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
	void (*SET_ENV_DEBUGp)(SEXP x, Rboolean v) = SET_ENV_DEBUG;
	void (*SET_SYMVALUEp)(SEXP x, SEXP v) = SET_SYMVALUE;
	SEXP (*SYMVALUEp)(SEXP x) = SYMVALUE;
    }
}

namespace {
    // Used in {,un}packGPBits():
    const unsigned int FRAME_LOCK_MASK = 1<<14;
    const unsigned int GLOBAL_FRAME_MASK = 1<<15;
}

Environment::Cache* Environment::s_cache;
Environment* Environment::s_base;
Environment* Environment::s_base_namespace;
Environment* Environment::s_global;

SEXP R_EmptyEnv;
SEXP R_BaseEnv;
SEXP R_GlobalEnv;
SEXP R_BaseNamespace;

void Environment::cleanup()
{
    delete s_cache;
}

void Environment::detachReferents()
{
    m_enclosing.detach();
    if (m_cached && m_frame)
	m_frame->decCacheCount();
    m_frame.detach();
    RObject::detachReferents();
}

// Define the preprocessor variable CHECK_CACHE to verify that the
// search list cache is delivering correct results.

pair<Environment*, Frame::Binding*>
Environment::findBinding(const Symbol* symbol)
{
    bool cache_miss = false;
    Environment* env = this;
#ifdef CHECK_CACHE
    EBPair cachepr(0, 0);
#endif
    while (env) {
	if (env->isCachePortal()) {
	    Cache::iterator it = s_cache->find(symbol);
	    if (it == s_cache->end())
		cache_miss = true;
#ifdef CHECK_CACHE
	    else cachepr = (*it).second;
#else
	    else return (*it).second;
#endif
	}
	Frame::Binding* bdg = env->frame()->binding(symbol);
	if (bdg) {
	    EBPair ans(env, bdg);
#ifdef CHECK_CACHE
	    if (cachepr.first && cachepr != ans)
		abort();
#endif
	    if (cache_miss)
		(*s_cache)[symbol] = ans;
	    return ans;
	}
	env = env->enclosingEnvironment();
    }
    return EBPair(0, 0);
}

void Environment::flushFromCache(const Symbol* sym)
{
    if (sym)
	s_cache->erase(sym);
    else {
	// Clear the cache, but retain the current number of buckets:
	size_t buckets = s_cache->bucket_count();
	s_cache->clear();
	s_cache->rehash(buckets);
    }
}

void Environment::initialize()
{
    // 509 is largest prime <= 512.  This will have capacity for 254
    // Symbols at load factor 0.5.
    s_cache = new Cache(509);
    s_cache->max_load_factor(0.5);
    static GCRoot<Environment> empty_env(GCNode::expose(new Environment(0)));
    R_EmptyEnv = empty_env.get();
    static GCRoot<Environment>
	base_env(GCNode::expose(new Environment(empty_env)));
    s_base = base_env.get();
    s_base->makeCached();
    R_BaseEnv = s_base;
    static GCRoot<Environment>
	global_env(GCNode::expose(new Environment(s_base)));
    s_global = global_env.get();
    s_global->makeCached();
    R_GlobalEnv = s_global;
    static GCRoot<Environment>
	base_namespace(GCNode::expose(new Environment(s_global,
						      s_base->frame())));
    s_base_namespace = base_namespace.get();
    R_BaseNamespace = s_base_namespace;
}

void Environment::makeCached()
{
    if (!m_cached && m_frame)
	m_frame->incCacheCount();
    m_cached = true;
}

unsigned int Environment::packGPBits() const
{
    unsigned int ans = RObject::packGPBits();
    if (m_locked) ans |= FRAME_LOCK_MASK;
    // if (m_globally_cached) ans |= GLOBAL_FRAME_MASK;
    return ans;
}

void  Environment::setEnclosingEnvironment(Environment* new_enclos)
{
    m_enclosing = new_enclos;
    // Recursively propagate participation in search list cache:
    if (m_cached) {
	Environment* env = m_enclosing;
	while (env && !env->m_cached) {
	    env->makeCached();
	    env = env->m_enclosing;
	}
	flushFromCache(0);
    }
}

void Environment::skipEnclosing()
{
    if (!m_enclosing)
	Rf_error(_("this Environment has no enclosing Environment."));
    if (m_enclosing->m_cached)
	flushFromCache(0);
    m_enclosing = m_enclosing->m_enclosing;
}

void Environment::slotBehind(Environment* anchor)
{
    if (!anchor || anchor == this)
	Rf_error("internal error in Environment::slotBehind()");
    // Propagate participation in search list cache:
    if (anchor->m_cached) {
	makeCached();
	flushFromCache(0);
    }
    m_enclosing = anchor->m_enclosing;
    anchor->m_enclosing = this;
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

void Environment::visitReferents(const_visitor* v) const
{
    const GCNode* enc = m_enclosing;
    const GCNode* frame = m_frame;
    RObject::visitReferents(v);
    if (enc)
	(*v)(enc);
    if (frame)
	(*v)(frame);
}

// ***** Free-standing functions *****

namespace {
    // Predicate used to test whether a Binding's value is a function.
    class FunctionTester : public unary_function<RObject*, bool> {
    public:
	FunctionTester(const Symbol* symbol)
	    : m_symbol(symbol)
	{}

	bool operator()(const RObject* obj);
    private:
	const Symbol* m_symbol;
    };

    bool FunctionTester::operator()(const RObject* obj)
    {
	if (obj == R_MissingArg)
	    Rf_error(_("argument \"%s\" is missing, with no default"),
		     m_symbol->name()->c_str());
	return FunctionBase::isA(obj);
    }
}

namespace CXXR {
    pair<Environment*, FunctionBase*>
    findFunction(const Symbol* symbol, Environment* env, bool inherits)
    {
	FunctionTester functest(symbol);
	pair<Environment*, RObject*> pr
	    = findTestedValue(symbol, env, functest, inherits);
	return make_pair(pr.first, static_cast<FunctionBase*>(pr.second));
    }
}
