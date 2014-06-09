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
#include <iostream>
#include <typeinfo>
#include "R_ext/Error.h"
#include "localization.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/ListFrame.hpp"
#include "CXXR/StdFrame.hpp"
#include "CXXR/StringVector.h"
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

Environment::Cache* Environment::s_search_path_cache;
Environment* Environment::s_base;
Environment* Environment::s_base_namespace;
Environment* Environment::s_empty;
Environment* Environment::s_global;

SEXP R_EmptyEnv;
SEXP R_BaseEnv;
SEXP R_GlobalEnv;
SEXP R_BaseNamespace;

// The implementation assumes that any loops in the node graph will
// include at least one Environment.
void Environment::LeakMonitor::operator()(const GCNode* node)
{
    if (typeid(*node) == typeid(Environment)) {
	const Environment* env = static_cast<const Environment*>(node);
	if (env->m_leaked)
	    return;
	env->m_leaked = true;
    }
    node->visitReferents(this);
}

void Environment::cleanup()
{
    delete s_search_path_cache;
    s_search_path_cache = 0;
}

void Environment::detachFrame()
{
    setOnSearchPath(false);
    m_frame = 0;
}

void Environment::detachReferents()
{
    setOnSearchPath(false);
    m_enclosing.detach();
    m_frame.detach();
    RObject::detachReferents();
}

// Define the preprocessor variable CHECK_CACHE to verify that the
// search list cache is delivering correct results.

Frame::Binding* Environment::findBinding(const Symbol* symbol)
{
    bool cache_miss = false;
    Environment* env = this;
#ifdef CHECK_CACHE
    Frame::Binding cache_binding = 0;
#endif
    while (env) {
	if (env->isSearchPathCachePortal()) {
	    Cache::iterator it = s_search_path_cache->find(symbol);
	    if (it == s_search_path_cache->end())
		cache_miss = true;
#ifdef CHECK_CACHE
	    else cache_binding = it->second;
#else
	    else return it->second;
#endif
	}
	Frame::Binding* bdg = env->frame()->binding(symbol);
	if (bdg) {
#ifdef CHECK_CACHE
	    if (cache_binding && cache_binding != bdg)
		abort();
#endif
	    if (cache_miss)
		(*s_search_path_cache)[symbol] = bdg;
	    return bdg;
	}
	env = env->enclosingEnvironment();
    }
    return 0;
}

// Environment::findNamespace() is in envir.cpp

// Environment::findPackage() is in envir.cpp

void Environment::flushFromSearchPathCache(const Symbol* sym)
{
    if (!s_search_path_cache)
	return;

    if (sym)
	s_search_path_cache->erase(sym);
    else {
	// Clear the cache, but retain the current number of buckets:
	size_t buckets = s_search_path_cache->bucket_count();
	s_search_path_cache->clear();
	s_search_path_cache->rehash(buckets);
    }
}

void Environment::initialize()
{
    // 509 is largest prime <= 512.  This will have capacity for 254
    // Symbols at load factor 0.5.
    s_search_path_cache = new Cache(509);
    s_search_path_cache->max_load_factor(0.5);
    GCStackRoot<Frame> empty_frame(CXXR_NEW(ListFrame));
    static GCRoot<Environment> empty_env(CXXR_NEW(Environment(0, empty_frame)));
    s_empty = empty_env.get();
    R_EmptyEnv = s_empty;
    GCStackRoot<Frame> base_frame(CXXR_NEW(StdFrame));
    static GCRoot<Environment>
	base_env(CXXR_NEW(Environment(empty_env, base_frame)));
    s_base = base_env.get();
    s_base->setOnSearchPath(true);
    R_BaseEnv = s_base;
    GCStackRoot<Frame> global_frame(CXXR_NEW(StdFrame));
    static GCRoot<Environment>
	global_env(CXXR_NEW(Environment(s_base, global_frame)));
    s_global = global_env.get();
    s_global->setOnSearchPath(true);
    R_GlobalEnv = s_global;
    static GCRoot<Environment>
	base_namespace(CXXR_NEW(Environment(s_global, s_base->frame())));
    s_base_namespace = base_namespace.get();
    R_BaseNamespace = s_base_namespace;
}

void Environment::setOnSearchPath(bool status) {
    if (status == m_on_search_path)
	return;

    m_on_search_path = status;
    if (!m_frame)
	return;

    if (status)
	m_frame->incCacheCount();
    else
	m_frame->decCacheCount();

    // Invalidate cache entries.
    std::vector<const Symbol*> symbols = frame()->symbols(true);
    for (std::vector<const Symbol*>::const_iterator symbol = symbols.begin();
	 symbol != symbols.end(); ++symbol) {
	flushFromSearchPathCache(*symbol);
    }
}

// Environment::namespaceSpec() is in envir.cpp

const char* Environment::package_s11n_aux(const StringVector* pkg_name)
{
    const char* name = (*pkg_name)[0]->c_str();
    Rf_warning(_("'%s' may not be available when loading"), name);
    return name;
}

unsigned int Environment::packGPBits() const
{
    unsigned int ans = RObject::packGPBits();
    if (m_locked) ans |= FRAME_LOCK_MASK;
    // if (m_globally_cached) ans |= GLOBAL_FRAME_MASK;
    return ans;
}

// Environment::packageName() in in envir.cpp

void  Environment::setEnclosingEnvironment(Environment* new_enclos)
{
    m_enclosing = new_enclos;
    // Recursively propagate participation in search list cache:
    if (m_on_search_path) {
	Environment* env = m_enclosing;
	while (env && !env->m_on_search_path) {
	    env->setOnSearchPath(true);
	    env = env->m_enclosing;
	}
    }
}

Environment* Environment::attachToSearchPath(int pos, StringVector* name)
{
    // Duplicate the environment.
    GCStackRoot<Frame> frame(static_cast<Frame*>(m_frame->clone()));
    GCStackRoot<Environment> new_env(expose(new Environment(0, frame)));
    new_env->setAttribute(NameSymbol, name);

    // Iterate through the search path to the environment just before where we
    // want to insert.
    // This will be either pos - 1 or the environment prior to base().
    Environment* where = global();
    for (int n = 1; n < pos - 1 && where->enclosingEnvironment() != base(); n++)
	where = where->enclosingEnvironment();

    // Insert the new environment after where.
    new_env->m_enclosing = where->m_enclosing;
    where->m_enclosing = new_env;
    new_env->setOnSearchPath(true);

    return new_env;
}

Environment* Environment::detachFromSearchPath(int pos) {
    if (pos == 1)
	error(_("invalid '%s' argument"), "pos");

    // Iterate through the search path to the environment just before where we
    // want to detach.
    Environment* where = global();
    for (int n = 1; n < pos - 1 && where->enclosingEnvironment() != empty(); n++)
	where = where->enclosingEnvironment();

    Environment *env_to_detach = where->enclosingEnvironment();
    if (env_to_detach == base())
	error(_("detaching \"package:base\" is not allowed"));
    if (env_to_detach == empty())
	error(_("invalid '%s' argument"), "pos");

    // Detach the environment after where.
    where->m_enclosing = env_to_detach->m_enclosing;
    env_to_detach->m_enclosing = 0;
    env_to_detach->setOnSearchPath(false);

    return env_to_detach;
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

BOOST_CLASS_EXPORT_IMPLEMENT(CXXR::Environment)

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
    FunctionBase*
    findFunction(const Symbol* symbol, Environment* env, bool inherits)
    {
	FunctionTester functest(symbol);
	RObject *value = findTestedValue(symbol, env, functest, inherits);
	return static_cast<FunctionBase*>(value);
    }
}

// Utility intended to be called from a debugger.  Prints out the
// names of the Symbols in an Environment, together with the addresses
// the Symbols are bound to.
namespace CXXR {
    void LS(SEXP s) {
	const Environment* env = SEXP_downcast<Environment*>(s);
	const Frame* frame = env->frame();
	vector<const Symbol*> syms = frame->symbols(true);
	for (vector<const Symbol*>::const_iterator it = syms.begin();
	     it != syms.end(); ++it) {
	    const Symbol* sym = *it;
	    const RObject* val = frame->binding(sym)->rawValue();
	    cout << '\"' << sym->name()->stdstring()
		 << "\" (\'CXXR::RObject\'*)" << val;
	    if (val)
		cout << " [" << typeid(*val).name() << ']';
	    cout << '\n';
	}
    }
}
