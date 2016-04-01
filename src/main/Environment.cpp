/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
 */

/** @file Environment.cpp
 *
 *
 * @brief Implementation of class rho:Environment and associated C
 * interface.
 */

#include "rho/Environment.hpp"

#include <cstdlib>
#include <iostream>
#include <typeinfo>
#include "R_ext/Error.h"
#include "localization.h"
#include "rho/BuiltInFunction.hpp"
#include "rho/FunctionBase.hpp"
#include "rho/ListFrame.hpp"
#include "rho/StdFrame.hpp"
#include "rho/StringVector.hpp"
#include "rho/Symbol.hpp"
#include "sparsehash/dense_hash_map"

using namespace std;
using namespace rho;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace rho {
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

    // Used by the cache.
    struct PointerHash {
	size_t operator()(const Symbol* value) const {
	    // Symbols are globally unique, so pointer equality is fine.
	    // The '>> 4' is because the low-order bits are generally
	    // zero due to alignment concerns.
	    return reinterpret_cast<intptr_t>(value) >> 4;
	}
    };
}

SEXP R_EmptyEnv;
SEXP R_BaseEnv;
SEXP R_GlobalEnv;
SEXP R_BaseNamespace;

class Environment::Cache : public google::dense_hash_map<
    const Symbol*, Frame::Binding*,
    PointerHash,
    std::equal_to<const Symbol*>,
    rho::Allocator<std::pair<const Symbol* const,
			      Frame::Binding*> >
    >
{ };

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

void Environment::detachFrame()
{
    setOnSearchPath(false);
    m_frame = nullptr;
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
    Cache* search_path_cache = searchPathCache();
    while (env) {
	if (env->isSearchPathCachePortal()) {
	    Cache::iterator it = search_path_cache->find(symbol);
	    if (it == search_path_cache->end())
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
		(*search_path_cache)[symbol] = bdg;
	    return bdg;
	}
	env = env->enclosingEnvironment();
    }
    return nullptr;
}

// Environment::findNamespace() is in envir.cpp

// Environment::findPackage() is in envir.cpp

void Environment::flushFromSearchPathCache(const Symbol* sym)
{
    Cache* search_path_cache = searchPathCache();

    if (sym)
	search_path_cache->erase(sym);
    else {
	// Clear the cache, but retain the current number of buckets:
	size_t buckets = search_path_cache->bucket_count();
	search_path_cache->clear();
	search_path_cache->rehash(buckets);
    }
}

Environment::Cache* Environment::createSearchPathCache()
{
    Cache* search_path_cache = new Cache();
    // Need a couple of pointers that won't be used elsewhere.
    static GCRoot<Symbol> empty_key = Symbol::createUnnamedSymbol();
    static GCRoot<Symbol> deleted_key = Symbol::createUnnamedSymbol();

    search_path_cache->set_empty_key(empty_key.get());
    search_path_cache->set_deleted_key(deleted_key.get());
    return search_path_cache;
}

Environment* Environment::createEmptyEnvironment()
{
    GCStackRoot<Frame> empty_frame(new ListFrame);
    return new Environment(0, empty_frame);
}

Environment* Environment::createBaseEnvironment()
{
    GCStackRoot<Frame> base_frame(new StdFrame);
    GCStackRoot<Environment> base(new Environment(empty(), base_frame));
    BuiltInFunction::addPrimitivesToEnvironment(base);
    return base;
}

Environment* Environment::createGlobalEnvironment()
{
    GCStackRoot<Frame> global_frame(new StdFrame);
    return new Environment(base(), global_frame);
}

Environment* Environment::createBaseNamespace()
{
    return new Environment(global(), base()->frame());
}

Environment::Cache* Environment::searchPathCache()
{
    static Cache* cache = createSearchPathCache();
    return cache;
}

void Environment::initialize()
{
    R_EmptyEnv = empty();
    R_BaseEnv = base();
    base()->setOnSearchPath(true);
    R_GlobalEnv = global();
    global()->setOnSearchPath(true);

    R_BaseNamespace = baseNamespace();
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
    GCStackRoot<Environment> new_env(new Environment(nullptr, frame));
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
    env_to_detach->m_enclosing = nullptr;
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

void Environment::nullEnvironmentError() {
    Rf_error(_("use of NULL environment is defunct"));
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

namespace rho {
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
namespace rho {
    void LS(SEXP s) {
	const Environment* env = SEXP_downcast<Environment*>(s);
	const Frame* frame = env->frame();
	vector<const Symbol*> syms = frame->symbols(true);
	for (vector<const Symbol*>::const_iterator it = syms.begin();
	     it != syms.end(); ++it) {
	    const Symbol* sym = *it;
	    const RObject* val = frame->binding(sym)->rawValue();
	    cout << '\"' << sym->name()->stdstring()
		 << "\" (\'rho::RObject\'*)" << val;
	    if (val)
		cout << " [" << typeid(*val).name() << ']';
	    cout << '\n';
	}
    }
}
