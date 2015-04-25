/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file WeakRef.cpp
 *
 * Class WeakRef.
 */

#include <cstdlib>
#include <iostream>

#include "CXXR/CommandTerminated.hpp"
#include "CXXR/Environment.h"
#include "CXXR/Evaluator.h"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/ProtectStack.h"
#include "CXXR/WeakRef.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

int WeakRef::s_count = 0;

namespace {
    // Used in {,un}packGPBits:
    const unsigned int FINALIZE_ON_EXIT_MASK = 2;
}

WeakRef::WeakRef(RObject* key, RObject* value, FunctionBase* R_finalizer,
		 bool finalize_on_exit)
    : m_Cfinalizer(0),
      m_ready_to_finalize(false),
      m_finalize_on_exit(finalize_on_exit)
{
    m_key = key;
    m_value = value;
    m_Rfinalizer = R_finalizer;
    m_self = this;

    getLive()->push_back(this);
    m_lit = std::prev(getLive()->end());

    if (!m_key)
	tombstone();
    else switch (m_key->sexptype()) {
    case ENVSXP:
    case EXTPTRSXP:
	break;
    default:
	Rf_error(_("can only weakly reference/finalize reference objects"));
    }
    ++s_count;
}

WeakRef::WeakRef(RObject* key, RObject* value, R_CFinalizer_t C_finalizer,
		 bool finalize_on_exit)
    : m_Cfinalizer(C_finalizer),
      m_ready_to_finalize(false), m_finalize_on_exit(finalize_on_exit)
{
    m_key = key;
    m_value = value;
    m_Rfinalizer = 0;
    m_self = this;

    getLive()->push_back(this);
    m_lit = std::prev(getLive()->end());

    if (!m_key)
	tombstone();
    ++s_count;
}

WeakRef::~WeakRef()
{
    WRList* wrl = wrList();
    if (wrl)
	wrl->erase(m_lit);
    --s_count;
}

bool WeakRef::check()
{
#ifndef NDEBUG
    // Check sizes:
    size_t total_size = getLive()->size() + getFinalizationPending()->size()
	+ getTombstone()->size();
    if (total_size != size_t(s_count)) {
	cerr << "WeakRef::check() : tally error\n"
	     << "live size: " << getLive()->size()
	     << "\nfinalization pending size: "
	     << getFinalizationPending()->size()
	     << "\ntombstone size: " << getTombstone()->size()
	     << "\ns_count: " << s_count << '\n';
	abort();
    }
    // Check the live list:
    for (const WeakRef* wr: *getLive()) {
	if (wr->m_ready_to_finalize) {
	    cerr << "Node on live list set READY_TO_FINALIZE\n";
	    abort();
	}
	if (!wr->m_key) {
	    cerr << "Node on live list with null key\n";
	    abort();
	}
    }
    // Check finalization pending:
    for (const WeakRef* wr: *getFinalizationPending()) {
	if (!wr->m_ready_to_finalize) {
	    cerr << "Node on finalization pending list not READY_TO_FINALIZE\n";
	    abort();
	}
	if (!wr->m_key) {
	    cerr << "Node on finalization pending list with null key\n";
	    abort();
	}
	if (!wr->m_Rfinalizer && !wr->m_Cfinalizer) {
	    cerr << "Node on finalization pending list without finalizer\n";
	    abort();
	}
    }
    // Check tombstone:
    for (const WeakRef* wr: *getTombstone()) {
	if (wr->m_ready_to_finalize) {
	    cerr << "Node on tombstone list set READY_TO_FINALIZE\n";
	    abort();
	}
	if (wr->m_key) {
	    cerr << "Node on tombstone list with non-null key\n";
	    abort();
	}
    }
#endif
    return true;
}

void WeakRef::detachReferents()
{
    m_key.detach();
    m_value.detach();
    m_Rfinalizer.detach();
    m_self.detach();
    RObject::detachReferents();
}

void WeakRef::finalize()
{
    R_CFinalizer_t Cfin = m_Cfinalizer;
    GCStackRoot<> key(m_key);
    GCStackRoot<FunctionBase> Rfin(m_Rfinalizer);
    // Do this now to ensure that finalizer is run only once, even if
    // an error occurs:
    tombstone();
    if (Cfin) Cfin(key);
    else if (Rfin) {
	GCStackRoot<PairList> tail(new PairList(key));
	GCStackRoot<Expression> e(new Expression(Rfin, tail));
	Evaluator::evaluate(e, Environment::global());
    }
}

WeakRef::WRList* WeakRef::getLive()
{
    static WRList* live = new WRList();
    return live;
}

WeakRef::WRList* WeakRef::getFinalizationPending()
{
    static WRList* finalization_pending = new WRList();
    return finalization_pending;
}

WeakRef::WRList* WeakRef::getTombstone()
{
    static WRList* tombstone = new WRList();
    return tombstone;
}

void WeakRef::markThru()
{
    WeakRef::check();
    WRList newlive;
    
    WRList* live = getLive();
    WRList* finalization_pending = getFinalizationPending();

    // Step 2-3 of algorithm.  Mark the value and R finalizer if the
    // key is marked.
    {
	unsigned int marks_applied;
	do {
	    GCNode::Marker marker;
	    WRList::iterator lit = live->begin();
	    while (lit != live->end()) {
		WeakRef* wr = *lit++;
		RObject* key = wr->key();
		if (key->isMarked()) {
		    RObject* value = wr->value();
		    if (value)
			marker(value);
		    FunctionBase* Rfinalizer = wr->m_Rfinalizer;
		    if (Rfinalizer)
			marker(Rfinalizer);
		    wr->transfer(live, &newlive);
		}
	    }
	    marks_applied = marker.marksApplied();
	} while (marks_applied > 0);
    }
    // Step 4 of algorithm.  Process references with unmarked keys.
    {
	GCNode::Marker marker;
	WRList::iterator lit = live->begin();
	while (lit != live->end()) {
	    WeakRef* wr = *lit++;
	    FunctionBase* Rfinalizer = wr->m_Rfinalizer;
	    if (Rfinalizer)
		marker(Rfinalizer);
	    if (Rfinalizer || wr->m_Cfinalizer) {
		marker(wr);
		marker(wr->m_key);
		wr->m_ready_to_finalize = true;
		wr->transfer(live, finalization_pending);
	    }
	    else {
		wr->tombstone();
		// Expose to reference-counting collection:
		wr->m_self = nullptr;
	    }
	}
    }
    // Step 5 of algorithm.  Mark all live references with reachable keys.
    {
	GCNode::Marker marker;
	live->splice(live->end(), newlive);
	for (WeakRef* wr: *live) {
	    marker(wr);
	}
    }
}

unsigned int WeakRef::packGPBits() const
{
    unsigned int ans = RObject::packGPBits();
    if (m_finalize_on_exit)
	ans |= FINALIZE_ON_EXIT_MASK;
    return ans;
}

void WeakRef::runExitFinalizers()
{
    WeakRef::check();
    WRList* live = getLive();
    WRList* finalization_pending = getFinalizationPending();

    WRList::iterator lit = live->begin();
    while (lit != live->end()) {
	WeakRef* wr = *lit++;
	if (wr->m_finalize_on_exit) {
	    wr->m_ready_to_finalize = true;
	    wr->transfer(live, finalization_pending);
	}
    }
    runFinalizers();
}

bool WeakRef::runFinalizers()
{
    WeakRef::check();
    WRList* finalization_pending = getFinalizationPending();

    bool finalizer_run = !finalization_pending->empty();
    WRList::iterator lit = finalization_pending->begin();
    while (lit != finalization_pending->end()) {
	WeakRef* wr = *lit++;
	GCStackRoot<> topExp(R_CurrentExpr);
	size_t savestack = ProtectStack::size();
	{
	    // An Evaluator is declared for the finalizer to
	    // insure that any errors that might occur do not spill into
	    // the call that triggered the collection:
	    Evaluator evalr;
	    try {
		wr->finalize();
	    }
	    catch (CommandTerminated) {
	    }
	    // Expose WeakRef to reference-counting collection:
	    wr->m_self = nullptr;
	}
	ProtectStack::restoreSize(savestack);
	R_CurrentExpr = topExp;
    }
    return finalizer_run;
}

void WeakRef::tombstone()
{
    WRList* oldl = wrList();
    m_key = nullptr;
    m_value = nullptr;
    m_Rfinalizer = nullptr;
    m_Cfinalizer = nullptr;
    m_ready_to_finalize = false;
    transfer(oldl, getTombstone());
}

void WeakRef::unpackGPBits(unsigned int gpbits)
{
    RObject::unpackGPBits(gpbits);
    m_finalize_on_exit = ((gpbits & FINALIZE_ON_EXIT_MASK) != 0);
}

WeakRef::WRList* WeakRef::wrList() const
{
    return m_ready_to_finalize ? getFinalizationPending() :
	(m_key ? getLive() : getTombstone());
}

// ***** C interface *****

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    FunctionBase* finf = nullptr;
    if (fin) {
	finf = dynamic_cast<FunctionBase*>(fin);
	if (!finf)
	    Rf_error(_("finalizer must be a function or NULL"));
    } 
    return new WeakRef(key, val, finf, onexit);
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
    return new WeakRef(key, val, fin, onexit);
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
    R_MakeWeakRef(s, nullptr, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
    R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
    R_MakeWeakRefC(s, nullptr, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
    R_RegisterCFinalizerEx(s, fun, FALSE);
}

void R_RunExitFinalizers()
{
    WeakRef::runExitFinalizers();
}

SEXP R_WeakRefKey(SEXP w)
{
    if (w->sexptype() != WEAKREFSXP)
	Rf_error(_("not a weak reference"));
    WeakRef* wr = static_cast<WeakRef*>(w);
    return wr->key();
}

SEXP R_WeakRefValue(SEXP w)
{
    if (w->sexptype() != WEAKREFSXP)
	Rf_error(_("not a weak reference"));
    WeakRef* wr = static_cast<WeakRef*>(w);
    SEXP v = wr->value();
    if (v && NAMED(v) != 2)
	SET_NAMED(v, 2);
    return v;
}
