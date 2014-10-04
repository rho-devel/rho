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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

WeakRef::WRList* WeakRef::s_live;
WeakRef::WRList* WeakRef::s_f10n_pending;
WeakRef::WRList* WeakRef::s_tombstone;
int WeakRef::s_count = 0;

namespace {
    // Used in {,un}packGPBits:
    const unsigned int FINALIZE_ON_EXIT_MASK = 2;
}

WeakRef::WeakRef(RObject* key, RObject* value, FunctionBase* R_finalizer,
		 bool finalize_on_exit)
    : m_key(key), m_value(value), m_Rfinalizer(R_finalizer),
      m_self(expose(this)), m_Cfinalizer(nullptr),
      m_lit(s_live->insert(s_live->end(), this)), m_ready_to_finalize(false),
      m_finalize_on_exit(finalize_on_exit)
{
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
    : m_key(key), m_value(value), m_Rfinalizer(nullptr), m_self(expose(this)),
      m_Cfinalizer(C_finalizer), m_lit(s_live->insert(s_live->end(), this)),
      m_ready_to_finalize(false), m_finalize_on_exit(finalize_on_exit)
{
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
    // Check sizes:
    if (int(s_live->size() + s_f10n_pending->size() + s_tombstone->size())
	!= s_count) {
	cerr << "WeakRef::check() : tally error\n"
	     << "s_live->size(): " << s_live->size()
	     << "\ns_f10n_pending->size(): " << s_f10n_pending->size()
	     << "\ns_tombstone->size(): " << s_tombstone->size()
	     << "\ns_count: " << s_count << '\n';
	abort();
    }
    // Check s_live:
    for (WRList::const_iterator lit = s_live->begin();
	 lit != s_live->end(); ++lit) {
	const WeakRef* wr = *lit;
	if (wr->m_ready_to_finalize) {
	    cerr << "Node on s_live set READY_TO_FINALIZE\n";
	    abort();
	}
	if (!wr->m_key) {
	    cerr << "Node on s_live with null key\n";
	    abort();
	}
    }
    // Check s_f10n_pending:
    for (WRList::const_iterator lit = s_f10n_pending->begin();
	 lit != s_f10n_pending->end(); ++lit) {
	const WeakRef* wr = *lit;
	if (!wr->m_ready_to_finalize) {
	    cerr << "Node on s_f10n_pending not READY_TO_FINALIZE\n";
	    abort();
	}
	if (!wr->m_key) {
	    cerr << "Node on s_f10n_pending with null key\n";
	    abort();
	}
	if (!wr->m_Rfinalizer && !wr->m_Cfinalizer) {
	    cerr << "Node on s_f10n_pending without finalizer\n";
	    abort();
	}
    }
    // Check s_tombstone:
    for (WRList::const_iterator lit = s_tombstone->begin();
	 lit != s_tombstone->end(); ++lit) {
	const WeakRef* wr = *lit;
	if (wr->m_ready_to_finalize) {
	    cerr << "Node on s_tombstone set READY_TO_FINALIZE\n";
	    abort();
	}
	if (wr->m_key) {
	    cerr << "Node on s_tombstone with non-null key\n";
	    abort();
	}
    }
    return true;
}

void WeakRef::cleanup()
{
    s_tombstone = nullptr;
    s_f10n_pending = nullptr;
    s_live = nullptr;
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
	GCStackRoot<PairList> tail(expose(new PairList(key)));
	GCStackRoot<Expression> e(expose(new Expression(Rfin, tail)));
	Evaluator::evaluate(e, Environment::global());
    }
}

void WeakRef::initialize()
{
    static WRList live, f10n_pending, tombstone;
    s_live = &live;
    s_f10n_pending = &f10n_pending;
    s_tombstone = &tombstone;
}

void WeakRef::markThru()
{
    WeakRef::check();
    WRList newlive;
    // Step 2-3 of algorithm.  Mark the value and R finalizer if the
    // key is marked.
    {
	unsigned int marks_applied;
	do {
	    GCNode::Marker marker;
	    WRList::iterator lit = s_live->begin();
	    while (lit != s_live->end()) {
		WeakRef* wr = *lit++;
		RObject* key = wr->key();
		if (key->isMarked()) {
		    RObject* value = wr->value();
		    if (value)
			marker(value);
		    FunctionBase* Rfinalizer = wr->m_Rfinalizer;
		    if (Rfinalizer)
			marker(Rfinalizer);
		    wr->transfer(s_live, &newlive);
		}
	    }
	    marks_applied = marker.marksApplied();
	} while (marks_applied > 0);
    }
    // Step 4 of algorithm.  Process references with unmarked keys.
    {
	GCNode::Marker marker;
	WRList::iterator lit = s_live->begin();
	while (lit != s_live->end()) {
	    WeakRef* wr = *lit++;
	    FunctionBase* Rfinalizer = wr->m_Rfinalizer;
	    if (Rfinalizer)
		marker(Rfinalizer);
	    if (Rfinalizer || wr->m_Cfinalizer) {
		marker(wr);
		marker(wr->m_key);
		wr->m_ready_to_finalize = true;
		wr->transfer(s_live, s_f10n_pending);
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
	s_live->splice(s_live->end(), newlive);
	for (WRList::iterator lit = s_live->begin();
	     lit != s_live->end(); ++lit) {
	    WeakRef* wr = *lit;
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
    WRList::iterator lit = s_live->begin();
    while (lit != s_live->end()) {
	WeakRef* wr = *lit++;
	if (wr->m_finalize_on_exit) {
	    wr->m_ready_to_finalize = true;
	    wr->transfer(s_live, s_f10n_pending);
	}
    }
    runFinalizers();
}

bool WeakRef::runFinalizers()
{
    WeakRef::check();
    bool finalizer_run = !s_f10n_pending->empty();
    WRList::iterator lit = s_f10n_pending->begin();
    while (lit != s_f10n_pending->end()) {
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
    transfer(oldl, s_tombstone);
}

void WeakRef::unpackGPBits(unsigned int gpbits)
{
    RObject::unpackGPBits(gpbits);
    m_finalize_on_exit = ((gpbits & FINALIZE_ON_EXIT_MASK) != 0);
}

WeakRef::WRList* WeakRef::wrList() const
{
    return m_ready_to_finalize ? s_f10n_pending :
	(m_key ? s_live : s_tombstone);
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
