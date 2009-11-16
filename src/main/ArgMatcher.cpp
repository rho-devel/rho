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

/** @file ArgMatcher.cpp
 *
 * Implementation of class ArgMatcher.
 */

#include "CXXR/ArgMatcher.hpp"

#include "CXXR/Environment.h"
#include "CXXR/PairList.h"
#include "CXXR/Promise.h"
#include "CXXR/Symbol.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

ArgMatcher::ArgMatcher(PairList* formals, Environment* defaults_env)
    : m_formals(formals), m_defaults_env(defaults_env)
{
    bool dotsseen = false;
    for (PairList* f = formals; f; f = f->tail()) {
	Symbol* sym = SEXP_downcast<Symbol*>(f->tag());
	if (!sym)
	    Rf_error(_("formal arguments must be named"));
	FormalData fdata = {sym, dotsseen, f->car()};
	if (sym == DotsSymbol) {
	    if (dotsseen)
		Rf_error(_("formals list contains more than one '...'"));
	    dotsseen = true;
	} else {
	    pair<FormalMap::const_iterator, bool> pr =
		m_formal_index.insert(make_pair(sym->name(),
						m_formal_data.size()));
	    if (!pr.second)
		Rf_error(_("duplicated name in formals list"));
	}
	m_formal_data.push_back(fdata);
    }
}

void ArgMatcher::detachReferents()
{
    m_formals.detach();
    m_defaults_env.detach();
    m_formal_data.clear();
    m_formal_index.clear();
}

void ArgMatcher::makeBinding(Frame* frame, const FormalData& fdata,
			     RObject* supplied_value)
{
    RObject* value = supplied_value;
    Frame::Binding::Origin origin = Frame::Binding::EXPLICIT;
    if (value == Symbol::missingArgument()) {
	if (fdata.value == Symbol::missingArgument())
	    origin = Frame::Binding::MISSING;
	else {
	    origin = Frame::Binding::DEFAULTED;
	    value = GCNode::expose(new Promise(fdata.value, m_defaults_env));
	}
    }
    Frame::Binding* bdg = frame->obtainBinding(fdata.symbol);
    bdg->setValue(value, origin);
}
    
void ArgMatcher::match(Frame* frame, PairList* supplied)
{
    vector<MatchStatus, Allocator<MatchStatus> >
	formals_status(m_formal_data.size(), UNMATCHED);
    for (PairList* s = supplied; s; s->tail()) {
	const CachedString* name = tag2cs(s->tag());
	if (name) {
	    FormalMap::const_iterator fit = m_formal_index.find(name);
	    if ((*fit).first == name) {
		// Exact match:
		unsigned int index = (*fit).second;
		const FormalData& fdata = m_formal_data[index];
		formals_status[index] = EXACT_TAG;
		makeBinding(frame, fdata, s->car());
	    }
	}
    }
}

// Implementation of ArgMatcher::tag2cs() is in match.cpp

void ArgMatcher::visitReferents(const_visitor* v) const
{
    if (m_formals) m_formals->conductVisitor(v);
    if (m_defaults_env) m_defaults_env->conductVisitor(v);
}
