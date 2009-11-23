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

#include "CXXR/DottedArgs.hpp"
#include "CXXR/Environment.h"
#include "CXXR/GCStackRoot.h"
#include "CXXR/PairList.h"
#include "CXXR/Promise.h"
#include "CXXR/Symbol.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

ArgMatcher::ArgMatcher(PairList* formals)
    : m_formals(formals), m_has_dots(false)
{
    for (PairList* f = formals; f; f = f->tail()) {
	Symbol* sym = SEXP_downcast<Symbol*>(f->tag());
	if (!sym)
	    Rf_error(_("formal arguments must be named"));
	if (sym == DotsSymbol) {
	    if (m_has_dots)
		Rf_error(_("formals list contains more than one '...'"));
	    if (f->car() != Symbol::missingArgument())
		Rf_error(_("'...' formal cannot have a default value"));
	    m_has_dots = true;
	} else {
	    FormalData fdata = {sym, m_has_dots, f->car()};
	    pair<FormalMap::const_iterator, bool> pr =
		m_formal_index.insert(make_pair(sym->name(),
						m_formal_data.size()));
	    if (!pr.second)
		Rf_error(_("duplicated name in formals list"));
	    m_formal_data.push_back(fdata);
	}
    }
}

void ArgMatcher::detachReferents()
{
    m_formals.detach();
    m_formal_data.clear();
    m_formal_index.clear();
    m_supplied_list.clear();
}

void ArgMatcher::handleDots(Frame* frame)
{
    Frame::Binding* bdg = frame->obtainBinding(DotsSymbol);
    if (m_supplied_list.empty())
	bdg->setValue(0, Frame::Binding::EXPLICIT);
    else {
	SuppliedList::iterator first = m_supplied_list.begin();
	DottedArgs* dotted_args
	    = expose(new DottedArgs((*first).value, 0, (*first).name));
	bdg->setValue(dotted_args, Frame::Binding::EXPLICIT);
	m_supplied_list.erase(first);
	GCStackRoot<PairList> tail;
	for (SuppliedList::const_reverse_iterator rit = m_supplied_list.rbegin();
	     rit != m_supplied_list.rend(); ++rit)
	    tail = PairList::construct((*rit).value, tail, (*rit).name);
	dotted_args->setTail(tail);
	m_supplied_list.clear();
    }
}
	     
bool ArgMatcher::isPrefix(const CachedString* shorter,
			  const CachedString* longer)
{
    const string& shortstr = shorter->stdstring();
    const string& longstr = longer->stdstring();
    return longstr.compare(0, shortstr.size(), shortstr) == 0;
}

void ArgMatcher::makeBinding(Environment* target_env, const FormalData& fdata,
			     RObject* supplied_value)
{
    RObject* value = supplied_value;
    Frame::Binding::Origin origin = Frame::Binding::EXPLICIT;
    if (value == Symbol::missingArgument()
	&& fdata.value != Symbol::missingArgument()) {
	origin = Frame::Binding::DEFAULTED;
	value = expose(new Promise(fdata.value, target_env));
    }
    Frame::Binding* bdg = target_env->frame()->obtainBinding(fdata.symbol);
    // Don't trump a previous binding with Symbol::missingArgument() :
    if (value != Symbol::missingArgument())
	bdg->setValue(value, origin);
}
    
void ArgMatcher::match(Environment* target_env, PairList* supplied,
		       Environment* supplieds_env)
{
    Frame* frame = target_env->frame();
    vector<MatchStatus, Allocator<MatchStatus> >
	formals_status(m_formal_data.size(), UNMATCHED);
    // Exact matches by tag:
    {
	unsigned int sindex = 0;
	for (PairList* s = supplied; s; s = s->tail()) {
	    ++sindex;
	    GCStackRoot<CachedString> name(tag2cs(s->tag()));
	    GCStackRoot<> value(s->car());
	    if (supplieds_env)
		value = expose(new Promise(value, supplieds_env));
	    FormalMap::const_iterator fmit 
		= (name ? m_formal_index.lower_bound(name)
		   : m_formal_index.end());
	    if (fmit != m_formal_index.end() && (*fmit).first == name) {
		// Exact tag match:
		unsigned int findex = (*fmit).second;
		const FormalData& fdata = m_formal_data[findex];
		formals_status[findex] = EXACT_TAG;
		makeBinding(target_env, fdata, value);
	    } else {
		// No exact tag match, so place supplied arg on list:
		SuppliedData supplied_data
		    = {GCEdge<CachedString>(name),
		       GCEdge<>(value), fmit, sindex};
		m_supplied_list.push_back(supplied_data);
	    }
	}
    }
    // Partial matches by tag:
    {
	SuppliedList::iterator slit = m_supplied_list.begin();
	while (slit != m_supplied_list.end()) {
	    SuppliedList::iterator next = slit;
	    ++next;
	    const SuppliedData& supplied_data = *slit;
	    FormalMap::const_iterator fmit = supplied_data.fm_iter;
	    // Within m_formal_index, skip formals formals following
	    // '...' and formals with exact matches:
	    while (fmit != m_formal_index.end()
		   && (m_formal_data[(*fmit).second].follows_dots
		       || formals_status[(*fmit).second] == EXACT_TAG))
		++fmit;
	    if (fmit != m_formal_index.end()
		&& isPrefix(supplied_data.name, (*fmit).first)) {
		// This is a potential partial match.  Remember the formal:
		unsigned int findex = (*fmit).second;
		// Has this formal already been partially matched? :
		if (formals_status[(*fmit).second] == PARTIAL_TAG)
		    Rf_error(_("formal argument '%s' matched by "
			       "multiple actual arguments"),
			     (*fmit).first->c_str());
		// Does supplied arg partially match anything else? :
		do {
		    ++fmit;
		} while (fmit != m_formal_index.end()
			 && formals_status[(*fmit).second] == EXACT_TAG);
		if (fmit != m_formal_index.end()
		    && isPrefix(supplied_data.name, (*fmit).first))
		    Rf_error(_("argument %d matches multiple formal arguments"),
			     supplied_data.index);
		// Partial match is OK:
		const FormalData& fdata = m_formal_data[findex];
		formals_status[findex] = PARTIAL_TAG;
		makeBinding(target_env, fdata, supplied_data.value);
		m_supplied_list.erase(slit);
	    }
	    slit = next;
	}
    }
    // Positional matching and default values:
    {
	const size_t numformals = m_formal_data.size();
	SuppliedList::iterator slit = m_supplied_list.begin();
	for (unsigned int findex = 0; findex < numformals; ++findex) {
	    if (formals_status[findex] == UNMATCHED) {
		const FormalData& fdata = m_formal_data[findex];
		RObject* value = Symbol::missingArgument();
		// Skip supplied arguments with names:
		while (slit != m_supplied_list.end() && (*slit).name.get())
		    ++slit;
		if (slit != m_supplied_list.end()) {
		    // Handle positional match:
		    const SuppliedData& supplied_data = *slit;
		    value = supplied_data.value;
		    formals_status[findex] = POSITIONAL;
		    m_supplied_list.erase(slit++);
		}
		makeBinding(target_env, fdata, value);
	    }
	}
    }
    // Any remaining supplied args are either rolled into ... or
    // there's an error:
    if (m_has_dots)
	handleDots(frame);
    else if (!m_supplied_list.empty())
	unusedArgsError();
}

// Implementation of ArgMatcher::tag2cs() is in match.cpp

// Implementation of ArgMatcher::unusedArgsError() is in match.cpp

void ArgMatcher::visitReferents(const_visitor* v) const
{
    if (m_formals) m_formals->conductVisitor(v);
    for (list<SuppliedData>::const_iterator it = m_supplied_list.begin();
	 it != m_supplied_list.end(); ++it) {
	const CachedString* name = (*it).name;
	if (name)
	    name->conductVisitor(v);
	RObject* value = (*it).value;
	if (value)
	    value->conductVisitor(v);
    }
}
