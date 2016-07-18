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
 *  https://www.R-project.org/Licenses/
 */

/** @file ArgMatcher.cpp
 *
 * Implementation of class ArgMatcher.
 */

#define R_NO_REMAP

#include "rho/ArgMatcher.hpp"

#include "rho/ArgList.hpp"
#include "rho/DottedArgs.hpp"
#include "rho/Environment.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/PairList.hpp"
#include "rho/Promise.hpp"
#include "rho/Symbol.hpp"
#include "rho/errors.hpp"
#include "boost/functional/hash.hpp"
#include "sparsehash/dense_hash_set"

using namespace std;
using namespace rho;

bool ArgMatcher::s_warn_on_partial_match = false;

ArgMatcher::ArgMatcher(const PairList* formals)
    : m_dots_position(-1)
{
    attachReference(m_formals, formals);

    for (const PairList* f = formals; f; f = f->tail()) {
	const Symbol* sym = dynamic_cast<const Symbol*>(f->tag());
	if (!sym)
	    Rf_error(_("invalid formal arguments for 'function'"));
	if (sym == DotsSymbol) {
	    if (has3Dots())
		Rf_error(_("formals list contains more than one '...'"));
	    if (f->car() != Symbol::missingArgument())
		Rf_error(_("'...' formal cannot have a default value"));
	    m_dots_position = m_formal_data.size();
	} else {
	    pair<FormalMap::const_iterator, bool> pr =
		m_formal_index.insert(make_pair(sym->name(),
						m_formal_data.size()));
	    if (!pr.second)
		Rf_error(_("duplicated name in formals list"));
	}

	FormalData fdata = {sym, has3Dots(), f->car(),
			    static_cast<unsigned>(m_formal_data.size()) };
	m_formal_data.push_back(fdata);
    }
}

void ArgMatcher::detachReferents()
{
    detachReference(m_formals);
    m_formal_data.clear();
    m_formal_index.clear();
}

bool ArgMatcher::isPrefix(const String* shorter, const String* longer)
{
    size_t length = shorter->size();
    return strncmp(shorter->c_str(), longer->c_str(), length) == 0;
}

ArgMatcher* ArgMatcher::make(Symbol* fml1, Symbol* fml2, Symbol* fml3,
			     Symbol* fml4, Symbol* fml5, Symbol* fml6)
{
    GCStackRoot<PairList> formals;
    if (fml6)
	formals = PairList::cons(Symbol::missingArgument(), formals, fml6);
    if (fml5)
	formals = PairList::cons(Symbol::missingArgument(), formals, fml5);
    if (fml4)
	formals = PairList::cons(Symbol::missingArgument(), formals, fml4);
    if (fml3)
	formals = PairList::cons(Symbol::missingArgument(), formals, fml3);
    if (fml2)
	formals = PairList::cons(Symbol::missingArgument(), formals, fml2);
    if (fml1)
	formals = PairList::cons(Symbol::missingArgument(), formals, fml1);
    return new ArgMatcher(formals);
}

void ArgMatcher::makeBinding(Environment* target_env, const FormalData& fdata,
			     Frame::Binding::Origin origin,
			     RObject* value)
{
    if (origin == Frame::Binding::DEFAULTED) {
	if (fdata.value != Symbol::missingArgument())
	    value = new Promise(fdata.value, target_env);
    }
    Frame::Binding* bdg = target_env->frame()->obtainBinding(fdata.symbol);
    // Don't trump a previous binding with Symbol::missingArgument() :
    if (value != Symbol::missingArgument())
	bdg->setValue(value, origin);
}

class ArgMatcher::MatchCallback {
public:
    typedef ArgMatcher::FormalData FormalData;
    typedef ArgMatcher::SuppliedData SuppliedData;
    typedef ArgMatcher::SuppliedList SuppliedList;
    typedef boost::iterator_range<std::vector<int>::const_iterator> ArgIndices;

    virtual void matchedArgument(const FormalData& formal,
				 int arg_index,
				 RObject* value) = 0;

    virtual void defaultValue(const FormalData& formal) = 0;

    virtual void dottedArgs(const FormalData& formal,
			    ArgIndices arg_indices,
			    const ArgList* all_args) = 0;
};

void ArgMatcher::matchWithCache(const ArgList* supplied,
				MatchCallback* callback,
				const ArgMatchInfo* matching) const
{
    for (int findex = 0; findex < m_formal_data.size(); findex++) {
	const FormalData& fdata = m_formal_data[findex];
	int sindex = matching->getSindex(findex);
	if (sindex >= 0) {
	    // This assumes that random access in an ArgList is O(1).
	    callback->matchedArgument(fdata, sindex, supplied->get(sindex));
	}
	else if (fdata.symbol != R_DotsSymbol) {
	    callback->defaultValue(fdata);
	}
	else {
	    callback->dottedArgs(fdata, matching->getDotArgs(), supplied);
	}
    }
}

struct ArgMatchInfo::Hash {
    size_t operator()(const ArgMatchInfo* item) const {
	size_t seed = 0;
	boost::hash_combine(seed, item->m_num_formals);
	boost::hash_combine(seed, item->m_values);
	return seed;
    }
};

namespace {

class ClosureMatchCallback : public ArgMatcher::MatchCallback
{
public:
    ClosureMatchCallback(Environment* target_env)
	: m_target_env(target_env) { }

    void matchedArgument(const FormalData& formal,
			 int arg_index, RObject* value) override
    {
	// If the value was missing, then use the default instead.
	if (value == Symbol::missingArgument()) {
	    defaultValue(formal);
	    return;
	}

	m_target_env->frame()->bind(formal.symbol, value,
				    Frame::Binding::EXPLICIT);
    }

    void defaultValue(const FormalData& formal) override
    {
    	if (formal.value == Symbol::missingArgument()) {
    	    // Create a value bound to Symbol::missingArgument()
    	    m_target_env->frame()->obtainBinding(formal.symbol);
	} else {
	    RObject* value = new Promise(formal.value, m_target_env);
	    m_target_env->frame()->bind(formal.symbol, value,
					Frame::Binding::DEFAULTED);
	}
    }

    void dottedArgs(const FormalData& formal,
		    ArgIndices arg_indices,
		    const ArgList* all_args) override {
	if (arg_indices.empty()) {
	    m_target_env->frame()->obtainBinding(DotsSymbol);
	    return;
	}

	ConsCell* dots = nullptr;
	for (int index : arg_indices) {
	    RObject* value = all_args->get(index);
	    const RObject* tag = all_args->getTag(index);
	    if (dots) {
		PairList* next_item = new PairList(value, nullptr, tag);
		dots->setTail(next_item);
		dots = next_item;
	    } else {
		dots = new DottedArgs(value, nullptr, tag);
		m_target_env->frame()->bind(DotsSymbol, dots,
					    Frame::Binding::EXPLICIT);
	    }
	}
    }

private:
    Environment* m_target_env;
};

class RecordArgMatchInfoCallback : public ArgMatcher::MatchCallback
{
public:
    RecordArgMatchInfoCallback(ArgMatchInfo* matching) : m_matching(matching) {}

    void matchedArgument(const FormalData& formal,
			 int arg_index, RObject* value) override {
	m_matching->m_values[formal.index] = arg_index;
    }

    void defaultValue(const FormalData& formal) override {
	m_matching->m_values[formal.index] = -1;
    }

    void dottedArgs(const FormalData& formal,
		    ArgIndices arg_indices,
		    const ArgList* all_args) override {
	m_matching->m_values.insert(m_matching->m_values.end(),
				    arg_indices.begin(), arg_indices.end());
    }
private:
    ArgMatchInfo* m_matching;
};

template<class T>
struct DereferencedEquality {
    bool operator()(const T* lhs, const T* rhs) const {
	if (lhs != nullptr && rhs != nullptr) {
	    return *lhs == *rhs;
	}
	return lhs == rhs;
    }
};

typedef google::dense_hash_set<const ArgMatchInfo*, ArgMatchInfo::Hash,
			       DereferencedEquality<ArgMatchInfo>>
	ArgMatchInfoCache;

ArgMatchInfoCache createMatchInfoCache() {
    ArgMatchInfoCache cache;
    cache.set_empty_key(nullptr);
    return cache;
}

}  // namespace

ArgMatchInfo::ArgMatchInfo(int num_formals, const PairList* args)
    : m_num_formals(num_formals), m_values(num_formals, -1)
 {
    for (const PairList* arg = args; arg; arg = arg->tail()) {
	m_tags.push_back(SEXP_downcast<const Symbol*>(arg->tag()));
    }
}

bool ArgMatchInfo::arglistTagsMatch(const PairList* args) const {
    const PairList* arg = args;
    size_t index;
    for (index = 0; index < m_tags.size() && arg != nullptr;
	 ++index, arg = arg->tail()) {
	if (m_tags[index] != args->tag())
	    return false;
    }
    if (index != m_tags.size() || arg != nullptr) {
	return false;
    }
    return true;
}



const ArgMatchInfo* ArgMatcher::createMatchInfo(const ArgList *args) const {
    static ArgMatchInfoCache s_interned_match_infos = createMatchInfoCache();

    if (args->has3Dots())
	return nullptr;

    ArgMatchInfo* matching = new ArgMatchInfo(numFormals(), args->list());
    RecordArgMatchInfoCallback callback(matching);
    match(args, &callback);

    // If there is an existing ArgMatchInfo with the same values, use that
    // instead to conserve memory use.
    auto inserted = s_interned_match_infos.insert(matching);
    if (!inserted.second) {
	delete matching;
    }
    return *inserted.first;
}

void ArgMatcher::match(Environment* target_env, const ArgList* supplied) const
{
    ClosureMatchCallback callback(target_env);
    match(supplied, &callback);
}

void ArgMatcher::match(Environment* target_env, const ArgList* supplied,
		       const ArgMatchInfo* matching) const
{
    ClosureMatchCallback callback(target_env);
    matchWithCache(supplied, &callback, matching);
}


void ArgMatcher::match(const ArgList* supplied,
		       MatchCallback* callback) const
{
    vector<MatchStatus, Allocator<MatchStatus> >
	formals_status(m_formal_data.size(), UNMATCHED);
    SuppliedList supplied_list;
    // Exact matches by tag:
    {
	unsigned int sindex = 0;
	for (const PairList* s = supplied->list(); s; s = s->tail(), ++sindex) {
	    const Symbol* tag = static_cast<const Symbol*>(s->tag());
	    const String* name = (tag ? tag->name() : nullptr);
	    RObject* value = s->car();
	    FormalMap::const_iterator fmit 
		= (name ? m_formal_index.lower_bound(name)
		   : m_formal_index.end());
	    if (fmit != m_formal_index.end() && (*fmit).first == name) {
		// Exact tag match:
		unsigned int findex = (*fmit).second;
		formals_status[findex] = EXACT_TAG;
		callback->matchedArgument(m_formal_data[findex], sindex,
					  value);
	    } else {
		// No exact tag match, so place supplied arg on list:
		SuppliedData supplied_data
		    = {tag, value, fmit, sindex};
		supplied_list.push_back(supplied_data);
	    }
	}
    }
    // Partial matches by tag:
    {
	SuppliedList::iterator slit = supplied_list.begin();
	while (slit != supplied_list.end()) {
	    SuppliedList::iterator next = slit;
	    ++next;
	    const SuppliedData& supplied_data = *slit;
	    const String* supplied_name
		= (supplied_data.tag ? supplied_data.tag->name() : nullptr);
	    FormalMap::const_iterator fmit = supplied_data.fm_iter;
	    // Within m_formal_index, skip formals formals following
	    // '...' and formals with exact matches:
	    while (fmit != m_formal_index.end()
		   && (m_formal_data[(*fmit).second].follows_dots
		       || formals_status[(*fmit).second] == EXACT_TAG))
		++fmit;
	    if (fmit != m_formal_index.end()
		&& isPrefix(supplied_name, (*fmit).first)) {
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
		    && isPrefix(supplied_name, (*fmit).first))
		    Rf_error(_("argument %d matches multiple formal arguments"),
			     supplied_data.index + 1);
		// Partial match is OK:
		if (s_warn_on_partial_match)
		    Rf_warning(_("partial argument match of '%s' to '%s'"),
			       supplied_name->c_str(),
			       (*fmit).first->c_str());
		formals_status[findex] = PARTIAL_TAG;
		callback->matchedArgument(m_formal_data[findex], supplied_data.index, supplied_data.value);
		supplied_list.erase(slit);
	    }
	    slit = next;
	}
    }
    // Positional matching and default values:
    {
	const size_t numformals = m_formal_data.size();
	SuppliedList::iterator slit = supplied_list.begin();
	for (unsigned int findex = 0; findex < numformals; ++findex) {
	    if (formals_status[findex] == UNMATCHED) {
		const FormalData& fdata = m_formal_data[findex];
		// Skip supplied arguments with tags:
		while (slit != supplied_list.end() && (*slit).tag)
		    ++slit;
		if (slit != supplied_list.end()
		    && !fdata.follows_dots) {
		    // Handle positional match:
		    const SuppliedData& supplied_data = *slit;
		    formals_status[findex] = POSITIONAL;
		    callback->matchedArgument(fdata, supplied_data.index,
					      supplied_data.value);
		    supplied_list.erase(slit++);
		} else if (findex != m_dots_position) {
		    callback->defaultValue(fdata);
		}
	    }
	}
    }

    // Any remaining supplied args are either rolled into ... or
    // there's an error:
    if (has3Dots()) {
	vector<int> dotted_arg_indices;
	for (const auto& sitem : supplied_list) {
	    dotted_arg_indices.push_back(sitem.index);
	}
	callback->dottedArgs(m_formal_data[m_dots_position],
			     boost::make_iterator_range(dotted_arg_indices),
			     supplied);
    }
    else if (!supplied_list.empty())
	unusedArgsError(supplied_list);
}

void ArgMatcher::propagateFormalBindings(const Environment* fromenv,
					 Environment* toenv) const
{
    const Frame* fromf = fromenv->frame();
    for (const FormalData& fdata : m_formal_data) {
	const Symbol* symbol = fdata.symbol;
	const Frame::Binding* frombdg = fromf->binding(symbol);
	if (!frombdg)
	    Rf_error(_("could not find symbol \"%s\" "
		       "in environment of the generic function"),
		     symbol->name()->c_str());
	RObject* val = frombdg->unforcedValue();
	if (frombdg->origin() == Frame::Binding::EXPLICIT) {
	    makeBinding(toenv, fdata, Frame::Binding::EXPLICIT, val);
	} else {
	    // Discard generic's defaults:
	    makeBinding(toenv, fdata, Frame::Binding::DEFAULTED,
			fdata.value);
	}
    }
    // m_formal_data excludes '...', so:
    if (has3Dots()) {
	const Frame::Binding* frombdg = fromf->binding(DotsSymbol);
	toenv->frame()->importBinding(frombdg);
    }
}

void ArgMatcher::stripFormals(Frame* input_frame) const
{
    const PairList* fcell = m_formals;
    while (fcell) {
	input_frame->erase(static_cast<const Symbol*>(fcell->tag()));
	fcell = fcell->tail();
    }
}

// Implementation of ArgMatcher::unusedArgsError() is in match.cpp

void ArgMatcher::visitReferents(const_visitor* v) const
{
    if (m_formals)
	(*v)(m_formals);
}

void ArgMatcher::applyToCoalescedReferences(std::function<void(const GCNode*)> fun) const
{
    if (m_formals) {
	fun(m_formals);
    }
}

PairList* ArgMatcher::makePairList(std::initializer_list<const char*> arg_names)
{
    PairList* result = PairList::make(arg_names.size());
    auto result_iter = result->begin();
    for (const char* arg : arg_names) {
	result_iter->setTag(Symbol::obtain(arg));
	++result_iter;
    }
    return result;
}
