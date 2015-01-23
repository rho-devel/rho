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

/** @file ArgList.cpp
 *
 * Implementation of class ArgList.
 */

#include "CXXR/ArgList.hpp"

#include <list>
#include "CXXR/DottedArgs.hpp"
#include "CXXR/Environment.h"
#include "CXXR/Evaluator.h"
#include "CXXR/Promise.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

// Implementation of ArgList::coerceTag() is in coerce.cpp

PairList* ArgList::append(PairList* object, PairList* last_element) {
    if (last_element)
	last_element->setTail(object);
    else
	setList(object);
    return object;
}

void ArgList::evaluate(Environment* env, bool allow_missing)
{
    if (m_status == EVALUATED)
	Rf_error("Internal error: ArgList already evaluated");
    if (m_first_arg_env && env != m_first_arg_env)
	Rf_error("Internal error: first arg of ArgList"
		 " previously evaluated in different environment");
    GCStackRoot<const PairList> oldargs(list());
    setList(nullptr);
    PairList* lastout = nullptr;
    unsigned int arg_number = 1;
    for (const PairList* inp = oldargs; inp; inp = inp->tail()) {
	RObject* incar = inp->car();
	if (incar == DotsSymbol) {
	    Frame::Binding* bdg = env->findBinding(CXXR::DotsSymbol);
	    if (!bdg)
		Rf_error(_("'...' used but not bound"));
	    RObject* h = bdg->forcedValue();
	    if (!h || h->sexptype() == DOTSXP) {
		ConsCell* dotlist = static_cast<DottedArgs*>(h);
		while (dotlist) {
		    RObject* dotcar = dotlist->car();
		    RObject* outcar = Symbol::missingArgument();
		    if (m_first_arg_env) {
			outcar = m_first_arg;
			m_first_arg = nullptr;
			m_first_arg_env = nullptr;
		    } else if (dotcar != Symbol::missingArgument())
			outcar = Evaluator::evaluate(dotcar, env);
		    PairList* cell = PairList::cons(outcar, nullptr, dotlist->tag());
		    lastout = append(cell, lastout);
		    dotlist = dotlist->tail();
		}
	    } else if (h != Symbol::missingArgument())
		Rf_error(_("'...' used in an incorrect context"));
	} else {
	    const RObject* tag = inp->tag();
	    PairList* cell = nullptr;
	    if (m_first_arg_env) {
		cell = PairList::cons(m_first_arg, nullptr, tag);
		m_first_arg = nullptr;
		m_first_arg_env = nullptr;
	    } else if (incar && incar->sexptype() == SYMSXP) {
		Symbol* sym = static_cast<Symbol*>(incar);
		if (sym == Symbol::missingArgument()) {
		    if (allow_missing)
			cell = PairList::cons(Symbol::missingArgument(), nullptr, tag);
		    else Rf_error(_("argument %d is empty"), arg_number);
		} else if (isMissingArgument(sym, env->frame())) {
		    if (allow_missing)
			cell = PairList::cons(Symbol::missingArgument(), nullptr, tag);
		    else Rf_error(_("'%s' is missing"),
				  sym->name()->c_str());
		}
	    }
	    if (!cell) {
		RObject* outcar = Evaluator::evaluate(incar, env);
		cell = PairList::cons(outcar, nullptr, inp->tag());
	    }
	    lastout = append(cell, lastout);
	}
	++arg_number;
    }
    m_status = EVALUATED;
}

void ArgList::merge(const ConsCell* extraargs)
{
    if (m_status != PROMISED)
	Rf_error("Internal error: ArgList::merge() requires PROMISED ArgList");
    // Convert extraargs into a doubly linked list:
    typedef std::list<pair<const RObject*, RObject*> > Xargs;
    Xargs xargs;
    for (const ConsCell* cc = extraargs; cc; cc = cc->tail())
	xargs.push_back(make_pair(cc->tag(), cc->car()));
    // Apply overriding arg values supplied in extraargs:
    PairList* last = nullptr;
    for (PairList* pl = mutable_list(); pl; pl = pl->tail()) {
	last = pl;
	const RObject* tag = pl->tag();
	if (tag) {
	    Xargs::iterator it = xargs.begin();
	    while (it != xargs.end() && (*it).first != tag)
		++it;
	    if (it != xargs.end()) {
		pl->setCar((*it).second);
		xargs.erase(it);
	    }
	}
    }
    // Append remaining extraargs:
    for (Xargs::const_iterator it = xargs.begin(); it != xargs.end(); ++it) {
	PairList* cell = PairList::cons((*it).second, nullptr, (*it).first);
	last = append(cell, last);
    }
}

pair<bool, RObject*> ArgList::firstArg(Environment* env)
{
    const PairList* elt = list();
    if (!elt)
	return pair<bool, RObject*>(false, nullptr);
    if (m_status == EVALUATED)
	return make_pair(true, elt->car());
    while (elt) {
	RObject* arg1 = elt->car();
	if (!arg1)
	    return pair<bool, RObject*>(true, nullptr);
	if (arg1 != DotsSymbol) {
	    m_first_arg = Evaluator::evaluate(arg1, env);
	    m_first_arg_env = env;
	    return make_pair(true, m_first_arg.get());
	}
	// If we get here it must be DotSymbol.
	Frame::Binding* bdg = env->findBinding(DotsSymbol);
	if (bdg && bdg->origin() != Frame::Binding::MISSING) {
	    RObject* val = bdg->forcedValue();
	    if (val) {
		if (val->sexptype() != DOTSXP)
		    Rf_error(_("'...' used in an incorrect context"));
		RObject* dots1 = static_cast<DottedArgs*>(val)->car();
		if (dots1->sexptype() != PROMSXP)
		    Rf_error(_("value in '...' is not a promise"));
		m_first_arg = Evaluator::evaluate(dots1, env);
		m_first_arg_env = env;
		return make_pair(true, m_first_arg.get());
	    }
	}
	elt = elt->tail();  // elt was unbound or missing DotsSymbol
    }
    return pair<bool, RObject*>(false, nullptr);
}

void ArgList::stripTags()
{
    for (PairList* p = mutable_list(); p; p = p->tail())
	p->setTag(nullptr);
}
	    
const Symbol* ArgList::tag2Symbol(const RObject* tag)
{
    return ((!tag || tag->sexptype() == SYMSXP)
	    ? static_cast<const Symbol*>(tag)
	    : coerceTag(tag));
}

void ArgList::wrapInPromises(Environment* env)
{
    if (m_status == PROMISED)
	Rf_error("Internal error:"
		 " ArgList already wrapped in Promises");
    if (m_status == EVALUATED)
	env = nullptr;
    else if (m_first_arg_env && env != m_first_arg_env)
	Rf_error("Internal error: first arg of ArgList"
		 " previously evaluated in different environment");
    GCStackRoot<const PairList> oldargs(list());
    setList(nullptr);
    PairList* lastout = nullptr;

    for (const PairList* inp = oldargs; inp; inp = inp->tail()) {
	RObject* rawvalue = inp->car();
	if (rawvalue == DotsSymbol) {
	    Frame::Binding* binding = env->findBinding(DotsSymbol);
	    if (binding) {
		RObject* dval = binding->forcedValue();
		if (!dval || dval->sexptype() == DOTSXP) {
		    ConsCell* dotlist = static_cast<ConsCell*>(dval);
		    while (dotlist) {
			Promise* prom;
			if (!m_first_arg_env)
			    prom = new Promise(dotlist->car(), env);
			else {
			    prom = new Promise(m_first_arg, nullptr);
			    m_first_arg = nullptr;
			    m_first_arg_env = nullptr;
			}
			const Symbol* tag = tag2Symbol(dotlist->tag());
			PairList* cell = PairList::cons(prom, nullptr, tag);
			lastout = append(cell, lastout);
			dotlist = dotlist->tail();
		    }
		} else if (dval != Symbol::missingArgument())
		    Rf_error(_("'...' used in an incorrect context"));
	    }
	} else {
	    const Symbol* tag = tag2Symbol(inp->tag());
	    RObject* value = Symbol::missingArgument();
	    if (m_first_arg_env) {
		value = new Promise(m_first_arg, nullptr);
		m_first_arg = nullptr;
		m_first_arg_env = nullptr;
	    } else if (rawvalue != Symbol::missingArgument())
		value = new Promise(rawvalue, env);
	    PairList* cell = PairList::cons(value, nullptr, tag);
	    lastout = append(cell, lastout);
	}
    }
    m_status = PROMISED;
}
