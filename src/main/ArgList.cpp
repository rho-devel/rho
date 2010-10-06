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

/** @file ArgList.cpp
 *
 * Implementation of class ArgList.
 */

#include "CXXR/ArgList.hpp"

#include <set>
#include "CXXR/DottedArgs.hpp"
#include "CXXR/Environment.h"
#include "CXXR/Evaluator.h"
#include "CXXR/Promise.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

// Implementation of ArgList::coerceTag() is in coerce.cpp

void ArgList::evaluate(Environment* env, bool allow_missing)
{
    if (m_evaluated)
	Rf_error("ArgList already evaluated");
    GCStackRoot<const PairList> oldargs(m_list->tail());
    m_list->setTail(0);
    PairList* lastout = m_list;
    unsigned int arg_number = 1;
    for (const PairList* inp = oldargs; inp; inp = inp->tail()) {
	RObject* incar = inp->car();
	if (incar == DotsSymbol) {
	    Frame::Binding* bdg = env->findBinding(CXXR::DotsSymbol).second;
	    if (!bdg)
		Rf_error(_("'...' used but not bound"));
	    RObject* h = bdg->value();
	    if (!h || h->sexptype() == DOTSXP) {
		ConsCell* dotlist = static_cast<DottedArgs*>(h);
		while (dotlist) {
		    RObject* dotcar = dotlist->car();
		    RObject* outcar = Symbol::missingArgument();
		    if (dotcar != Symbol::missingArgument())
			outcar = Evaluator::evaluate(dotcar, env);
		    PairList* cell = PairList::cons(outcar, 0, dotlist->tag());
		    lastout->setTail(cell);
		    lastout = lastout->tail();
		    dotlist = dotlist->tail();
		}
	    } else if (h != Symbol::missingArgument())
		Rf_error(_("'...' used in an incorrect context"));
	} else {
	    const RObject* tag = inp->tag();
	    PairList* cell = 0;
	    if (incar && incar->sexptype() == SYMSXP) {
		Symbol* sym = static_cast<Symbol*>(incar);
		if (sym == Symbol::missingArgument()) {
		    if (allow_missing)
			cell = PairList::cons(Symbol::missingArgument(), 0, tag);
		    else Rf_error(_("argument %d is empty"), arg_number);
		} else if (isMissingArgument(sym, env->frame())) {
		    if (allow_missing)
			cell = PairList::cons(Symbol::missingArgument(), 0, tag);
		    else Rf_error(_("'%s' is missing"), sym->name()->c_str());
		}
	    }
	    if (!cell) {
		RObject* outcar = Evaluator::evaluate(incar, env);
		cell = PairList::cons(outcar, 0, inp->tag());
	    }
	    lastout->setTail(cell);
	    lastout = lastout->tail();
	}
	++arg_number;
    }
    m_evaluated = true;
}

void ArgList::merge(const ConsCell* extraargs)
{
    if (m_evaluated || m_wrapped)
	Rf_error("Internal error in ArgList::merge()");
    GCStackRoot<const PairList> oldargs(m_list->tail());
    m_list->setTail(0);
    PairList* lastout = m_list;
    set<const RObject*> ntags;  // Tags within extraargs
    for (const ConsCell* inp = extraargs; inp; inp = inp->tail()) {
	const RObject* tag = inp->tag();
	if (tag)
	    ntags.insert(tag);
	lastout->setTail(PairList::cons(inp->car(), 0, tag));
	lastout = lastout->tail();
    }
    for (const PairList* inp = oldargs; inp; inp = inp->tail()) {
	const RObject* tag = inp->tag();
	if (!tag || ntags.count(tag) == 0) {
	    lastout->setTail(PairList::cons(inp->car(), 0, tag));
	    lastout = lastout->tail();
	}
    }
}
	    
void ArgList::wrapInPromises(Environment* env)
{
    GCStackRoot<PairList> oldargs(m_list->tail());
    m_list->setTail(0);
    PairList* lastout = m_list;
    for (const PairList* inp = oldargs; inp; inp = inp->tail()) {
	RObject* rawvalue = inp->car();
	if (rawvalue == DotsSymbol) {
	    pair<Environment*, Frame::Binding*> pr
		= env->findBinding(DotsSymbol);
	    if (pr.first) {
		RObject* dval = pr.second->value();
		if (!dval || dval->sexptype() == DOTSXP) {
		    ConsCell* dotlist = static_cast<ConsCell*>(dval);
		    while (dotlist) {
			Promise* prom
			    = GCNode::expose(new Promise(dotlist->car(), env));
			const Symbol* tag = tag2Symbol(dotlist->tag());
			lastout->setTail(PairList::cons(prom, 0, tag));
			lastout = lastout->tail();
			dotlist = dotlist->tail();
		    }
		} else if (dval != Symbol::missingArgument())
		    Rf_error(_("'...' used in an incorrect context"));
	    }
	} else {
	    const Symbol* tag = tag2Symbol(inp->tag());
	    RObject* value = Symbol::missingArgument();
	    if (rawvalue != Symbol::missingArgument())
		value = GCNode::expose(new Promise(rawvalue, env));
	    lastout->setTail(PairList::cons(value, 0, tag));
	    lastout = lastout->tail();
	}
    }
    m_wrapped = true;
}
