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
#include "CXXR/Promise.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

// Implementation of ArgList::coerceTag() is in coerce.cpp

void ArgList::merge(const ConsCell* extraargs)
{
    if (m_evaluated || m_wrapped)
	Rf_error(_("Internal error in ArgList::merge()"));
    GCStackRoot<const PairList> oldargs(m_list);
    GCStackRoot<PairList> newargs(PairList::cons(0));  // dummy first element
    PairList* outp = newargs;
    set<const RObject*> ntags;  // Tags within dotargs
    for (const ConsCell* inp = extraargs; inp; inp = inp->tail()) {
	const RObject* tag = inp->tag();
	if (tag)
	    ntags.insert(tag);
	outp->setTail(PairList::cons(inp->car(), 0, tag));
	outp = outp->tail();
    }
    for (const PairList* inp = oldargs; inp; inp = inp->tail()) {
	const RObject* tag = inp->tag();
	if (!tag || ntags.count(tag) == 0) {
	    outp->setTail(PairList::cons(inp->car(), 0, tag));
	    outp = outp->tail();
	}
    }
    m_list = newargs->tail();
}
	    
void ArgList::wrapInPromises(Environment* env)
{
    GCStackRoot<PairList> newargs(PairList::cons(0));  // dummy first element
    PairList* last = newargs;
    for (const PairList* inp = m_list; inp; inp = inp->tail()) {
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
			last->setTail(PairList::cons(prom, 0, tag));
			last = last->tail();
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
	    last->setTail(PairList::cons(value, 0, tag));
	    last = last->tail();
	}
    }
    m_list = newargs->tail();
    m_wrapped = true;
}
