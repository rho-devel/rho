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

/** @file Evaluator.cpp
 *
 * Implementation of class Evaluator.
 */

#include "CXXR/Evaluator.h"

#include "CXXR/DottedArgs.hpp"
#include "CXXR/Environment.h"
#include "CXXR/GCStackRoot.h"
#include "CXXR/Symbol.h"
#include "CXXR/errors.h"

using namespace std;
using namespace CXXR;

Rboolean R_Visible = TRUE;

unsigned int Evaluator::s_depth = 0;
unsigned int Evaluator::s_depth_threshold = 5000;
unsigned int Evaluator::s_depth_limit = 5000;
unsigned int Evaluator::s_countdown = 1000;
unsigned int Evaluator::s_countdown_start = 1000;
bool Evaluator::s_profiling = false;

namespace {
    int R_MIN_EXPRESSIONS_OPT = 25;
    int R_MAX_EXPRESSIONS_OPT = 500000;
}

RObject* Evaluator::evaluate(RObject* object, Environment* env)
{
    // The use of depthsave below is necessary because of the
    // possibility of non-local returns from evaluation.  Without this
    // an "expression too complex error" is quite likely.
    unsigned int depthsave = s_depth++;
    if (s_depth > s_depth_threshold) {
	enableExtraDepth(true);
	Rf_errorcall(0, _("evaluation nested too deeply: "
			  "infinite recursion / options(expressions=)?"));
    }
    R_CheckStack();
    if (--s_countdown == 0) {
	R_CheckUserInterrupt();
	s_countdown = s_countdown_start;
    }
#ifdef Win32
    // This is an inlined version of Rwin_fpreset (src/gnuwin/extra.c)
    // and resets the precision, rounding and exception modes of a
    // ix86 fpu.
    __asm__ ( "fninit" );
#endif
    enableResultPrinting(true);
    RObject* ans = 0;
    if (object)
	ans = object->evaluate(env);
    s_depth = depthsave;
    return ans;
}

pair<unsigned int, PairList*>
Evaluator::mapEvaluate(PairList* inlist, Environment* env)
{
    // Outlist has a dummy element at the front, to simplify coding:
    GCStackRoot<PairList> outlist(GCNode::expose(new PairList));
    unsigned int first_missing_arg = 0;
    unsigned int arg_number = 1;
    PairList* lastout = outlist;
    while (inlist) {
	RObject* incar = inlist->car();
	if (incar == DotsSymbol) {
	    Frame::Binding* bdg = findBinding(CXXR::DotsSymbol, env).second;
	    if (!bdg)
		Rf_error(_("'...' used but not bound"));
	    RObject* h = bdg->value();
	    if (!h || h->sexptype() == DOTSXP) {
		ConsCell* dotlist = static_cast<DottedArgs*>(h);
		while (dotlist) {
		    RObject* dotcar = dotlist->car();
		    RObject* outcar = Symbol::missingArgument();
		    if (dotcar != Symbol::missingArgument())
			outcar = evaluate(dotcar, env);
		    lastout->setTail(PairList::construct(outcar, 0,
							 dotlist->tag()));
		    lastout = lastout->tail();
		    dotlist = dotlist->tail();
		}
	    } else if (h != Symbol::missingArgument())
		Rf_error(_("'...' used in an incorrect context"));
	} else {
	    RObject* outcar = Symbol::missingArgument();
	    if (incar != Symbol::missingArgument()
		&& !(Rf_isSymbol(incar) && R_isMissing(incar, env)))
		outcar = evaluate(incar, env);
	    else if (first_missing_arg == 0)
		first_missing_arg = arg_number;
	    lastout->setTail(PairList::construct(outcar, 0, inlist->tag()));
	    lastout = lastout->tail();
	}
	inlist = inlist->tail();
	++arg_number;
    }
    return make_pair(first_missing_arg, outlist->tail());
}
		
void Evaluator::setDepthLimit(int depth)
{
    if (depth < R_MIN_EXPRESSIONS_OPT || depth > R_MAX_EXPRESSIONS_OPT)
	Rf_error(_("'expressions' parameter invalid, allowed %d...%d"),
	      R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
    s_depth_threshold = s_depth_limit = depth;
}
