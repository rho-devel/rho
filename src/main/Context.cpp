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

/** @file Context.cpp
 *
 * Implementation of class CXXR::Context.
 */

#include "CXXR/Context.hpp"

using namespace std;
using namespace CXXR;

GCRoot<> R_HandlerStack;
GCRoot<> R_RestartStack;
RObject* R_Srcref;

Context::Context(Expression* the_call, Environment* call_env,
		 FunctionBase* function, Environment* working_env,
		 PairList* promise_args)
    : nextcontext(Context::innermost()), evaldepth(Evaluator::depth()),
      callflag(working_env ? RETURN : BUILTIN), srcref(R_Srcref),
      call(the_call), sysparent(call_env), intsusp(R_interrupts_suspended),
      handlerstack(R_HandlerStack), restartstack(R_RestartStack),
      callfun(function), cloenv(working_env), promargs(promise_args), 
      m_generic(false)
{
#ifdef BYTECODE
    nodestack = R_BCNodeStackTop;
#ifdef BC_INT_STACK
    intstack = R_BCIntStackTop;
#endif
#endif
    Evaluator::current()->m_innermost_context = this;
}

Context::~Context()
{
#ifdef BYTECODE
    R_BCNodeStackTop = nodestack;
# ifdef BC_INT_STACK
    R_BCIntStackTop = intstack;
# endif
#endif
    R_RestartStack = restartstack;
    R_HandlerStack = handlerstack;
    if (cloenv && conexit) {
	GCStackRoot<> onx(conexit);
	Rboolean savevis = R_Visible;
	// Prevent recursion:
	conexit = 0;
	Evaluator::enableExtraDepth(true);
	try {
	    Evaluator::evaluate(onx, cloenv);
	}
	// Don't allow exceptions to escape:
	catch (...) {}
	R_Visible = savevis;
    }
    R_interrupts_suspended = intsusp;
    R_Srcref = srcref;
    Evaluator::setDepth(evaldepth);
    Evaluator::current()->m_innermost_context = nextcontext;
}
    
