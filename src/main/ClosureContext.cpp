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

/** @file Evaluator_Context.cpp
 *
 * Implementation of class CXXR::Evaluator::Context.
 */

#include "CXXR/ClosureContext.hpp"

#include "CXXR/Environment.h"

using namespace std;
using namespace CXXR;

GCRoot<> R_HandlerStack;
GCRoot<> R_RestartStack;

ClosureContext::ClosureContext(const Expression* the_call, Environment* call_env,
			       const FunctionBase* function,
			       Environment* working_env,
			       const PairList* promise_args)
    : FunctionContext(the_call, call_env, function),
      m_interrupts_suspended(R_interrupts_suspended),
      m_handlerstack(R_HandlerStack), m_restartstack(R_RestartStack),
      m_working_env(working_env), m_promise_args(promise_args)
{
    setType(CLOSURE);
}

ClosureContext::~ClosureContext()
{
    R_RestartStack = m_restartstack;
    R_HandlerStack = m_handlerstack;
    if (m_onexit) {
	GCStackRoot<> onx(m_onexit);
	Rboolean savevis = R_Visible;
	// Prevent recursion:
	m_onexit = 0;
	Evaluator::enableExtraDepth(true);
	try {
	    Evaluator::evaluate(onx, m_working_env);
	}
	// Don't allow exceptions to escape:
	catch (...) {}
	R_Visible = savevis;
    }
    R_interrupts_suspended = m_interrupts_suspended;
}
    
ClosureContext* ClosureContext::innermost(Evaluator::Context* start)
{
    while (start && start->type() != CLOSURE)
	start = start->nextOut();
    return static_cast<ClosureContext*>(start);
}
