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

Evaluator::Context::Context(Expression* the_call, Environment* call_env,
			    FunctionBase* function, Environment* working_env,
			    PairList* promise_args)
    : m_next_out(innermost()), m_eval_depth(Evaluator::depth()),
      m_srcref(R_Srcref), m_call(the_call), m_call_env(call_env),
      m_interrupts_suspended(R_interrupts_suspended),
      m_handlerstack(R_HandlerStack), m_restartstack(R_RestartStack),
      m_function(function), m_working_env(working_env),
      m_promise_args(promise_args), m_generic(false)
{
#ifdef BYTECODE
    m_nodestack = R_BCNodeStackTop;
#ifdef BC_INT_STACK
    m_intstack = R_BCIntStackTop;
#endif
#endif
    Evaluator::current()->m_innermost_context = this;
}

Evaluator::Context::~Context()
{
#ifdef BYTECODE
    R_BCNodeStackTop = m_nodestack;
# ifdef BC_INT_STACK
    R_BCIntStackTop = m_intstack;
# endif
#endif
    R_RestartStack = m_restartstack;
    R_HandlerStack = m_handlerstack;
    if (m_working_env && m_onexit) {
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
    R_Srcref = m_srcref;
    Evaluator::setDepth(m_eval_depth);
    Evaluator::current()->m_innermost_context = m_next_out;
}
    
