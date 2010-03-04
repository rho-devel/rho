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

#include "CXXR/ProtectStack.h"
#include "CXXR/RAllocStack.h"

using namespace std;
using namespace CXXR;

GCRoot<> R_HandlerStack;
GCRoot<> R_RestartStack;
RObject* R_Srcref;
GCRoot<> R_ReturnedValue;

Context::Context()
    : nextcontext(R_GlobalContext), cstacktop(ProtectStack::size()),
      evaldepth(Evaluator::depth()),vmax(vmaxget()),
      intsusp(R_interrupts_suspended), handlerstack(R_HandlerStack),
      restartstack(R_RestartStack), srcref(R_Srcref)
{
#ifdef BYTECODE
    nodestack = R_BCNodeStackTop;
#ifdef BC_INT_STACK
    intstack = R_BCIntStackTop;
#endif
#endif
    R_GlobalContext = this;
}

Context::~Context()
{
    R_HandlerStack = handlerstack;
    R_RestartStack = restartstack;
    if (cloenv && conexit) {
	GCStackRoot<> onx(conexit);
	GCStackRoot<> returnval(R_ReturnedValue);
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
	R_ReturnedValue = returnval;
    }
    ProtectStack::restoreSize(cstacktop);
    Evaluator::setDepth(evaldepth);
    vmaxset(vmax);
    R_interrupts_suspended = intsusp;
#ifdef BYTECODE
    R_BCNodeStackTop = nodestack;
# ifdef BC_INT_STACK
    R_BCIntStackTop = intstack;
# endif
#endif
    R_Srcref = srcref;
    R_GlobalContext = nextcontext;
}
    
