/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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

void ClosureContext::runOnExit()
{
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
    
ClosureContext* ClosureContext::innermost(Evaluator::Context* start)
{
    while (start && start->type() != CLOSURE)
	start = start->nextOut();
    return static_cast<ClosureContext*>(start);
}
