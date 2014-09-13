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

/** @file Evaluator.cpp
 *
 * Implementation of class Evaluator.
 */

#include "CXXR/Evaluator.h"

#include "CXXR/Environment.h"
#include "CXXR/StackChecker.hpp"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	SEXP (*evalp)(SEXP, SEXP) = Rf_eval;
    }
}

Rboolean R_Visible = TRUE;
int R_interrupts_pending = 0;
Rboolean R_interrupts_suspended = FALSE;

unsigned int Evaluator::s_countdown = 1000;
unsigned int Evaluator::s_countdown_start = 1000;
Evaluator* Evaluator::s_current = 0;
bool Evaluator::s_profiling = false;

RObject* Evaluator::evaluate(RObject* object, Environment* env)
{
    IncrementStackDepthScope scope;

    enableResultPrinting(true);
    return object ? object->evaluate(env) : nullptr;
}

void Evaluator::checkForUserInterrupts()
{
    R_CheckUserInterrupt();
    s_countdown = s_countdown_start;
}
