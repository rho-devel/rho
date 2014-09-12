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

#include "CXXR/DottedArgs.hpp"
#include "CXXR/Environment.h"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/Symbol.h"
#include "CXXR/errors.h"

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

unsigned int Evaluator::s_depth = 0;
unsigned int Evaluator::s_depth_threshold = 5000;
unsigned int Evaluator::s_depth_limit = 5000;
unsigned int Evaluator::s_countdown = 1000;
unsigned int Evaluator::s_countdown_start = 1000;
Evaluator* Evaluator::s_current = 0;
bool Evaluator::s_profiling = false;

namespace {
    unsigned int R_MIN_EXPRESSIONS_OPT = 25;
    unsigned int R_MAX_EXPRESSIONS_OPT = 500000;
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

void Evaluator::setDepthLimit(unsigned int depth)
{
    if (depth < R_MIN_EXPRESSIONS_OPT || depth > R_MAX_EXPRESSIONS_OPT)
	Rf_error(_("'expressions' parameter invalid, allowed %d...%d"),
		 R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
    s_depth_threshold = s_depth_limit = depth;
}

void Evaluator::checkForUserInterrupts()
{
    R_CheckUserInterrupt();
    s_countdown = s_countdown_start;
}
