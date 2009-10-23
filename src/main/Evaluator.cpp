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

#include "localization.h"
#include "R_ext/Error.h"

using namespace std;
using namespace CXXR;

Rboolean R_Visible = TRUE;

unsigned int Evaluator::s_depth = 0;
unsigned int Evaluator::s_depth_threshold = 5000;
unsigned int Evaluator::s_depth_limit = 5000;
unsigned int Evaluator::s_countdown = 1000;
unsigned int Evaluator::s_countdown_start = 1000;

namespace {
    int R_MIN_EXPRESSIONS_OPT = 25;
    int R_MAX_EXPRESSIONS_OPT = 500000;
}

// Implementation of Evaluator::evaluate() is in eval.cpp (for the time being)

void Evaluator::setDepthLimit(int depth)
{
    if (depth < R_MIN_EXPRESSIONS_OPT || depth > R_MAX_EXPRESSIONS_OPT)
	Rf_error(_("'expressions' parameter invalid, allowed %d...%d"),
	      R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
    s_depth_threshold = s_depth_limit = depth;
}
