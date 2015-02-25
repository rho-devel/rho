/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

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
Evaluator* Evaluator::s_current = nullptr;
bool Evaluator::s_profiling = false;

void Evaluator::checkForUserInterrupts()
{
    R_CheckUserInterrupt();
    s_countdown = s_countdown_start;
}
