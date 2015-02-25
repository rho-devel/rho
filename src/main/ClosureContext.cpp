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

/** @file Evaluator_Context.cpp
 *
 * Implementation of class CXXR::Evaluator::Context.
 */

#include "CXXR/ClosureContext.hpp"

#include "CXXR/Environment.h"
#include "CXXR/StackChecker.hpp"

using namespace std;
using namespace CXXR;

GCRoot<> R_HandlerStack;
GCRoot<> R_RestartStack;

void ClosureContext::runOnExit()
{
    GCStackRoot<> onx(m_onexit.get());
    Rboolean savevis = R_Visible;
    // Prevent recursion:
    m_onexit = nullptr;
    try {
	DisableStackCheckingScope scope;
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
