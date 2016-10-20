/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
 */

/** @file Evaluator_Context.cpp
 *
 * Implementation of class Evaluator::Context.
 */

#include "rho/ClosureContext.hpp"

#include "rho/Environment.hpp"
#include "rho/StackChecker.hpp"

using namespace std;
using namespace rho;

GCRoot<PairList> rho::R_HandlerStack;
GCRoot<PairList> rho::R_RestartStack;

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
