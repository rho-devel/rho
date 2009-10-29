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

/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file OrdinaryBuiltInFunction.cpp
 *
 * Implementation of class CXXR::OrdinaryBuiltInFunction.
 */

#include "CXXR/OrdinaryBuiltInFunction.hpp"

#include "RCNTXT.h"
#include "CXXR/Evaluator.h"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.h"

using namespace std;
using namespace CXXR;

RObject* OrdinaryBuiltInFunction::innerApply(Expression* call, PairList* args,
					     Environment* env)
{
    pair<unsigned int, PairList*> pr = Evaluator::mapEvaluate(args, env);
    if (pr.first != 0)
	BuiltInFunction::missingArgumentError(this, args, pr.first);
    GCStackRoot<> evaluated_args(pr.second);
    if (Evaluator::profiling() || kind() == PP_FOREIGN) {
	RCNTXT cntxt;
	Rf_begincontext(&cntxt, CTXT_BUILTIN, call, Environment::base(),
		     Environment::base(), 0, 0);
	RObject* ans = function()(call, this, evaluated_args, env);
	Rf_endcontext(&cntxt);
	return ans;
    } else {
	return function()(call, this, evaluated_args, env);
    }
}

const char* OrdinaryBuiltInFunction::typeName() const
{
    return staticTypeName();
}
