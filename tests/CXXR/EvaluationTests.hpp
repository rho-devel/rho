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

/*
 *  R : A Computer Language for Statistical Data Analysis
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

#ifndef CXXR_TESTS_CXXR_EVALUATION_TESTS_HPP
#define CXXR_TESTS_CXXR_EVALUATION_TESTS_HPP

#include "gtest/gtest.h"
#include "CXXR/Environment.h"
#include "CXXR/RObject.h"
#include "TestHelpers.hpp"

#include <vector>

struct SingleTest {
    const char* expression;
    const char* expected_result;
};

// TODO(kmillar): This is only capable of testing operations that don't cause
//   errors or warnings.  The edge cases need testing too.
class Executor {
public:
    virtual CXXR::RObject* parseAndEval(
	const std::string& expression,
	CXXR::Environment* env = newTestEnv()) = 0;

    static CXXR::Environment* newTestEnv();
    static CXXR::RObject* parse(std::string expression);
    static CXXR::RObject* parseAndEvalWithInterpreter(
	const std::string& expression,
	CXXR::Environment* env = newTestEnv());
};

class EvaluatorTest : public ::testing::TestWithParam<Executor*> {
public:
    void runEvaluatorTests(const std::vector<SingleTest> &tests);
    void runSingleTest(SingleTest test);
};

class ControlFlowTest : public EvaluatorTest { };

#endif  // CXXR_TESTS_CXXR_EVALUATION_TESTS_HPP
