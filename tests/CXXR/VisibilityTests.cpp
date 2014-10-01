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

#include "EvaluationTests.hpp"

struct SingleVisibilityTest {
    const char* expression;
    bool expected_visibility;
};

class VisibilityTest : public EvaluatorTest {
public:
    void runVisibilityTests(const std::vector<SingleVisibilityTest>& tests)
    {
	for (const SingleVisibilityTest& test : tests) {
	    CXXR::Evaluator::enableResultPrinting(true);
	    GetParam()->parseAndEval(test.expression);

	    bool visibility = CXXR::Evaluator::resultPrinted();
	    EXPECT_EQ(test.expected_visibility, visibility);
	}
    }
 };

TEST_P(VisibilityTest, Simple)
{
    runVisibilityTests({
	    { "1", true },
	    { "invisible()", false },
    });
}

TEST_P(VisibilityTest, Begin)
{
    runVisibilityTests({
	    { "{ 1 }", true },
	    { "{ invisible() }", false },
	    { "{ invisible(); 1 }", true },
	    { "{ }", true },
    });
}

TEST_P(VisibilityTest, Paren)
{
    runVisibilityTests({
	    { "( 1 )", true },
	    { "( invisible() )", true },
    });
}

TEST_P(VisibilityTest, If)
{
    runVisibilityTests({
	    { "if(TRUE) 1", true },
	    { "if(TRUE) invisible(1)", false },

	    { "if(FALSE) 1", false }, // Returns an invisible NULL.
	    { "if(FALSE) 1 else 2", true },
	    { "if(FALSE) 1 else invisible(2)", false },

	    { "if(invisible(TRUE)) 1", true },
	    { "if(invisible(TRUE)) invisible(1)", false },
	    { "if(invisible(FALSE)) 1", false },
	    { "if(invisible(FALSE)) 1 else 2", true },
	    { "if(invisible(FALSE)) 1 else invisible(2)", false },
    });
}

TEST_P(VisibilityTest, For)
{
    // For loops always return an invisible NULL.
    runVisibilityTests({
	    { "for (i in 1:3) 2", false },
	    { "for (i in c()) 2", false },
	    { "for (i in 1:3) break", false },
     });
}

TEST_P(VisibilityTest, While)
{
    // While loops always return an invisible NULL.
    runVisibilityTests({
	    { "while (FALSE) 2", false },
	    { "while (TRUE) break", false },
     });
}

TEST_P(VisibilityTest, Repeat)
{
    // Repeat loops always return an invisible NULL.
    runVisibilityTests({
	    { "repeat break", false },
     });
}

TEST_P(VisibilityTest, Assign)
{
    runVisibilityTests({
	    { "x <- 1", false },
	    { "x <- y <- 1", false },
	    { "x <- c(1, 2); x[[0]] <- 0", false },
     });
}


TEST_P(VisibilityTest, InvisibleArgumentsToEnvProfile)
{
    // env.profile() is implemented as a BUILTINSXP with SOFT_ON visibility,
    // called via .Internal().
    // It should ignore the visibility of its arguments.
    runVisibilityTests({
	    { "env.profile(.GlobalEnv)", true },
	    { "env.profile(invisible(.GlobalEnv))", true },
    });
}

TEST_P(VisibilityTest, InvisibleArgumentsToIsRecursive)
{
    // is.recursive is implemented as a BUILTINSXP primitive with SOFT_ON
    // visibility.
    // It should ignore the visibility of its arguments.
    runVisibilityTests({
	    { "is.recursive(.GlobalEnv)", true },
	    { "is.recursive(invisible(.GlobalEnv))", true },
	    { "is.recursive(1)", true },
	    { "is.recursive(invisible(1))", true },
    });
}

// Instantiations of the test types.
INSTANTIATE_TEST_CASE_P(InterpreterVisibiltyTest,
			VisibilityTest,
			testing::Values(Executor::InterpreterExecutor()));

INSTANTIATE_TEST_CASE_P(JITVisibilityTest,
			VisibilityTest,
			testing::Values(Executor::JITExecutor()));

