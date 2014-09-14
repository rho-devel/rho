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

class ControlFlowTest : public EvaluatorTest { };

TEST_P(ControlFlowTest, Braces)
{
    runEvaluatorTests({
	{ "{ }", "NULL" },
	{ "{ 1 }", "1" },
	{ "{ 1; 2 }", "2" },
	{ "{ cos(0) }", "1" },
	});
}

TEST_P(ControlFlowTest, Parens)
{
    runEvaluatorTests({
	{ "( NULL )", "NULL" },
	{ "( 1 )", "1" },
	{ "( cos(0) )", "1" },
	});
}	

TEST_P(ControlFlowTest, If)
{
    runEvaluatorTests({
	// if() with a scalar logical.
	{ "if (TRUE) 1", "1" },
	{ "if (FALSE) 1", "NULL" },
	{ "if (TRUE) 1 else 2", "1" },
	{ "if (FALSE) 1 else 2", "2" },
	
	// if() with a scalar numeric.
	{ "if (0.0) 1", "NULL" },
	{ "if (1.1) 1", "1" },
	{ "if (2.1) 1", "1" },
	{ "if (0.0) 1 else 2", "2" },
	{ "if (1.1) 1 else 2", "1" },
	{ "if (2.1) 1 else 2", "1" },
	
	// if() with a scalar integer.
	{ "if (1L) 1", "1" },
	{ "if (2L) 1", "1" },
	{ "if (0L) 1", "NULL" },
	{ "if (1L) 1 else 2", "1" },
	{ "if (2L) 1 else 2", "1" },
	{ "if (0L) 1 else 2", "2" },
	});
}

TEST_P(ControlFlowTest, While)
{
    runEvaluatorTests({
	    { "while(FALSE) 1", "NULL" },
	    { "while(FALSE) identity()", "NULL" },
	    { "{ i <- 0; while(i < 3) i <- i + 1; i }", "3" }
	});
}

TEST_P(ControlFlowTest, Repeat)
{
    runEvaluatorTests({
	    { "repeat { break }", "NULL" },
	    { "{ i <- 0; repeat { if (i > 3) break; i <- i + 1 }; i }", "4" },
	});
}

// TODO(kmillar): Test break, next, for

INSTANTIATE_TEST_CASE_P(InterpreterControlFlowTest,
                        ControlFlowTest,
			testing::Values(Executor::InterpreterExecutor()));

INSTANTIATE_TEST_CASE_P(JITControlFlowTest,
                        ControlFlowTest,
			testing::Values(Executor::JITExecutor()));

