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

	{ "`(`()", Error("0 arguments passed to '(' which requires 1") },
	{ "`(`(1, 2)", Error("2 arguments passed to '(' which requires 1") },
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

	// Error and corner cases.
	{ "`if`(TRUE)", "NULL" },
	{ "`if`()", Error("argument is of length zero") },
	{ "if(logical(0)) 1", Error("argument is of length zero") },
	{ "if (NA) 1" , Error("missing value where TRUE/FALSE needed") },
	{ "if (NA_integer_) 1",
		Error("argument is not interpretable as logical") },
	{ "if (NA_character_) 1",
		Error("argument is not interpretable as logical") },
	{ "if (NA_real_) 1",
		Error("argument is not interpretable as logical") },
	{ "if (NA_complex_) 1",
		Error("argument is not interpretable as logical") },

	{ "if (NA) 1" , Error("missing value where TRUE/FALSE needed") },
	{ "if (NA) 1" , Error("missing value where TRUE/FALSE needed") },

	{ "if (c(TRUE, FALSE)) 1", "1",
		Warning("only the first element will be used") },
	{ "if (c(FALSE, TRUE)) 1", "NULL",
		Warning("only the first element will be used") },
	{ "if (c(FALSE, TRUE)) 1 else 2", "2",
		Warning("only the first element will be used") },
	{ "if (c(NA, TRUE)) 1 else 2",
		Error("missing value where TRUE/FALSE needed"),
		Warning("only the first element will be used") },
	});
}

TEST_P(ControlFlowTest, While)
{
    runEvaluatorTests({
	    { "while(FALSE) 1", "NULL" },
	    { "while(FALSE) identity()", "NULL" },
	    { "while(TRUE) { break }", "NULL" },
	    { "{ i <- 0; while(i < 3) i <- i + 1; i }", "3" },

	    // Error cases.
	    { "while(NA) 1",
		    Error("missing value where TRUE/FALSE needed") },
	    { "while(c(FALSE, TRUE)) 1", "NULL",
		    Warning("only the first element will be used") },
	    { "while('foo') 1",
		    Error("argument is not interpretable as logical") },

	    { "`while`()", Error("argument is of length zero") },
	    // As of R 3.1.1, these aren't errors.
	    // { "`while`(TRUE)", Error("1 arguments passed to 'while'") },
	    // { "`while`(TRUE, 1, 2)", Error("3 arguments passed to 'while'") },
	});
}

TEST_P(ControlFlowTest, Repeat)
{
    runEvaluatorTests({
	    { "repeat { break }", "NULL" },
	    { "{ i <- 0; repeat { if (i > 3) break; i <- i + 1 }; i }", "4" },

	    // As of R 3.1.1, these aren't errors.
	    // { "`repeat`()", Error("0 arguments passed to 'repeat'") },
	    // { "`repeat`(1, 2)", Error("2 arguments passed to 'repeat'") },
	});
}

// TODO(kmillar): Test break, next, for

INSTANTIATE_TEST_CASE_P(InterpreterControlFlowTest,
                        ControlFlowTest,
			testing::Values(Executor::InterpreterExecutor()));

INSTANTIATE_TEST_CASE_P(JITControlFlowTest,
                        ControlFlowTest,
			testing::Values(Executor::JITExecutor()));

