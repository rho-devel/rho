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

#include "gtest/gtest.h"
#include "CXXR/Closure.h"
#include "CXXR/CommandTerminated.hpp"
#include "CXXR/Evaluator.h"
#include "CXXR/StdFrame.hpp"
#include "Defn.h"
#include "Parse.h"

#undef parse

using namespace CXXR;

void EvaluatorTest::runEvaluatorTests(const std::vector<SingleTest>& tests)
{
    for (const SingleTest& test : tests) {
	runSingleTest(test);
    }
}

static bool errorMatches(const std::string& expected_error,
			 const std::string& actual_error)
{
    return actual_error.find(expected_error) != std::string::npos;
}

void EvaluatorTest::runErrorTest(SingleTest test)
{
    // This is a test that should cause a failure.
    // Check that it does fail, and that the error message is correct.
    ASSERT_NE(nullptr, test.expected_error_message_);
    EXPECT_THROW(GetParam()->parseAndEval(test.expression_),
		 CommandTerminated);
    
    std::string error_message = R_curErrorBuf();
    EXPECT_PRED2(errorMatches,
		 test.expected_error_message_, error_message);

    // TODO(kmillar): verrorcall_dflt prints the warnings, so this fails.
    // checkWarnings(test);
}

void EvaluatorTest::checkWarnings(SingleTest test)
{
    ASSERT_EQ(test.expected_warnings_.size(), R_CollectWarnings);
    if (test.expected_warnings_.empty())
    {
	return;
    }

    const StringVector* actual_warnings
	= SEXP_downcast<StringVector*>(R_Warnings->getAttribute(NamesSymbol));
    for (int i = 0; i < R_CollectWarnings; ++i)
    {
	EXPECT_PRED2(errorMatches,
		     test.expected_warnings_[i],
		     (*actual_warnings)[i]->stdstring());
    }
}

void EvaluatorTest::clearWarnings()
{
    R_CollectWarnings = 0;
    R_Warnings = R_NilValue;
}

void EvaluatorTest::runSingleTest(SingleTest test)
{
    clearWarnings();

    if (test.expected_error_message_) {
	runErrorTest(test);
	return;
    }

    // Evaluate the expression with the configured evaluator.
    GCStackRoot<> result(GetParam()->parseAndEval(test.expression_));
    
    // Evaluate the expected result with the interpreter.
    GCStackRoot<> expected_result(
	Executor::parseAndEvalWithInterpreter(test.expected_result_));
    EXPECT_IDENTICAL(expected_result.get(), result.get());

    checkWarnings(test);
}

Environment* Executor::newTestEnv()
{
    // Every evaluation is done in a freshly created environment, so that
    // side effects don't propogate.
    // Only the base environment is visible from this environment.
    GCStackRoot<Frame> frame(CXXR_NEW(StdFrame()));
    return CXXR_NEW(Environment(Environment::base(), frame));
}

RObject* Executor::parse(std::string expression)
{
    ParseStatus parse_status;
    IoBuffer buffer;
    R_IoBufferInit(&buffer);
    expression = expression + ";";  // Terminate the expression.
    R_IoBufferPuts(const_cast<char*>(expression.c_str()), &buffer);
    R_IoBufferReadReset(&buffer);

    RObject* parse = R_Parse1Buffer(&buffer, true, &parse_status);
    R_IoBufferFree(&buffer);

    EXPECT_EQ(ParseStatus::PARSE_OK, parse_status);
    if (parse_status != PARSE_OK) return nullptr;
    if (expression != "NULL;") {
	EXPECT_TRUE(parse != nullptr);
    }
    return parse;
}

RObject* Executor::parseAndEvalWithInterpreter(const std::string& expression,
					       Environment* env)
{
    GCStackRoot<> protect_env(env);
    GCStackRoot<> parsed_expression(parse(expression));
    return Evaluator::evaluate(parsed_expression, env);
}

// Executors to use.

class InterpreterExecutor : public Executor {
public:
    virtual RObject* parseAndEval(
	const std::string& expression, Environment* env)
    {
	return parseAndEvalWithInterpreter(expression, env);
    }
};

Executor* Executor::InterpreterExecutor() {
    return new class InterpreterExecutor();
}

#ifdef ENABLE_LLVM_JIT
class JITExecutor : public Executor {
public:
    virtual RObject* parseAndEval(
	const std::string& expression, Environment* env)
    {
	GCStackRoot<> protect_env(env);
	
	// Wrap the expression in a function so it can be compiled.
	std::string function = "function() " + expression;
	GCStackRoot<> parsed_funtion(parse(function));
    
	RObject* parse_eval = Evaluator::evaluate(parsed_funtion, env);
	EXPECT_TRUE(parse_eval != nullptr);
	GCStackRoot<Closure> closure(dynamic_cast<Closure*>(parse_eval));
	EXPECT_TRUE(closure.get() != nullptr);
	
	// Compile the function.
	closure->compile();

	// And call it.
	ArgList args(nullptr, ArgList::Status::PROMISED);
	GCStackRoot<Expression> call(CXXR_NEW(Expression(closure)));
	return closure->invoke(env, &args, call);
    }
};

Executor* Executor::JITExecutor() {
    return new class JITExecutor();
}
#else

// The JIT is disabled.  Use the interpreter instead.
Executor* Executor::JITExecutor() {
    return Executor::InterpreterExecutor();
}

#endif  // ENABLE_LLVM_JIT
