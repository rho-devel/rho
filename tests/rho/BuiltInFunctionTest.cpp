/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  http://www.r-project.org/Licenses/
 */

#include "gtest/gtest.h"
#include "rho/BuiltInFunction.hpp"

using namespace rho;

static void checkPrimitiveFunction(BuiltInFunction* function,
				   const char* name)
{
    ASSERT_TRUE(function != nullptr);
    EXPECT_STREQ(name, function->name());
    EXPECT_FALSE(function->viaDotInternal());
}

static void checkInternalFunction(BuiltInFunction* function,
				  const char* name)
{
    ASSERT_TRUE(function != nullptr);
    EXPECT_STREQ(name, function->name());
    EXPECT_TRUE(function->viaDotInternal());
}

// 'if' is the first element in the function table in names.cpp.
TEST(BuiltInFunctionTest, obtainPrimitiveSucceedsForIf) {
    BuiltInFunction* f = BuiltInFunction::obtainPrimitive("if");
    checkPrimitiveFunction(f, "if");
}

TEST(BuiltInFunctionTest, obtainInternalFailsForIf) {
    EXPECT_EQ(nullptr, BuiltInFunction::obtainInternal("if"));
}

// 'La_svd_cmplx' is the last element in the function table in names.cpp.
TEST(BuiltInFunctionTest, obtainPrimitiveFailsForLaSvdCmplx) {
    EXPECT_EQ(nullptr, BuiltInFunction::obtainPrimitive("La_svd_cmplx"));
}

TEST(BuiltInFunctionTest, obtainInternalSucceedsForLaSvdCmplx) {
    BuiltInFunction* f = BuiltInFunction::obtainInternal("La_svd_cmplx");
    checkInternalFunction(f, "La_svd_cmplx");
}

// 'no_such_function' doesn't exist.
TEST(BuiltInFunctionTest, obtainPrimitiveFailsForNoSuchFunction) {
    EXPECT_EQ(nullptr, BuiltInFunction::obtainPrimitive("no_such_function"));
}

TEST(BuiltInFunctionTest, obtainInternalFailsForNoSuchFunction) {
    EXPECT_EQ(nullptr, BuiltInFunction::obtainInternal("no_such_function"));
}

// null symbol
TEST(BuiltInFunctionTest, obtainInternalFailsForNull) {
    EXPECT_EQ(nullptr, BuiltInFunction::obtainInternal(nullptr));
}
