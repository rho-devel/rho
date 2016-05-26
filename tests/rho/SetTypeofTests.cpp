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
#include "TestHelpers.hpp"
#include "rho/LogicalVector.hpp"
#include "rho/IntVector.hpp"
#include "Rinternals.h"

using namespace rho;

TEST(SetTypeOf, LogicalToIntPreservesValues) {
    LogicalVector* value = LogicalVector::create({true, false, Logical::NA(),
                true, false, Logical::NA() });
    EXPECT_EQ(true, (*value)[2].isNA());
    SET_TYPEOF(value, INTSXP);
    IntVector* expected_result = IntVector::create({ 1, 0, NA_INTEGER,
                1, 0, NA_INTEGER });
    EXPECT_IDENTICAL(expected_result, value);
}

TEST(SetTypeOf, ZeroLengthLogicalToIntWorks) {
    LogicalVector* value = LogicalVector::create(0);
    SET_TYPEOF(value, INTSXP);
    IntVector* expected_result = IntVector::create(0);
    EXPECT_IDENTICAL(expected_result, value);
}
