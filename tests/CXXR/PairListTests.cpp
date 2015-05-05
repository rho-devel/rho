/*
 *  R : A Computer Language for Statistical Data Analysis
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

#include "gtest/gtest.h"
#include "CXXR/PairList.h"
#include "CXXR/IntVector.h"
#include "CXXR/RealVector.h"

using namespace CXXR;

TEST(PairListTest, MakeEmpty) {
    PairList* result = PairList::make(0);
    EXPECT_EQ(0, listLength(result));
    EXPECT_EQ(NULL, result);

    result = PairList::make(0, nullptr);
    EXPECT_EQ(0, listLength(result));
    EXPECT_EQ(NULL, result);
}

TEST(PairListTest, MakeSingleton) {
    PairList* result = PairList::make(1);
    EXPECT_EQ(1, listLength(result));
    EXPECT_EQ(NULL, result->car());
    EXPECT_EQ(NULL, result->tag());
    EXPECT_EQ(NULL, result->tail());

    RObject* args[] = { IntVector::createScalar(2) };
    result = PairList::make(1, args);
    EXPECT_EQ(1, listLength(result));
    EXPECT_EQ(args[0], result->car());
    EXPECT_EQ(NULL, result->tag());
    EXPECT_EQ(NULL, result->tail());
}

TEST(PairListTest, MakePair) {
    PairList* result = PairList::make(2);
    EXPECT_EQ(2, listLength(result));
    EXPECT_EQ(NULL, result->car());
    EXPECT_EQ(NULL, result->tag());
    PairList* next = result->tail();
    EXPECT_EQ(NULL, next->car());
    EXPECT_EQ(NULL, next->tag());
    EXPECT_EQ(NULL, next->tail());

    RObject* args[] = { IntVector::createScalar(2),
			RealVector::createScalar(3.0) };
    result = PairList::make(2, args);
    EXPECT_EQ(2, listLength(result));
    EXPECT_EQ(args[0], result->car());
    EXPECT_EQ(NULL, result->tag());
    next = result->tail();
    EXPECT_EQ(args[1], next->car());
    EXPECT_EQ(NULL, next->tag());
    EXPECT_EQ(NULL, next->tail());
}
