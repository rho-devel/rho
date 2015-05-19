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
#include "CXXR/Symbol.h"

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

TEST(PairListTest, CopyTags) {
    RObject* dest_args[] = { IntVector::createScalar(2),
			RealVector::createScalar(3.0) };
    PairList* dest = PairList::make(2, dest_args);

    RObject* src_args[] = { IntVector::createScalar(-2),
			     RealVector::createScalar(-3.0) };
    PairList* src = PairList::make(2, src_args);
    src->setTag(Symbol::obtain("tag1"));
    src->tail()->setTag(Symbol::obtain("tag2"));

    dest->copyTagsFrom(src);

    ASSERT_EQ(2, listLength(dest));
    EXPECT_EQ(dest_args[0], dest->car());
    EXPECT_EQ(Symbol::obtain("tag1"), dest->tag());
    PairList* next = dest->tail();
    EXPECT_EQ(dest_args[1], next->car());
    EXPECT_EQ(Symbol::obtain("tag2"), next->tag());
    EXPECT_EQ(NULL, next->tail());
}

TEST(PairListTest, CopyNullTag) {
    RObject* dest_args[] = { IntVector::createScalar(2),
			RealVector::createScalar(3.0) };
    PairList* dest = PairList::make(2, dest_args);

    RObject* src_args[] = { IntVector::createScalar(-2),
			     RealVector::createScalar(-3.0) };
    PairList* src = PairList::make(2, src_args);
    src->tail()->setTag(Symbol::obtain("tag2"));

    dest->copyTagsFrom(src);

    ASSERT_EQ(2, listLength(dest));
    EXPECT_EQ(dest_args[0], dest->car());
    EXPECT_EQ(NULL, dest->tag());
    PairList* next = dest->tail();
    EXPECT_EQ(dest_args[1], next->car());
    EXPECT_EQ(Symbol::obtain("tag2"), next->tag());
    EXPECT_EQ(NULL, next->tail());
}

TEST(PairListTest, CopyTagsFromShortList) {
    RObject* dest_args[] = { IntVector::createScalar(2),
			RealVector::createScalar(3.0) };
    PairList* dest = PairList::make(2, dest_args);

    RObject* src_args[] = { IntVector::createScalar(-2) };
    PairList* src = PairList::make(1, src_args);
    src->setTag(Symbol::obtain("tag1"));

    dest->copyTagsFrom(src);

    ASSERT_EQ(2, listLength(dest));
    EXPECT_EQ(dest_args[0], dest->car());
    EXPECT_EQ(Symbol::obtain("tag1"), dest->tag());
    PairList* next = dest->tail();
    EXPECT_EQ(dest_args[1], next->car());
    EXPECT_EQ(NULL, next->tag());
    EXPECT_EQ(NULL, next->tail());
}

TEST(PairListTest, CopyTagsFromLongList) {
    RObject* dest_args[] = { IntVector::createScalar(2),
			RealVector::createScalar(3.0) };
    PairList* dest = PairList::make(2, dest_args);

    RObject* src_args[] = { IntVector::createScalar(-2),
			    RealVector::createScalar(-3.0),
			    IntVector::createScalar(-1) };
    PairList* src = PairList::make(3, src_args);
    src->setTag(Symbol::obtain("tag1"));
    src->tail()->setTag(Symbol::obtain("tag2"));
    src->tail()->tail()->setTag(Symbol::obtain("tag3"));

    dest->copyTagsFrom(src);

    ASSERT_EQ(2, listLength(dest));
    EXPECT_EQ(dest_args[0], dest->car());
    EXPECT_EQ(Symbol::obtain("tag1"), dest->tag());
    PairList* next = dest->tail();
    EXPECT_EQ(dest_args[1], next->car());
    EXPECT_EQ(Symbol::obtain("tag2"), next->tag());
    EXPECT_EQ(NULL, next->tail());
}

TEST(PairListTest, CopyTagsFromNothing) {
    RObject* dest_args[] = { IntVector::createScalar(2),
			RealVector::createScalar(3.0) };
    PairList* dest = PairList::make(2, dest_args);
    dest->copyTagsFrom(nullptr);
    ASSERT_EQ(2, listLength(dest));
    EXPECT_EQ(dest_args[0], dest->car());
    EXPECT_EQ(NULL, dest->tag());
    PairList* next = dest->tail();
    EXPECT_EQ(dest_args[1], next->car());
    EXPECT_EQ(NULL, next->tag());
    EXPECT_EQ(NULL, next->tail());
}
