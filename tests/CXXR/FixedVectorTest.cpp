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
#include "CXXR/FixedVector.hpp"
#include "CXXR/IntVector.h"
#include "CXXR/ListVector.h"
#include "CXXR/RealVector.h"

using namespace CXXR;

struct ReferentChecker : public GCNode::const_visitor
{
    ReferentChecker(std::set<const GCNode*> nodes) : m_expected_nodes(nodes){}

    void operator()(const GCNode* node) override {
        EXPECT_TRUE(m_expected_nodes.find(node) != m_expected_nodes.end());
        m_expected_nodes.erase(node);
    }
    bool ok() const {
        return m_expected_nodes.empty();
    }

    std::set<const GCNode*> m_expected_nodes;
};

TEST(ListVectorTest, VisitReferentsWorks)
{
    ListVector* vector = ListVector::create(3);

    (*vector)[0] = IntVector::createScalar(2);
    (*vector)[1] = nullptr;
    (*vector)[2] = RealVector::createScalar(3.1);

    ReferentChecker checker(std::set<const GCNode*>({
                // NB: (*vector)[1] is missing here because it is nullptr and
                // never gets passed to the visitor.
                (*vector)[0].get(), (*vector)[2].get() }));
    vector->visitReferents(&checker);
    EXPECT_TRUE(checker.ok());
}

TEST(IntegerVectorTest, ScalarConstructor) {
    IntVector* scalar = IntVector::createScalar(17);
    ASSERT_EQ(1, scalar->size());
    EXPECT_EQ(17, (*scalar)[0]);
}

TEST(IntegerVectorTest, InitializerListConstructor) {
    IntVector* object = IntVector::create({ 7, 14, 8, 1 });
    ASSERT_EQ(4, object->size());
    EXPECT_EQ( 7, (*object)[0]);
    EXPECT_EQ(14, (*object)[1]);
    EXPECT_EQ( 8, (*object)[2]);
    EXPECT_EQ( 1, (*object)[3]);
}

TEST(IntegerVectorTest, ShortInitializerListConstructor) {
    IntVector* object = IntVector::create({ 7 });
    ASSERT_EQ(1, object->size());
    EXPECT_EQ( 7, (*object)[0]);

    object = IntVector::create({ });
    EXPECT_EQ(0, object->size());
}
