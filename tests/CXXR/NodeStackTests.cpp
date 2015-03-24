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
#include "CXXR/IntVector.h"
#include "CXXR/NodeStack.hpp"
#include "CXXR/RObject.h"
#include "TestHelpers.hpp"

namespace CXXR {
namespace {
class IntVectorChecker : public GCNode::const_visitor {
 public:
  IntVectorChecker(const std::vector<int>& expected_values) :
      expected_values_(expected_values), next_expected_value_(0) {}
  void operator()(const GCNode* node) {
    // Make sure we get the right node type.
    const IntVector* int_vec_node = dynamic_cast<const IntVector*>(node);
    EXPECT_NE(nullptr, int_vec_node);
    EXPECT_LT(next_expected_value_, expected_values_.size());
    EXPECT_EQ((*int_vec_node)[0], expected_values_[next_expected_value_++]);
  }

 private:
  const std::vector<int> expected_values_;
  int next_expected_value_;
};
}  // namespace

class NodeStackTest : public ::testing::Test {
 public:
  NodeStackTest() {}
 private:
  GCManager::GCInhibitor gc_inhibitor_;  // This disables garbage collection.
};

// Makes sure we can create a NodeStack object, i.e., checks the constructor.
TEST_F(NodeStackTest, checkCreation) {
  NodeStack zero_initial_size(0);
  EXPECT_EQ(zero_initial_size.size(), 0);
  NodeStack nonzero_initial_size(512);
  EXPECT_EQ(nonzero_initial_size.size(), 0);
}

// Tries to push some objects onto the stack.
TEST_F(NodeStackTest, checkPush) {
  NodeStack stack(16);
  auto int_vec = IntVector::createScalar(0);
  EXPECT_EQ(0, stack.size());
  stack.push(int_vec);
  EXPECT_EQ(1, stack.size());
  stack.push(int_vec);
  EXPECT_EQ(2, stack.size());
}

// Tries a combination of pushes and pops.
TEST_F(NodeStackTest, checkPushAndPop) {
  NodeStack stack(16);
  auto int_vec1 = IntVector::createScalar(1);
  auto int_vec2 = IntVector::createScalar(2);
  {
    // Push a couple of values and pop with topnpop().
    EXPECT_EQ(0, stack.size());
    stack.push(int_vec1);
    stack.push(int_vec2);
    EXPECT_EQ(2, stack.size());
    auto top1 = stack.topnpop();
    EXPECT_EQ(1, stack.size());
    EXPECT_EQ((*static_cast<IntVector *>(top1))[0], 2);
    auto top2 = stack.topnpop();
    EXPECT_EQ(0, stack.size());
    EXPECT_EQ((*static_cast<IntVector *>(top2))[0], 1);
  }
  {
    // Push a couple of values and pop with pop().
    EXPECT_EQ(0, stack.size());
    stack.push(int_vec1);
    stack.push(int_vec2);
    EXPECT_EQ(2, stack.size());
    stack.pop(2);
    EXPECT_EQ(stack.size(), 0);
  }
}

// Tries several pushes, exceeding the initially specified stack size.
TEST_F(NodeStackTest, checkSeveralPushesToGrowStack) {
  NodeStack stack(16);
  auto int_vec = IntVector::createScalar(0);
  // Push a several pointers.
  for (int i = 0; i < 64; i++) {
    stack.push(int_vec);
    EXPECT_EQ(stack.size(), i+1);
  }
  // Now, pop all of them.
  stack.pop(64);
  EXPECT_EQ(stack.size(), 0);
}

// Reads values on the stack as RObject pointers.
TEST_F(NodeStackTest, checkElementReadAccessAsRObject) {
  NodeStack stack(16);
  auto int_vec1 = IntVector::createScalar(1);
  auto int_vec2 = IntVector::createScalar(2);
  auto int_vec3 = IntVector::createScalar(3);
  stack.push(int_vec1);
  stack.push(int_vec2);
  stack.push(int_vec3);
  const RObject* elt1 = stack[0];
  const RObject* elt2 = stack[1];
  const RObject* elt3 = stack[2];
  EXPECT_EQ((*static_cast<const IntVector *>(elt1))[0], 1);
  EXPECT_EQ((*static_cast<const IntVector *>(elt2))[0], 2);
  EXPECT_EQ((*static_cast<const IntVector *>(elt3))[0], 3);
}

// Reads values on the stack as ElementProxy objects.
TEST_F(NodeStackTest, checkElementReadAccessWithElementProxy) {
  NodeStack stack(16);
  auto int_vec1 = IntVector::createScalar(1);
  auto int_vec2 = IntVector::createScalar(2);
  auto int_vec3 = IntVector::createScalar(3);
  stack.push(int_vec1);
  stack.push(int_vec2);
  stack.push(int_vec3);
  NodeStack::ElementProxy elt1 = stack[0];
  NodeStack::ElementProxy elt2 = stack[1];
  NodeStack::ElementProxy elt3 = stack[2];
  EXPECT_EQ((*static_cast<const IntVector *>((RObject*)(elt1)))[0], 1);
  EXPECT_EQ((*static_cast<const IntVector *>((RObject*)(elt2)))[0], 2);
  EXPECT_EQ((*static_cast<const IntVector *>((RObject*)(elt3)))[0], 3);
}

// Changes values on the stack using ElementProxy objects.
TEST_F(NodeStackTest, checkElementWriteAccessWithElementProxy) {
  NodeStack stack(16);
  auto int_vec1 = IntVector::createScalar(1);
  auto int_vec2 = IntVector::createScalar(2);
  auto int_vec3 = IntVector::createScalar(3);
  stack.push(int_vec1);
  stack.push(int_vec2);
  stack.push(int_vec3);
  NodeStack::ElementProxy elt1 = stack[0];
  NodeStack::ElementProxy elt2 = stack[1];
  NodeStack::ElementProxy elt3 = stack[2];
  elt1 = int_vec3;
  elt2 = int_vec3;
  EXPECT_EQ((*static_cast<const IntVector *>((RObject*)(elt1)))[0], 3);
  EXPECT_EQ((*static_cast<const IntVector *>((RObject*)(elt2)))[0], 3);
  EXPECT_EQ((*static_cast<const IntVector *>((RObject*)(elt3)))[0], 3);
}

// Test the visitRoots method.
TEST_F(NodeStackTest, checkVisitRoots) {
  NodeStack stack(16);
  stack.push(IntVector::createScalar(1));
  stack.push(IntVector::createScalar(2));
  stack.push(IntVector::createScalar(3));
  IntVectorChecker int_vector_checker(std::vector<int>{1, 2, 3});
  stack.visitRoots(&int_vector_checker);
}

// Tests Scope.
TEST_F(NodeStackTest, checkScope) {
  NodeStack stack(16);
  stack.push(IntVector::createScalar(1));
  EXPECT_EQ(stack.size(), 1);
  {
    // When a Scope object goes out of scope it should cause those elements to
    // get popped that were pushed after its creation.
    NodeStack::Scope scope(&stack);
    stack.push(IntVector::createScalar(2));
    stack.push(IntVector::createScalar(3));
    stack.push(IntVector::createScalar(4));
    EXPECT_EQ(stack.size(), 4);
  }
  // As "scope" goes out of scope, elements 2, 3, and 4 should get popped.
  EXPECT_EQ(stack.size(), 1);
  {
    // Scope should have no effect if nothing is pushed after its creation.
    NodeStack::Scope scope(&stack);
    EXPECT_EQ(stack.size(), 1);
  }
  EXPECT_EQ(stack.size(), 1);
}

// Tests that the call to protectAll increases the ref-count of the objects on
// the stack so that they do not get garbage collected.
TEST_F(NodeStackTest, checkProtectAll) {
  NodeStack stack(16);
  auto d1 = IntVector::createScalar(1);
  auto d2 = IntVector::createScalar(2);
  auto d3 = IntVector::createScalar(3);
  stack.push(d1);
  stack.push(d2);
  stack.push(d3);
  EXPECT_EQ(stack.size(), 3);
  EXPECT_EQ(GCTestHelper::getRefCount(d1), 0);
  EXPECT_EQ(GCTestHelper::getRefCount(d2), 0);
  EXPECT_EQ(GCTestHelper::getRefCount(d3), 0);
  stack.protectAll();
  EXPECT_EQ(GCTestHelper::getRefCount(d1), 1);
  EXPECT_EQ(GCTestHelper::getRefCount(d2), 1);
  EXPECT_EQ(GCTestHelper::getRefCount(d3), 1);

  // Popping should "unprotect" the popped object(s).
  auto popped = stack.topnpop();
  EXPECT_EQ(popped, d3);
  EXPECT_EQ(GCTestHelper::getRefCount(popped), 0);
  stack.pop(2);
  EXPECT_EQ(GCTestHelper::getRefCount(d2), 0);
  EXPECT_EQ(GCTestHelper::getRefCount(d1), 0);
}
}  // namespace CXXR
