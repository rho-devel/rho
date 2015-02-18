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

#include "gtest/gtest.h"
#include "CXXR/NodeStack.hpp"
#include "CXXR/RObject.h"
#include "TestHelpers.hpp"

namespace CXXR {
namespace {
class DummyIntSxp : public RObject {
 public:
  DummyIntSxp(int id = 0) : RObject(INTSXP), id_(id) {}
  inline int GetId() const { return id_; }

 private:
  virtual ~DummyIntSxp() {}  // Force creation on heap.
  int id_;                   // Some id to be able to verify the object.
};

class DummyIntSxpChecker : public GCNode::const_visitor {
 public:
  DummyIntSxpChecker(const std::vector<int>& expected_values) :
      expected_values_(expected_values), next_expected_value_(0) {}
  void operator()(const GCNode* node) {
    // Make sure we get the DummyIntSxp node type.
    EXPECT_NE(nullptr, dynamic_cast<const DummyIntSxp*>(node));
    EXPECT_LT(next_expected_value_, expected_values_.size());
    EXPECT_EQ(static_cast<const DummyIntSxp*>(node)->GetId(),
              expected_values_[next_expected_value_++]);
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
  GCNode::GCInhibitor gc_inhibitor_;  // This disables garbage collection.
};

// Makes sure we can create a NodeStack object, i.e., check the constructor.
TEST_F(NodeStackTest, checkCreation) {
  NodeStack zero_initial_size(0);
  EXPECT_EQ(zero_initial_size.size(), 0);
  NodeStack nonzero_initial_size(512);
  EXPECT_EQ(nonzero_initial_size.size(), 0);
}

// Tries to push some objects onto the stack.
TEST_F(NodeStackTest, checkPush) {
  NodeStack stack(16);
  auto dummy_int_sxp = new DummyIntSxp();
  EXPECT_EQ(0, stack.size());
  stack.push(dummy_int_sxp);
  EXPECT_EQ(1, stack.size());
  stack.push(dummy_int_sxp);
  EXPECT_EQ(2, stack.size());
}

// Tries a combination of pushes and pops.
TEST_F(NodeStackTest, checkPushAndPop) {
  NodeStack stack(16);
  auto dummy_int_sxp1 = new DummyIntSxp(1);
  auto dummy_int_sxp2 = new DummyIntSxp(2);
  {
    // Push a couple of values and pop with topnpop().
    EXPECT_EQ(0, stack.size());
    stack.push(dummy_int_sxp1);
    stack.push(dummy_int_sxp2);
    EXPECT_EQ(2, stack.size());
    auto top1 = stack.topnpop();
    EXPECT_EQ(1, stack.size());
    EXPECT_EQ(static_cast<DummyIntSxp *>(top1)->GetId(), 2);
    auto top2 = stack.topnpop();
    EXPECT_EQ(0, stack.size());
    EXPECT_EQ(static_cast<DummyIntSxp *>(top2)->GetId(), 1);
  }
  {
    // Push a couple of values and pop with pop().
    EXPECT_EQ(0, stack.size());
    stack.push(dummy_int_sxp1);
    stack.push(dummy_int_sxp2);
    EXPECT_EQ(2, stack.size());
    stack.pop(2);
    EXPECT_EQ(stack.size(), 0);
  }
}

// Tries several pushes, exceeding the initially specified stack size.
TEST_F(NodeStackTest, checkSeveralPushesToGrowStack) {
  NodeStack stack(16);
  auto dummy_int_sxp = new DummyIntSxp();
  // Push a several pointers.
  for (int i = 0; i < 64; i++) {
    stack.push(dummy_int_sxp);
    EXPECT_EQ(stack.size(), i+1);
  }
  // Now, pop all of them.
  stack.pop(64);
  EXPECT_EQ(stack.size(), 0);
}

// Reads values on the stack as RObject pointers.
TEST_F(NodeStackTest, checkElementReadAccessAsRObject) {
  NodeStack stack(16);
  auto dummy_int_sxp1 = new DummyIntSxp(1);
  auto dummy_int_sxp2 = new DummyIntSxp(2);
  auto dummy_int_sxp3 = new DummyIntSxp(3);
  stack.push(dummy_int_sxp1);
  stack.push(dummy_int_sxp2);
  stack.push(dummy_int_sxp3);
  const RObject* elt1 = stack[0];
  const RObject* elt2 = stack[1];
  const RObject* elt3 = stack[2];
  EXPECT_EQ(static_cast<const DummyIntSxp *>(elt1)->GetId(), 1);
  EXPECT_EQ(static_cast<const DummyIntSxp *>(elt2)->GetId(), 2);
  EXPECT_EQ(static_cast<const DummyIntSxp *>(elt3)->GetId(), 3);
}

// Reads values on the stack as ElementProxy objects.
TEST_F(NodeStackTest, checkElementReadAccessWithElementProxy) {
  NodeStack stack(16);
  auto dummy_int_sxp1 = new DummyIntSxp(1);
  auto dummy_int_sxp2 = new DummyIntSxp(2);
  auto dummy_int_sxp3 = new DummyIntSxp(3);
  stack.push(dummy_int_sxp1);
  stack.push(dummy_int_sxp2);
  stack.push(dummy_int_sxp3);
  NodeStack::ElementProxy elt1 = stack[0];
  NodeStack::ElementProxy elt2 = stack[1];
  NodeStack::ElementProxy elt3 = stack[2];
  EXPECT_EQ(static_cast<const DummyIntSxp *>((RObject*)(elt1))->GetId(), 1);
  EXPECT_EQ(static_cast<const DummyIntSxp *>((RObject*)(elt2))->GetId(), 2);
  EXPECT_EQ(static_cast<const DummyIntSxp *>((RObject*)(elt3))->GetId(), 3);
}

// Changes values on the stack using ElementProxy objects.
TEST_F(NodeStackTest, checkElementWriteAccessWithElementProxy) {
  NodeStack stack(16);
  auto dummy_int_sxp1 = new DummyIntSxp(1);
  auto dummy_int_sxp2 = new DummyIntSxp(2);
  auto dummy_int_sxp3 = new DummyIntSxp(3);
  stack.push(dummy_int_sxp1);
  stack.push(dummy_int_sxp2);
  stack.push(dummy_int_sxp3);
  NodeStack::ElementProxy elt1 = stack[0];
  NodeStack::ElementProxy elt2 = stack[1];
  NodeStack::ElementProxy elt3 = stack[2];
  elt1 = dummy_int_sxp3;
  elt2 = dummy_int_sxp3;
  EXPECT_EQ(static_cast<const DummyIntSxp *>((RObject*)(elt1))->GetId(), 3);
  EXPECT_EQ(static_cast<const DummyIntSxp *>((RObject*)(elt2))->GetId(), 3);
  EXPECT_EQ(static_cast<const DummyIntSxp *>((RObject*)(elt3))->GetId(), 3);
}

// Test the visitRoots method.
TEST_F(NodeStackTest, checkVisitRoots) {
  NodeStack stack(16);
  stack.push(new DummyIntSxp(1));
  stack.push(new DummyIntSxp(2));
  stack.push(new DummyIntSxp(3));
  DummyIntSxpChecker dummy_int_checker(std::vector<int>{1, 2, 3});
  stack.visitRoots(&dummy_int_checker);
}

// Tests Scope.
TEST_F(NodeStackTest, checkScope) {
  NodeStack stack(16);
  stack.push(new DummyIntSxp(1));
  EXPECT_EQ(stack.size(), 1);
  {
    // When a Scope object goes out of scope it should cause those elements to
    // get popped that were pushed after its creation.
    NodeStack::Scope scope(&stack);
    stack.push(new DummyIntSxp(2));
    stack.push(new DummyIntSxp(3));
    stack.push(new DummyIntSxp(4));
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
  auto d1 = new DummyIntSxp(1);
  auto d2 = new DummyIntSxp(2);
  auto d3 = new DummyIntSxp(3);
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
