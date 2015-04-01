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
#include "CXXR/Frame.hpp"
#include "CXXR/ListFrame.hpp"
#include "CXXR/RealVector.h"
#include "CXXR/StdFrame.hpp"

#ifdef ENABLE_LLVM_JIT
#include "CXXR/jit/CompiledFrame.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"
using CXXR::JIT::CompiledFrame;
using CXXR::JIT::FrameDescriptor;
#endif

using namespace CXXR;

typedef Frame* (*FrameConstructor)();

class FrameTest : public ::testing::TestWithParam<FrameConstructor> {
public:
    FrameTest()
    {
	symbol1 = Symbol::obtain("test_symbol_1");
	symbol2 = Symbol::obtain("test_symbol_2");
	symbol3 = Symbol::obtain("test_symbol_3");

	value1 = RealVector::createScalar(1.1);
	value2 = RealVector::createScalar(2.1);
	value3 = RealVector::createScalar(3.1);
    }

    Frame* new_frame()
    {
	const FrameConstructor& make_frame = GetParam();
	return (*make_frame)();
    }

protected:
    Symbol* symbol1;
    Symbol* symbol2;
    Symbol* symbol3;
    GCRoot<RObject> value1;
    GCRoot<RObject> value2;
    GCRoot<RObject> value3;
};

TEST_P(FrameTest, IsInitiallyEmpty) {
    Frame* frame = new_frame();
    EXPECT_EQ(0, frame->size());
    EXPECT_TRUE(frame->symbols(true).empty());
    EXPECT_TRUE(frame->symbols(false).empty());
    EXPECT_EQ(nullptr, frame->asPairList());
    // TODO: Assert that bindingRange() is empty
    EXPECT_EQ(nullptr, frame->binding(symbol1));
    EXPECT_EQ(nullptr, frame->binding(symbol2));
    EXPECT_EQ(nullptr, frame->binding(symbol3));
    EXPECT_EQ(0, frame->size());
}

TEST_P(FrameTest, BindingInitialization) {
    Frame* frame = new_frame();
    Frame::Binding* binding = frame->bind(symbol1, value1);

    ASSERT_NE(nullptr, binding);
    EXPECT_EQ(symbol1, binding->symbol());
    EXPECT_EQ(value1, binding->rawValue());
    EXPECT_EQ(value1, binding->forcedValue());
    EXPECT_EQ(frame, binding->frame());
}

TEST_P(FrameTest, AddAndRetrieveOneItem) {
    Frame* frame = new_frame();
    Frame::Binding* binding = frame->bind(symbol1, value1);

    EXPECT_EQ(1, frame->size());
    ASSERT_EQ(1, frame->symbols(true).size());
    ASSERT_EQ(1, frame->symbols(false).size());
    EXPECT_EQ(symbol1, frame->symbols(true)[0]);

    EXPECT_EQ(binding, frame->binding(symbol1));
    EXPECT_EQ(nullptr, frame->binding(symbol2));
    EXPECT_EQ(nullptr, frame->binding(symbol3));
}

TEST_P(FrameTest, AddAndRetrieveTwoItem) {
    Frame* frame = new_frame();
    Frame::Binding* binding1 = frame->bind(symbol1, value1);
    Frame::Binding* binding2 = frame->bind(symbol2, value2);

    EXPECT_EQ(2, frame->size());
    EXPECT_EQ(2, frame->symbols(true).size());
    EXPECT_EQ(2, frame->symbols(false).size());

    EXPECT_EQ(binding1, frame->binding(symbol1));
    EXPECT_EQ(binding2, frame->binding(symbol2));
    EXPECT_EQ(nullptr, frame->binding(symbol3));
}

TEST_P(FrameTest, EraseItem) {
    Frame* frame = new_frame();
    frame->bind(symbol1, value1);
    Frame::Binding* binding2 = frame->bind(symbol2, value2);
    frame->erase(symbol1);

    EXPECT_EQ(1, frame->size());
    ASSERT_EQ(1, frame->symbols(true).size());
    ASSERT_EQ(1, frame->symbols(false).size());
    EXPECT_EQ(symbol2, frame->symbols(true)[0]);

    EXPECT_EQ(nullptr, frame->binding(symbol1));
    EXPECT_EQ(binding2, frame->binding(symbol2));
    EXPECT_EQ(nullptr, frame->binding(symbol3));
}

TEST_P(FrameTest, ImportBindings) {
    Frame* from_frame = new_frame();
    from_frame->bind(symbol1, value1);
    from_frame->bind(symbol2, value2);

    Frame* to_frame = new_frame();
    to_frame->bind(symbol3, value3);
    EXPECT_EQ(1, to_frame->size());
    to_frame->importBindings(from_frame, true);

    // Check that from_frame hasn't changed.
    EXPECT_EQ(2, from_frame->size());
    EXPECT_EQ(value1, from_frame->binding(symbol1)->rawValue());
    EXPECT_EQ(value2, from_frame->binding(symbol2)->rawValue());

    // Check that to_frame has the expected contents.
    EXPECT_EQ(3, to_frame->size());
    EXPECT_EQ(value1, to_frame->binding(symbol1)->rawValue());
    EXPECT_EQ(value2, to_frame->binding(symbol2)->rawValue());
    EXPECT_EQ(value3, to_frame->binding(symbol3)->rawValue());
}

TEST_P(FrameTest, DotSymbols) {
    Frame* frame = new_frame();
    frame->bind(symbol1, value1);
    frame->bind(Symbol::obtain(".a"), value2);
    frame->bind(Symbol::obtainDotDotSymbol(100), value3);

    EXPECT_EQ(3, frame->size());
    std::vector<const Symbol*> visible_symbols = frame->symbols(false);
    EXPECT_EQ(1, visible_symbols.size());
    EXPECT_EQ(symbol1, visible_symbols[0]);

    std::vector<const Symbol*> all_symbols = frame->symbols(true);
    EXPECT_EQ(3, all_symbols.size());
    EXPECT_TRUE(find(all_symbols.begin(), all_symbols.end(), symbol1)
		!= all_symbols.end());
    EXPECT_TRUE(find(all_symbols.begin(), all_symbols.end(),
		     Symbol::obtain(".a")) != all_symbols.end());
    EXPECT_TRUE(find(all_symbols.begin(), all_symbols.end(),
		     Symbol::obtainDotDotSymbol(100)) != all_symbols.end());
}

// TODO(kmillar): add more tests.

static Frame* MakeStdFrame() {
    return new StdFrame();
}
INSTANTIATE_TEST_CASE_P(StdFrameTest,
			FrameTest,
			::testing::Values(MakeStdFrame));

static Frame* MakeListFrame() {
    return new StdFrame();
}
INSTANTIATE_TEST_CASE_P(ListFrameTest,
			FrameTest,
			::testing::Values(MakeListFrame));

#ifdef ENABLE_LLVM_JIT
static Frame* MakeEmptyCompiledFrame() {
  GCStackRoot<FrameDescriptor> descriptor(
      new FrameDescriptor(std::initializer_list<const Symbol*>{},
			  std::initializer_list<const Symbol*>{}));
    return new CompiledFrame(descriptor);
}
INSTANTIATE_TEST_CASE_P(EmptyCompiledFrameTest,
			FrameTest,
			::testing::Values(
			    MakeEmptyCompiledFrame));

// The tests are slightly different depending on which of symbol1, symbol2 and
// symbol3 are in the frame descriptor, so do all three.
static Frame* MakeOneItemCompiledFrame1() {
  GCStackRoot<FrameDescriptor> descriptor(
      new FrameDescriptor(
          std::initializer_list<const Symbol*>{ Symbol::obtain("test_symbol_1") },
          std::initializer_list<const Symbol*>{}));
    return new CompiledFrame(descriptor);
}
INSTANTIATE_TEST_CASE_P(OneItemCompiledFrameTest1,
			FrameTest,
			::testing::Values(
			    MakeOneItemCompiledFrame1));

static Frame* MakeOneItemCompiledFrame2() {
  GCStackRoot<FrameDescriptor> descriptor(
      new FrameDescriptor(
          std::initializer_list<const Symbol*>{ Symbol::obtain("test_symbol_2") },
          std::initializer_list<const Symbol*>{}));
    return new CompiledFrame(descriptor);
}
INSTANTIATE_TEST_CASE_P(OneItemCompiledFrameTest2,
			FrameTest,
			::testing::Values(
			    MakeOneItemCompiledFrame2));

static Frame* MakeOneItemCompiledFrame3() {
  GCStackRoot<FrameDescriptor> descriptor(
      new FrameDescriptor(
          std::initializer_list<const Symbol*>{ Symbol::obtain("test_symbol_3") },
          std::initializer_list<const Symbol*>{}));
    return new CompiledFrame(descriptor);
}
INSTANTIATE_TEST_CASE_P(OneItemCompiledFrameTest3,
			FrameTest,
			::testing::Values(
			    MakeOneItemCompiledFrame3));
#endif
