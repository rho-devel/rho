#include "gtest/gtest.h"
#include "CXXR/jit/CompiledFrame.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"
#include "CXXR/Frame.hpp"
#include "CXXR/ListFrame.hpp"
#include "CXXR/RealVector.h"
#include "CXXR/StdFrame.hpp"

using namespace CXXR;
using CXXR::JIT::CompiledFrame;
using CXXR::JIT::FrameDescriptor;

typedef Frame* (*FrameConstructor)();

class FrameTest : public ::testing::TestWithParam<FrameConstructor> {
public:
    FrameTest()
    {
	symbol1 = Symbol::obtain("test_symbol_1");
	symbol2 = Symbol::obtain("test_symbol_2");
	symbol3 = Symbol::obtain("test_symbol_3");

	value1 = CXXR_NEW(RealVector({ 1.1 }));
	value2 = CXXR_NEW(RealVector({ 2.1 }));
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

// TODO(kmillar): add more tests.

static Frame* MakeStdFrame() {
    return CXXR_NEW(StdFrame());
}
INSTANTIATE_TEST_CASE_P(StdFrameTest,
			FrameTest,
			::testing::Values(MakeStdFrame));

static Frame* MakeListFrame() {
    return CXXR_NEW(StdFrame());
}
INSTANTIATE_TEST_CASE_P(ListFrameTest,
			FrameTest,
			::testing::Values(MakeListFrame));

static Frame* MakeEmptyCompiledFrame() {
  GCStackRoot<FrameDescriptor> descriptor(
      CXXR_NEW(FrameDescriptor(std::initializer_list<const Symbol*>{},
                               std::initializer_list<const Symbol*>{})));
    return CXXR_NEW(CompiledFrame(descriptor));
}
INSTANTIATE_TEST_CASE_P(EmptyCompiledFrameTest,
			FrameTest,
			::testing::Values(
			    MakeEmptyCompiledFrame));

// The tests are slightly different depending on which of symbol1, symbol2 and
// symbol3 are in the frame descriptor, so do all three.
static Frame* MakeOneItemCompiledFrame1() {
  GCStackRoot<FrameDescriptor> descriptor(
      CXXR_NEW(FrameDescriptor(
          std::initializer_list<const Symbol*>{ Symbol::obtain("test_symbol_1") },
          std::initializer_list<const Symbol*>{})));
    return CXXR_NEW(CompiledFrame(descriptor));
}
INSTANTIATE_TEST_CASE_P(OneItemCompiledFrameTest1,
			FrameTest,
			::testing::Values(
			    MakeOneItemCompiledFrame1));

static Frame* MakeOneItemCompiledFrame2() {
  GCStackRoot<FrameDescriptor> descriptor(
      CXXR_NEW(FrameDescriptor(
          std::initializer_list<const Symbol*>{ Symbol::obtain("test_symbol_2") },
          std::initializer_list<const Symbol*>{})));
    return CXXR_NEW(CompiledFrame(descriptor));
}
INSTANTIATE_TEST_CASE_P(OneItemCompiledFrameTest2,
			FrameTest,
			::testing::Values(
			    MakeOneItemCompiledFrame2));

static Frame* MakeOneItemCompiledFrame3() {
  GCStackRoot<FrameDescriptor> descriptor(
      CXXR_NEW(FrameDescriptor(
          std::initializer_list<const Symbol*>{ Symbol::obtain("test_symbol_3") },
          std::initializer_list<const Symbol*>{})));
    return CXXR_NEW(CompiledFrame(descriptor));
}
INSTANTIATE_TEST_CASE_P(OneItemCompiledFrameTest3,
			FrameTest,
			::testing::Values(
			    MakeOneItemCompiledFrame3));
