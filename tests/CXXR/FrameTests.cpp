#include "gtest/gtest.h"
#include "CXXR/Frame.hpp"
#include "CXXR/ListFrame.hpp"
#include "CXXR/StdFrame.hpp"

using namespace CXXR;

typedef Frame* (*FrameConstructor)();

// Parameter is a frame constructor.
class FrameTest : public ::testing::TestWithParam<FrameConstructor> {
public:
    Frame* new_frame() {
	return (*GetParam())();
    }
};

TEST_P(FrameTest, IsInitiallyEmpty) {
    Frame* frame = new_frame();
    ASSERT_EQ(0, frame->size());
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
