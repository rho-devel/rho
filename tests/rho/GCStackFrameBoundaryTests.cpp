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
#include "rho/GCStackFrameBoundary.hpp"
#include "rho/GCManager.hpp"
#include "rho/RealVector.hpp"
#include "rho/RObject.hpp"

using namespace rho;

TEST(GCStackFrameBoundaryTest, RefCountIsZeroWithoutBarrier) {
    // Since we're testing stack roots, these tests may not always have the
    // correct stack roots declared at GC points, so disable GC.
    GCManager::GCInhibitor no_gc;

    RObject* object1 = RealVector::createScalar(1);
    EXPECT_FALSE(isOnStackBitSet(object1));

    RObject* object2 = RealVector::createScalar(2);
    EXPECT_FALSE(isOnStackBitSet(object1));
    EXPECT_FALSE(isOnStackBitSet(object2));
}

TEST(GCStackFrameBoundaryTest, BarrierIncrementsAndDecrementsCount) {
    GCManager::GCInhibitor no_gc;

    RObject* object1 = RealVector::createScalar(1);

    RObject* object2_ = GCStackFrameBoundary::withStackFrameBoundary(
	[=]()
	{
	    RObject* object2 = RealVector::createScalar(2);

	    EXPECT_FALSE(isOnStackBitSet(object1));
	    EXPECT_FALSE(isOnStackBitSet(object2));

	    GCStackFrameBoundary::advanceBarrier();
	    EXPECT_TRUE(isOnStackBitSet(object1));
	    EXPECT_FALSE(isOnStackBitSet(object2));
	    return object2;
	});
    EXPECT_FALSE(isOnStackBitSet(object1));
    EXPECT_FALSE(isOnStackBitSet(object2_));
}

TEST(GCStackFrameBoundaryTest, NestedBoundarys) {
    GCManager::GCInhibitor no_gc;

    RObject* object1 = RealVector::createScalar(1);

    GCStackFrameBoundary::withStackFrameBoundary(
	[=]()
	{
	    GCStackFrameBoundary::withStackFrameBoundary(
		[=]()
		{
		    GCStackFrameBoundary::advanceBarrier();

		    EXPECT_TRUE(isOnStackBitSet(object1));
		    return (RObject*)nullptr;
		});
	    EXPECT_TRUE(isOnStackBitSet(object1));
	    return (RObject*)nullptr;
	});
    EXPECT_FALSE(isOnStackBitSet(object1));
}

TEST(GCStackFrameBoundaryTest, NestedAdvances) {
    GCManager::GCInhibitor no_gc;

    RObject* object1 = RealVector::createScalar(1);

    GCStackFrameBoundary::withStackFrameBoundary(
	[=]()
	{
	    GCStackFrameBoundary::advanceBarrier();
	    EXPECT_TRUE(isOnStackBitSet(object1));

	    GCStackFrameBoundary::withStackFrameBoundary(
		[=]()
		{
		    GCStackFrameBoundary::advanceBarrier();

		    EXPECT_TRUE(isOnStackBitSet(object1));
		    return (RObject*)nullptr;
		});
	    EXPECT_TRUE(isOnStackBitSet(object1));
	    return (RObject*)nullptr;
	});
    EXPECT_FALSE(isOnStackBitSet(object1));
}
