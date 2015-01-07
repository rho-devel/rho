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

#include "TestHelpers.hpp"
#include "CXXR/GCStackFrameBoundary.hpp"
#include "CXXR/RealVector.h"
#include "CXXR/RObject.h"

using namespace CXXR;

TEST(GCStackFrameBoundaryTest, RefCountIsZeroWithoutBarrier) {
    // Since we're testing stack roots, these tests may not always have the
    // correct stack roots declared at GC points, so disable GC.
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));
    EXPECT_EQ(0, getRefCount(object1));

    RObject* object2 = CXXR_NEW(RealVector(2));
    EXPECT_EQ(0, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2));
}

TEST(GCStackFrameBoundaryTest, BarrierIncrementsAndDecrementsCount) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));

    RObject* object2_ = GCStackFrameBoundary::withStackFrameBoundary(
	[=]()
	{
	    RObject* object2 = CXXR_NEW(RealVector(2));

	    EXPECT_EQ(0, getRefCount(object1));
	    EXPECT_EQ(0, getRefCount(object2));

	    GCStackFrameBoundary::advanceBarrier();
	    EXPECT_GE(getRefCount(object1), 1);
	    EXPECT_EQ(getRefCount(object2), 0);
	    return object2;
	});
    EXPECT_EQ(0, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2_));
}

TEST(GCStackFrameBoundaryTest, NestedBoundarys) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));

    GCStackFrameBoundary::withStackFrameBoundary(
	[=]()
	{
	    GCStackFrameBoundary::withStackFrameBoundary(
		[=]()
		{
		    GCStackFrameBoundary::advanceBarrier();

		    EXPECT_GE(getRefCount(object1), 1);
		    return (RObject*)nullptr;
		});
	    EXPECT_GE(getRefCount(object1), 1);
	    return (RObject*)nullptr;
	});
    EXPECT_EQ(0, getRefCount(object1));
}

TEST(GCStackFrameBoundaryTest, NestedAdvances) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));

    GCStackFrameBoundary::withStackFrameBoundary(
	[=]()
	{
	    GCStackFrameBoundary::advanceBarrier();
	    EXPECT_GE(getRefCount(object1), 1);

	    GCStackFrameBoundary::withStackFrameBoundary(
		[=]()
		{
		    GCStackFrameBoundary::advanceBarrier();

		    EXPECT_GE(getRefCount(object1), 1);
		    return (RObject*)nullptr;
		});
	    EXPECT_GE(getRefCount(object1), 1);
	    return (RObject*)nullptr;
	});
    EXPECT_EQ(0, getRefCount(object1));
}
