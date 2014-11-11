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

static unsigned char getRefCount(const GCNode* node) {
    return GCTestHelper::getRefCount(node);
}

TEST(GCStackFrameBoundaryTest, RefCountIsZeroWithoutBarrier) {
    // Since we're testing stack roots, these tests may not always have the
    // correct stack roots declared at GC points, so disable GC.
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));
    ASSERT_EQ(0, getRefCount(object1));

    GCStackRoot<> root1(object1);
    ASSERT_EQ(0, getRefCount(object1));

    RObject* object2 = CXXR_NEW(RealVector(2));
    ASSERT_EQ(0, getRefCount(object1));
    ASSERT_EQ(0, getRefCount(object2));
}


TEST(GCStackFrameBoundaryTest, BarrierIncrementsAndDecrementsCount) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));
    RObject* object2 = CXXR_NEW(RealVector(2));

    GCStackRoot<> root1(object1);
    {
	GCStackFrameBoundary boundary;
	ASSERT_EQ(0, getRefCount(object1));
	ASSERT_EQ(0, getRefCount(object2));

	GCStackRoot<> root2(object2);  // After the boundary, so not incremented.
	GCStackFrameBoundary::advanceBarrier();
	ASSERT_EQ(1, getRefCount(object1));
	ASSERT_EQ(0, getRefCount(object2));
    }

    ASSERT_EQ(0, getRefCount(object1));
    ASSERT_EQ(0, getRefCount(object2));
}

TEST(GCStackFrameBoundaryTest, NestedBoundarysUpdateRefCountsOnce) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));
    RObject* object2 = CXXR_NEW(RealVector(2));
    RObject* object3 = CXXR_NEW(RealVector(3));

    GCStackRoot<> root1(object1);
    {
	GCStackFrameBoundary boundary1;

	GCStackRoot<> root2(object2);

	{
	    GCStackFrameBoundary boundary2;

	    GCStackRoot<> root3(object3);
	    GCStackFrameBoundary::advanceBarrier();

	    ASSERT_EQ(1, getRefCount(object1));
	    ASSERT_EQ(1, getRefCount(object2));
	    ASSERT_EQ(0, getRefCount(object3));
	}
	ASSERT_EQ(1, getRefCount(object1));
	ASSERT_EQ(0, getRefCount(object2));
	ASSERT_EQ(0, getRefCount(object3));
    }
    ASSERT_EQ(0, getRefCount(object1));
    ASSERT_EQ(0, getRefCount(object2));
    ASSERT_EQ(0, getRefCount(object3));
}

TEST(GCStackFrameBoundaryTest, AdvancingNestedBoundarysUpdateRefCountsOnce) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));
    RObject* object2 = CXXR_NEW(RealVector(2));
    RObject* object3 = CXXR_NEW(RealVector(3));

    GCStackRoot<> root1(object1);
    {
	GCStackFrameBoundary boundary1;
	GCStackFrameBoundary::advanceBarrier();

	GCStackRoot<> root2(object2);

	ASSERT_EQ(1, getRefCount(object1));
	ASSERT_EQ(0, getRefCount(object2));
	ASSERT_EQ(0, getRefCount(object3));

	{
	    GCStackFrameBoundary boundary2;

	    GCStackRoot<> root3(object3);
	    GCStackFrameBoundary::advanceBarrier();

	    ASSERT_EQ(1, getRefCount(object1));
	    ASSERT_EQ(1, getRefCount(object2));
	    ASSERT_EQ(0, getRefCount(object3));
	}

	ASSERT_EQ(1, getRefCount(object1));
	ASSERT_EQ(0, getRefCount(object2));
	ASSERT_EQ(0, getRefCount(object3));
    }
    ASSERT_EQ(0, getRefCount(object1));
    ASSERT_EQ(0, getRefCount(object2));
    ASSERT_EQ(0, getRefCount(object3));
}

TEST(GCStackFrameBoundaryTest, MultipleRoots) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));
    GCStackRoot<> root1(object1);
    GCStackRoot<> root2(object1);
    ASSERT_EQ(0, getRefCount(object1));

    {
	GCStackFrameBoundary boundary;
	GCStackFrameBoundary::advanceBarrier();

	ASSERT_EQ(2, getRefCount(object1));
    }
    ASSERT_EQ(0, getRefCount(object1));
}
