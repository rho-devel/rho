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
#include "CXXR/GCNode.hpp"
#include "CXXR/RealVector.h"
#include "CXXR/RObject.h"

using namespace CXXR;

TEST(GCRootTest, UpdatesReferenceCounts) {
    // Since we're testing gc roots, these tests may not always have the
    // roots declared at GC points, so disable GC.
    GCNode::GCInhibitor no_gc;
    
    RObject* object1 = CXXR_NEW(RealVector(1));
    EXPECT_EQ(0, getRefCount(object1));

    {
	GCRoot<> root1(object1);
	EXPECT_EQ(1, getRefCount(object1));
	{
	    GCRoot<> root2(object1);
	    EXPECT_EQ(2, getRefCount(object1));
	}		  
	EXPECT_EQ(1, getRefCount(object1));
    }		  
    EXPECT_EQ(0, getRefCount(object1));
}


TEST(GCRootTest, TestAssignment) {
    GCNode::GCInhibitor no_gc;
    
    RObject* object1 = CXXR_NEW(RealVector(1));
    RObject* object2 = CXXR_NEW(RealVector(2));

    EXPECT_EQ(0, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2));

    GCRoot<> root1;
    root1 = object1;
    EXPECT_EQ(1, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2));

    root1 = object2;
    EXPECT_EQ(0, getRefCount(object1));
    EXPECT_EQ(1, getRefCount(object2));

    GCRoot<> root2 = object1;
    EXPECT_EQ(1, getRefCount(object1));
    EXPECT_EQ(1, getRefCount(object2));

    root1 = root2;
    EXPECT_EQ(2, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2));
}

TEST(GCRootTest, OutOfOrderDestruction) {
    GCNode::GCInhibitor no_gc;

    RObject* object1 = CXXR_NEW(RealVector(1));
    RObject* object2 = CXXR_NEW(RealVector(2));
    RObject* object3 = CXXR_NEW(RealVector(3));

    GCRoot<>* root1 = new GCRoot<>(object1);
    GCRoot<>* root2 = new GCRoot<>(object2);
    GCRoot<>* root3 = new GCRoot<>(object3);

    EXPECT_EQ(1, getRefCount(object1));
    EXPECT_EQ(1, getRefCount(object2));
    EXPECT_EQ(1, getRefCount(object3));

    // In the other tests, roots are destroyed in order 3-2-1.
    delete root2;
    EXPECT_EQ(1, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2));
    EXPECT_EQ(1, getRefCount(object3));

    delete root3;
    EXPECT_EQ(1, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2));
    EXPECT_EQ(0, getRefCount(object3));

    delete root1;
    EXPECT_EQ(0, getRefCount(object1));
    EXPECT_EQ(0, getRefCount(object2));
    EXPECT_EQ(0, getRefCount(object3));

    GCRoot<> tmp(object1);  // check that the structure isn't corrupted.
}
