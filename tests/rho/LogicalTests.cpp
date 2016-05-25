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
#include "rho/LogicalVector.hpp"

using namespace rho;

TEST(Logical, InitializeWithNA_Logical) {
    // Logical l = NA_LOGICAL;  // Intentional compilation error.
    Logical l(NA_LOGICAL);
    EXPECT_EQ(true, l.isNA());
    EXPECT_EQ(true, Logical::NA().isNA());
}

TEST(Logical, equalsWorks) {
    Logical True = true;
    Logical False = false;
    Logical NA = Logical::NA();

    EXPECT_TRUE(True.equals(True).isTrue());
    EXPECT_TRUE(True.equals(False).isFalse());
    EXPECT_TRUE(True.equals(NA).isNA());

    EXPECT_TRUE(False.equals(True).isFalse());
    EXPECT_TRUE(False.equals(False).isTrue());
    EXPECT_TRUE(False.equals(NA).isNA());

    EXPECT_TRUE(NA.equals(True).isNA());
    EXPECT_TRUE(NA.equals(False).isNA());
    EXPECT_TRUE(NA.equals(NA).isNA());
}

TEST(Logical, identicalWorks) {
    Logical True = true;
    Logical False = false;
    Logical NA = Logical::NA();

    EXPECT_EQ(true, True.identical(True));
    EXPECT_EQ(false, True.identical(False));
    EXPECT_EQ(false, True.identical(NA));

    EXPECT_EQ(false, False.identical(True));
    EXPECT_EQ(true, False.identical(False));
    EXPECT_EQ(false, False.identical(NA));

    EXPECT_EQ(false, NA.identical(True));
    EXPECT_EQ(false, NA.identical(False));
    EXPECT_EQ(true, NA.identical(NA));
}

TEST(Logical, notWorks) {
    Logical True = true;
    Logical False = false;
    Logical NA = Logical::NA();

    EXPECT_TRUE((!True).isFalse());
    EXPECT_TRUE((!False).isTrue());
    EXPECT_TRUE((!NA).isNA());
}

TEST(Logical, orWorks) {
    Logical True = true;
    Logical False = false;
    Logical NA = Logical::NA();

    EXPECT_TRUE((True || True).isTrue());
    EXPECT_TRUE((True || False).isTrue());
    EXPECT_TRUE((True || NA).isTrue());

    EXPECT_TRUE((False || True).isTrue());
    EXPECT_TRUE((False || False).isFalse());
    EXPECT_TRUE((False || NA).isNA());

    EXPECT_TRUE((NA || True).isTrue());
    EXPECT_TRUE((NA || False).isNA());
    EXPECT_TRUE((NA || NA).isNA());
}

TEST(Logical, andWorks) {
    Logical True = true;
    Logical False = false;
    Logical NA = Logical::NA();

    EXPECT_TRUE((True && True).isTrue());
    EXPECT_TRUE((True && False).isFalse());
    EXPECT_TRUE((True && NA).isNA());

    EXPECT_TRUE((False && True).isFalse());
    EXPECT_TRUE((False && False).isFalse());
    EXPECT_TRUE((False && NA).isFalse());

    EXPECT_TRUE((NA && True).isNA());
    EXPECT_TRUE((NA && False).isFalse());
    EXPECT_TRUE((NA && NA).isNA());
}

TEST(Logical, convertToIntWorks) {
    Logical True = true;
    Logical False = false;
    Logical NA = Logical::NA();

    EXPECT_EQ(1, static_cast<int>(True));
    EXPECT_EQ(0, static_cast<int>(False));
    EXPECT_EQ(NA_INTEGER, static_cast<int>(NA));
}

TEST(Logical, convertToDoubleWorks) {
    Logical True = true;
    Logical False = false;
    Logical NA = Logical::NA();

    EXPECT_EQ(1., static_cast<double>(True));
    EXPECT_EQ(0., static_cast<double>(False));
    EXPECT_TRUE(R_IsNA(static_cast<double>(NA)));
}
