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
#include "Rembedded.h"
#include "CXXR/Evaluator.h"

int main(int argc, char**argv)
{
 ::testing::InitGoogleTest(&argc, argv);

  const char *r_args[] = { "R", "--vanilla", "--slave" };

  // TODO(kmillar): remove the need for an evaluator here.  It breaks all
  //  existing code that uses Rf_initEmbeddedR.
  CXXR::Evaluator evaluator;
  Rf_initEmbeddedR(3, const_cast<char **>(r_args));

  return RUN_ALL_TESTS();
}
