/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
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

#ifndef RHO_TESTS_RHO_TEST_HELPERS_HPP
#define RHO_TESTS_RHO_TEST_HELPERS_HPP

#include "gtest/gtest.h"
#include "rho/RObject.hpp"
#include "Rinternals.h"

#define EXPECT_IDENTICAL(x, y) EXPECT_PRED3(R_compute_identical, (x), (y), 0)

namespace rho {
inline std::ostream& operator<<(std::ostream& os, const RObject* object) {
    // TODO(kmillar): output to *os instead.
    Rf_PrintValue(const_cast<rho::RObject*>(object));
    return os;
}
  
// This class exists purely to provide access to private members of the GCNode  
// class for testing purposes.
class GCTestHelper {
public:
    static unsigned char getRefCount(const GCNode* node) {
	return node->getRefCount();
    }

    static bool isOnStackBitSet(const GCNode* node) {
	return node->isOnStackBitSet();
    }
};

inline unsigned char getRefCount(const GCNode* node) {
    return GCTestHelper::getRefCount(node);
}

inline bool isOnStackBitSet(const GCNode* node) {
    return GCTestHelper::isOnStackBitSet(node);
}

}  // namespace rho

#endif  // RHO_TESTS_RHO_TEST_HELPERS_HPP
