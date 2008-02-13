/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008 Andrew Runnalls.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file ExpressionVector.cpp
 *
 * Implementation of class ExpressionVector and related functions.
 */

#include "CXXR/ExpressionVector.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace {
    Rboolean (*isExpressionptr)(SEXP s) = Rf_isExpression;
    SEXP (*SET_XVECTOR_ELTp)(SEXP x, int i, SEXP v) = SET_XVECTOR_ELT;
    SEXP (*XVECTOR_ELTp)(const SEXP x, int i) = XVECTOR_ELT;
}
