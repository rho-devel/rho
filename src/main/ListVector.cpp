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
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file ListVector.cpp
 *
 * Implementation of class ListVector and related functions.
 *
 * @todo Tidy up handling of names attribute, in particular to get rid
 * of <tt>const_cast</tt>.
 */

#include "CXXR/ListVector.h"

#include "Rinternals.h"
#include "CXXR/ExpressionVector.h"

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace {
    SEXP (*SET_VECTOR_ELTp)(SEXP x, int i, SEXP v) = SET_VECTOR_ELT;
    SEXP (*VECTOR_ELTp)(const SEXP x, int i) = VECTOR_ELT;
}

ListVector::ListVector(const ExpressionVector& ev)
    : EdgeVector<RObject*, VECSXP>(ev.size())
{
    for (unsigned int i = 0; i < size(); ++i)
	(*this)[i] = ev[i];
    SEXP names = Rf_getAttrib(const_cast<ExpressionVector*>(&ev),
			      R_NamesSymbol);
    if (names)
	Rf_setAttrib(this, R_NamesSymbol, names);
}
