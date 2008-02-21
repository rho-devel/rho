/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007 Andrew Runnalls.
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

/** @file VectorBase.cpp
 *
 * @brief Implementation of class VectorBase and related functions.
 */

#include "R_ext/Error.h"
#include "CXXR/VectorBase.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	int (*LENGTHptr)(SEXP x) = LENGTH;
	void (*SET_TRUELENGTHptr)(SEXP x, int v) = SET_TRUELENGTH;
	int (*TRUELENGTHptr)(SEXP x) = TRUELENGTH;
    }
}

void VectorBase::resize(size_t new_size)
{
    if (new_size > m_size)
	error("VectorBase::resize() : requested size exceeds current size.");
    m_size = new_size;
}

// Rf_allocVector is still in memory.cpp (for the time being).

void SETLENGTH(SEXP x, int v)
{
    CXXR::VectorBase* vb = dynamic_cast<CXXR::VectorBase*>(x);
    if (!vb) error("SETLENGTH invoked for a non-vector.");
    vb->resize(v);
}
