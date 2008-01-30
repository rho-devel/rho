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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file VectorBase.cpp
 *
 * Implementation of class VectorBase and related functions.
 */

#include "CXXR/VectorBase.h"

namespace {
    int (*lengthptr)(SEXP x) = LENGTH;
    void (*setlengthptr)(SEXP x, int v) = SETLENGTH;
    void (*settruelengthptr)(SEXP x, int v) = SET_TRUELENGTH;
    SEXP* (*stringptrptr)(SEXP x) = STRING_PTR;
    int (*truelengthptr)(SEXP x) = TRUELENGTH;
}

// Rf_allocVector is still in memory.cpp (for the time being).
