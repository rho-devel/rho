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

/** @file REnvironment.cpp
 *
 * At present, this file simply forces the generation of non-inlined
 * versions of inlined functions declared in REnvironment.h where
 * these are intended to be callable from C.  It is also used to check
 * that REnvironment.h is self-contained, i.e. #includes anything it
 * needs, and doesn't rely on anything having been previously
 * #included in the enclosing source file.
 */

#include "CXXR/REnvironment.h"

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace {
    SEXP (*enclosp)(SEXP x) = ENCLOS;
    int (*envflagsp)(SEXP x) = ENVFLAGS;
    SEXP (*hashtabp)(SEXP x) = HASHTAB;
    Rboolean (*isEnvironmentptr)(SEXP s) = Rf_isEnvironment;
    SEXP (*framep)(SEXP x) = FRAME;
    void (*setenvflagsp)(SEXP x, int v) = SET_ENVFLAGS;
}
