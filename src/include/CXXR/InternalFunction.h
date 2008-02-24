/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2007
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

/** @file InternalFunction.h
 * @brief C interface associated with the future InternalFunction class.
 */

#ifndef RINTERNALFUNCTION_H
#define RINTERNALFUNCTION_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Accessor functions.*/

// Primitive access functions:

/**
 * @param x Pointer to a RInternalFunction.
 * @return The offset of this function within the function table.
 * @todo Ought to be private.
 */
#ifndef __cplusplus
int PRIMOFFSET(SEXP x);
#else
inline int PRIMOFFSET(SEXP x) {return x->u.primsxp.offset;}
#endif

/**
 * Set the object's table offset.
 * @param x Pointer to a RInternalFunction.
 * @param v The required offset value.
 * @todo Ought to be private.
 */
#ifndef __cplusplus
void SET_PRIMOFFSET(SEXP x, int v);
#else
inline void SET_PRIMOFFSET(SEXP x, int v) {x->u.primsxp.offset = v;}
#endif

#ifdef __cplusplus
}
#endif

#endif /* RINTERNALFUNCTION_H */
