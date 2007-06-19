/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2007
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file RInternalFunction.h
 * The future RInternalFunction class.
 */

#ifndef RINTERNALFUNCTION_H
#define RINTERNALFUNCTION_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus

#ifdef USE_RINTERNALS

/* Primitive Access Macros */
#define PRIMOFFSET(x)           ((x)->u.primsxp.offset)
#define SET_PRIMOFFSET(x,v)	(((x)->u.primsxp.offset)=(v))

#endif // USE_RINTERNALS

#endif /* __cplusplus */

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

// Primitive access functions:

/**
 * @param x Pointer to a RInternalFunction.
 * @return The offset of this function within the function table.
 * @todo Ought to be private.
 */
int (PRIMOFFSET)(SEXP x);

/**
 * Set the object's table offset.
 * @param x Pointer to a RInternalFunction.
 * @param v The required offset value.
 * @todo Ought to be private.
 */
void (SET_PRIMOFFSET)(SEXP x, int v);

#ifdef __cplusplus
}
#endif

#endif /* RINTERNALFUNCTION_H */
