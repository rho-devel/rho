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

/** @file RClosure.h
 * The future RClosure class.
 */

#ifndef RCLOSURE_H
#define RCLOSURE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus

#ifdef USE_RINTERNALS

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.formals)
#define BODY(x)		((x)->u.closxp.body)
#define CLOENV(x)	((x)->u.closxp.env)
#define DEBUG(x)	((x)->sxpinfo.debug)
#define SET_DEBUG(x,v)	(((x)->sxpinfo.debug)=(v))

#endif // USE_RINTERNALS

#endif /* __cplusplus */

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

/* Closure Access Functions */

/**
 * @param x Pointer a closure object.
 * @return Pointer to the formals list of x.
 */
SEXP (FORMALS)(SEXP x);

/**
 * @param x Pointer a closure object.
 * @return Pointer to the body of x.
 */
SEXP (BODY)(SEXP x);

/**
 * @param x Pointer a closure object.
 * @return Pointer to the environment of x.
 */
SEXP (CLOENV)(SEXP x);

/**
 * @param x Pointer a closure object.
 * @return \c true if debugging is set, i.e. evaluations of the
 *         function should run under the browser.
 * @todo return \c Rboolean; make \a x \c const.
 */
int  (DEBUG)(SEXP x);

/**
 * @todo Used with tracemem.  Will need review.  Should it be in
 * RObject.h?
 */
int  (TRACE)(SEXP x);

/**
 * Set the debugging state of a closure object.
 * @param x Pointer a closure object.
 * @param v The new debugging state.
 * @todo Make \a v \c Rboolean.
 */
void (SET_DEBUG)(SEXP x, int v);

/**
 * @todo Used with tracemem.  Will need review.  Should it be in
 * RObject.h?
 */
void (SET_TRACE)(SEXP x, int v);

/**
 * Set the formals of a closure object.
 * @param x Pointer a closure object.
 * @return Pointer to the new formals list of x.
 */
void SET_FORMALS(SEXP x, SEXP v);

/**
 * Set the body of a closure object.
 * @param x Pointer a closure object.
 * @return Pointer to the new body of x.
 */
void SET_BODY(SEXP x, SEXP v);

/**
 * Set the environment of a closure object.
 * @param x Pointer a closure object.
 * @return Pointer to the new environment of x.
 */
void SET_CLOENV(SEXP x, SEXP v);

#ifdef __cplusplus
}
#endif

#endif /* RCLOSURE_H */
