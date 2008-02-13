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

/** @file RClosure.h
 * The future RClosure class.
 */

#ifndef RCLOSURE_H
#define RCLOSURE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Accessor functions.*/

/* Closure Access Functions */

/**
 * @param x Pointer a closure object.
 * @return Pointer to the body of x.
 */
#ifndef __cplusplus
SEXP BODY(SEXP x);
#else
inline SEXP BODY(SEXP x) {return x->u.closxp.body;}
#endif

/**
 * @param x Pointer a closure object.
 * @return Pointer to the environment of x.
 */
#ifndef __cplusplus
SEXP CLOENV(SEXP x);
#else
inline SEXP CLOENV(SEXP x) {return x->u.closxp.env;}
#endif

/**
 * @param x Pointer a closure object.
 * @return \c true if debugging is set, i.e. evaluations of the
 *         function should run under the browser.
 */
#ifndef __cplusplus
Rboolean DEBUG(SEXP x);
#else
inline Rboolean DEBUG(SEXP x) {return Rboolean(x->m_debug);}
#endif

/**
 * @param x Pointer a closure object.
 * @return Pointer to the formals list of x.
 */
#ifndef __cplusplus
SEXP FORMALS(SEXP x);
#else
inline SEXP FORMALS(SEXP x) {return x->u.closxp.formals;}
#endif

/**
 * Set the debugging state of a closure object.
 * @param x Pointer a closure object.
 * @param v The new debugging state.
 */
#ifndef __cplusplus
void SET_DEBUG(SEXP x, Rboolean v);
#else
inline void SET_DEBUG(SEXP x, Rboolean v) {x->m_debug = v;}
#endif

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
