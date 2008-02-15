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

/** @file Promise.h
 * @brief C interface associated with the future Promise class.
 */

#ifndef RPROMISE_H
#define RPROMISE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Accessor functions. */

/* Promise Access Functions */

/**
 * @param x Pointer to a promise.
 * @return Pointer to the expression to be evaluated.
 */
#ifndef __cplusplus
SEXP PRCODE(SEXP x);
#else
inline SEXP PRCODE(SEXP x) {return x->u.promsxp.expr;}
#endif

/**
 * @param x Pointer to a promise.
 * @return Pointer to the environment in which the expression is to be
 *         evaluated.  Set NULL when the promise has been evaluated.
 */
#ifndef __cplusplus
SEXP PRENV(SEXP x);
#else
inline SEXP PRENV(SEXP x) {return x->u.promsxp.env;}
#endif

/**
 * @param x Pointer to a promise.
 * @return Pointer to the value of the expression (once evaluated?).
 */
#ifndef __cplusplus
SEXP PRVALUE(SEXP x);
#else
inline SEXP PRVALUE(SEXP x) {return x->u.promsxp.value;}
#endif

/**
 * @param x Pointer to a promise.
 * @return ?
 * @deprecated Will need to be fixed.
 */
#ifndef __cplusplus
int PRSEEN(SEXP x);
#else
inline int PRSEEN(SEXP x) {return x->m_gpbits;}
#endif

/**
 * @param x Pointer to a promise.
 * @deprecated Will need to be fixed.
 */
#ifndef __cplusplus
void SET_PRSEEN(SEXP x, int v);
#else
inline void SET_PRSEEN(SEXP x, int v) {x->m_gpbits = v;}
#endif

/**
 * Set environment
 * @param x Pointer to a promise.
 * @param v Pointer to the environment in which the expression is to
 *          be evaluated.
 * @todo Probably ought to be private or done in the constructor.
 */
void SET_PRENV(SEXP x, SEXP v);

/**
 * Set value of promise.
 * @param x Pointer to a promise.
 * @param v Pointer to the value to be assigned to the promise.
 * @todo Probably ought to be private.
 */
void SET_PRVALUE(SEXP x, SEXP v);

/**
 * Set expression
 * @param x Pointer to a promise.
 * @param v Pointer to the expression to be associated with the promise.
 * @todo Probably ought to be private or done in the constructor.
 */
void SET_PRCODE(SEXP x, SEXP v);

#ifdef __cplusplus
}
#endif

#endif /* RPROMISE_H */
