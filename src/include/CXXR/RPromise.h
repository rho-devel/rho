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

/** @file RPromise.h
 * The future RPromise class.
 */

#ifndef RPROMISE_H
#define RPROMISE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus

#ifdef USE_RINTERNALS

/* Promise Access Macros */
#define PRCODE(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRVALUE(x)	((x)->u.promsxp.value)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))

#endif // USE_RINTERNALS

#endif /* __cplusplus */

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

/* Promise Access Functions */

/**
 * @param x Pointer to a promise.
 * @return Pointer to the expression to be evaluated.
 */
SEXP (PRCODE)(SEXP x);

/**
 * @param x Pointer to a promise.
 * @return Pointer to the environment in which the expression is to be
 *         evaluated.  Set NULL when the promise has been evaluated.
 */
SEXP (PRENV)(SEXP x);

/**
 * @param x Pointer to a promise.
 * @return Pointer to the value of the expression (once evaluated?).
 */
SEXP (PRVALUE)(SEXP x);

/**
 * @param x Pointer to a promise.
 * @return ?
 * @deprecated Will need to be fixed.
 */
int  (PRSEEN)(SEXP x);

/**
 * @param x Pointer to a promise.
 * @deprecated Will need to be fixed.
 */
void (SET_PRSEEN)(SEXP x, int v);

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
