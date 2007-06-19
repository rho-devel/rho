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

/** @file REnvironment.h
 * The future REnvironment class.
 */

#ifndef RENVIRONMENT_H
#define RENVIRONMENT_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus

#ifdef USE_RINTERNALS

#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define HASHTAB(x)	((x)->u.envsxp.hashtab)
#define ENVFLAGS(x)	((x)->sxpinfo.gp)	/* for environments */
#define SET_ENVFLAGS(x,v)	(((x)->sxpinfo.gp)=(v))

#endif // USE_RINTERNALS

#endif /* __cplusplus */

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

/* Environment Access Functions */

/**
 * @param x Pointer to an \c REnvironment.
 * @return Pointer to the frame of \a x .
 */
SEXP (FRAME)(SEXP x);

/**
 * @param x Pointer to an \c REnvironment.
 * @return Pointer to \a x 's enclosing environment.
 */
SEXP (ENCLOS)(SEXP x);

/**
 * @param x Pointer to an \c REnvironment.
 * @return Pointer to \a x 's hash table (may be NULL).
 */
SEXP (HASHTAB)(SEXP x);

/**
 * @param x Pointer to an \c REnvironment.
 * @return \a x 's environment flags.
 * @deprecated
 */
int  (ENVFLAGS)(SEXP x);

/**
 * Set environment flags.
 * @param x Pointer to an \c REnvironment.
 * @param v The new flags.
 * @deprecated
 */
void (SET_ENVFLAGS)(SEXP x, int v);

/**
 * Set environment's frame.
 * @param x Pointer to an \c REnvironment.
 * @param v Pointer to the new frame.
 * @todo Probably should be private.
 */
void SET_FRAME(SEXP x, SEXP v);

/**
 * Set environment's enclosing environment.
 * @param x Pointer to an \c REnvironment.
 * @param v Pointer to the new enclosing environment.
 * @todo Probably should be private.
 */
void SET_ENCLOS(SEXP x, SEXP v);

/**
 * Set environment's hash table.
 * @param x Pointer to an \c REnvironment.
 * @param v Pointer to the hash table.
 * @todo Probably should be private.
 */
void SET_HASHTAB(SEXP x, SEXP v);

#ifdef __cplusplus
}
#endif

#endif /* RENVIRONMENT_H */
