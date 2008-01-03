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

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is an environment.
 */
#ifndef __cplusplus
Rboolean Rf_isEnvironment(SEXP s);
#else
inline Rboolean Rf_isEnvironment(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == ENVSXP);
}
#endif

/* Accessor functions. */

/* Environment Access Functions */

/**
 * @param x Pointer to an \c REnvironment.
 * @return Pointer to the frame of \a x .
 */
#ifndef __cplusplus
SEXP FRAME(SEXP x);
#else
inline SEXP FRAME(SEXP x) {return x->u.envsxp.frame;}
#endif

/**
 * @param x Pointer to an \c REnvironment.
 * @return Pointer to \a x 's enclosing environment.
 */
#ifndef __cplusplus
SEXP ENCLOS(SEXP x);
#else
inline SEXP ENCLOS(SEXP x) {return x->u.envsxp.enclos;}
#endif

/**
 * @param x Pointer to an \c REnvironment.
 * @return Pointer to \a x 's hash table (may be NULL).
 */
#ifndef __cplusplus
SEXP HASHTAB(SEXP x);
#else
inline SEXP HASHTAB(SEXP x) {return x->u.envsxp.hashtab;}
#endif

/**
 * @param x Pointer to an \c REnvironment.
 * @return \a x 's environment flags.
 * @deprecated
 */
#ifndef __cplusplus
int ENVFLAGS(SEXP x);
#else
inline int ENVFLAGS(SEXP x) {return x->m_gpbits;}
#endif

/**
 * Set environment flags.
 * @param x Pointer to an \c REnvironment.
 * @param v The new flags.
 * @deprecated
 */
#ifndef __cplusplus
void SET_ENVFLAGS(SEXP x, int v);
#else
inline void SET_ENVFLAGS(SEXP x, int v) {x->m_gpbits = v;}
#endif

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
