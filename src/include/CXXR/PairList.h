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

/** @file PairList.h
 * @brief C interface associated with the future PairList class.
 */

#ifndef RPAIRLIST_H
#define RPAIRLIST_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Accessor functions. */

/**
 * @param e Pointer to a list.
 * @return Pointer to the value of the list head, or 0 if \a e is
 * a null pointer.
 */
#ifndef __cplusplus
SEXP CAR(SEXP e);
#else
inline SEXP CAR(SEXP e)  {return e ? e->u.listsxp.carval : 0;}
#endif

/**
 * @param e Pointer to a list.
 * @return Pointer to the tail of the list, or 0 if \a e is
 * a null pointer.
 */
#ifndef __cplusplus
SEXP CDR(SEXP e);
#else
inline SEXP CDR(SEXP e) {return e ? e->u.listsxp.cdrval : 0;}
#endif

/**
 * Equivalent to CAR(CAR(e)).
 */
#ifndef __cplusplus
SEXP CAAR(SEXP e);
#else
inline SEXP CAAR(SEXP e) {return CAR(CAR(e));}
#endif

/**
 * Equivalent to CDR(CAR(e)).
 */
#ifndef __cplusplus
SEXP CDAR(SEXP e);
#else
inline SEXP CDAR(SEXP e) {return CDR(CAR(e));}
#endif

/**
 * Equivalent to CAR(CDR(e)).
 */
#ifndef __cplusplus
SEXP CADR(SEXP e);
#else
inline SEXP CADR(SEXP e) {return CAR(CDR(e));}
#endif

/**
 * Equivalent to CDR(CDR(e)).
 */
#ifndef __cplusplus
SEXP CDDR(SEXP e);
#else
inline SEXP CDDR(SEXP e) {return CDR(CDR(e));}
#endif

/**
 * Equivalent to CAR(CDR(CDR(e))).
 */
#ifndef __cplusplus
SEXP CADDR(SEXP e);
#else
inline SEXP CADDR(SEXP e) {return CAR(CDR(CDR(e)));}
#endif

/**
 * Equivalent to CAR(CDR(CDR(CDR(e)))).
 */
#ifndef __cplusplus
SEXP CADDDR(SEXP e);
#else
inline SEXP CADDDR(SEXP e) {return CAR(CDR(CDR(CDR(e))));}
#endif

/**
 * Equivalent to CAR(CDR(CDR(CDR(CDR(e))))).
 */
#ifndef __cplusplus
SEXP CAD4R(SEXP e);
#else
inline SEXP CAD4R(SEXP e) {return CAR(CDR(CDR(CDR(CDR(e)))));}
#endif

#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */

#ifndef __cplusplus
int MISSING(SEXP x);
#else
inline int MISSING(SEXP x) {return x->m_gpbits & MISSING_MASK;}
#endif

/**
 * @param e Pointer to a list.
 * @return Pointer to the tag (key) of the list head, or 0 if \a e is
 * a null pointer.
 */
#ifndef __cplusplus
SEXP TAG(SEXP e);
#else
inline SEXP TAG(SEXP e) {return e ? e->u.listsxp.tagval : 0;}
#endif

#ifndef __cplusplus
void SET_MISSING(SEXP x, int v);
#else
inline void SET_MISSING(SEXP x, int v)
{
    int other_flags = x->m_gpbits & ~MISSING_MASK;
    x->m_gpbits = other_flags | v;
}
#endif

/**
 * Set the tag of a list element.
 * @param x Pointer to a list.
 * @param y Pointer an \c RObject representing the new tag of the list head..
 */
void SET_TAG(SEXP x, SEXP y);

/**
 * Set the value of the first element of list.
 * @param x Pointer to a list.
 * @param y Pointer an \c RObject representing the new value of the
 *          list head.
 */
SEXP SETCAR(SEXP x, SEXP y);

/**
 * Replace the tail of a list.
 * @param x Pointer to a list.
 * @param y Pointer an \c RObject representing the new tail of the list.
 */
SEXP SETCDR(SEXP x, SEXP y);

/**
 * Set the value of the second element of list.
 * @param x Pointer to a list.
 * @param y Pointer an \c RObject representing the new value of the
 *          second element of the list.
 */
SEXP SETCADR(SEXP x, SEXP y);

/**
 * Set the value of the third element of list.
 * @param x Pointer to a list.
 * @param y Pointer an \c RObject representing the new value of the
 *          third element of the list.
 */
SEXP SETCADDR(SEXP x, SEXP y);

/**
 * Set the value of the fourth element of list.
 * @param x Pointer to a list.
 * @param y Pointer an \c RObject representing the new value of the
 *          fourth element of the list.
 */
SEXP SETCADDDR(SEXP x, SEXP y);

/**
 * Set the value of the fifth element of list.
 * @param x Pointer to a list.
 * @param y Pointer an \c RObject representing the new value of the
 *          fifth element of the list.
 */
SEXP SETCAD4R(SEXP e, SEXP y);

/**
 * @brief Create a list of NULL values.
 * @param n Number of elements required in the list.
 * @return Pointer to the created list.
 * @todo Make \a n unsigned
 */
SEXP Rf_allocList(unsigned int n);

/**
 * @brief Create an RObject...of listlike type
 *
 * Despite the general name, the code (in memory.cpp) seems to assume
 * that the resulting object with have CAR, CDR, TAG etc.
 *
 * @param t The stype of the required object.
 * @return Pointer to the created object.
 */
SEXP Rf_allocSExp(SEXPTYPE t);

#ifdef __cplusplus
}
#endif

#endif /* RPAIRLIST_H */
