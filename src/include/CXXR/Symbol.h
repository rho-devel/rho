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

/** @file RSymbol.h
 * The future RSymbol class.
 */

#ifndef RSYMBOL_H
#define RSYMBOL_H

#include "CXXR/RObject.h"

#ifdef __cplusplus
extern "C" {
#endif

#define DDVAL_MASK	1

/* Accessor functions. */

/* Symbol Access Functions */

/**
 * Symbol name.
 * @param x Pointer to an \c RSymbol.
 * @return Pointer to \c RObject representings \a x's name.
 */
#ifndef __cplusplus
SEXP PRINTNAME(SEXP x);
#else
inline SEXP PRINTNAME(SEXP x) {return x->u.symsxp.pname;}
#endif

/**
 * Symbol value.
 * @param x Pointer to an \c RSymbol.
 * @return Pointer to \c RObject representings \a x's value.
 */
#ifndef __cplusplus
SEXP SYMVALUE(SEXP x);
#else
inline SEXP SYMVALUE(SEXP x) {return x->u.symsxp.value;}
#endif

/**
 * Internal function value.
 * @param x Pointer to an \c RSymbol.
 * @return ? If \a x represents and internal function, the corresponding
 * \c RObject, otherwise NULL..
 */
#ifndef __cplusplus
SEXP INTERNAL(SEXP x);
#else
inline SEXP INTERNAL(SEXP x) {return x->u.symsxp.internal;}
#endif

/**
 * Did symbol arise from ... expression?
 * @param x Pointer to an \c RSymbol.
 * @return \c true iff this symbol arose from a ... expression.
 */
#ifndef __cplusplus
Rboolean DDVAL(const SEXP x);
#else
inline Rboolean DDVAL(const SEXP x)
{
    return Rboolean(x->m_gpbits & DDVAL_MASK);
}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_DDVAL(SEXP x, int v);
#else
inline void SET_DDVAL(SEXP x, int v)
{
    if (v) x->m_gpbits |= DDVAL_MASK;
    else x->m_gpbits &= ~DDVAL_MASK;
}
#endif

/**
 * Set symbol's name.
 * @param x Pointer to an \c RSymbol.
 * @param v Pointer to an \c RObject representing the new name.
 */
void SET_PRINTNAME(SEXP x, SEXP v);

/**
 * Set symbol's value.
 * @param x Pointer to an \c RSymbol.
 * @param v Pointer to an \c RObject representing the new value.
 */
void SET_SYMVALUE(SEXP x, SEXP v);

/**
 * Set internal function.
 * @param x Pointer to an \c RSymbol.
 * @param v Pointer to an \c RObject representing an internal function.
 */
void SET_INTERNAL(SEXP x, SEXP v);

#ifdef __cplusplus
}
#endif

#endif /* RSYMBOL_H */
