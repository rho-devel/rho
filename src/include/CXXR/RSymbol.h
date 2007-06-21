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

#ifdef __cplusplus

#ifdef USE_RINTERNALS

/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)
#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#define SET_DDVAL_BIT(x) (((x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) (((x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) ((v) ? SET_DDVAL_BIT(x) : UNSET_DDVAL_BIT(x)) /* for ..1, ..2 etc */

#endif // USE_RINTERNALS

#endif /* __cplusplus */

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

/* Symbol Access Functions */

/**
 * Symbol name.
 * @param x Pointer to an \c RSymbol.
 * @return Pointer to \c RObject representings \a x's name.
 */
SEXP (PRINTNAME)(SEXP x);


/**
 * Symbol value.
 * @param x Pointer to an \c RSymbol.
 * @return Pointer to \c RObject representings \a x's value.
 */
SEXP (SYMVALUE)(SEXP x);

/**
 * Internal function value.
 * @param x Pointer to an \c RSymbol.
 * @return ? If \a x represents and internal function, the corresponding
 * \c RObject, otherwise NULL..
 */
SEXP (INTERNAL)(SEXP x);

/**
 * Did symbol arise from ... expression?
 * @param x Pointer to an \c RSymbol.
 * @return \c true iff this symbol arose from a ... expression.
 */
Rboolean (DDVAL)(const SEXP x);

/**
 * @deprecated Ought to be private.
 */
void (SET_DDVAL)(SEXP x, int v);

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
