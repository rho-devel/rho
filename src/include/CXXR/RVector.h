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

/** @file RVector.h
 * The future RVector class.
 */

#ifndef RVECTOR_H
#define RVECTOR_H

#include "CXXR/RObject.h"
#include "R_ext/Complex.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned char Rbyte;

#ifdef __cplusplus

struct vecsxp_struct {
    R_len_t	length;
    R_len_t	truelength;
};

/* The generational collector uses a reduced version of SEXPREC as a
   header in vector nodes.  The layout MUST be kept consistent with
   the SEXPREC definition.  The standard SEXPREC takes up 7 words on
   most hardware; this reduced version should take up only 6 words.
   In addition to slightly reducing memory use, this can lead to more
   favorable data alignment on 32-bit architectures like the Intel
   Pentium III where odd word alignment of doubles is allowed but much
   less efficient than even word alignment. */
typedef struct VECTOR_SEXPREC {
    SEXPREC_HEADER;
    struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;

typedef union { VECTOR_SEXPREC s; double align; } SEXPREC_ALIGN;

#ifdef USE_RINTERNALS

/* Vector Access Macros */
#define TRUELENGTH(x)	(reinterpret_cast<VECSEXP>(x)->vecsxp.truelength)
#define SETLENGTH(x,v)		((reinterpret_cast<VECSEXP>(x)->vecsxp.length)=(v))
#define SET_TRUELENGTH(x,v)	((reinterpret_cast<VECSEXP>(x)->vecsxp.truelength)=(v))

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define DATAPTR(x)	(reinterpret_cast<SEXPREC_ALIGN *>(x) + 1)
#define CHAR(x)		(reinterpret_cast<char *>(DATAPTR(x)))
#define LOGICAL(x)	(reinterpret_cast<int *>(DATAPTR(x)))
#define INTEGER(x)	(reinterpret_cast<int *>(DATAPTR(x)))
#define RAW(x)		(reinterpret_cast<Rbyte *>(DATAPTR(x)))
#define COMPLEX(x)	(reinterpret_cast<Rcomplex *>(DATAPTR(x)))
#define REAL(x)		(reinterpret_cast<double *>(DATAPTR(x)))
#define STRING_ELT(x,i)	(reinterpret_cast<SEXP *>(DATAPTR(x))[i])
#define VECTOR_ELT(x,i)	(reinterpret_cast<SEXP *>(DATAPTR(x))[i])
#define STRING_PTR(x)	(reinterpret_cast<SEXP *>(DATAPTR(x)))
#define VECTOR_PTR(x)	(reinterpret_cast<SEXP *>(DATAPTR(x)))

/* CHARSXP charset bits */

# define LATIN1_MASK (1<<2)
# define IS_LATIN1(x) ((x)->sxpinfo.gp & LATIN1_MASK)
# define SET_LATIN1(x) (((x)->sxpinfo.gp) |= LATIN1_MASK)
# define UNSET_LATIN1(x) (((x)->sxpinfo.gp) &= ~LATIN1_MASK)
# define UTF8_MASK (1<<3)
# define IS_UTF8(x) ((x)->sxpinfo.gp & UTF8_MASK)
# define SET_UTF8(x) (((x)->sxpinfo.gp) |= UTF8_MASK)
# define UNSET_UTF8(x) (((x)->sxpinfo.gp) &= ~UTF8_MASK)

#endif // USE_RINTERNALS

#endif /* __cplusplus */

#ifndef USE_RINTERNALS
#define CHAR(x)         R_CHAR(x)
#endif

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

/* Vector Access Functions */

/**
 * @param x Pointer to an \c RVector .
 *
 * @return The length of \a x, or 0 if \a x is a null pointer.  (In
 *         the case of certain hash tables, this means the 'capacity'
 *         of \a x , not all of which may be used.)
 */
#ifndef __cplusplus
int LENGTH(SEXP x);
#else
inline int LENGTH(SEXP x)
{
    return x ? reinterpret_cast<VECSEXP>(x)->vecsxp.length : 0;
}
#endif

/**
 * @param x Pointer to an \c RVector .
 * @return The 'true length' of \a x.  According to the R Internals
 *         document for R 2.4.1, this is only used for certain hash
 *         tables, and signifies the number of used slots in the
 *         table.
 */
int  (TRUELENGTH)(SEXP x);

/**
 * Set length of vector.
 * @param x Pointer to an \c RVector .
 * @param v The required new length.
 */
void (SETLENGTH)(SEXP x, int v);

/**
 * Set 'true length' of vector.
 * @param x Pointer to an \c RVector .
 * @param v The required new 'true length'.
 */
void (SET_TRUELENGTH)(SEXP x, int v);

/**
 * @param x Pointer to an \c RVector .
 * @return Pointer to \a x 's data, interpreted as character data.
 */
char  *(R_CHAR)(SEXP x);

/**
 * @param x Pointer to an \c RVector representing logical data.
 * @return Pointer to \a x 's data.
 */
int *(LOGICAL)(SEXP x);

/**
 * @param x Pointer to an \c RVector .
 * @return Pointer to \a x 's data, interpreted as integer data.
 */
int  *(INTEGER)(SEXP x);

/**
 * @param x Pointer to an \c RVector .
 * @return Pointer to \a x 's data, interpreted as raw bytes.
 */
Rbyte *(RAW)(SEXP x);

/**
 * @param x Pointer to an \c RVector .
 * @return Pointer to \a x 's data, interpreted as real numbers.
 */
double *(REAL)(SEXP x);

/**
 * @param x Pointer to an \c RVector .
 * @return Pointer to \a x 's data, interpreted as complex numbers.
 */
Rcomplex *(COMPLEX)(SEXP x);

/**
 * Extract element of character string.
 * @param x Pointer to an \c RVector representing a character string.
 * @param i Index of the required element
 * @return Pointer to extracted \i 'th element.
 */
SEXP (STRING_ELT)(SEXP x, int i);

/**
 * Extract element of vector.
 * @param x Pointer to an \c RVector .
 * @param i Index of the required element
 * @return Pointer to extracted \i 'th element.
 */
SEXP (VECTOR_ELT)(SEXP x, int i);

/**
 * Set element of character string.
 * @param x Pointer to an \c RVector representing a character string.
 * @param i Index of the required element
 * @param v Pointer to \c RObject representing the new value.
 */
void SET_STRING_ELT(SEXP x, int i, SEXP v);

/**
 * Set element of vector.
 * @param x Pointer to an \c RVector .
 * @param i Index of the required element
 * @param v Pointer to \c RObject representing the new value.
 */
SEXP SET_VECTOR_ELT(SEXP x, int i, SEXP v);

/**
 * @param x Pointer to an \c RVector representing a vector of string
 *          objects.
 * @return Pointer to the start of \a x 's data, thus interpreted.
 */
SEXP *(STRING_PTR)(SEXP x);

/**
 * @param x Pointer to an \c RVector representing a vector of vector
 *          objects.
 * @return Pointer to the start of \a x 's data, thus interpreted.
 */
SEXP *(VECTOR_PTR)(SEXP x);

/**
 * @param x Pointer to an \c RVector representing a character string.
 * @return true iff \a x is marked as having LATIN1 encoding.
 */
Rboolean (IS_LATIN1)(const SEXP x);

/**
 * @brief Set LATIN1 encoding.
 * @param x Pointer to an \c RVector representing a character string.
 */
void (SET_LATIN1)(SEXP x);

/**
 * @brief Unset LATIN1 encoding.
 * @param x Pointer to an \c RVector representing a character string.
 */
void (UNSET_LATIN1)(SEXP x);

/**
 * @param x Pointer to an \c RVector representing a character string.
 * @return true iff \a x is marked as having UTF8 encoding.
 */
Rboolean (IS_UTF8)(const SEXP x);

/**
 * @brief Set UTF8 encoding.
 * @param x Pointer to an \c RVector representing a character string.
 */
void (SET_UTF8)(SEXP x);

/**
 * @brief Unset UTF8 encoding.
 * @param x Pointer to an \c RVector representing a character string.
 */
void (UNSET_UTF8)(SEXP x);

/**
 * @brief Create a vector object.
 *
 *  Allocate a vector object.  This ensures only validity of list-like
 *  SEXPTYPES (as the elements must be initialized).  Initializing of
 *  other vector types is done in do_makevector.
 * @param stype The type of vector required.
 * @param length The length of the vector to be created.
 * @return Pointer to the created vector.
 */
SEXP allocVector(SEXPTYPE stype, R_len_t length);

#define allocString(n)		allocVector(CHARSXP, n)

#ifdef __cplusplus
}
#endif

#endif /* RVECTOR_H */
