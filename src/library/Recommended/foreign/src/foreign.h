/*
 *
 *  Common header file for the foreign package for R
 *
 *  Copyright 2000-2000 Saikat DebRoy <saikat@stat.wisc.edu>
 *                      Douglas M. Bates <bates@stat.wisc.edu>,
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301  USA
 *
 */

#ifndef FOREIGN_H
#define FOREIGN_H

#include <R.h>
#include <Rinternals.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("foreign", String)
#define gettext_noop(String) (String)
#else
#define _(String) (String)
#define gettext_noop(String) (String)
#endif

#define CN_TYPE_BIG     1
#define CN_TYPE_LITTLE  2
#define CN_TYPE_XPORT   3
#define CN_TYPE_IEEEB   CN_TYPE_BIG
#define CN_TYPE_IEEEL   CN_TYPE_LITTLE

#define BIG 4321
#define LITTLE 1234
#define UNKNOWN 0000

#ifdef WORDS_BIGENDIAN
# define CN_TYPE_NATIVE CN_TYPE_IEEEB
# define endian BIG
#else
# define CN_TYPE_NATIVE CN_TYPE_IEEEL
# define endian LITTLE
#endif /* not WORDS_BIGENDIAN */

typedef int R_int32;
/* typedef short int16; unused */

typedef double R_flt64;
/* typedef float flt32; unused */

#define FPREP_IEEE754 754
#define FPREP FPREP_IEEE754

#ifdef max
# undef max
#endif
#ifdef min
# undef min
#endif
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define min(a,b) ((a) <= (b) ? (a) : (b))

#endif /* FOREIGN_H */
