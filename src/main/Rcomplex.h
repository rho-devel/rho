/* $Id$ */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000-6       	    The R Development Core Team.
 *  Copyright (C) 2005		    The R Foundation
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* Prototypes of functions within complex.c needed elsewhere. */

#ifndef RCOMPLEX_H
#define RCOMPLEX_H 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>		/* -> ../include/R_ext/Complex.h */

#ifdef __cplusplus
extern "C" {
#endif

void attribute_hidden z_prec_r(Rcomplex *r, Rcomplex *x, double digits);

#ifdef __cplusplus
}
#endif

#endif /* RCOMPLEX_H */
