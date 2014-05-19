/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2009-2012 The R Core Team.
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* This is an experimental facility for printing low-level information
   about R objects. It is not intended to be exposed at the top level
   but rather used as a debugging/inspection facility. It is not
   necessarily complete - feel free to add missing pieces. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <R_ext/Print.h>

// This is currently a no-op in CXXR (and quite likely always will be):
/* pre is the prefix, v is the object to inspect, deep specifies
   the recursion behavior (0 = no recursion, -1 = [sort of] unlimited
   recursion, positive numbers define the maximum recursion depth)
   and pvec is the max. number of vector elements to show  */
static void inspect_tree(int pre, SEXP v, int deep, int pvec) {
}

/* internal API - takes one mandatory argument (object to inspect) and
   two optional arguments (deep and pvec - see above), positional argument
   matching only */
SEXP attribute_hidden do_inspect(SEXP call, SEXP op, SEXP args, SEXP env) {
    SEXP obj = CAR(args);
    int deep = -1;
    int pvec = 5;
    if (CDR(args) != R_NilValue) {
	deep = asInteger(CADR(args));
	if (CDDR(args) != R_NilValue)
	    pvec = asInteger(CADDR(args));
    }
	
    inspect_tree(0, CAR(args), deep, pvec);
    return obj;
}

/* the following functions can be use internally and for debugging purposes -
   so far they are not used in any actual code */
SEXP attribute_hidden R_inspect(SEXP x) {
    inspect_tree(0, x, -1, 5);
    return x;
}

SEXP attribute_hidden R_inspect3(SEXP x, int deep, int pvec) {
    inspect_tree(0, x, deep, pvec);
    return x;
}
