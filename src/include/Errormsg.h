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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *                2000-8      the R Core Team
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

#ifndef ERRORMSG_H
#define ERRORMSG_H

/* Used in ../main/sub*.c */

#ifdef __cplusplus
extern "C" {
#endif
#define R_MSG_subs_o_b	_("subscript out of bounds")
#define R_MSG_ob_nonsub _("object of type '%s' is not subsettable")

/*---- Packaged Error & Warning Messages ---- 
 *---- ================================= ----*/

/* ---> Handling & I18n 
 * via ErrorMessage() and WarningMessage() in ../../main/errors.c */

typedef enum {
    /* Argument list length and type errors */

    ERROR_NUMARGS = 1,
    ERROR_ARGTYPE = 2,
    ERROR_INCOMPAT_ARGS = 3,

    /* General type and length incompatibilities */

    ERROR_TSVEC_MISMATCH = 100,

    ERROR_UNIMPLEMENTED	= 9998,
    ERROR_UNKNOWN = 9999
} R_ERROR;


typedef enum {

    WARNING_coerce_NA	= 101,
    WARNING_coerce_INACC= 102,
    WARNING_coerce_IMAG = 103,

    WARNING_UNKNOWN = 9999
} R_WARNING;

#ifdef __cplusplus
}
#endif

#endif

