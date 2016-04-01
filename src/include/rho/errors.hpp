/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file errors.h
 *
 * @brief Convenience header to give rho core code access to CR error
 * handling.
 */

#ifndef RHO_ERRORS_H
#define RHO_ERRORS_H

#include "R_ext/Error.h"
#include "localization.h"
#include "rho/RObject.hpp"

extern "C" {
    void R_CheckStack(void);
    void Rf_errorcall(SEXP, const char *, ...);
    void Rf_warningcall(SEXP, const char *, ...);

    /* Find a better home for the following in due course! */
    void R_CheckUserInterrupt(void);
}

#endif /* RHO_ERRORS_H */
