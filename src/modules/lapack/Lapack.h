/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/* C declarations of principal Lapack routines,
 * i.e.,
 * those with a .Call() interface defined via ./Lapack.c
 * (and half a dozen more declaration/interface/registration files
 */

#include <R_ext/Complex.h>
#include <R_ext/RS.h>

/* use declarations in R_ext/Lapack.h (instead of having them there *and* here)
  but ``delete'' the 'extern' there : */
#define La_extern
#define BLAS_extern

#include <R_ext/Lapack.h>

#undef La_extern
#undef BLAS_extern
