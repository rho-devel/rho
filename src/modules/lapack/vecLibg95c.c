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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <AvailabilityMacros.h> /* for MAC_OS_X_VERSION_10_* -- present on 10.2+ (according to Apple) */
/* Since OS X 10.8 vecLib requires Accelerate to be included first (which in turn includes vecLib) */
#if defined MAC_OS_X_VERSION_10_8 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1040
#include <Accelerate/Accelerate.h>
#else
#include <vecLib/vecLib.h>
#endif

void F77_FUNC_(rcblas_cdotu_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotu) 
{ cblas_cdotu_sub(*N, X, *incX, Y, *incY, dotu); }

void F77_FUNC_(rcblas_cdotc_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotc)
{ cblas_cdotc_sub(*N, X, *incX, Y, *incY, dotc); }

void F77_FUNC_(rcblas_zdotu_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotu)
{ cblas_zdotu_sub(*N, X, *incX, Y, *incY, dotu); }

void F77_FUNC_(rcblas_zdotc_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotc)
{ cblas_zdotc_sub(*N, X, *incX, Y, *incY, dotc); }
