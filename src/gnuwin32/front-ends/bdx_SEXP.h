/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*******************************************************************************
 *  BDX: Binary Data eXchange format library
 *  Copyright (C) 1999-2006 Thomas Baier
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 * 
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 *  Conversion functions from SEXP to BDX and vice versa.
 *
 ******************************************************************************/

#ifndef _BDX_SEXP_H_
#define _BDX_SEXP_H_

#ifdef __cplusplus
extern "C" {
#endif

/* forward declaration */
struct SEXPREC;
struct _BDX_Data;

/*
 * BDX conversion functions:
 *
 *   These functions are used to convert to/from the BDX data format
 *   On success, the functions return 0, on error a negative number
 *
 * return codes:
 *
 *   0 ... success
 *  -1 ... BDX version mismatch
 *  -2 ... unsupported data types
 *  -3 ... error accessing memory/data
 *  -4 ... invalid argument to function
 *  -5 ... error coercing/marshalling data
 *  -6 ... empty/unknown symbol
 */
int BDX2SEXP(struct _BDX_Data const* pBDXData,struct SEXPREC** pSEXPData);
int SEXP2BDX(struct SEXPREC const* SEXPData,struct _BDX_Data** ppBDXData);

#ifdef __cplusplus
}
#endif

#endif
