/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001, 2004  The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* Included by R.h: API on Windows */

/* don't disallow including this one more than once */

/* This is intended to be called from other header files, so not callable
   from C++ */

#undef LibExtern
#undef LibImport
#undef LibExport

/* Don't try to include CYGWIN here: decorating some symbols breaks
   the auto-export that it relies on, even if R_DLL_BUILD were set. */
#ifdef WIN32 /* WIN32 as does not depend on config.h */
#define LibImport __declspec(dllimport)
#define LibExport __declspec(dllexport)
#else
#define LibImport
#define LibExport
#endif

#if defined(R_DLL_BUILD)
#define LibExtern extern
#else
#define LibExtern extern LibImport
#endif
