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

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2007  The R Development Core Team.
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
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file Rvalgrind.h
 *
 * \#defines relating to the use of valgrind.  CXXR uses environment
 * variables to manage valgrind instrumentation, rather than having
 * this controlled by the <tt>configure</tt> script.  If the \c
 * NVALGRIND environment variable is defined, all valgrind
 * instrumentation is suppressed; otherwise, the instrumentation level
 * is controlled by the environment variable \c VALGRIND_LEVEL, as
 * follows: <UL> <LI>Level 0 is no additional instrumentation; this is
 * the default.</LI>
 *
 * <LI>Level 1 marks as uninitialized newly-created numeric, logical,
 * and integer vectors, and R_alloc()/S_alloc() memory.
 * This level is aimed primarily at enabling users of R (including
 * writers of imported code) to detect use of uninitialized data.</LI>
 *
 * <LI>Level 2 marks free memory within CellPools as inaccessible, and
 * the contents of newly allocated blocks from CellPools as
 * uninitialised.  This provides further protection against array
 * overruns and the use of stale pointers.</LI>
 *
 * <LI>Level 3 redzones memory blocks allocated via
 * CXXR::Heap.  At present this is simply a trailing 1-byte
 * redzone.  CR also countenances the possibility that
 * <tt>VALGRIND_LEVEL > 2</tt> but does not document its meaning.
 * </UL>
 *
 * It may be necessary to define \c NVALGRIND for a non-gcc compiler
 * on a supported architecture if it has different syntax for inline
 * assembly language from gcc.
 *
 * For Win32, Valgrind is useful only if running under Wine,
 */

#ifndef RVALGRIND_H
#define RVALGRIND_H 1

/* NVALGRIND trumps everything else.*/
#ifdef NVALGRIND
#undef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif

#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#define NVALGRIND
#endif

#ifdef Win32
# ifndef USE_VALGRIND_FOR_WINE
# define NVALGRIND 1
#endif
#endif

#ifndef NVALGRIND
#include "memcheck.h"
#endif

#endif  /* RVALGRIND_H */
