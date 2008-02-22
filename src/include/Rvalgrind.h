/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2007  The R Development Core Team.
 *  Copyright (C) 2007-8 Andrew Runnalls
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
 * \#defines relating to the use of valgrind.  These are controlled by the
 * \c --with-valgrind-instrumentation= option to configure, which sets
 * \c VALGRIND_LEVEL to the supplied value (default 0) and defines \c
 * NVALGRIND if the value is 0.
 * <UL>
 * <LI>Level 0 is no additional instrumentation</LI>
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

// We ignore the config.h settings of VALGRIND_LEVEL and NVALGRIND if
// VALGRIND_LEVEL is already defined at this point, e.g. in compiling
// test programs.
#ifdef VALGRIND_LEVEL
#define OVERRIDE_VALGRIND_LEVEL VALGRIND_LEVEL
#undef VALGRIND_LEVEL
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef OVERRIDE_VALGRIND_LEVEL
#undef VALGRIND_LEVEL
#undef NVALGRIND
#define VALGRIND_LEVEL OVERRIDE_VALGRIND_LEVEL
#if VALGRIND_LEVEL==0
#define NVALGRIND
#endif
#endif

#ifdef Win32
# ifndef USE_VALGRIND_FOR_WINE
# define NVALGRIND 1
#endif
#endif

#ifndef NVALGRIND
#include "memcheck.h"
#endif

#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif

#endif  /* RVALGRIND_H */
