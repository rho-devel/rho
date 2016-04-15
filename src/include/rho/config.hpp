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

/** @file config.hpp
 *
 * @brief Build configuration options specific to rho.
 *
 * This file contains documentation and definitions of preprocessor
 * variables that configure the behaviour of rho.  However, it
 * excludes preprocessor variables that are controlled by options to
 * the autoconf-generated <tt>configure</tt> script: for information
 * on these see <tt>./configure --help</tt>.
 *
 * As distributed this file represents a configuration suitable for speed.
 * development, with numerous facilities enable for checking and
 * debugging.  For maximum speed, it is recommended that this file be
 * modified by enabling the definitions of NDEBUG and
 * disabling all other definitions.
 */

#ifndef RHO_CONFIG_HPP
#define RHO_CONFIG_HPP

/** @def AGGRESSIVE_GC
 *
 * By default, GCNode::operator new initiates a reference-count-based
 * garbage collection (GCNode::gclite()) only when the number of bytes
 * allocated has risen by a certain margin from the number allocated
 * after the last such collection.  However, if rho is compiled with
 * the preprocessor variable AGGRESSIVE_GC define, every call to
 * GCNode::operator new will initiate a reference-count-based garbage
 * collection.  When used in conjunction with NO_CELLPOOLS and address
 * sanitizer (or valgrind), this can help to detect and diagnose gaps in
 * the protection of nodes against garbage collection.
 */
#ifdef DOXYGEN
#define AGGRESSIVE_GC
#endif

/** @def NDEBUG
 *
 * @brief Suppress some runtime checks.
 *
 * By default, rho includes code to check that rho::GCStackRoot
 * objects are destroyed in the reverse order of creation, and that a
 * node is <code>UNPROTECT</code>ed in the same RCNTXT as it was
 * <code>PROTECT</code>ed. If NDEBUG is defined, these checks are
 * omitted. Not recommended during development.
 */
#ifdef DOXYGEN
#define NDEBUG
#endif


/* PROVENANCE_TRACKING is controlled by the
 * --enable-provenance-tracking option to configure, and will be
 * defined (or not) in config.h .
 */

/** @def RARE_GC
 *
 * @brief Suppress reference-counting garbage collection.
 *
 * By default, rho will delete any rho::GCNode whose reference count
 * has fallen to zero as a preliminary to allocating memory for a new
 * rho::GCNode.
 * Defining RARE_GC suppresses the default behaviour, and results in
 * rho::GCNode objects being deleted only as part of the mark-sweep
 * garbage collection process, initiated when a memory utilisation
 * threshold is exceeded.
 */
#ifdef DOXYGEN
#define RARE_GC
#endif

/** @def CHECKED_SEXP_DOWNCAST
 *
 * @brief Check downcasts within the rho::RObject class hierarchy.
 *
 * If enabled, rho implements the templated function
 * rho::SEXP_downcast<PtrOut, PtrIn>() using
 * <code>dynamic_cast</code>, to verify that the argument object is of
 * an appropriate type for the requested cast.
 */
#ifdef DOXYGEN
#define CHECKED_SEXP_DOWNCAST
#endif

#ifdef __GNUC__
#  ifdef __i386__
#    define HOT_FUNCTION __attribute__((hot, fastcall))
#  else
#    define HOT_FUNCTION __attribute__((hot))
#  endif
#else
#  define HOT_FUNCTION
#endif

#endif // RHO_CONFIG_HPP
