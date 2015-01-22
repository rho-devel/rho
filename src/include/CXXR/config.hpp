/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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
 * @brief Build configuration options specific to CXXR.
 *
 * This file contains documentation and definitions of preprocessor
 * variables that configure the behaviour of CXXR.  However, it
 * excludes preprocessor variables that are controlled by options to
 * the autoconf-generated <tt>configure</tt> script: for information
 * on these see <tt>./configure --help</tt>.
 *
 * As distributed, this file represents a configuration suitable for
 * development, with numerous facilities enable for checking and
 * debugging.  For maximum speed, it is recommended that this file be
 * modified by enabling the definitions of NDEBUG and
 * UNCHECKED_SEXP_DOWNCAST, and disabling all other definitions.
 */

#ifndef CXXR_CONFIG_HPP
#define CXXR_CONFIG_HPP

/** @def AGGRESSIVE_GC
 *
 * By default, GCNode::operator new initiates a reference-count-based
 * garbage collection (GCNode::gclite()) only when the number of bytes
 * allocated has risen by a certain margin from the number allocated
 * after the last such collection.  However, if CXXR is compiled with
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
 * By default, CXXR includes code to check that CXXR::GCStackRoot
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
 * By default, CXXR will delete any CXXR::GCNode whose reference count
 * has fallen to zero as a preliminary to allocating memory for a new
 * CXXR::GCNode.
 * Defining RARE_GC suppresses the default behaviour, and results in
 * CXXR::GCNode objects being deleted only as part of the mark-sweep
 * garbage collection process, initiated when a memory utilisation
 * threshold is exceeded.
 */
#ifdef DOXYGEN
#define RARE_GC
#endif

/** @def UNCHECKED_SEXP_DOWNCAST
 *
 * @brief Don't check downcasts within the CXXR::RObject class hierarchy.
 *
 * By default, CXXR implements the templated function
 * CXXR::SEXP_downcast<PtrOut, PtrIn>() using
 * <code>dynamic_cast</code>, to verify that the argument object is of
 * an appropriate type for the requested cast. If
 * UNCHECKED_SEXP_DOWNCAST is defined, CXXR uses instead a
 * <code>static_cast</code>, i.e. it in effect assumes that the
 * downcast is legal. Not recommended during development.
 */
#ifdef DOXYGEN
#define UNCHECKED_SEXP_DOWNCAST
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

#endif // CXXR_CONFIG_HPP
