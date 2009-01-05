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

/** @file uncxxr.h
 *
 * @brief Macros used to provide cues to the uncxxr.pl script.
 *
 * Where CR source files have been converted into C++ and otherwise
 * adapted for use in CXXR, the script uncxxr.pl tries as far as
 * possible to reverse the systematic changes.  It is used when
 * upgrading to a new version of R to distinguish substantive from
 * routine changes.
 *
 * This header file defines various macros that are used to provide
 * cues to uncxxr.pl to how it should operate: the macro expansions
 * defined below are the ones required in CXXR; uncxxr.pl will expand
 * them differently, reflecting CR usage.
 */

#ifndef UNCXXR_H
#define UNCXXR_H

/* Where CXXR inserts additional const declarations into source files
 * inherited from CR, try to use this macro so that the change can be
 * reversed by the uncxxr.pl script, which deletes any occurrence of
 * CXXRconst.
 */
#define CXXRconst const

/* uncxxr.pl deletes the characters CXXRNOCAST so as to leave a
 * C-style cast to type t.  CXXR uses this macro in files inherited from
 * CX to suppress casts (usually of NULL) that are unnecessary in C++
 * (and probably in C also).
 */
#define CXXRNOCAST(t)

/* uncxxr.pl replaces this by 'attribute_hidden'. */
#define CXXRnot_hidden

/* uncxxr.pl deletes this. */
#define CXXRunsigned unsigned

#endif /* UNCXXR_H */
