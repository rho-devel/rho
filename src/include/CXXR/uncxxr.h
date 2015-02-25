/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @brief uncxxr.pl deletes 'CXXRBUILTINFUNCTION::'
 */
#define CXXRBUILTINFUNCTION BuiltInFunction

/** @brief const in CXXR, not const in CR.
 *
 * Where CXXR inserts additional const declarations into source files
 * inherited from CR, try to use this macro so that the change can be
 * reversed by the uncxxr.pl script, which deletes any occurrence of
 * CXXRCONST.
 */
#define CXXRCONST const

/** @brief uncxxr.pl replaces this by 'expr'.
 *
 * This macro is used in former CR code where an explicit constructor
 * expression needs to be used in C++.
 */
#define CXXRCONSTRUCT(type, expr) type(expr)

/** @brief  uncxxr.pl replaces this by 'expr'.
 *
 * This macro is used in former CR code where a const_cast is needed
 * in C++.
 */
#define CXXRCCAST(type, expr) const_cast<type>(expr)

/** @brief uncxxr.pl replaces this by 'expr'.
 *
 * This macro is used in former CR code where a static_cast is needed
 * in C++.
 */
#define CXXRSCAST(type, expr) static_cast<type>(expr)

/** @brief uncxxr.pl replaces this by '0'.
 *
 * This macro is used in former CR code which uses 0 where C++ needs
 * <tt>FALSE</tt>.
 */
#define CXXRFALSE FALSE

/** @brief Suppress cast unnecessary in C++.
 *
 * uncxxr.pl deletes the characters CXXRNOCAST so as to leave a
 * C-style cast to type t.  CXXR uses this macro in files inherited
 * from CR to suppress casts (usually of NULL) that are unnecessary in
 * C++ (and probably in C also).
 */
#define CXXRNOCAST(t)

/** @brief uncxxr.pl replaces this by '1'.
 *
 * This macro is used in former CR code which uses 1 where C++ needs
 * <tt>TRUE</tt>.
 */
#define CXXRTRUE TRUE

/** @brief uncxxr.pl deletes this.
 *
 * This macro is used in former CR code where CXXR needs
 * <tt>unsigned</tt>.
 */
#define CXXRUNSIGNED unsigned

#endif /* UNCXXR_H */
