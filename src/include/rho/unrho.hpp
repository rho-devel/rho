/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file unrho.h
 *
 * @brief Macros used to provide cues to the unrho.pl script.
 *
 * Where CR source files have been converted into C++ and otherwise
 * adapted for use in rho, the script unrho.pl tries as far as
 * possible to reverse the systematic changes.  It is used when
 * upgrading to a new version of R to distinguish substantive from
 * routine changes.
 *
 * This header file defines various macros that are used to provide
 * cues to unrho.pl to how it should operate: the macro expansions
 * defined below are the ones required in rho; unrho.pl will expand
 * them differently, reflecting CR usage.
 */

#ifndef UNRHO_H
#define UNRHO_H

/** @brief const in rho, not const in CR.
 *
 * Where rho inserts additional const declarations into source files
 * inherited from CR, try to use this macro so that the change can be
 * reversed by the unrho.pl script, which deletes any occurrence of
 * RHOCONST.
 */
#define RHOCONST const

/** @brief unrho.pl replaces this by 'expr'.
 *
 * This macro is used in former CR code where an explicit constructor
 * expression needs to be used in C++.
 */
#define RHOCONSTRUCT(type, expr) type(expr)

/** @brief  unrho.pl replaces this by 'expr'.
 *
 * This macro is used in former CR code where a const_cast is needed
 * in C++.
 */
#define RHO_C_CAST(type, expr) const_cast<type>(expr)

/** @brief unrho.pl replaces this by 'expr'.
 *
 * This macro is used in former CR code where a static_cast is needed
 * in C++.
 */
#define RHO_S_CAST(type, expr) static_cast<type>(expr)

/** @brief unrho.pl replaces this by '0'.
 *
 * This macro is used in former CR code which uses 0 where C++ needs
 * <tt>FALSE</tt>.
 */
#define RHO_FALSE FALSE

/** @brief Suppress cast unnecessary in C++.
 *
 * unrho.pl deletes the characters RHO_NO_CAST so as to leave a
 * C-style cast to type t.  rho uses this macro in files inherited
 * from CR to suppress casts (usually of NULL) that are unnecessary in
 * C++ (and probably in C also).
 */
#define RHO_NO_CAST(t)

/** @brief unrho.pl replaces this by '1'.
 *
 * This macro is used in former CR code which uses 1 where C++ needs
 * <tt>TRUE</tt>.
 */
#define RHO_TRUE TRUE

#endif /* UNRHO_H */
