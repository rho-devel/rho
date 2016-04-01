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

/** @file AddressSanitizer.h
 * @brief Macros to provide information to the address sanitizer when it is
 *   being used.
 */

#ifndef RHO_ADDRESS_SANITIZER_H
#define RHO_ADDRESS_SANITIZER_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __has_feature
#define HAVE_ASAN __has_feature(address_sanitizer)
#else
#define HAVE_ASAN defined(__SANITIZE_ADDRESS__)
#endif

#if HAVE_ASAN
void __asan_poison_memory_region(void const volatile *addr, size_t size);

void __asan_unpoison_memory_region(void const volatile *addr, size_t size);

#  define HAVE_ADDRESS_SANITIZER
#  define ASAN_POISON_MEMORY_REGION(addr, size) \
  __asan_poison_memory_region((addr), (size))
#  define ASAN_UNPOISON_MEMORY_REGION(addr, size) \
  __asan_unpoison_memory_region((addr), (size))

#  define NO_SANITIZE_ADDRESS __attribute__((no_sanitize_address))

#else  // ! HAVE_ASAN

#  define ASAN_POISON_MEMORY_REGION(addr, size) \
  ((void)(addr), (void)(size))
#  define ASAN_UNPOISON_MEMORY_REGION(addr, size) \
  ((void)(addr), (void)(size))

#  define NO_SANITIZE_ADDRESS

#endif

#ifdef __cplusplus
}  // extern "C"
#endif

#endif  // RHO_ADDRESS_SANITIZER_H
