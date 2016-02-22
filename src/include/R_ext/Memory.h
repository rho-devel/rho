/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2016  The R Core Team
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
 *  https://www.R-project.org/Licenses/
 *
 *
 * Memory Allocation (garbage collected) --- INCLUDING S compatibility ---
 */

/* Included by R.h: API */

#ifndef R_EXT_MEMORY_H_
#define R_EXT_MEMORY_H_

#ifndef NO_C_HEADERS
# if defined(__cplusplus) && !defined(DO_NOT_USE_CXX_HEADERS)
#  include <cstddef>
# else
#  include <stddef.h> /* for size_t */
# endif
#endif

#ifdef  __cplusplus
extern "C" {
#endif

void*   vmaxget(void);
void	vmaxset(const void *);

void	R_gc(void);
int	R_gc_running();

char*	R_alloc(size_t, int);
long double *R_allocLD(size_t nelem);
char*	S_alloc(long, int);
char*	S_realloc(char *, long, long, int);

#ifdef  __cplusplus
}
#endif

#endif /* R_EXT_MEMORY_H_ */
