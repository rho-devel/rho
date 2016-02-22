/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005   The R Core Team
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
 */

/* Included by R.h: API */

#ifndef R_ERROR_H_
#define R_ERROR_H_

#ifdef  __cplusplus
extern "C" {
#endif

#if defined(__GNUC__) && __GNUC__ >= 3
#define NORET __attribute__((noreturn))
#else
#define NORET
#endif

void NORET Rf_error(const char *, ...);
void NORET UNIMPLEMENTED(const char *);
void NORET WrongArgCount(const char *);

void	Rf_warning(const char *, ...);
void 	R_ShowMessage(const char *s);
    

#ifdef  __cplusplus
}
#endif

#ifndef R_NO_REMAP
#define error Rf_error
#define warning Rf_warning
#endif


#endif /* R_ERROR_H_ */
