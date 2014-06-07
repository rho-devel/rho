/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2007 The R Core Team.
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
 *  http://www.r-project.org/Licenses/
 */

/* Included by R.h: API */

#ifndef R_RS_H
#define R_RS_H

#ifndef NO_C_HEADERS
# include <string.h>		/* for memcpy, memset */
#endif

#include <Rconfig.h>		/* for F77_APPEND_UNDERSCORE */

#ifdef  __cplusplus
extern "C" {
#endif

/* S Like Error Handling */

#include <R_ext/Error.h>	/* for error and warning */

#ifndef STRICT_R_HEADERS

#define R_PROBLEM_BUFSIZE	4096
/* Parentheses added for FC4 with gcc4 and -D_FORTIFY_SOURCE=2 */
#define PROBLEM			{char R_problem_buf[R_PROBLEM_BUFSIZE];(sprintf)(R_problem_buf,
#define MESSAGE                 {char R_problem_buf[R_PROBLEM_BUFSIZE];(sprintf)(R_problem_buf,
#define ERROR			),error(R_problem_buf);}
#define RECOVER(x)		),error(R_problem_buf);}
#define WARNING(x)		),warning(R_problem_buf);}
#define LOCAL_EVALUATOR		/**/
#define NULL_ENTRY		/**/
#define WARN			WARNING(NULL)

#endif

/* S Like Memory Management */

extern void *R_chk_calloc(size_t, size_t);
extern void *R_chk_realloc(void *, size_t);
extern void R_chk_free(void *);

#ifdef __cplusplus
#ifndef STRICT_R_HEADERS
/* S-PLUS 3.x but not 5.x NULLs the pointer in the following */
#define Calloc(n, t)   reinterpret_cast<t *>(R_chk_calloc(size_t(n), sizeof(t) ))
#define Realloc(p,n,t) reinterpret_cast<t *>(R_chk_realloc( p, size_t((n) * sizeof(t)) ))
#define Free(p)        (R_chk_free(p), (p) = NULL)
#endif
#define R_Calloc(n, t)   reinterpret_cast<t *>(R_chk_calloc( size_t(n), sizeof(t) ))
#define R_Realloc(p,n,t) reinterpret_cast<t *>(R_chk_realloc( (p), (size_t)((n) * sizeof(t)) ))
#define R_Free(p)      (R_chk_free(p), (p) = NULL)
#define Memcpy(p,q,n)  memcpy( p, q, size_t( (n) * sizeof(*p) ) )
/* added for 3.0.0 */
#define Memzero(p,n)  memset(p, 0, size_t(n) * sizeof(*p))
#else  /* not __cplusplus */
#ifndef STRICT_R_HEADERS
/* S-PLUS 3.x but not 5.x NULLs the pointer in the following */
#define Calloc(n, t)   (t *) R_chk_calloc( (size_t) (n), sizeof(t) )
#define Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (size_t)((n) * sizeof(t)) )
#define Free(p)        (R_chk_free( (void *)(p) ), (p) = NULL)
#endif
#define R_Calloc(n, t)   (t *) R_chk_calloc( (size_t) (n), sizeof(t) )
#define R_Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (size_t)((n) * sizeof(t)) )
#define R_Free(p)      (R_chk_free( (void *)(p) ), (p) = NULL)
#define Memcpy(p,q,n)  memcpy( p, q, (size_t)(n) * sizeof(*p) )
/* added for 3.0.0 */
#define Memzero(p,n)  memset(p, 0, (size_t)(n) * sizeof(*p))

#endif  /* __cplusplus */


#ifdef __cplusplus
#define CallocCharBuf(n) reinterpret_cast<char *>(R_chk_calloc(size_t((n)+1), sizeof(char)))
#else
#define CallocCharBuf(n) (char *) R_chk_calloc((size_t) ((n)+1), sizeof(char))
#endif

/* S Like Fortran Interface */
/* These may not be adequate everywhere. Convex had _ prepending common
   blocks, and some compilers may need to specify Fortran linkage */

#ifdef HAVE_F77_UNDERSCORE
# define F77_CALL(x)	x ## _
#else
# define F77_CALL(x)	x
#endif
#define F77_NAME(x)    F77_CALL(x)
#define F77_SUB(x)     F77_CALL(x)
#define F77_COM(x)     F77_CALL(x)
#define F77_COMDECL(x) F77_CALL(x)

#ifndef NO_CALL_R
void	call_R(char*, long, void**, char**, long*, char**, long, char**);
#endif

#ifdef  __cplusplus
}
#endif

#endif /* R_RS_H */
