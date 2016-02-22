/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005 R Core Team
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* Internal header, not installed */

#ifndef R_PARSE_H
#define R_PARSE_H

#include <R_ext/Parse.h>
#include <IOStuff.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Public interface */
/* SEXP R_ParseVector(SEXP, int, ParseStatus *, SEXP); in R_ext/Parse.h */

/* Private interface */

typedef struct SrcRefState SrcRefState;

struct SrcRefState {

    Rboolean keepSrcRefs;	/* Whether to attach srcrefs to objects as they are parsed */
    Rboolean didAttach;		/* Record of whether a srcref was attached */
    SEXP SrcFile;		/* The srcfile object currently being parsed */
    SEXP Original;		/* The underlying srcfile object */
    PROTECT_INDEX SrcFileProt;	/* The SrcFile may change */
    PROTECT_INDEX OriginalProt; /* ditto */
    SEXP data;			/* Detailed info on parse */
    SEXP text;
    SEXP ids;
    int data_count;
    				/* Position information about the current parse */
    int xxlineno;		/* Line number according to #line directives */
    int xxcolno;		/* Character number on line */
    int xxbyteno;		/* Byte number on line */
    int xxparseno;              /* Line number ignoring #line directives */
    
    SrcRefState* prevState;
};

void InitParser(void);

void R_InitSrcRefState(void);
void R_FinalizeSrcRefState(void);

SEXP R_Parse1Buffer(IoBuffer*, int, ParseStatus *); /* in ReplIteration,
						       R_ReplDLLdo1 */
SEXP R_ParseBuffer(IoBuffer*, int, ParseStatus *, SEXP, SEXP); /* in source.c */
SEXP R_Parse1File(FILE*, int, ParseStatus *); /* in R_ReplFile */
SEXP R_ParseFile(FILE*, int, ParseStatus *, SEXP);  /* in edit.c */

#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status, SEXP srcfile);

	/* Report a parse error */
	
void NORET parseError(SEXP call, int linenum);

#ifdef __cplusplus
}
#endif

#endif /* not R_PARSE_H */
