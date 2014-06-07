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
 *  Copyright (C) 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2005  R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_IOSTUFF_H
#define R_IOSTUFF_H

/*
 *  I/O Support for Consoles and Character Vectors
 *
 *  This code provides analogues for the stdio routines "fgetc" and
 *  (formerly) "ungetc" for "consoles" and character vectors.  These routines
 *  are used for parsing input from the console window and character
 *  vectors.
 */

#include <Defn.h>
#include <stdio.h>

#define IOBSIZE 4096

#ifdef __cplusplus
extern "C" {
#endif

typedef struct BufferListItem {
	unsigned char			buf[IOBSIZE];
	struct BufferListItem	*next;
} BufferListItem;

typedef struct IoBuffer {
	BufferListItem	*start_buf;		/* First buffer item */
	BufferListItem	*write_buf;		/* Write pointer location */
	unsigned char	*write_ptr;		/* Write pointer location */
	int		 write_offset;		/* Write pointer location */
	BufferListItem	*read_buf;		/* Read pointer location */
	unsigned  char	*read_ptr;		/* Read pointer location */
	int		 read_offset;		/* Read pointer location */
} IoBuffer;


typedef struct TextBuffer {
	void    *vmax;			        /* Memory stack top */
	unsigned char	*buf;			/* Line buffer */
	unsigned char	*bufp;			/* Line buffer location */
	SEXP	text;				/* String Vector */
	int	ntext;				/* Vector length */
	int	offset;				/* Offset within vector */
} TextBuffer;

#ifndef __MAIN__
extern
#else
attribute_hidden
#endif
IoBuffer R_ConsoleIob;	    			/* Console IO Buffer */

/*- some of these really could be void */
int R_IoBufferInit(IoBuffer*);
int R_IoBufferFree(IoBuffer*);
int R_IoBufferReadReset(IoBuffer*);
int R_IoBufferWriteReset(IoBuffer*);
int R_IoBufferGetc(IoBuffer*);
int R_IoBufferPutc(int, IoBuffer*);
int R_IoBufferPuts(char*, IoBuffer*);
int R_IoBufferReadOffset(IoBuffer*);

int R_TextBufferInit(TextBuffer*, SEXP);
int R_TextBufferFree(TextBuffer*);
int R_TextBufferGetc(TextBuffer*);

#ifdef __cplusplus
}
#endif

#endif /* not R_IOSTUFF_H */
