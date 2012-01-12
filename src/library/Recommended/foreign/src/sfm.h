/* PSPP - computes sample statistics.
   Copyright (C) 1997-9, 2000 Free Software Foundation, Inc.
   Written by Ben Pfaff <blp@gnu.org>.
   Modified 2000 Saikat DebRoy <saikat@stat.wisc.edu>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, a copy is available at
   http://www.r-project.org/Licenses/
*/

#if !sfm_h
#define sfm_h 1

#include "foreign.h"

/* System file manager (sfm).

   This module is in charge of reading and writing system files.  For
   now, only ordinary system files are supported; in the future, PC+
   compatible system files should be supported, too.  sfm is an
   fhuser, so see file-handle.h for the fhuser interface.  */

/* Information produced by sfm_read_dictionary() that doesn't fit into
   a dictionary struct. */
struct sfm_read_info
{
    char creation_date[10];	/* `dd mmm yy' plus a null. */
    char creation_time[9];	/* `hh:mm:ss' plus a null. */
    int endianness;		/* BIG or LITTLE. */
    int compressed;		/* 0=no, 1=yes. */
    int ncases;			/* -1 if unknown. */
    char product[61];		/* Product name plus a null. */
    int encoding;		/* 2 or 3 or Windows codepage? */
};

struct dictionary;
struct file_handle;
union value;

struct dictionary *sfm_read_dictionary (struct file_handle *,
					struct sfm_read_info *);
int sfm_read_case (struct file_handle *, union value *, struct dictionary *);
void sfm_maybe_close (struct file_handle *);

/* Information needed by sfm_write_dictionary(). */
struct sfm_write_info
{
    /* Read by sfm_write_dictionary(). */
    struct file_handle *h;	/* File handle. */
    struct dictionary *dict;	/* Primary dictionary. */
    int compress;		/* 1=compress, 0=do not compress. */

    /* Written by sfm_write_dictionary(). */
    int case_size;		/* Number of R_flt64 elements per case. */
};

int sfm_write_dictionary (struct sfm_write_info *);
int sfm_write_case (struct file_handle *, const R_flt64* elem, int n_elem);

#endif /* !sfm_h */
