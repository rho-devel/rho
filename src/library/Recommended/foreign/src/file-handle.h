/* PSPP - computes sample statistics.
   Copyright (C) 1997-9, 2000 Free Software Foundation, Inc.
   Written by Ben Pfaff <blp@gnu.org>.
   Modified for R foreign library by Saikat DebRoy <saikat@stat.wisc.edu>.

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

#if !file_handle_h
#define file_handle_h 1

/* File handle provider (fhp).

   This module provides file handles in the form of file_handle
   structures to the dfm and sfm modules, which are known as file
   handle users (fhusers).  fhp does not know anything about file
   contents. */

#include <stddef.h>

/* Record formats. */
enum
  {
    FH_RF_FIXED,		/* Fixed length records. */
    FH_RF_VARIABLE,		/* Variable length records. */
    FH_RF_SPANNED		/* ? */
  };

/* File modes. */
enum
  {
    FH_MD_CHARACTER,		/* Character data. */
    FH_MD_IMAGE,		/* ? */
    FH_MD_BINARY,		/* Character and/or binary data. */
    FH_MD_MULTIPUNCH,		/* Column binary data (not supported). */
    FH_MD_360			/* ? */
  };

struct file_handle;

/* Services that fhusers provide to fhp. */
struct fh_ext_class
  {
    int magic;			/* Magic identifier for fhuser. */
    const char *name;		/* String identifier for fhuser. */

    void (*close) (struct file_handle *);
				/* Closes any associated file, etc. */
  };

/* A file location.  */
struct file_locator
  {
    const char *filename;		/* Filename. */
    int line_number;			/* Line number. */
  };

/* Opaque structure.  The `ext' member is an exception for use by
   subclasses.  `where.ln' is also acceptable. */
struct file_handle
  {
    /* name must be the first member. */
    const char *name;		/* File handle identifier. */
    char *norm_fn;		/* Normalized filename. */
    char *fn;			/* Filename as provided by user. */
    struct file_locator where;	/* Used for reporting error messages. */

    int recform;		/* One of FH_RF_*. */
    size_t lrecl;		/* Length of records for FH_RF_FIXED. */
    int mode;			/* One of FH_MD_*. */

    struct fh_ext_class *class;	/* Polymorphism support. */
    void *ext;			/* Extension struct for fhuser use. */
  };

/* All the file handles in the system. */
extern struct avl_tree *files;

/* Pointer to the file handle that corresponds to data in the command
   file entered via BEGIN DATA/END DATA. */
extern struct file_handle *inline_file;

/* Opening and closing handles. */
struct file_handle *fh_get_handle_by_name (const char name[9]);
struct file_handle *fh_get_handle_by_filename (const char *filename);
void fh_close_handle (struct file_handle *handle);

/* Handle info. */
const char *fh_handle_name (struct file_handle *handle);
char *fh_handle_filename (struct file_handle *handle);
size_t fh_record_width (struct file_handle *handle);

#endif /* !file_handle.h */

extern void fh_init_files(void);
