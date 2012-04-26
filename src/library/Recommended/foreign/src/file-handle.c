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

#include <errno.h>
#include <stdlib.h>
#include <R.h>
#include "avl.h"
#include "file-handle.h"
#include "var.h"
#include "foreign.h"
/* (headers) */

#undef DEBUGGING
/*#define DEBUGGING 1*/

avl_tree *files;
struct file_handle *inline_file;

static void init_file_handle (struct file_handle * handle);

/* (specification)
   "FILE HANDLE" (fh_):
     name=string;
     recform=recform:fixed/!variable/spanned;
     lrecl=integer;
     mode=mode:!character/image/binary/multipunch/_360.
*/
/* (declarations) */
/* (functions) */


/* File handle functions. */

/* Sets up some fields in H; caller should fill in
   H->{NAME,NORM_FN,FN}. */
static void
init_file_handle (struct file_handle *h)
{
  h->recform = FH_RF_VARIABLE;
  h->mode = FH_MD_CHARACTER;
  h->ext = NULL;
  h->class = NULL;
}


/* Returns the handle corresponding to FILENAME.  Creates the handle
   if no handle exists for that file.  All filenames are normalized
   first, so different filenames referring to the same file will
   return the same file handle. */
struct file_handle *
fh_get_handle_by_filename (const char *filename)
{
  struct file_handle f, *fp;
  char *fn;
  char *name;
  int len;

  /* Get filename. */
  len = strlen (filename);
  fn = Calloc(len + 1, char);
  strcpy(fn, filename);

  /* Create handle name with invalid identifier character to prevent
     conflicts with handles created with FILE HANDLE. */
  name = Calloc (len + 2, char);
  name[0] = '*';
  strcpy (&name[1], fn);

  f.name = name;
  fp = R_avl_find (files, &f);
  if (!fp)
    {
      fp = Calloc (1, struct file_handle);
      init_file_handle (fp);
      fp->name = name;
      fp->where.filename = fp->fn = fp->norm_fn = fn;
      R_avl_insert (files, fp);
    }
  else
    {
      Free (fn);
      Free (name);
    }
  return fp;
}

/* Returns the handle with identifier NAME, if it exists; otherwise
   reports error to user and returns NULL. */
struct file_handle *
fh_get_handle_by_name (const char name[9])
{
  struct file_handle f, *fp;
  f.name = (char *) name;
  fp = R_avl_find (files, &f);

  if (!fp)
    error (_("file handle `%s' has not been previously declared on FILE HANDLE"), name);
  return fp;
}

/* Returns the identifier of file HANDLE.  If HANDLE was created by
   referring to a filename (i.e., DATA LIST FILE='yyy' instead of FILE
   HANDLE XXX='yyy'), returns the filename, enclosed in double quotes.
   Return value is in a static buffer.

   Useful for printing error messages about use of file handles.  */
const char *
fh_handle_name (struct file_handle *h)
{
  static char *buf = NULL;

  if (buf)
    {
      Free (buf);
      buf = NULL;
    }
  if (!h)
    return NULL;

  if (h->name[0] == '*')
    {
      int len = strlen (h->fn);

      buf = Calloc (len + 3, char);
      strcpy (&buf[1], h->fn);
      buf[0] = buf[len + 1] = '"';
      buf[len + 2] = 0;
      return buf;
    }
  return h->name;
}

/* Closes the stdio FILE associated with handle H.  Frees internal
   buffers associated with that file.  Does *not* destroy the file
   handle H.  (File handles are permanent during a session.)  */
void
fh_close_handle (struct file_handle *h)
{
  if (h == NULL)
    return;

#if 0
  warning ("Closing %s%s.\n", fh_handle_name (h),
	   h->class == NULL ? " (already closed)" : "");
#endif

  if (h->class)
    h->class->close (h);
  h->class = NULL;
  if(h->ext) Free(h->ext);
  h->ext = NULL;
}

/* Compares names of file handles A and B. */
static int
cmp_file_handle (const void *a, const void *b, void *foo)
{
  return strcmp (((struct file_handle *) a)->name,
		 ((struct file_handle *) b)->name);
}

/* Initialize the AVL tree of file handles; inserts the "inline file"
   inline_file. */

static char inline_filename[] = "<Inline File>";

void
fh_init_files (void)
{
  /* Create AVL tree. */
  files = R_avl_create (cmp_file_handle, NULL);

  /* Insert inline file. */
  inline_file = Calloc (1, struct file_handle);
  init_file_handle (inline_file);
  inline_file->name = "INLINE";
  inline_file->where.filename
      = inline_file->fn = inline_file->norm_fn = inline_filename;
  inline_file->where.line_number = 0;
  R_avl_insert (files, inline_file);
}

/* Returns the (normalized) filename associated with file handle H. */
char *
fh_handle_filename (struct file_handle * h)
{
  return h->norm_fn;
}

/* Returns the width of a logical record on file handle H. */
size_t
fh_record_width (struct file_handle *h)
{
  if (h == inline_file)
    return 80;
  else if (h->recform == FH_RF_FIXED)
    return h->lrecl;
  else
    return 1024;
}

/*
   Local variables:
   mode: c
   End:
*/
