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

#if !pfm_h
#define pfm_h 1

/* Portable file manager (pfm).

   This module is in charge of reading and writing portable files.
   pfm is an fhuser, so see file-handle.h for the fhuser interface.  */

/* Portable file types. */
enum
  {
    PFM_COMM,
    PFM_TAPE
  };

/* Information produced by pfm_read_dictionary() that doesn't fit into
   a dictionary struct. */
struct pfm_read_info
  {
    char creation_date[11];	/* `dd mm yyyy' plus a null. */
    char creation_time[9];	/* `hh:mm:ss' plus a null. */
    char product[61];		/* Product name plus a null. */
    char subproduct[61];	/* Subproduct name plus a null. */
    int ncases;			/* -1 if unknown. */
  };

struct dictionary;
struct file_handle;
union value;

struct dictionary *pfm_read_dictionary (struct file_handle *,
					struct pfm_read_info *);
int pfm_read_case (struct file_handle *, union value *, struct dictionary *);

int pfm_write_dictionary (struct file_handle *, struct dictionary *);
int pfm_write_case (struct file_handle *, const union value *elem);

#endif /* !pfm_h */
