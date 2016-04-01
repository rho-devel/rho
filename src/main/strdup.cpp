/* $Id$
 *
 * This file is part of Rho, a project to refactor the R interpreter
 * into C++.  It may consist in whole or in part of program code and
 * documentation taken from the R project itself, incorporated into
 * Rho (and possibly MODIFIED) under the terms of the GNU General Public
 * Licence.
 * 
 * Rho is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 * copyrights and copyright restrictions as may be stated below.
 * 
 * Rho is not part of the R project, and bugs and other issues should
 * not be reported via r-bugs or other R project channels; instead refer
 * to the Rho website.
 * */

/* strdup.c -- return a newly allocated copy of a string
   Copyright (C) 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, a copy is available at
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#else
char *malloc ();
char *strcpy ();
#endif

/* Return a newly allocated copy of STR,
   or 0 if out of memory. */

char *
strdup (const char *str)
{
  char *newstr;

  newstr = (char *) malloc (strlen (str) + 1);
  if (newstr)
    strcpy (newstr, str);
  return newstr;
}
