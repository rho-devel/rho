/* PSPP - computes sample statistics.
   Copyright (C) 1997-9, 2000, 2001 Free Software Foundation, Inc.
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

/* Record Type 1: General Information. */
struct sysfile_header
  {
    char rec_type[4];		/* Record-type code, "$FL2". */
    char prod_name[60];		/* Product identification. */
    R_int32 layout_code;	/* 2. */
    R_int32 case_size;		/* Number of `value's per case. */
    R_int32 compressed;		/* 1=compressed, 0=not compressed. */
    R_int32 weight_index;	/* 1-based index of weighting var, or zero. */
    R_int32 ncases;		/* Number of cases, -1 if unknown. */
    R_flt64 bias;		/* Compression bias (100.0). */
    char creation_date[9];	/* `dd mmm yy' creation date of file. */
    char creation_time[8];	/* `hh:mm:ss' 24-hour creation time. */
    char file_label[64];	/* File label. */
    char padding[3];		/* Ignored padding. */
  };

/* Record Type 2: Variable. */
struct sysfile_variable
  {
    R_int32 rec_type;		/* 2. */
    R_int32 type;		/* 0=numeric, 1-255=string width,
				   (allowed to be up to 65535 in 0.8-24)
				   -1=continued string. */
    R_int32 has_var_label;	/* 1=has a variable label, 0=doesn't. */
    R_int32 n_missing_values;	/* Missing value code of -3,-2,0,1,2, or 3. */
    R_int32 print;	/* Print format. */
    R_int32 write;	/* Write format. */
    char name[8];		/* Variable name. */
    /* The rest of the structure varies. */
  };
