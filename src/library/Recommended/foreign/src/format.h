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

#if !format_h
#define format_h 1

/* Display format types. */
/* See the definitions of these functions and variables when modifying
   this list:
   misc.c:convert_fmt_ItoO()
   sfm-read.c:parse_format_spec()
   data-in.c:parse_string_as_format()
   data-out.c:convert_format_to_string(). */
#define DEFFMT(LABEL, NAME, N_ARGS, IMIN_W, IMAX_W, OMIN_W, OMAX_W,	\
	       CAT, OUTPUT, SPSS_FMT)					\
	LABEL,
enum
  {
#include "pspp-format-def.h"
    FMT_NUMBER_OF_FORMATS
  };
#undef DEFFMT

/* Describes one of the display formats above. */
struct fmt_desc
  {
    char name[9];		/* `DATETIME' is the longest name. */
    int n_args;			/* 1=width; 2=width.decimals. */
    int Imin_w, Imax_w;		/* Bounds on input width. */
    int Omin_w, Omax_w;		/* Bounds on output width. */
    int cat;			/* Categories. */
    int output;			/* Output format. */
    int spss;			/* Equivalent SPSS output format. */
  };

/* Display format categories. */
enum
  {
    FCAT_BLANKS_SYSMIS = 001,	/* 1=All-whitespace means SYSMIS. */
    FCAT_EVEN_WIDTH = 002,	/* 1=Width must be even. */
    FCAT_STRING = 004,		/* 1=String input/output format. */
    FCAT_SHIFT_DECIMAL = 010,	/* 1=Automatically shift decimal point
				   on output--used for fixed-point
				   formats. */
    FCAT_OUTPUT_ONLY = 020	/* 1=This is not an input format. */
  };

/* Display format. */
struct fmt_spec
  {
    int type;			/* One of the above constants. */
    int w;			/* Width. */
    int d;			/* Number of implied decimal places. */
  };

/* Descriptions of all the display formats above. */
extern struct fmt_desc formats[];

/* Translates SPSS formats to PSPP formats. */
extern const int translate_fmt[40];

union value;

int parse_format_specifier (struct fmt_spec *input, int allow_xt);
int parse_format_specifier_name (const char **cp, int allow_xt);
int check_input_specifier (const struct fmt_spec *spec);
int check_output_specifier (const struct fmt_spec *spec);
int check_string_specifier (const struct fmt_spec *spec, int min_len);
void convert_fmt_ItoO (const struct fmt_spec *input, struct fmt_spec *output);
int parse_string_as_format (const char *s, int len, const struct fmt_spec *fp,
			    int fc, union value *v);
int data_out (char *s, const struct fmt_spec *fp, const union value *v);
char *fmt_to_string (const struct fmt_spec *);
void num_to_string (double v, char *s, int w, int d);

#endif /* !format_h */
