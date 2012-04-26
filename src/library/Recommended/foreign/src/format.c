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

#include <ctype.h>
#include <stdlib.h>
#include "foreign.h"
#include "format.h"

#define DEFFMT(LABEL, NAME, N_ARGS, IMIN_W, IMAX_W, OMIN_W, OMAX_W, CAT, \
	       OUTPUT, SPSS_FMT) \
	{NAME, N_ARGS, IMIN_W, IMAX_W, OMIN_W, OMAX_W, CAT, OUTPUT, SPSS_FMT},
struct fmt_desc formats[FMT_NUMBER_OF_FORMATS + 1] =
{
#include "pspp-format-def.h"
  {"",         -1, -1,  -1, -1,   -1, 0000, -1, -1},
};

const int translate_fmt[40] =
  {
    -1, FMT_A, FMT_AHEX, FMT_COMMA, FMT_DOLLAR, FMT_F, FMT_IB,
    FMT_PIBHEX, FMT_P, FMT_PIB, FMT_PK, FMT_RB, FMT_RBHEX, -1,
    -1, FMT_Z, FMT_N, FMT_E, -1, -1, FMT_DATE, FMT_TIME,
    FMT_DATETIME, FMT_ADATE, FMT_JDATE, FMT_DTIME, FMT_WKDAY,
    FMT_MONTH, FMT_MOYR, FMT_QYR, FMT_WKYR, FMT_PCT, FMT_DOT,
    FMT_CCA, FMT_CCB, FMT_CCC, FMT_CCD, FMT_CCE, FMT_EDATE,
    FMT_SDATE,
  };

#if 0
int
parse_format_specifier_name (const char **cp, int allow_xt)
{
  struct fmt_desc *f;
  char *ep;
  int x;

  ep = ds_value (&tokstr);
  while (isalpha ((unsigned char) *ep))
    ep++;
  x = *ep;
  *ep = 0;

  for (f = formats; f->name[0]; f++)
    if (!strcmp (f->name, ds_value (&tokstr)))
      {
	int indx = f - formats;

	*ep = x;
	if (cp)
	  *cp = ep;

	if (!allow_xt && (indx == FMT_T || indx == FMT_X))
	  {
	    error ("X and T format specifiers not allowed here."));
	    return -1;
	  }
	return indx;
      }

  error ("%s is not a valid data format.", ds_value (&tokstr));
  *ep = x;
  return -1;
}
#endif
/* Converts F to its string representation (for instance, "F8.2") and
   returns a pointer to a static buffer containing that string. */
char *
fmt_to_string (const struct fmt_spec *f)
{
  static char buf[32];

  if (formats[f->type].n_args >= 2)
    sprintf (buf, "%s%d.%d", formats[f->type].name, f->w, f->d);
  else
    sprintf (buf, "%s%d", formats[f->type].name, f->w);
  return buf;
}

int
check_input_specifier (const struct fmt_spec *spec)
{
  struct fmt_desc *f;
  char *str;

  f = &formats[spec->type];
  str = fmt_to_string (spec);
  if (spec->type == FMT_X)
    return 1;
  if (f->cat & FCAT_OUTPUT_ONLY)
    {
      error(_("format %s may not be used as an input format"), f->name);
      return 0;
    }
  if (spec->w < f->Imin_w || spec->w > f->Imax_w)
    {
      error(_("input format %s specifies a bad width %d.  Format %s requires a width between %d and %d"),
	   str, spec->w, f->name, f->Imin_w, f->Imax_w);
      return 0;
    }
  if ((f->cat & FCAT_EVEN_WIDTH) && spec->w % 2)
    {
      error (_("input format %s specifies an odd width %d, but format %s requires an even width between %d and %d"),
	     str, spec->w, f->name, f->Imin_w, f->Imax_w);
      return 0;
    }
  if (f->n_args > 1 && (spec->d < 0 || spec->d > 16))
    {
      error (_("Input format %s specifies a bad number of implied decimal places %d.  Input format %s allows up to 16 implied decimal places"),
	     str, spec->d, f->name);
      return 0;
    }
  return 1;
}

int
check_output_specifier (const struct fmt_spec *spec)
{
  struct fmt_desc *f;
  char *str;

  f = &formats[spec->type];
  str = fmt_to_string (spec);
  if (spec->type == FMT_X)
    return 1;
  if (spec->w < f->Omin_w || spec->w > f->Omax_w)
    {
      error (_("output format %s specifies a bad width %d.  Format %s requires a width between %d and %d"),
	   str, spec->w, f->name, f->Omin_w, f->Omax_w);
      return 0;
    }
  if (spec->d > 1
      && (spec->type == FMT_F || spec->type == FMT_COMMA
	  || spec->type == FMT_DOLLAR)
      && spec->w < f->Omin_w + 1 + spec->d)
    {
      error (_("output format %s requires minimum width %d to allow %d decimal places.  Try %s%d.%d instead of %s"),
	   f->name, f->Omin_w + 1 + spec->d, spec->d, f->name,
	   f->Omin_w + 1 + spec->d, spec->d, str);
      return 0;
    }
  if ((f->cat & FCAT_EVEN_WIDTH) && spec->w % 2)
    {
      error (_("output format %s specifies an odd width %d, but output format %s requires an even width between %d and %d"),
	     str, spec->w, f->name, f->Omin_w, f->Omax_w);
      return 0;
    }
  if (f->n_args > 1 && (spec->d < 0 || spec->d > 16))
    {
      error (_("Output format %s specifies a bad number of implied decimal places %d.  Output format %s allows a number of implied decimal places between 1 and 16"),
	  str, spec->d, f->name);
      return 0;
    }
  return 1;
}

/* If a string variable has width W, you can't display it with a
   format specifier with a required width MIN_LEN>W. */
int
check_string_specifier (const struct fmt_spec *f, int min_len)
{
  if ((f->type == FMT_A && min_len > f->w)
      || (f->type == FMT_AHEX && min_len * 2 > f->w))
    {
      error (_("cannot display a string variable of width %d with format specifier %s"),
	     min_len, fmt_to_string (f));
      return 0;
    }
  return 1;
}

void
convert_fmt_ItoO (const struct fmt_spec *input, struct fmt_spec *output)
{
  output->type = formats[input->type].output;
  output->w = input->w;
  if (output->w > formats[output->type].Omax_w)
    output->w = formats[output->type].Omax_w;
  output->d = input->d;

  switch (input->type)
    {
    case FMT_F:
    case FMT_N:
      if (output->d > 1 && output->w < 2 + output->d)
	output->w = 2 + output->d;
      break;
    case FMT_E:
      output->w = max (max (input->w, input->d+7), 10);
      output->d = max (input->d, 3);
      break;
    case FMT_COMMA:
    case FMT_DOT:
      /* nothing is necessary */
      break;
    case FMT_DOLLAR:
    case FMT_PCT:
      if (output->w < 2)
	output->w = 2;
      break;
    case FMT_PIBHEX:
      {
	static const int map[] = {4, 6, 9, 11, 14, 16, 18, 21};
	if (input->w % 2 != 0 || input->w < 2 || input->w > 16)
	    error("convert_fmt_ItoO : assert failed");
	output->w = map[input->w / 2 - 1];
	break;
      }
    case FMT_RBHEX:
      output->w = 8, output->d = 2;	/* FIXME */
      break;
    case FMT_IB:
    case FMT_PIB:
    case FMT_P:
    case FMT_PK:
    case FMT_RB:
      if (input->d < 1)
	output->w = 8, output->d = 2;
      else
	output->w = 9 + input->d;
      break;
    case FMT_CCA:
    case FMT_CCB:
    case FMT_CCC:
    case FMT_CCD:
    case FMT_CCE:
      error("convert_fmt_ItoO : invalid input->type : %d", input->type);
    case FMT_Z:
    case FMT_A:
      /* nothing is necessary */
      break;
    case FMT_AHEX:
      output->w = input->w / 2;
      break;
    case FMT_DATE:
    case FMT_EDATE:
    case FMT_SDATE:
    case FMT_ADATE:
    case FMT_JDATE:
      /* nothing is necessary */
      break;
    case FMT_QYR:
      if (output->w < 6)
	output->w = 6;
      break;
    case FMT_MOYR:
      /* nothing is necessary */
      break;
    case FMT_WKYR:
      if (output->w < 8)
	output->w = 8;
      break;
    case FMT_TIME:
    case FMT_DTIME:
    case FMT_DATETIME:
    case FMT_WKDAY:
    case FMT_MONTH:
      /* nothing is necessary */
      break;
    default:
      error("convert_fmt_ItoO : invalid input->type : %d", input->type);
    }
}

#if 0
int
parse_format_specifier (struct fmt_spec *input, int allow_xt)
{
  struct fmt_spec spec;
  struct fmt_desc *f;
  const char *cp;
  char *cp2;
  int type, w, d;

  if (token != T_ID)
    {
      error ("format specifier expected");
      return 0;
    }
  type = parse_format_specifier_name (&cp, allow_xt);
  if (type == -1)
    return 0;
  f = &formats[type];

  w = strtol (cp, &cp2, 10);
  if (cp2 == cp && type != FMT_X)
    {
      error ("Data format %s does not specify a width.",
	   ds_value (&tokstr));
      return 0;
    }

  cp = cp2;
  if (f->n_args > 1 && *cp == '.')
    {
      cp++;
      d = strtol (cp, &cp2, 10);
      cp = cp2;
    }
  else
    d = 0;

  if (*cp)
    {
      error ("Data format %s is not valid.", ds_value (&tokstr));
      return 0;
    }
  lex_get ();

  spec.type = type;
  spec.w = w;
  spec.d = d;
  *input = spec;

  return 1;
}
#endif
