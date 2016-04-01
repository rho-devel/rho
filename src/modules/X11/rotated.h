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

/* ************************************************************************ */


/* Header file for the `xvertext 5.0' routines.

   Copyright (c) 1993 Alan Richardson (mppa3@uk.ac.sussex.syma) */


/* ************************************************************************ */

#ifndef _XVERTEXT_INCLUDED_
#define _XVERTEXT_INCLUDED_


#define XV_VERSION	5.0
#define XV_COPYRIGHT \
      "xvertext routines Copyright (c) 1993 Alan Richardson"


/* ---------------------------------------------------------------------- */


typedef enum {One_Font, Font_Set} R_FontType;

typedef struct R_XFont
{
    R_FontType type;
    XFontStruct *font;
    XFontSet fontset;
    int height;
    int ascent;
    int descent;
} R_XFont;


/* ---------------------------------------------------------------------- */

/* Protoized : C++ or ANSI C */
/* only XRotDrawString is used in R */
double	XRotVersion(char*, int);
void	XRotSetMagnification(double);
void	XRotSetBoundingBoxPad(int);
int	XRotDrawString(Display*, XFontStruct*, double,
		       Drawable, GC, int, int, const char*);
int	XRotDrawImageString(Display*, XFontStruct*, double,
			    Drawable, GC, int, int, const char*);
int	XRotDrawAlignedString(Display*, XFontStruct*, double,
			      Drawable, GC, int, int, const char*, int);
int	XRotDrawAlignedImageString(Display*, XFontStruct*, double,
				   Drawable, GC, int, int, const char*, int);
XPoint *XRotTextExtents(Display*, XFontStruct*, double,
			int, int, const char*, int);

/* addition in 2.1.0 */
int	XRfRotDrawString(Display*, R_XFont*, double,
			 Drawable, GC, int, int, const char*);

/* ---------------------------------------------------------------------- */
#endif /* _XVERTEXT_INCLUDED_ */
