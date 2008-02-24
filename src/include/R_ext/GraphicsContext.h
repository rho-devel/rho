/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-5 The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* Used by third-party graphics devices */

/* 2007/06/02 arr: This material was formerly in GraphicsEngine.h, but has
 * been separated out to avoid reciprocal dependencies between GraphicsEngine.h
 * and GraphicsDevice.h
 */

#ifndef R_GRAPHICSCONTEXT_H_
#define R_GRAPHICSCONTEXT_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
 *  Some line end/join constants
 */
typedef enum {
  GE_ROUND_CAP  = 1,
  GE_BUTT_CAP   = 2,
  GE_SQUARE_CAP = 3
} R_GE_lineend;

typedef enum {
  GE_ROUND_JOIN = 1,
  GE_MITRE_JOIN = 2,
  GE_BEVEL_JOIN = 3
} R_GE_linejoin;

/* 
 * A structure containing graphical parameters 
 *
 * This is how graphical parameters are passed from graphics systems
 * to the graphics engine AND from the graphics engine to graphics
 * devices.
 *
 * Devices are not *required* to honour graphical parameters
 * (e.g., alpha transparency is going to be tough for some)
 */
typedef struct {
    /*
     * Colours
     *
     * NOTE:  Alpha transparency included in col & fill
     */
    int col;             /* pen colour (lines, text, borders, ...) */
    int fill;            /* fill colour (for polygons, circles, rects, ...) */
    double gamma;        /* Gamma correction */
    /* 
     * Line characteristics
     */
    double lwd;          /* Line width (roughly number of pixels) */
    int lty;             /* Line type (solid, dashed, dotted, ...) */
    R_GE_lineend lend;   /* Line end */
    R_GE_linejoin ljoin; /* line join */
    double lmitre;       /* line mitre */
    /*
     * Text characteristics
     */
    double cex;          /* Character expansion (font size = fontsize*cex) */
    double ps;           /* Font size in points */
    double lineheight;   /* Line height (multiply by font size) */
    int fontface;        /* Font face (plain, italic, bold, ...) */ /* 2007/06/03 arr: consider declaring this as FontType (enum currently declared in plotmath.cpp) */
    char fontfamily[201]; /* Font family */
} R_GE_gcontext;

#ifdef __cplusplus
}
#endif

#endif /* R_GRAPHICSCONTEXT_ */
