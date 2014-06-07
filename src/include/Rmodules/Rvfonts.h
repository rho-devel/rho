/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

#ifndef R_VFONTS_MODULE_H
#define R_VFONTS_MODULE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*R_GE_VTextRoutine)(double x, double y, const char * const s, 
				  double x_justify, double y_justify, 
				  double rotation,
				  const pGEcontext gc, pGEDevDesc dd);

typedef double (*R_GE_VStrWidthRoutine)(const char *s, 
					const pGEcontext gc, pGEDevDesc dd);

typedef double (*R_GE_VStrHeightRoutine)(const char *s, 
					 const pGEcontext gc, pGEDevDesc dd);

typedef struct {
    R_GE_VTextRoutine GEVText;
    R_GE_VStrWidthRoutine GEVStrWidth;
    R_GE_VStrHeightRoutine GEVStrHeight;
} VfontRoutines;

void R_GE_setVFontRoutines(R_GE_VStrWidthRoutine vwidth, 
			   R_GE_VStrHeightRoutine vheight, 
			   R_GE_VTextRoutine vtext);

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* ifndef R_VFONTS_MODULE_H */
