/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006-8  The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* A header for use with alternative front-ends. Not formally part of
 * the API so subject to change without notice. */

#ifndef REMBEDDED_H_
#define REMBEDDED_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <R_ext/Boolean.h>

extern int Rf_initEmbeddedR(int argc, char *argv[]);
extern void Rf_endEmbeddedR(int fatal);

/* From here on down can be helpful in writing tailored startup and
   termination code */

#ifndef LibExtern
# define LibExtern extern
#endif

int Rf_initialize_R(int ac, char **av);
void setup_Rmainloop(void);
extern void R_ReplDLLinit(void);
extern int R_ReplDLLdo1(void);

void R_setStartTime(void);  /* 2007/06/11 arr: also prototyped in Rinterface.h */
extern void R_RunExitFinalizers(void);
extern void CleanEd(void);
extern void Rf_KillAllDevices(void);
LibExtern int R_DirtyImage;
extern void R_CleanTempDir(void);
LibExtern char *R_TempDir;    
extern void R_SaveGlobalEnv(void);


#ifdef unix
void fpu_setup(Rboolean start);
#endif

#ifdef Win32
extern char *getDLLVersion(void), *getRUser(void), *get_R_HOME(void);
extern void setup_term_ui(void);
LibExtern int UserBreak;
extern Rboolean AllDevicesKilled;
extern void editorcleanall(void);
extern int GA_initapp(int, char **);
extern void GA_appcleanup(void);
extern void readconsolecfg(void);
#endif

#ifdef __cplusplus
}
#endif

#endif /* REMBEDDED_H_ */
