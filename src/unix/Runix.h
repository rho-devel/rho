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

#include <R_ext/RStartup.h>

void fpu_setup(Rboolean);	/* ./sys-unix.c */

void Rstd_read_history(const char *s);

void Rstd_Suicide(const char *s);
void Rstd_ShowMessage(const char *s);
int  Rstd_ReadConsole(const char *prompt, unsigned char *buf, int len,
		      int addtohistory);
void Rstd_WriteConsole(const char *buf, int len);
void Rstd_WriteConsoleEx(const char *buf, int len, int otype);
void Rstd_ResetConsole(void);
void Rstd_FlushConsole(void);
void Rstd_ClearerrConsole(void);
void Rstd_Busy(int which);
void NORET Rstd_CleanUp(SA_TYPE saveact, int status, int runLast);
int  Rstd_ShowFiles(int nfile, const char **file, const char **headers,
		    const char *wtitle, Rboolean del, const char *pager);
int  Rstd_ChooseFile(int _new, char *buf, int len);
void Rstd_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rstd_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rstd_addhistory(SEXP call, SEXP op, SEXP args, SEXP env);

void R_load_X11_shlib(void);
