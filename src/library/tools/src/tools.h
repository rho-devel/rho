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

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-11   The R Core Team.
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

#ifndef R_TOOLS_H
#define R_TOOLS_H

#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tools", String)
#else
#define _(String) (String)
#endif

SEXP delim_match(SEXP x, SEXP delims);
SEXP dirchmod(SEXP dr);
SEXP Rmd5(SEXP files);
SEXP check_nonASCII(SEXP text, SEXP ignore_quotes);
SEXP check_nonASCII2(SEXP text);
SEXP doTabExpand(SEXP strings, SEXP starts);
SEXP ps_kill(SEXP pid, SEXP signal);
SEXP ps_sigs(SEXP);
SEXP ps_priority(SEXP pid, SEXP value);
SEXP codeFilesAppend(SEXP f1, SEXP f2);
SEXP getfmts(SEXP format);
SEXP startHTTPD(SEXP sIP, SEXP sPort);
SEXP stopHTTPD(void);

SEXP C_parseLatex(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP C_parseRd(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP C_deparseRd(SEXP e, SEXP state);

#endif
