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
 *  file console.h
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004-7      The R Foundation
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

typedef window console;
typedef window pager;
typedef window dataeditor;
typedef window editor;



void
setconsoleoptions(char *fnname,int fnsty, int fnpoints,
		  int rows, int cols, int consx, int consy,
		  rgb nfg, rgb nufg, rgb nbg, rgb high,
		  int pgr, int pgc, int multiplewindows, int widthonresize,
		  int bufbytes, int buflines, int buffered);
pager newpager(char *title, char *filename, char *header, int unlinkonexit);
console newconsole(char *name, int flags);
int  consolereads(console c, char *prompt, char *buf, int len,
		  int addtohistory);
int  consolewrites(console c, const char *s);
int  consolecancopy(console c);
int  consolecanpaste(console c);
void consolecopy(console c);
void consolepaste(console c);
void consolepastecmds(console c);
void consoleselectall(console c);
void consolecmd(console c, char *cmd);
void consolenewline(console c);
void consolehelp();
void consolesetbrk(console c, actionfn, char ch, char mod);
void consoletogglelazy(console c);
int  consolegetlazy(console c);
void consoleflush(console c);
void consoleprint(console c);
void consolesavefile(console c, int pager);
void drawconsole(control c, rect r);
void consoleclear(control c);

extern int setWidthOnResize;
extern int consolebuffered;
int consolecols(console c);
void pagerbclose(control m);
