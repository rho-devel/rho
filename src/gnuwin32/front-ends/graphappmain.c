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

#include <windows.h>

/* The mingw-runtime startup code has _argc and _argv as visible
   symbols, as do the MS compilers.

   The mingw-w64-crt is different.
*/

extern void 
GA_startgraphapp(HINSTANCE Instance, HINSTANCE PrevInstance, int CmdShow);

int PASCAL
WinMain (HINSTANCE Instance, HINSTANCE PrevInstance, LPSTR CmdLine,
	 int CmdShow)
{
    extern void AppMain(int argc, char **argv);

#ifdef _W64
    extern int __argc;
    extern char **__argv;

    GA_startgraphapp(Instance, PrevInstance, CmdShow);
    AppMain(__argc, __argv);
#else
    extern int _argc;
    extern char **_argv;

    GA_startgraphapp(Instance, PrevInstance, CmdShow);
    AppMain(_argc, _argv);
#endif
    return 0;
}
