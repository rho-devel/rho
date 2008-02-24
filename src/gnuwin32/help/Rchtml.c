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
Can be built with VC++6 by
cl /MT /Ox /c Rchtml.c
link /dll /out:Rchtml.dll Rchtml.obj user32.lib htmlhelp.lib advapi32.lib
*/

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <htmlhelp.h>

static char htmlref[256];

__declspec(dllexport) void Rchtml(char **file, char **ptopic, int *error)
{
    char *topic =*ptopic;
    HWND rc;
    
    strcpy(htmlref, *file);
    if(topic && strlen(topic)) {
	strcat(htmlref, "::/");
	strcat(htmlref, topic);
	strcat(htmlref, ".html");
    }
    rc = HtmlHelp(GetDesktopWindow(), htmlref, HH_DISPLAY_TOPIC, 0);
    *error = (rc == (HWND)NULL);
}
