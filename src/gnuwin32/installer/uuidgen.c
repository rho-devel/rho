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

#define _OLEAUT32_

#include <stdio.h>
#include <unknwn.h>

GUID guid;
WORD* wstrGUID[100];
char strGUID[100];
int count, i;

int main (int argc, char* argv[]) 
{

    if (argc != 2) {
	fprintf (stderr, "SYNTAX: UUIDGEN <number-of-GUIDs-to-generate>\n");
	return 1;
    }
    count = atoi (argv[1]);
    for (i = 0; i < count; i++) {
	CoCreateGuid (&guid);
	StringFromCLSID (&guid, wstrGUID);
	WideCharToMultiByte (CP_ACP, 0, *wstrGUID, -1, strGUID, MAX_PATH, NULL, NULL);
	strGUID[strlen(strGUID)-1] = '\0';
	printf ("%s\n", strGUID+1);
    }
    return 0;
}
