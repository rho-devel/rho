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

#include <stdio.h>
#include "getline.h"


main()
/* 
 * just echo user input lines, letting user edit them and move through
 * history list
 */
{
    char *p;

    do {
	p = getline("PROMPT>>>> ");
	gl_histadd(p);
	fputs(p, stdout);
    } while (*p != 0);
}
