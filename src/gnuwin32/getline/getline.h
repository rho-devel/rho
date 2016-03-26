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

#ifndef GETLINE_H
#define GETLINE_H

#include <stddef.h>

typedef size_t (*gl_strwidth_proc)(char *);

/* read a line of input */
int             getline(const char *prompt, char *buf, int maxlen);  

void            gl_setwidth(int);		/* specify width of screen */
void            gl_histadd(const char *);	/* adds entries to hist */
void		gl_strwidth(gl_strwidth_proc);	/* to bind gl_strlen */
void		gl_loadhistory(const char *);
void		gl_savehistory(const char *, int size);
void            gl_hist_init(int, int);		/* set up history buffer */
char    	*gl_hist_next(void);	/* return ptr to next item */
char    	*gl_hist_prev(void);	/* return ptr to prev item */

extern int 	(*gl_in_hook)(char *);
extern int 	(*gl_out_hook)(char *);
extern int	(*gl_tab_hook)(char *, int, int *);

#ifdef Win32
extern void 	(*gl_events_hook)(void);
#endif

#endif /* GETLINE_H */
