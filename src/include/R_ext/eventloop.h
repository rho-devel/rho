/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2007 The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* 
   For use by alternative front-ends and packages which need to share
   the R event loop (on all platforms).

   Not part of the API and subject to change without notice.
 */

#ifndef R_EXT_EVENTLOOP_H
#define R_EXT_EVENTLOOP_H

#ifndef NO_C_HEADERS
# ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>	/* for fd_set according to recent POSIX */
# endif
/* NOTE: Needed at least on FreeBSD so that fd_set is defined. */
# include <sys/types.h>
#endif

#ifdef  __cplusplus
extern "C" {
#endif

#define XActivity 1
#define StdinActivity 2

typedef void (*InputHandlerProc)(void *userData); 

typedef struct _InputHandler {

  int activity;
  int fileDescriptor;
  InputHandlerProc handler;

  struct _InputHandler *next;

    /* Whether we should be listening to this file descriptor or not. */
  int active;

    /* Data that can be passed to the routine as its only argument.
       This might be a user-level function or closure when we implement
       a callback to R mechanism. 
     */
  void *userData;

} InputHandler;


extern InputHandler *initStdinHandler(void);
extern void consoleInputHandler(unsigned char *buf, int len);

extern InputHandler *addInputHandler(InputHandler *handlers, int fd, InputHandlerProc handler, int activity);
extern InputHandler *getInputHandler(InputHandler *handlers, int fd);
extern int           removeInputHandler(InputHandler **handlers, InputHandler *it);
extern InputHandler *getSelectedHandler(InputHandler *handlers, fd_set *mask);
extern fd_set *R_checkActivity(int usec, int ignore_stdin);
extern fd_set *R_checkActivityEx(int usec, int ignore_stdin, void (*intr)(void));
extern void R_runHandlers(InputHandler *handlers, fd_set *mask);

extern int R_SelectEx(int  n,  fd_set  *readfds,  fd_set  *writefds,
		      fd_set *exceptfds, struct timeval *timeout,
		      void (*intr)(void));

#ifdef __SYSTEM__
#ifndef __cplusplus   /* Would get duplicate conflicting symbols*/
InputHandler *R_InputHandlers;
#endif
#else
extern InputHandler *R_InputHandlers;
#endif

extern void (* R_PolledEvents)(void);
extern int R_wait_usec;

#ifdef  __cplusplus
}
#endif

#endif /* R_EXT_EVENTLOOP_H */
