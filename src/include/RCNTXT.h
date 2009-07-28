/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2007  The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file RCNTXT.h
 *
 * Struct RCNTXT and related functions.
 */

#ifndef RCNTXT_H
#define RCNTXT_H

#include "R_ext/Boolean.h"
#include "R_ext/libextern.h"
#include "CXXR/RObject.h"

#define BYTECODE

#ifdef __cplusplus
#include "CXXR/GCStackRoot.h"
#endif

#ifdef BYTECODE
# ifdef BC_INT_STACK
    typedef union { void *p; int i; } IStackval;
# endif
#endif

    /* Stack entry for pending promises */
    typedef struct RPRSTACK {
	SEXP promise;
	struct RPRSTACK *next;
    } RPRSTACK;

#ifdef __cplusplus
    /* Evaluation Context Structure */
    struct RCNTXT {
	struct RCNTXT *nextcontext; /* The next context up the chain */
	int callflag;		    /* The context "type" */
	/* JMP_BUF cjmpbuf; */	    /* C stack and register information */
	unsigned int cstacktop;	    /* Top of the pointer protection stack */
	int evaldepth;	            /* evaluation depth at inception */
	CXXR::GCStackRoot<> promargs;   /* Promises supplied to closure */
	CXXR::GCStackRoot<> callfun;    /* The closure called */
	CXXR::GCStackRoot<> sysparent;  /* environment the closure was called from */
	CXXR::GCStackRoot<> call;       /* The call that effected this context*/
	CXXR::GCStackRoot<> cloenv;	/* The environment */
	CXXR::GCStackRoot<> conexit;	/* Interpreted "on.exit" code */
	void (*cend)(void *);	    /* C "on.exit" thunk */
	void *cenddata;		    /* data for C "on.exit" thunk */
	void *vmax;	            /* size of R_alloc stack */
	int intsusp;                /* interrupts are suspended */
	CXXR::GCStackRoot<> handlerstack;  /* condition handler stack */
	CXXR::GCStackRoot<> restartstack;  /* stack of available restarts */
	struct RPRSTACK *prstack;   /* stack of pending promises */
#ifdef BYTECODE
	SEXP *nodestack;
# ifdef BC_INT_STACK
	IStackval *intstack;
# endif
#endif
    };

    typedef RCNTXT* context;

extern "C" {
    /* The Various Context Types.

    * In general the type is a bitwise OR of the values below.
    * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
    * Only functions should have the third bit turned on;
    * this allows us to move up the context stack easily
    * with either RETURN's or GENERIC's or RESTART's.
    * If you add a new context type for functions make sure
    *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
    */
    enum {
	CTXT_TOPLEVEL = 0,
	CTXT_NEXT	  = 1,
	CTXT_BREAK	  = 2,
	CTXT_LOOP	  = 3,	/* break OR next target */
	CTXT_FUNCTION = 4,
	CTXT_CCODE	  = 8,
	CTXT_RETURN	  = 12,
	CTXT_BROWSER  = 16,
	CTXT_GENERIC  = 20,
	CTXT_RESTART  = 32,
	CTXT_BUILTIN  = 64  /* used in profiling */
    };

    /*
      TOP   0 0 0 0 0 0  = 0
      NEX   1 0 0 0 0 0  = 1
      BRE   0 1 0 0 0 0  = 2
      LOO   1 1 0 0 0 0  = 3
      FUN   0 0 1 0 0 0  = 4
      CCO   0 0 0 1 0 0  = 8
      BRO   0 0 0 0 1 0  = 16
      RET   0 0 1 1 0 0  = 12
      GEN   0 0 1 0 1 0  = 20
      RES   0 0 0 0 0 0 1 = 32
      BUI   0 0 0 0 0 0 0 1 = 64
    */

#define IS_RESTART_BIT_SET(flags) ((flags) & CTXT_RESTART)
#define SET_RESTART_BIT_ON(flags) (flags |= CTXT_RESTART)
#define SET_RESTART_BIT_OFF(flags) (flags &= ~CTXT_RESTART)

    LibExtern RCNTXT* R_Toplevel;     /* The ultimate toplevel environment */
    LibExtern RCNTXT* R_ToplevelContext;  /* The toplevel environment */
    LibExtern RCNTXT* R_GlobalContext;    /* The global environment */

    void Rf_begincontext(RCNTXT*, int, SEXP, SEXP, SEXP, SEXP, SEXP);
    SEXP Rf_dynamicfindVar(SEXP, RCNTXT*);
    void Rf_endcontext(RCNTXT*);
    void Rf_findcontext(int, SEXP, SEXP);
    int Rf_framedepth(RCNTXT*);
    void Rf_jump_to_toplevel(void);
    void R_InsertRestartHandlers(RCNTXT *, Rboolean);
    void R_JumpToContext(RCNTXT *, int, SEXP);
    SEXP R_syscall(int,RCNTXT*);
    int R_sysparent(int,RCNTXT*);
    SEXP R_sysframe(int,RCNTXT*);
    SEXP R_sysfunction(int,RCNTXT*);
    void R_run_onexits(RCNTXT *);
    void R_restore_globals(RCNTXT *);

}  /* extern "C" */
#endif  /* __cplusplus */

#endif /* RCNTXT_H */
