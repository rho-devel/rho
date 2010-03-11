/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2006   The R Development Core Team.
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
 *
 *
 *  Contexts:
 *
 *  A linked-list of execution contexts is kept so that control-flow
 *  constructs like "next", "break" and "return" will work.  It is also
 *  used for error returns to top-level.
 *
 *  Contexts are allocated on the stack as the evaluator invokes itself
 *  recursively.  The memory is reclaimed naturally on return through
 *  the recursions.
 *
 *  A context contains the following information (and more):
 *
 *	nextcontext	the next level context
 *	cstacktop	the current level of the pointer protection stack
 *	callflag	the context "type"
 *	call		the call (name of function, or expression to
 *			get the function) that effected this
 *			context if a closure, otherwise often NULL.
 *	callfun		the function, if this was a closure.
 *	cloenv		for closures, the environment of the closure.
 *	sysparent	the environment the closure was called from
 *	conexit		code for on.exit calls, to be executed in cloenv
 *			at exit from the closure (normal or abnormal).
 *	vmax		the current setting of the R_alloc stack
 *	srcref		the srcref at the time of the call
 *
 *  Context types can be one of:
 *
 *	Context::BREAK	target for "break"
 *	Context::NEXT	target for "next"
 *	Context::LOOP	target for either "break" or "next"
 *	Context::RETURN	target for "return" (i.e. a closure)
 *	Context::BROWSER	target for "return" to exit from browser
 *	Context::RESTART	a function call to restart was made inside the
 *			closure.
 *
 *	Code (such as the sys.xxx) that looks for Context::RETURN must also
 *	look for a Context::RESTART and Context::GENERIC.
 *	The mechanism used by restart is to change
 *	the context type; error/errorcall then looks for a RESTART and does
 *	a long jump there if it finds one.
 *
 *  A context is created with a call to
 *
 *	void begincontext(Context *cptr, int flags,
 *			  SEXP syscall, SEXP env, SEXP
 *			  sysp, SEXP promargs, SEXP callfun)
 *
 *  which sets up the context pointed to by cptr in the appropriate way.
 *  When the context goes "out-of-scope" the destructor
 *  restores the previous context.
 *
 *  The non-local jump to a given context takes place in a call to
 *
 *	void findcontext(int mask, SEXP env, SEXP val)
 *
 *  This causes "val" to be stuffed into a globally accessable place and
 *  then a search to take place back through the context list for an
 *  appropriate context.  The kind of context sort is determined by the
 *  value of "mask".  The value of mask should be the logical OR of all
 *  the context types desired.
 *
 *  The value of "mask" is returned as the value of the setjump call at
 *  the level longjumped to.  This is used to distinguish between break
 *  and next actions.
 *
 *  Contexts can be used as a wrapper around functions that create windows
 *  or open files. These can then be shut/closed gracefully if an error
 *  occurs.
 */

// For debugging:
#include <iostream>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

#include "CXXR/CommandTerminated.hpp"
#include "CXXR/Context.hpp"
#include "CXXR/Evaluator.h"
#include "CXXR/JMPException.hpp"

using namespace std;
using namespace CXXR;

/* jumpfun - jump to the named context */

static void jumpfun(Context * cptr, int mask, SEXP val)
{
    R_ReturnedValue = val;
    // cout << __FILE__":" << __LINE__ << " About to throw JMPException("
    //	 << cptr << ", " << mask << ")\n" << flush;
    throw JMPException(cptr, mask);
}


/* begincontext - begin an execution context */

/* begincontext is used in dataentry.c and modules */
void begincontext(Context * cptr, Context::Type flags,
		  SEXP syscall, SEXP env, SEXP sysp,
		  SEXP promargs, SEXP callfun)
{
    cptr->callflag = flags;
    cptr->call = syscall;
    cptr->cloenv = SEXP_downcast<Environment*>(env);
    cptr->sysparent = sysp;
    cptr->promargs = promargs;
    cptr->callfun = callfun;
}


/* findcontext - find the correct context */

void attribute_hidden findcontext(int mask, SEXP env, SEXP val)
{
    Context *cptr;
    cptr = Context::innermost();
    if (mask & Context::LOOP) {		/* break/next */
	for (cptr = Context::innermost();
	     cptr != NULL;
	     cptr = cptr->nextcontext)
	    if (cptr->callflag & Context::LOOP && cptr->cloenv == env )
		jumpfun(cptr, mask, val);
	error(_("no loop to break from, jumping to top level"));
    }
    else {				/* return; or browser */
	for (cptr = Context::innermost();
	     cptr != NULL;
	     cptr = cptr->nextcontext)
	    if ((cptr->callflag & mask) && cptr->cloenv == env)
		jumpfun(cptr, mask, val);
	error(_("no function to return from, jumping to top level"));
    }
}

void attribute_hidden R_JumpToContext(Context *target, int mask, SEXP val)
{
    Context *cptr;
    for (cptr = Context::innermost();
	 cptr != NULL;
	 cptr = cptr->nextcontext)
	if (cptr == target)
	    jumpfun(cptr, mask, val);
    error(_("target context is not on the stack"));
}


/* R_sysframe - look back up the context stack until the */
/* nth closure context and return that cloenv. */
/* R_sysframe(0) means the R_GlobalEnv environment */
/* negative n counts back from the current frame */
/* positive n counts up from the globalEnv */

SEXP attribute_hidden R_sysframe(int n, Context *cptr)
{
    if (n == 0)
	return(R_GlobalEnv);

    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = -n;

    if(n < 0)
	errorcall(Context::innermost()->call,
		  _("not that many frames on the stack"));

    while (cptr) {
	if (cptr->callflag & Context::FUNCTION ) {
	    if (n == 0) {  /* we need to detach the enclosing env */
		return cptr->cloenv;
	    }
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if(n == 0)
	return R_GlobalEnv;
    else
	errorcall(Context::innermost()->call,
		  _("not that many frames on the stack"));
    return R_NilValue;	   /* just for -Wall */
}


/* We need to find the environment that can be returned by sys.frame */
/* (so it needs to be on the cloenv pointer of a context) that matches */
/* the environment where the closure arguments are to be evaluated. */
/* It would be much simpler if sysparent just returned cptr->sysparent */
/* but then we wouldn't be compatible with S. */

int attribute_hidden R_sysparent(int n, Context *cptr)
{
    int j;
    SEXP s;
    if(n <= 0)
	errorcall(Context::innermost()->call,
		  _("only positive values of 'n' are allowed"));
    while (cptr && n > 1) {
	if (cptr->callflag & Context::FUNCTION )
	    n--;
	cptr = cptr->nextcontext;
    }
    /* make sure we're looking at a return context */
    while (cptr && !(cptr->callflag & Context::FUNCTION) )
	cptr = cptr->nextcontext;
    if (!cptr)
	return 0;
    // Foll. 3 lines probably soon redundant in CXXR:
    s = cptr->sysparent;
    if(s == R_GlobalEnv)
	return 0;
    j = 0;
    while (cptr != NULL ) {
	if (cptr->callflag & Context::FUNCTION) {
	    j++;
	    if( cptr->cloenv == s )
		n=j;
	}
	cptr = cptr->nextcontext;
    }
    n = j - n + 1;
    if (n < 0)
	n = 0;
    return n;
}

int attribute_hidden framedepth(Context *cptr)
{
    int nframe = 0;
    while (cptr) {
	if (cptr->callflag & Context::FUNCTION )
	    nframe++;
	cptr = cptr->nextcontext;
    }
    return nframe;
}

SEXP attribute_hidden R_syscall(int n, Context *cptr)
{
    /* negative n counts back from the current frame */
    /* positive n counts up from the globalEnv */
    SEXP result;

    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = - n;
    if(n < 0)
	errorcall(Context::innermost()->call,
		  _("not that many frames on the stack"));
    while (cptr) {
	if (cptr->callflag & Context::FUNCTION ) {
	    if (n == 0) {
	    	PROTECT(result = duplicate(cptr->call));
	    	if (cptr->srcref && !isNull(cptr->srcref))
	    	    setAttrib(result, R_SrcrefSymbol, duplicate(cptr->srcref));
	    	UNPROTECT(1);
	    	return result;
	    } else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    errorcall(Context::innermost()->call, _("not that many frames on the stack"));
    return R_NilValue;	/* just for -Wall */
}

SEXP attribute_hidden R_sysfunction(int n, Context *cptr)
{
    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = - n;
    if (n < 0)
	errorcall(Context::innermost()->call,
		  _("not that many frames on the stack"));
    while (cptr) {
	if (cptr->callflag & Context::FUNCTION ) {
	    if (n == 0)
		return duplicate(cptr->callfun);  /***** do we need to DUP? */
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    errorcall(Context::innermost()->call, _("not that many frames on the stack"));
    return R_NilValue;	/* just for -Wall */
}

/* some real insanity to keep Duncan sane */

/* This should find the caller's environment (it's a .Internal) and
   then get the context of the call that owns the environment.  As it
   is, it will restart the wrong function if used in a promise.
   L.T. */
SEXP attribute_hidden do_restart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Context *cptr;

    checkArity(op, args);

    if( !isLogical(CAR(args)) || LENGTH(CAR(args))!= 1 )
	return(R_NilValue);
    for(cptr = Context::innermost()->nextcontext; cptr;
	    cptr = cptr->nextcontext) {
	if (cptr->callflag & Context::FUNCTION) {
	    SET_RESTART_BIT_ON(cptr->callflag);
	    break;
	}
    }
    if( cptr)
	error(_("no function to restart"));
    return(R_NilValue);
}

/* count how many contexts of the specified type are present on the stack */
/* browser contexts are a bit special because they are transient and for  */
/* any closure context with the debug bit set one will be created; so we  */
/* need to count those as well                                            */
int countContexts(int ctxttype, int browser) {
    int n=0;
    Context *cptr;

    cptr = Context::innermost();
    while( cptr ) {
        if( cptr->callflag == ctxttype ) 
            n++;
        else if( browser ) {
           if(cptr->callflag & Context::FUNCTION && ENV_DEBUG(cptr->cloenv) )
              n++;
        }
        cptr = cptr->nextcontext;
    }
    return n;
}
  
   
/* functions to support looking up information about the browser */
/* contexts that are in the evaluation stack */

SEXP attribute_hidden do_sysbrowser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval=R_NilValue;
    Context *cptr;
    int n;

    checkArity(op, args);
    n = asInteger(CAR(args));
    if(n < 1 ) error(_("number of contexts must be positive"));

    /* first find the closest  browser context */
    cptr = Context::innermost();
    while (cptr) {
        if (cptr->callflag == Context::BROWSER) {
                break;
        }
        cptr = cptr->nextcontext;
    }
    /* error if not a browser context */

    if( !cptr )
        error(_("no browser context to query"));

    switch (PRIMVAL(op)) {
    case 1: /* text */
    case 2: /* condition */
        /* first rewind to the right place if needed */
        /* note we want n>1, as we have already      */
        /* rewound to the first context              */
        if( n > 1 ) {
           while (cptr && n > 0 ) {
               if (cptr->callflag == Context::BROWSER) {
                   n--;
                   break;
               }
               cptr = cptr->nextcontext;
           }
        }
        if( !(cptr->callflag == Context::BROWSER) )
           error(_("not that many calls to browser are active"));

        if( PRIMVAL(op) == 1 )
            rval = CAR(cptr->promargs);
        else
            rval = CADR(cptr->promargs);
        break;
    case 3: /* turn on debugging n levels up */
        while ( cptr && n > 0 ) {
            if (cptr->callflag & Context::FUNCTION) 
                  n--;
            cptr = cptr->nextcontext;
        } 
        if( !(cptr->callflag & Context::FUNCTION) )
           error(_("not that many functions on the call stack"));
        else
           SET_RDEBUG(cptr->cloenv, CXXRTRUE);
        break;
    }
    return(rval);
}

/* An implementation of S's frame access functions. They usually count */
/* up from the globalEnv while we like to count down from the currentEnv. */
/* So if the argument is negative count down if positive count up. */
/* We don't want to count the closure that do_sys is contained in, so the */
/* indexing is adjusted to handle this. */

SEXP attribute_hidden do_sys(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i, n  = -1, nframe;
    SEXP rval, t;
    Context *cptr;

    checkArity(op, args);
    /* first find the context that sys.xxx needs to be evaluated in */
    cptr = Context::innermost();
    t = cptr->sysparent;
    while (cptr) {
	if (cptr->callflag & Context::FUNCTION )
	    if (cptr->cloenv == t)
		break;
	cptr = cptr->nextcontext;
    }

    if (length(args) == 1) n = asInteger(CAR(args));

    switch (PRIMVAL(op)) {
    case 1: /* parent */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "n");
	i = nframe = framedepth(cptr);
	/* This is a pretty awful kludge, but the alternative would be
	   a major redesign of everything... -pd */
	while (n-- > 0)
	    i = R_sysparent(nframe - i + 1, cptr);
	return ScalarInteger(i);
    case 2: /* call */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return R_syscall(n, cptr);
    case 3: /* frame */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return R_sysframe(n, cptr);
    case 4: /* sys.nframe */
	return ScalarInteger(framedepth(cptr));
    case 5: /* sys.calls */
	nframe = framedepth(cptr);
	PROTECT(rval = allocList(nframe));
	t=rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_syscall(i, cptr));
	UNPROTECT(1);
	return rval;
    case 6: /* sys.frames */
	nframe = framedepth(cptr);
	PROTECT(rval = allocList(nframe));
	t = rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_sysframe(i, cptr));
	UNPROTECT(1);
	return rval;
    case 7: /* sys.on.exit */
	if( Context::innermost()->nextcontext != NULL)
	    return Context::innermost()->nextcontext->conexit;
	else
	    return R_NilValue;
    case 8: /* sys.parents */
	nframe = framedepth(cptr);
	rval = allocVector(INTSXP, nframe);
	for(i = 0; i < nframe; i++)
	    INTEGER(rval)[i] = R_sysparent(nframe - i, cptr);
	return rval;
    case 9: /* sys.function */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' value"), "which");
	return(R_sysfunction(n, cptr));
    default:
	error(_("internal error in 'do_sys'"));
	return R_NilValue;/* just for -Wall */
    }
}

SEXP attribute_hidden do_parentframe(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n;
    SEXP t;
    Context *cptr;

    checkArity(op, args);
    t = CAR(args);
    n = asInteger(t);

    if(n == NA_INTEGER || n < 1 )
	error(_("invalid '%s' value"), "n");

    cptr = Context::innermost();
    t = cptr->sysparent;
    while (cptr){
	if (cptr->callflag & Context::FUNCTION ) {
	    if (cptr->cloenv == t)
	    {
		if (n == 1)
		    return cptr->sysparent;
		n--;
		t = cptr->sysparent;
	    }
	}
	cptr = cptr->nextcontext;
    }
    return R_GlobalEnv;
}

/* R_ToplevelExec - call fun(data) within an Evaluator and
   insure that this function cannot be left by an exception.  R errors in
   the call to fun will result in a jump to top level. The return
   value is TRUE if fun returns normally, FALSE if it results in a
   jump to top level. */

Rboolean R_ToplevelExec(void (*fun)(void *), void *data)
{
    volatile SEXP topExp;
    Rboolean result;

    PROTECT(topExp = R_CurrentExpr);

    {
	Evaluator evalr;
	try {
	    fun(data);
	    result = TRUE;
	}
	catch (CommandTerminated) {
	    result = FALSE;
	}
	catch (JMPException& e) {
	    cerr << "CXXR internal error: unexpected JMPException\n";
	    abort();
	}
    }

    R_CurrentExpr = topExp;
    UNPROTECT(1);

    return result;
}



/*
  This is a simple interface for evaluating R expressions
  from C with a guarantee that one will return to the
  point in the code from which the call was made (if it does
  return at all).
  This uses R_TopleveExec to do this.  It is important
  in applications that embed R or wish to make general
  callbacks to R with error handling.

  It is currently hidden with a data structure definition
  and C routine visible only here. The R_tryEval() is the
  only visible aspect. This can be lifted into the header
  files if necessary. (DTL)

  R_tryEval is in Rinternals.h (so public), but not in the API.
 */
typedef struct {
    SEXP expression;
    GCStackRoot<> val;
    SEXP env;
} ProtectedEvalData;

static void
protectedEval(void *d)
{
    ProtectedEvalData *data = static_cast<ProtectedEvalData *>(d);
    SEXP env = R_GlobalEnv;
    if(data->env) {
	env = data->env;
    }
    data->val = eval(data->expression, env);
}

SEXP
R_tryEval(SEXP e, SEXP env, int *ErrorOccurred)
{
    Rboolean ok;
    ProtectedEvalData data;

    data.expression = e;
    data.val = NULL;
    data.env = env;

    ok = R_ToplevelExec(protectedEval, &data);
    if (ErrorOccurred) {
	*ErrorOccurred = (ok == FALSE);
    }
    if (ok == FALSE)
	data.val = NULL;

    return(data.val);
}
