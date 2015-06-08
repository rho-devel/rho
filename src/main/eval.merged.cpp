/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2014	The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

/** @file eval.cpp
 *
 * General evaluation of expressions, including implementation of R flow
 * control constructs, and R profiling.
 */

#undef HASHING

#define R_NO_REMAP

// For debugging:
#include <iostream>

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Fileio.h>
#include "arithmetic.h"
#include "basedecl.h"

#include "CXXR/ArgList.hpp"
#include "CXXR/BailoutContext.hpp"
#include "CXXR/ByteCode.hpp"
#include "CXXR/ClosureContext.hpp"
#include "CXXR/DottedArgs.hpp"
#include "CXXR/GCStackFrameBoundary.hpp"
#include "CXXR/ListFrame.hpp"
#include "CXXR/LoopBailout.hpp"
#include "CXXR/LoopException.hpp"
#include "CXXR/ProvenanceTracker.h"
#include "CXXR/ReturnBailout.hpp"
#include "CXXR/ReturnException.hpp"
#include "CXXR/S3Launcher.hpp"

using namespace std;
using namespace CXXR;

/* BC_PROILFING needs to be enabled at build time. It is not enabled
   by default as enabling it disabled the more efficient threaded code
   implementation of the byte code interpreter. */
#ifdef BC_PROFILING
static Rboolean bc_profiling = FALSE;
#endif

#ifdef R_PROFILING

/* BDR 2000-07-15
   Profiling is now controlled by the R function Rprof(), and should
   have negligible cost when not enabled.
*/

/* A simple mechanism for profiling R code.  When R_PROFILING is
   enabled, eval will write out the call stack every PROFSAMPLE
   microseconds using the SIGPROF handler triggered by timer signals
   from the ITIMER_PROF timer.  Since this is the same timer used by C
   profiling, the two cannot be used together.  Output is written to
   the file PROFOUTNAME.  This is a plain text file.  The first line
   of the file contains the value of PROFSAMPLE.  The remaining lines
   each give the call stack found at a sampling point with the inner
   most function first.

   To enable profiling, recompile eval.c with R_PROFILING defined.  It
   would be possible to selectively turn profiling on and off from R
   and to specify the file name from R as well, but for now I won't
   bother.

   The stack is traced by walking back along the context stack, just
   like the traceback creation in jump_to_toplevel.  One drawback of
   this approach is that it does not show BUILTIN's since they don't
   get a context.  With recent changes to pos.to.env it seems possible
   to insert a context around BUILTIN calls to that they show up in
   the trace.  Since there is a cost in establishing these contexts,
   they are only inserted when profiling is enabled. [BDR: we have since
   also added contexts for the BUILTIN calls to foreign code.]

   One possible advantage of not tracing BUILTIN's is that then
   profiling adds no cost when the timer is turned off.  This would be
   useful if we want to allow profiling to be turned on and off from
   within R.

   One thing that makes interpreting profiling output tricky is lazy
   evaluation.  When an expression f(g(x)) is profiled, lazy
   evaluation will cause g to be called inside the call to f, so it
   will appear as if g is called by f.

   L. T.  */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>		/* for CreateEvent, SetEvent */
# include <process.h>		/* for _beginthread, _endthread */
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <signal.h>
#endif /* not Win32 */

static FILE *R_ProfileOutfile = nullptr;
static int R_Mem_Profiling=0;
static int R_GC_Profiling = 0;                     /* indicates GC profiling */
static int R_Line_Profiling = 0;                   /* indicates line profiling, and also counts the filenames seen (+1) */
static char **R_Srcfiles;			   /* an array of pointers into the filename buffer */
static size_t R_Srcfile_bufcount;                  /* how big is the array above? */
static GCRoot<> R_Srcfiles_buffer = nullptr;              /* a big RAWSXP to use as a buffer for filenames and pointers to them */
static int R_Profiling_Error;		   /* record errors here */

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;
#endif /* Win32 */

/* Careful here!  These functions are called asynchronously, maybe in the middle of GC,
   so don't do any allocations */

/* This does a linear search through the previously recorded filenames.  If
   this one is new, we try to add it.  FIXME:  if there are eventually
   too many files for an efficient linear search, do hashing. */

static int getFilenum(const char* filename) {
    int fnum;

    for (fnum = 0; fnum < R_Line_Profiling-1
		   && strcmp(filename, R_Srcfiles[fnum]); fnum++);

    if (fnum == R_Line_Profiling-1) {
	size_t len = strlen(filename);
	if (fnum >= CXXRCONSTRUCT(int, R_Srcfile_bufcount)) { /* too many files */
	    R_Profiling_Error = 1;
	    return 0;
	}
	if (R_Srcfiles[fnum] - reinterpret_cast<char*>(RAW(R_Srcfiles_buffer)) + len + 1 > length(R_Srcfiles_buffer)) {
	      /* out of space in the buffer */
	    R_Profiling_Error = 2;
	    return 0;
	}
	strcpy(R_Srcfiles[fnum], filename);
	R_Srcfiles[fnum+1] = R_Srcfiles[fnum] + len + 1;
	*(R_Srcfiles[fnum+1]) = '\0';
	R_Line_Profiling++;
    }

    return fnum + 1;
}

/* These, together with sprintf/strcat, are not safe -- we should be
   using snprintf and such and computing needed sizes, but these
   settings are better than what we had. LT */

#define PROFBUFSIZ 10500
#define PROFITEMMAX  500
#define PROFLINEMAX (PROFBUFSIZ - PROFITEMMAX)

/* It would also be better to flush the buffer when it gets full,
   even if the line isn't complete. But this isn't possible if we rely
   on writing all line profiling files first.  With these sizes
   hitting the limit is fairly unlikely, but if we do then the output
   file is wrong. Maybe writing an overflow marker of some sort would
   be better.  LT */

static void lineprof(char* buf, SEXP srcref)
{
    size_t len;
    if (srcref && !Rf_isNull(srcref) && (len = strlen(buf)) < PROFLINEMAX) {
	int fnum, line = Rf_asInteger(srcref);
	SEXP srcfile = Rf_getAttrib(srcref, R_SrcfileSymbol);
	const char *filename;

	if (!srcfile || TYPEOF(srcfile) != ENVSXP) return;
	srcfile = Rf_findVar(Rf_install("filename"), srcfile);
	if (TYPEOF(srcfile) != STRSXP || !length(srcfile)) return;
	filename = CHAR(STRING_ELT(srcfile, 0));

	if ((fnum = getFilenum(filename)))
	    snprintf(buf+len, PROFBUFSIZ - len, "%d#%d ", fnum, line);
    }
}

/* FIXME: This should be done wih a proper configure test, also making
   sure that the pthreads library is linked in. LT */
#ifndef Win32
#if (defined(__APPLE__) || defined(_REENTRANT) || defined(HAVE_OPENMP)) && \
     ! defined(HAVE_PTHREAD)
# define HAVE_PTHREAD
#endif
#ifdef HAVE_PTHREAD
# include <pthread.h>
static pthread_t R_profiled_thread;
# endif
#endif

static void doprof(int sig)  /* sig is ignored in Windows */
{
    char buf[PROFBUFSIZ];
    size_t bigv, smallv, nodes;
    size_t len;
    int prevnum = R_Line_Profiling;

    buf[0] = '\0';

#ifdef Win32
    SuspendThread(MainThread);
#elif defined(HAVE_PTHREAD)
    if (! pthread_equal(pthread_self(), R_profiled_thread)) {
	pthread_kill(R_profiled_thread, sig);
	return;
    }
#endif /* Win32 */

    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if((len = strlen(buf)) < PROFLINEMAX)
		snprintf(buf+len, PROFBUFSIZ - len,
			 ":%lu:%lu:%lu:%lu:", 
			 (unsigned long) smallv, (unsigned long) bigv,
			 (unsigned long) nodes, get_duplicate_counter());
	    reset_duplicate_counter();
    }

// R_gc_running() not (yet) implemented in CXXR:
#if 0
    if (R_GC_Profiling && R_gc_running())
	strcat(buf, "\"<GC>\" ");
#endif

    if (R_Line_Profiling)
	lineprof(buf, R_Srcref);

// CXXR FIXME: not yet adapted for CXXR:
#if 0
    for (Evaluator::Context* cptr = Evaluator::Context::innermost();
	 cptr; cptr = cptr->nextOut()) {
	if (TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < PROFLINEMAX) {
		strcat(buf, "\"");
		strcat(buf, TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
			"<Anonymous>");
		strcat(buf, "\" ");
		if (R_Line_Profiling)
		    lineprof(buf, cptr->srcref);
	    }
	}
    }
#endif

    /* I believe it would be slightly safer to place this _after_ the
       next two bits, along with the signal() call. LT */
#ifdef Win32
    ResumeThread(MainThread);
#endif /* Win32 */

    for (int i = prevnum; i < R_Line_Profiling; i++)
	fprintf(R_ProfileOutfile, "#File %d: %s\n", i, R_Srcfiles[i-1]);

    if(strlen(buf))
	fprintf(R_ProfileOutfile, "%s\n", buf);

#ifndef Win32
    signal(SIGPROF, doprof);
#endif /* not Win32 */

}

#ifdef Win32
/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait);

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof(0);
    }
}
#else /* not Win32 */
static void doprof_null(int sig)
{
    signal(SIGPROF, doprof_null);
}
#endif /* not Win32 */


static void R_EndProfiling(void)
{
#ifdef Win32
    SetEvent(ProfileEvent);
    CloseHandle(MainThread);
#else /* not Win32 */
    struct itimerval itv;

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, nullptr);
    signal(SIGPROF, doprof_null);

#endif /* not Win32 */
    if(R_ProfileOutfile) fclose(R_ProfileOutfile);
    R_ProfileOutfile = nullptr;
    Evaluator::enableProfiling(false);
    if (R_Srcfiles_buffer) {
	R_ReleaseObject(R_Srcfiles_buffer);
	R_Srcfiles_buffer = nullptr;
    }
    if (R_Profiling_Error)
	Rf_warning(_("source files skipped by Rprof; please increase '%s'"),
		R_Profiling_Error == 1 ? "numfiles" : "bufsize");
}

static void R_InitProfiling(SEXP filename, int append, double dinterval,
			    int mem_profiling, int gc_profiling,
			    int line_profiling, int numfiles, int bufsize)
{
#ifndef Win32
    struct itimerval itv;
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();
#endif
    int interval;

    interval = int( 1e6 * dinterval + 0.5);
    if(R_ProfileOutfile != nullptr) R_EndProfiling();
    R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_ProfileOutfile == nullptr)
	Rf_error(_("Rprof: cannot open profile file '%s'"),
	      Rf_translateChar(filename));
    if(mem_profiling)
	fprintf(R_ProfileOutfile, "memory profiling: ");
    if(gc_profiling)
	fprintf(R_ProfileOutfile, "GC profiling: ");
    if(line_profiling)
	fprintf(R_ProfileOutfile, "line profiling: ");
    fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);

    R_Mem_Profiling=mem_profiling;
    if (mem_profiling)
	reset_duplicate_counter();

    R_Profiling_Error = 0;
    R_Line_Profiling = line_profiling;
    R_GC_Profiling = gc_profiling;
    if (line_profiling) {
	/* Allocate a big RAW vector to use as a buffer.  The first len1 bytes are an array of pointers
	   to strings; the actual strings are stored in the second len2 bytes. */
	R_Srcfile_bufcount = numfiles;
	size_t len1 = R_Srcfile_bufcount*sizeof(char *), len2 = bufsize;
	R_PreserveObject( R_Srcfiles_buffer = Rf_allocVector(RAWSXP, len1 + len2) );
 //	memset(RAW(R_Srcfiles_buffer), 0, len1+len2);
	R_Srcfiles = reinterpret_cast<char **>( RAW(R_Srcfiles_buffer));
	R_Srcfiles[0] = reinterpret_cast<char *>(RAW(R_Srcfiles_buffer)) + len1;
	*(R_Srcfiles[0]) = '\0';
    }

#ifdef Win32
    /* need to duplicate to make a real handle */
    DuplicateHandle(Proc, GetCurrentThread(), Proc, &MainThread,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    wait = interval/1000;
    if(!(ProfileEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
       (_beginthread(ProfileThread, 0, &wait) == -1))
	R_Suicide("unable to create profiling thread");
    Sleep(wait/2); /* suspend this thread to ensure that the other one starts */
#else /* not Win32 */
#ifdef HAVE_PTHREAD
    R_profiled_thread = pthread_self();
#endif

    signal(SIGPROF, doprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, nullptr) == -1)
	R_Suicide("setting profile timer failed");
#endif /* not Win32 */
    Evaluator::enableProfiling(true);
}

extern "C"
SEXP do_Rprof(SEXP args)
{
    SEXP filename;
    int append_mode, mem_profiling, gc_profiling, line_profiling;
    double dinterval;
    int numfiles, bufsize;

#ifdef BC_PROFILING
    if (bc_profiling) {
	Rf_warning(_("cannot use R profiling while byte code profiling"));
	return R_NilValue;
    }
#endif
    if (!Rf_isString(filename = CAR(args)) || (LENGTH(filename)) != 1)
	Rf_error(_("invalid '%s' argument"), "filename");
					      args = CDR(args);
    append_mode = Rf_asLogical(CAR(args));       args = CDR(args);
    dinterval = Rf_asReal(CAR(args));            args = CDR(args);
    mem_profiling = Rf_asLogical(CAR(args));     args = CDR(args);
    gc_profiling = Rf_asLogical(CAR(args));      args = CDR(args);
    line_profiling = Rf_asLogical(CAR(args));    args = CDR(args);
    numfiles = Rf_asInteger(CAR(args));	      args = CDR(args);
    if (numfiles < 0)
	Rf_error(_("invalid '%s' argument"), "numfiles");
    bufsize = Rf_asInteger(CAR(args));
    if (bufsize < 0)
	Rf_error(_("invalid '%s' argument"), "bufsize");

    filename = STRING_ELT(filename, 0);
    if (LENGTH(filename))
	R_InitProfiling(filename, append_mode, dinterval, mem_profiling,
			gc_profiling, line_profiling, numfiles, bufsize);
    else
	R_EndProfiling();
    return R_NilValue;
}
#else /* not R_PROFILING */
SEXP do_Rprof(SEXP args)
{
    Rf_error(_("R profiling is not available on this system"));
    return R_NilValue;		/* -Wall */
}
#endif /* not R_PROFILING */

/* NEEDED: A fixup is needed in browser, because it can trap errors,
 *	and currently does not reset the limit to the right value. */

static SEXP forcePromise(SEXP e)
{
<<<<<<< eval.cpp
    Promise* prom = SEXP_downcast<Promise*>(e);
    return prom->force();
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (PRVALUE(e) == R_UnboundValue) {
	RPRSTACK prstack;
	SEXP val;
	if(PRSEEN(e)) {
	    if (PRSEEN(e) == 1)
		errorcall(R_GlobalContext->call,
			  _("promise already under evaluation: recursive default argument reference or earlier problems?"));
	    else warningcall(R_GlobalContext->call,
			     _("restarting interrupted promise evaluation"));
	}
	/* Mark the promise as under evaluation and push it on a stack
	   that can be used to unmark pending promises if a jump out
	   of the evaluation occurs. */
	SET_PRSEEN(e, 1);
	prstack.promise = e;
	prstack.next = R_PendingPromises;
	R_PendingPromises = &prstack;

	val = eval(PRCODE(e), PRENV(e));

	/* Pop the stack, unmark the promise and set its value field.
	   Also set the environment to R_NilValue to allow GC to
	   reclaim the promise environment; this is also useful for
	   fancy games with delayedAssign() */
	R_PendingPromises = prstack.next;
	SET_PRSEEN(e, 0);
	SET_PRVALUE(e, val);
	SET_PRENV(e, R_NilValue);
    }
    return PRVALUE(e);
}

/* Return value of "e" evaluated in "rho". */

/* some places, e.g. deparse2buff, call this with a promise and rho = NULL */
SEXP eval(SEXP e, SEXP rho)
{
    SEXP op, tmp;
    static int evalcount = 0;

    if (!rho)
	error("'rho' cannot be C NULL: detected in C-level eval");
    if (!isEnvironment(rho)) 
	error("'rho' must be an environment not %s: detected in C-level eval", 
	      type2char(TYPEOF(rho)));

    /* Save the current srcref context. */

    SEXP srcrefsave = R_Srcref;

    /* The use of depthsave below is necessary because of the
       possibility of non-local returns from evaluation.  Without this
       an "expression too complex error" is quite likely. */

    int depthsave = R_EvalDepth++;

    /* We need to explicit set a NULL call here to circumvent attempts
       to deparse the call in the error-handler */
    if (R_EvalDepth > R_Expressions) {
	R_Expressions = R_Expressions_keep + 500;
	errorcall(R_NilValue,
		  _("evaluation nested too deeply: infinite recursion / options(expressions=)?"));
    }
    R_CheckStack();
    if (++evalcount > 1000) { /* was 100 before 2.8.0 */
	R_CheckUserInterrupt();
	evalcount = 0 ;
    }

    tmp = R_NilValue;		/* -Wall */
#ifdef Win32
    /* This is an inlined version of Rwin_fpreset (src/gnuwin/extra.c)
       and resets the precision, rounding and exception modes of a ix86
       fpu.
     */
    __asm__ ( "fninit" );
#endif

    R_Visible = TRUE;
    switch (TYPEOF(e)) {
    case NILSXP:
    case LISTSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
    case S4SXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case ENVSXP:
    case CLOSXP:
    case VECSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case EXPRSXP:
	tmp = e;
	/* Make sure constants in expressions are NAMED before being
	   used as values.  Setting NAMED to 2 makes sure weird calls
	   to replacement functions won't modify constants in
	   expressions.  */
	if (NAMED(tmp) != 2) SET_NAMED(tmp, 2);
	break;
    case BCODESXP:
	tmp = bcEval(e, rho, TRUE);
	    break;
    case SYMSXP:
	if (e == R_DotsSymbol)
	    error(_("'...' used in an incorrect context"));
	if( DDVAL(e) )
		tmp = ddfindVar(e,rho);
	else
		tmp = findVar(e, rho);
	if (tmp == R_UnboundValue)
	    error(_("object '%s' not found"), CHAR(PRINTNAME(e)));
	/* if ..d is missing then ddfindVar will signal */
	else if (tmp == R_MissingArg && !DDVAL(e) ) {
	    const char *n = CHAR(PRINTNAME(e));
	    if(*n) error(_("argument \"%s\" is missing, with no default"),
			 CHAR(PRINTNAME(e)));
	    else error(_("argument is missing, with no default"));
	}
	else if (TYPEOF(tmp) == PROMSXP) {
	    if (PRVALUE(tmp) == R_UnboundValue) {
		/* not sure the PROTECT is needed here but keep it to
		   be on the safe side. */
		PROTECT(tmp);
		tmp = forcePromise(tmp);
		UNPROTECT(1);
	    }
	    else tmp = PRVALUE(tmp);
	    SET_NAMED(tmp, 2);
	}
	else if (!isNull(tmp) && NAMED(tmp) < 1)
	    SET_NAMED(tmp, 1);
	break;
    case PROMSXP:
	if (PRVALUE(e) == R_UnboundValue)
	    /* We could just unconditionally use the return value from
	       forcePromise; the test avoids the function call if the
	       promise is already evaluated. */
	    forcePromise(e);
	tmp = PRVALUE(e);
	/* This does _not_ change the value of NAMED on the value tmp,
	   in contrast to the handling of promises bound to symbols in
	   the SYMSXP case above.  The reason is that one (typically
	   the only) place promises appear in source code is as
	   wrappers for the RHS value in replacement function calls for
	   complex assignment expression created in applydefine().  If
	   the RHS value is freshly created it will have NAMED = 0 and
	   we want it to stay that way or a BUILTIN or SPECIAL
	   replacement function might have to duplicate the value
	   before inserting it to avoid creating cycles.  (Closure
	   replacement functions will get the value via the SYMSXP case
	   from evaluating their 'value' argument so the value will
	   end up getting duplicated if NAMED = 2.) LT */
	break;
    case LANGSXP:
	if (TYPEOF(CAR(e)) == SYMSXP)
	    /* This will throw an error if the function is not found */
	    PROTECT(op = findFun(CAR(e), rho));
	else
	    PROTECT(op = eval(CAR(e), rho));

	if(RTRACE(op) && R_current_trace_state()) {
	    Rprintf("trace: ");
	    PrintValue(e);
	}
	if (TYPEOF(op) == SPECIALSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    PROTECT(CDR(e));
	    R_Visible = flag != 1;
	    tmp = PRIMFUN(op) (e, op, CDR(e), rho);
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		if(strcmp(nm, "for")
		   && strcmp(nm, "repeat") && strcmp(nm, "while")
		   && strcmp(nm, "[[<-") && strcmp(nm, "on.exit"))
		    printf("vis: special %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == BUILTINSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    RCNTXT cntxt;
	    PROTECT(tmp = evalList(CDR(e), rho, e, 0));
	    if (flag < 2) R_Visible = flag != 1;
	    /* We used to insert a context only if profiling,
	       but helps for tracebacks on .C etc. */
	    if (R_Profiling || (PPINFO(op).kind == PP_FOREIGN)) {
		SEXP oldref = R_Srcref;
		begincontext(&cntxt, CTXT_BUILTIN, e,
			     R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
		R_Srcref = NULL;
		tmp = PRIMFUN(op) (e, op, tmp, rho);
		R_Srcref = oldref;
		endcontext(&cntxt);
	    } else {
		tmp = PRIMFUN(op) (e, op, tmp, rho);
	    }
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		printf("vis: builtin %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == CLOSXP) {
	    PROTECT(tmp = promiseArgs(CDR(e), rho));
	    tmp = applyClosure(e, op, tmp, rho, R_BaseEnv);
	    UNPROTECT(1);
	}
	else
	    error(_("attempt to apply non-function"));
	UNPROTECT(1);
	break;
    case DOTSXP:
	error(_("'...' used in an incorrect context"));
    default:
	UNIMPLEMENTED_TYPE("eval", e);
    }
    R_EvalDepth = depthsave;
    R_Srcref = srcrefsave;
    return (tmp);
=======
    if (PRVALUE(e) == R_UnboundValue) {
	RPRSTACK prstack;
	SEXP val;
	if(PRSEEN(e)) {
	    if (PRSEEN(e) == 1)
		errorcall(R_GlobalContext->call,
			  _("promise already under evaluation: recursive default argument reference or earlier problems?"));
	    else warningcall(R_GlobalContext->call,
			     _("restarting interrupted promise evaluation"));
	}
	/* Mark the promise as under evaluation and push it on a stack
	   that can be used to unmark pending promises if a jump out
	   of the evaluation occurs. */
	SET_PRSEEN(e, 1);
	prstack.promise = e;
	prstack.next = R_PendingPromises;
	R_PendingPromises = &prstack;

	val = eval(PRCODE(e), PRENV(e));

	/* Pop the stack, unmark the promise and set its value field.
	   Also set the environment to R_NilValue to allow GC to
	   reclaim the promise environment; this is also useful for
	   fancy games with delayedAssign() */
	R_PendingPromises = prstack.next;
	SET_PRSEEN(e, 0);
	SET_PRVALUE(e, val);
        SET_NAMED (val, 2);
	SET_PRENV(e, R_NilValue);
    }
    return PRVALUE(e);
}

/* Return value of "e" evaluated in "rho". */

/* some places, e.g. deparse2buff, call this with a promise and rho = NULL */
SEXP eval(SEXP e, SEXP rho)
{
    SEXP op, tmp;
    static int evalcount = 0;

    R_Visible = TRUE;

    /* this is needed even for self-evaluating objects or something like
       'while (TRUE) NULL' will not be interruptable */
    if (++evalcount > 1000) { /* was 100 before 2.8.0 */
	R_CheckUserInterrupt();
#ifndef IMMEDIATE_FINALIZERS
	/* finalizers are run here since this should only be called at
	   points where running arbitrary code should be safe */
	R_RunPendingFinalizers();
#endif
	evalcount = 0 ;
    }

    /* handle self-evluating objects with minimal overhead */
    switch (TYPEOF(e)) {
    case NILSXP:
    case LISTSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
    case S4SXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case ENVSXP:
    case CLOSXP:
    case VECSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case EXPRSXP:
	/* Make sure constants in expressions are NAMED before being
	   used as values.  Setting NAMED to 2 makes sure weird calls
	   to replacement functions won't modify constants in
	   expressions.  */
	if (NAMED(e) <= 1) SET_NAMED(e, 2);
	return e;
    default: break;
    }

    if (!rho)
	error("'rho' cannot be C NULL: detected in C-level eval");
    if (!isEnvironment(rho))
	error("'rho' must be an environment not %s: detected in C-level eval",
	      type2char(TYPEOF(rho)));

    /* Save the current srcref context. */

    SEXP srcrefsave = R_Srcref;

    /* The use of depthsave below is necessary because of the
       possibility of non-local returns from evaluation.  Without this
       an "expression too complex error" is quite likely. */

    int depthsave = R_EvalDepth++;

    /* We need to explicit set a NULL call here to circumvent attempts
       to deparse the call in the error-handler */
    if (R_EvalDepth > R_Expressions) {
	R_Expressions = R_Expressions_keep + 500;
	errorcall(R_NilValue,
		  _("evaluation nested too deeply: infinite recursion / options(expressions=)?"));
    }
    R_CheckStack();

    tmp = R_NilValue;		/* -Wall */
#ifdef Win32
    /* This is an inlined version of Rwin_fpreset (src/gnuwin/extra.c)
       and resets the precision, rounding and exception modes of a ix86
       fpu.
     */
    __asm__ ( "fninit" );
#endif

    switch (TYPEOF(e)) {
    case BCODESXP:
	tmp = bcEval(e, rho, TRUE);
	    break;
    case SYMSXP:
	if (e == R_DotsSymbol)
	    error(_("'...' used in an incorrect context"));
	if( DDVAL(e) )
		tmp = ddfindVar(e,rho);
	else
		tmp = findVar(e, rho);
	if (tmp == R_UnboundValue)
	    error(_("object '%s' not found"), EncodeChar(PRINTNAME(e)));
	/* if ..d is missing then ddfindVar will signal */
	else if (tmp == R_MissingArg && !DDVAL(e) ) {
	    const char *n = CHAR(PRINTNAME(e));
	    if(*n) error(_("argument \"%s\" is missing, with no default"),
			 CHAR(PRINTNAME(e)));
	    else error(_("argument is missing, with no default"));
	}
	else if (TYPEOF(tmp) == PROMSXP) {
	    if (PRVALUE(tmp) == R_UnboundValue) {
		/* not sure the PROTECT is needed here but keep it to
		   be on the safe side. */
		PROTECT(tmp);
		tmp = forcePromise(tmp);
		UNPROTECT(1);
	    }
	    else tmp = PRVALUE(tmp);
	    SET_NAMED(tmp, 2);
	}
	else if (!isNull(tmp) && NAMED(tmp) == 0)
	    SET_NAMED(tmp, 1);
	break;
    case PROMSXP:
	if (PRVALUE(e) == R_UnboundValue)
	    /* We could just unconditionally use the return value from
	       forcePromise; the test avoids the function call if the
	       promise is already evaluated. */
	    forcePromise(e);
	tmp = PRVALUE(e);
	/* This does _not_ change the value of NAMED on the value tmp,
	   in contrast to the handling of promises bound to symbols in
	   the SYMSXP case above.  The reason is that one (typically
	   the only) place promises appear in source code is as
	   wrappers for the RHS value in replacement function calls for
	   complex assignment expression created in applydefine().  If
	   the RHS value is freshly created it will have NAMED = 0 and
	   we want it to stay that way or a BUILTIN or SPECIAL
	   replacement function might have to duplicate the value
	   before inserting it to avoid creating cycles.  (Closure
	   replacement functions will get the value via the SYMSXP case
	   from evaluating their 'value' argument so the value will
	   end up getting duplicated if NAMED = 2.) LT */
	break;
    case LANGSXP:
	if (TYPEOF(CAR(e)) == SYMSXP)
	    /* This will throw an error if the function is not found */
	    PROTECT(op = findFun(CAR(e), rho));
	else
	    PROTECT(op = eval(CAR(e), rho));

	if(RTRACE(op) && R_current_trace_state()) {
	    Rprintf("trace: ");
	    PrintValue(e);
	}
	if (TYPEOF(op) == SPECIALSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    PROTECT(CDR(e));
	    R_Visible = flag != 1;
	    tmp = PRIMFUN(op) (e, op, CDR(e), rho);
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		if(strcmp(nm, "for")
		   && strcmp(nm, "repeat") && strcmp(nm, "while")
		   && strcmp(nm, "[[<-") && strcmp(nm, "on.exit"))
		    printf("vis: special %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == BUILTINSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    RCNTXT cntxt;
	    PROTECT(tmp = evalList(CDR(e), rho, e, 0));
	    if (flag < 2) R_Visible = flag != 1;
	    /* We used to insert a context only if profiling,
	       but helps for tracebacks on .C etc. */
	    if (R_Profiling || (PPINFO(op).kind == PP_FOREIGN)) {
		SEXP oldref = R_Srcref;
		begincontext(&cntxt, CTXT_BUILTIN, e,
			     R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
		R_Srcref = NULL;
		tmp = PRIMFUN(op) (e, op, tmp, rho);
		R_Srcref = oldref;
		endcontext(&cntxt);
	    } else {
		tmp = PRIMFUN(op) (e, op, tmp, rho);
	    }
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		printf("vis: builtin %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == CLOSXP) {
	    PROTECT(tmp = promiseArgs(CDR(e), rho));
	    tmp = applyClosure(e, op, tmp, rho, R_NilValue);
	    UNPROTECT(1);
	}
	else
	    error(_("attempt to apply non-function"));
	UNPROTECT(1);
	break;
    case DOTSXP:
	error(_("'...' used in an incorrect context"));
    default:
	UNIMPLEMENTED_TYPE("eval", e);
    }
    R_EvalDepth = depthsave;
    R_Srcref = srcrefsave;
    return (tmp);
>>>>>>> eval.c
}

attribute_hidden
void Rf_SrcrefPrompt(const char * prefix, SEXP srcref)
{
    /* If we have a valid srcref, use it */
    if (srcref && srcref != R_NilValue) {
	if (TYPEOF(srcref) == VECSXP) srcref = VECTOR_ELT(srcref, 0);
	SEXP srcfile = Rf_getAttrib(srcref, R_SrcfileSymbol);
	if (TYPEOF(srcfile) == ENVSXP) {
	    SEXP filename = Rf_findVar(Rf_install("filename"), srcfile);
	    if (Rf_isString(filename) && length(filename)) {
		Rprintf(_("%s at %s#%d: "), prefix,
			CHAR(STRING_ELT(filename, 0)),
			Rf_asInteger(srcref));
		return;
	    }
	}
    }
    /* default: */
    Rprintf("%s: ", prefix);
}

/* Apply SEXP op of type CLOSXP to actuals */

static void loadCompilerNamespace(void)
{
    SEXP fun, arg, expr;

    PROTECT(fun = Rf_install("getNamespace"));
    PROTECT(arg = Rf_mkString("compiler"));
    PROTECT(expr = Rf_lang2(fun, arg));
    Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(3);
}

static int R_disable_bytecode = 0;

void attribute_hidden R_init_jit_enabled(void)
{
    /* Need to force the lazy loading promise to avoid recursive
       promise evaluation when JIT is enabled. Might be better to do
       this in baseloader.R. */
    eval(install(".ArgsEnv"), R_BaseEnv);

    if (R_jit_enabled <= 0) {
	char *enable = getenv("R_ENABLE_JIT");
	if (enable != nullptr) {
	    int val = atoi(enable);
	    if (val > 0)
		loadCompilerNamespace();
	    R_jit_enabled = val;
	}
    }

    if (R_compile_pkgs <= 0) {
	char *compile = getenv("R_COMPILE_PKGS");
	if (compile != nullptr) {
	    int val = atoi(compile);
	    if (val > 0)
		R_compile_pkgs = TRUE;
	    else
		R_compile_pkgs = FALSE;
	}
    }

    if (R_disable_bytecode <= 0) {
	char *disable = getenv("R_DISABLE_BYTECODE");
	if (disable != nullptr) {
	    int val = atoi(disable);
	    if (val > 0)
		R_disable_bytecode = TRUE;
	    else
		R_disable_bytecode = FALSE;
	}
    }
}

SEXP attribute_hidden R_cmpfun(SEXP fun)
{
    SEXP packsym, funsym, call, fcall, val;

    packsym = Rf_install("compiler");
    funsym = Rf_install("tryCmpfun");

    PROTECT(fcall = Rf_lang3(R_TripleColonSymbol, packsym, funsym));
    PROTECT(call = Rf_lang2(fcall, fun));
    val = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

/* Unused in CXXR pending FIXMEs
static SEXP R_compileExpr(SEXP expr, SEXP rho)
{
    SEXP packsym, funsym, quotesym;
    SEXP qexpr, call, fcall, val;

    packsym = Rf_install("compiler");
    funsym = Rf_install("compile");
    quotesym = Rf_install("quote");

    PROTECT(fcall = Rf_lang3(R_DoubleColonSymbol, packsym, funsym));
    PROTECT(qexpr = Rf_lang2(quotesym, expr));
    PROTECT(call = Rf_lang3(fcall, qexpr, rho));
    val = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
    return val;
}

static SEXP R_compileAndExecute(SEXP call, SEXP rho)
{
    int old_enabled = R_jit_enabled;
    ByteCode* code;
    SEXP val;

    R_jit_enabled = 0;
    PROTECT(call);
    PROTECT(rho);
    PROTECT(code = SEXP_downcast<ByteCode*>(R_compileExpr(call, rho)));
    R_jit_enabled = old_enabled;
    Environment* env = SEXP_downcast<Environment*>(rho);
    val = code->evaluate(env);
    UNPROTECT(3);
    return val;
}
*/

SEXP attribute_hidden do_enablejit(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    int old = R_jit_enabled, newi;
    op->checkNumArgs(num_args, call);
    newi = Rf_asInteger(args[0]);
    if (newi > 0)
	loadCompilerNamespace();
    R_jit_enabled = newi;
    return Rf_ScalarInteger(old);
}

SEXP attribute_hidden do_compilepkgs(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    int old = R_compile_pkgs, newi;
    op->checkNumArgs(num_args, call);
    newi = Rf_asLogical(args[0]);
    if (newi != NA_LOGICAL && newi)
	loadCompilerNamespace();
    R_compile_pkgs = newi;
    return Rf_ScalarLogical(old);
}

/* forward declaration */
static SEXP bytecodeExpr(SEXP);

/* this function gets the srcref attribute from a statement block,
   and confirms it's in the expected format */

static R_INLINE SEXP getBlockSrcrefs(SEXP call)
{
    SEXP srcrefs = Rf_getAttrib(call, R_SrcrefSymbol);
    if (TYPEOF(srcrefs) == VECSXP) return srcrefs;
    return R_NilValue;
}

/* this function extracts one srcref, and confirms the format */
/* It assumes srcrefs has already been validated to be a VECSXP or NULL */

static R_INLINE SEXP getSrcref(SEXP srcrefs, int ind)
{
    SEXP result;
    if (!Rf_isNull(srcrefs)
	&& length(srcrefs) > ind
	&& !Rf_isNull(result = VECTOR_ELT(srcrefs, ind))
	&& TYPEOF(result) == INTSXP
	&& length(result) >= 6)
	return result;
    return R_NilValue;
}

<<<<<<< eval.cpp
void Closure::DebugScope::startDebugging() const
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
SEXP applyClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedenv)
=======
SEXP applyClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedvars)
>>>>>>> eval.c
{
<<<<<<< eval.cpp
    const ClosureContext* ctxt = ClosureContext::innermost();
    const Expression* call = ctxt->call();
    Environment* working_env = ctxt->workingEnvironment();
    working_env->setSingleStepping(true);
    /* switch to interpreted version when debugging compiled code */
    if (m_closure->body()->sexptype() == BCODESXP) {
	Closure* closure = const_cast<Closure*>(m_closure);
	closure->m_body = bytecodeExpr(closure->m_body);
    }
    Rprintf("debugging in: ");
    // Print call:
    {
	int old_bl = R_BrowseLines;
	int blines = Rf_asInteger(Rf_GetOption(Rf_install("deparse.max.lines"),
					       R_BaseEnv));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SEXP formals, actuals, savedrho;
    volatile SEXP body, newrho;
    SEXP f, a, tmp;
    RCNTXT cntxt;

    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals */
    /* arglist = the tagged list of arguments */

    /* protection against rho = NULL */
    // these are deliberately not translated
    if (!rho)
	errorcall(call, 
		  "'rho' cannot be C NULL: detected in C-level applyClosure");
    if (!isEnvironment(rho)) 
	errorcall(call, "'rho' must be an environment not %s: detected in C-level applyClosure", 
		  type2char(TYPEOF(rho)));

    formals = FORMALS(op);
    body = BODY(op);
    savedrho = CLOENV(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    /*  Set up a context with the call in it so error has access to it */

    begincontext(&cntxt, CTXT_RETURN, call, savedrho, rho, arglist, op);

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  Ideally this environment sould be
	hashed.  */

    PROTECT(actuals = matchArgs(formals, arglist, call));
    PROTECT(newrho = NewEnvironment(formals, actuals, savedrho));

    /*  Use the default code for unbound formals.  FIXME: It looks like
	this code should preceed the building of the environment so that
	this will also go into the hash table.  */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    f = formals;
    a = actuals;
    while (f != R_NilValue) {
	if (CAR(a) == R_MissingArg && CAR(f) != R_MissingArg) {
	    SETCAR(a, mkPROMISE(CAR(f), newrho));
	    SET_MISSING(a, 2);
	}
	f = CDR(f);
	a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedenv != R_NilValue) {
	for (tmp = FRAME(suppliedenv); tmp != R_NilValue; tmp = CDR(tmp)) {
	    for (a = actuals; a != R_NilValue; a = CDR(a))
		if (TAG(a) == TAG(tmp))
		    break;
	    if (a == R_NilValue)
		/* Use defineVar instead of earlier version that added
		   bindings manually */
		defineVar(TAG(tmp), CAR(tmp), newrho);
	}
    }

    /*  Terminate the previous context and start a new one with the
	correct environment. */

    endcontext(&cntxt);

    /*  If we have a generic function we need to use the sysparent of
	the generic as the sysparent of the method because the method
	is a straight substitution of the generic.  */

    if( R_GlobalContext->callflag == CTXT_GENERIC )
	begincontext(&cntxt, CTXT_RETURN, call,
		     newrho, R_GlobalContext->sysparent, arglist, op);
    else
	begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist, op);

    /* Get the srcref record from the closure object */

    R_Srcref = getAttrib(op, R_SrcrefSymbol);

    /* The default return value is NULL.  FIXME: Is this really needed
       or do we always get a sensible value returned?  */

    tmp = R_NilValue;

    /* Debugging */

    SET_RDEBUG(newrho, RDEBUG(op) || RSTEP(op));
    if( RSTEP(op) ) SET_RSTEP(op, 0);
    if (RDEBUG(newrho)) {
	int old_bl = R_BrowseLines,
	    blines = asInteger(GetOption1(install("deparse.max.lines")));
	SEXP savesrcref;
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
=======
    SEXP formals, actuals, savedrho;
    volatile SEXP body, newrho;
    SEXP f, a, tmp;
    RCNTXT cntxt;

    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals */
    /* arglist = the tagged list of arguments */

    /* protection against rho = NULL */
    // these are deliberately not translated
    if (!rho)
	errorcall(call,
		  "'rho' cannot be C NULL: detected in C-level applyClosure");
    if (!isEnvironment(rho))
	errorcall(call, "'rho' must be an environment not %s: detected in C-level applyClosure",
		  type2char(TYPEOF(rho)));

    formals = FORMALS(op);
    body = BODY(op);
    savedrho = CLOENV(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    /*  Set up a context with the call in it so error has access to it */

    begincontext(&cntxt, CTXT_RETURN, call, savedrho, rho, arglist, op);

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  Ideally this environment sould be
	hashed.  */

    PROTECT(actuals = matchArgs(formals, arglist, call));
    PROTECT(newrho = NewEnvironment(formals, actuals, savedrho));

    /* Turn on reference counting for the binding cells so local
       assignments arguments increment REFCNT values */
    for (a = actuals; a != R_NilValue; a = CDR(a))
	ENABLE_REFCNT(a);

    /*  Use the default code for unbound formals.  FIXME: It looks like
	this code should preceed the building of the environment so that
	this will also go into the hash table.  */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    f = formals;
    a = actuals;
    while (f != R_NilValue) {
	if (CAR(a) == R_MissingArg && CAR(f) != R_MissingArg) {
	    SETCAR(a, mkPROMISE(CAR(f), newrho));
	    SET_MISSING(a, 2);
	}
	f = CDR(f);
	a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedvars != R_NilValue)
        addMissingVarsToNewEnv(newrho, suppliedvars);

    if (R_envHasNoSpecialSymbols(newrho))
	SET_NO_SPECIAL_SYMBOLS(newrho);

    /*  Terminate the previous context and start a new one with the
        correct environment. */

    endcontext(&cntxt);

    /*  If we have a generic function we need to use the sysparent of
	the generic as the sysparent of the method because the method
	is a straight substitution of the generic.  */

    if( R_GlobalContext->callflag == CTXT_GENERIC )
	begincontext(&cntxt, CTXT_RETURN, call,
		     newrho, R_GlobalContext->sysparent, arglist, op);
    else
	begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist, op);

    /* Get the srcref record from the closure object */

    R_Srcref = getAttrib(op, R_SrcrefSymbol);

    /* The default return value is NULL.  FIXME: Is this really needed
       or do we always get a sensible value returned?  */

    tmp = R_NilValue;

    /* Debugging */

    SET_RDEBUG(newrho, (RDEBUG(op) && R_current_debug_state()) || RSTEP(op)
		     || (RDEBUG(rho) && R_BrowserLastCommand == 's')) ;
    if( RSTEP(op) ) SET_RSTEP(op, 0);
    if (RDEBUG(newrho)) {
	int old_bl = R_BrowseLines,
	    blines = asInteger(GetOption1(install("deparse.max.lines")));
	SEXP savesrcref;
	cntxt.browserfinish = 0; /* Don't want to inherit the "f" */
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
>>>>>>> eval.c
	if(blines != NA_INTEGER && blines > 0)
	    R_BrowseLines = blines;
	Rf_PrintValueRec(const_cast<Expression*>(call), nullptr);
	R_BrowseLines = old_bl;
    }
<<<<<<< eval.cpp
    Rprintf("debug: ");
    Rf_PrintValue(m_closure->m_body);
    do_browser(const_cast<Expression*>(call), const_cast<Closure*>(m_closure),
	       const_cast<PairList*>(ctxt->promiseArgs()), working_env);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }

    endcontext(&cntxt);

    if (RDEBUG(op)) {
	Rprintf("exiting from: ");
	PrintValueRec(call, rho);
    }
    UNPROTECT(3);
    return (tmp);
=======
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }
    cntxt.returnValue = tmp; /* make it available to on.exit */
    endcontext(&cntxt);

    if (RDEBUG(op) && R_current_debug_state()) {
	Rprintf("exiting from: ");
	PrintValueRec(call, rho);
    }
    UNPROTECT(3);
    return (tmp);
>>>>>>> eval.c
}

void Closure::DebugScope::endDebugging() const
{
<<<<<<< eval.cpp
    const ClosureContext* ctxt = ClosureContext::innermost();
    try {
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    volatile SEXP body;
    SEXP tmp;
    RCNTXT cntxt;

    body = BODY(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist, op);

    /* The default return value is NULL.  FIXME: Is this really needed
       or do we always get a sensible value returned?  */

    tmp = R_NilValue;

    /* Debugging */

    SET_RDEBUG(newrho, RDEBUG(op) || RSTEP(op));
    if( RSTEP(op) ) SET_RSTEP(op, 0);
    if (RDEBUG(op)) {
	SEXP savesrcref;
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
	PrintValueRec(call,rho);
	/* Find out if the body is function with only one statement. */
	if (isSymbol(CAR(body)))
	    tmp = findFun(CAR(body), rho);
	else
	    tmp = eval(CAR(body), rho);
	savesrcref = R_Srcref;
	PROTECT(R_Srcref = getSrcref(getBlockSrcrefs(body), 0));
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(body);
	do_browser(call, op, R_NilValue, newrho);
	R_Srcref = savesrcref;
	UNPROTECT(1);
    }

    /*  It isn't completely clear that this is the right place to do
	this, but maybe (if the matchArgs above reverses the
	arguments) it might just be perfect.  */

#ifdef  HASHING
#define HASHTABLEGROWTHRATE  1.2
    {
	SEXP R_NewHashTable(int, double);
	SEXP R_HashFrame(SEXP);
	int nargs = length(arglist);
	HASHTAB(newrho) = R_NewHashTable(nargs, HASHTABLEGROWTHRATE);
	newrho = R_HashFrame(newrho);
    }
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }

    endcontext(&cntxt);

    if (RDEBUG(op)) {
=======
    volatile SEXP body;
    SEXP tmp;
    RCNTXT cntxt;

    body = BODY(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist, op);
/* *** from here on : "Copy-Paste from applyClosure" (~ l.965) above ***/

    /* The default return value is NULL.  FIXME: Is this really needed
       or do we always get a sensible value returned?  */

    tmp = R_NilValue;

    /* Debugging */

    SET_RDEBUG(newrho, (RDEBUG(op) && R_current_debug_state()) || RSTEP(op)
		     || (RDEBUG(rho) && R_BrowserLastCommand == 's')) ;
    if( RSTEP(op) ) SET_RSTEP(op, 0);
    //  RDEBUG(op) .. FIXME? applyClosure has RDEBUG(newrho) which has just been set
    if (RDEBUG(op) && R_current_debug_state()) {
	int old_bl = R_BrowseLines,
	    blines = asInteger(GetOption1(install("deparse.max.lines")));
	SEXP savesrcref;
	cntxt.browserfinish = 0; /* Don't want to inherit the "f" */
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
	if(blines != NA_INTEGER && blines > 0)
	    R_BrowseLines = blines;
	PrintValueRec(call,rho);
	R_BrowseLines = old_bl;

	/* Is the body a bare symbol (PR#6804) */
	if (!isSymbol(body) & !isVectorAtomic(body)){
	/* Find out if the body is function with only one statement. */
	if (isSymbol(CAR(body)))
	    tmp = findFun(CAR(body), rho);
	else
	    tmp = eval(CAR(body), rho);
	}
	savesrcref = R_Srcref;
	PROTECT(R_Srcref = getSrcref(getBlockSrcrefs(body), 0));
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(body);
	do_browser(call, op, R_NilValue, newrho);
	R_Srcref = savesrcref;
	UNPROTECT(1);
    }

    /*  It isn't completely clear that this is the right place to do
	this, but maybe (if the matchArgs above reverses the
	arguments) it might just be perfect.  */

#ifdef  HASHING
    {
	SEXP R_NewHashTable(int);
	SEXP R_HashFrame(SEXP);
	int nargs = length(arglist);
	HASHTAB(newrho) = R_NewHashTable(nargs);
	newrho = R_HashFrame(newrho);
    }
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }
    cntxt.returnValue = tmp; /* make it available to on.exit */
    endcontext(&cntxt);

    if (RDEBUG(op) && R_current_debug_state()) {
>>>>>>> eval.c
	Rprintf("exiting from: ");
	int old_bl = R_BrowseLines;
	int blines
	    = Rf_asInteger(Rf_GetOption(Rf_install("deparse.max.lines"),
					R_BaseEnv));
	if(blines != NA_INTEGER && blines > 0)
	    R_BrowseLines = blines;
	Rf_PrintValueRec(const_cast<Expression*>(ctxt->call()), nullptr);
	R_BrowseLines = old_bl;
    }
    // Don't allow exceptions to escape destructor:
    catch (...) {}
}   

/* **** FIXME: Temporary code to execute S4 methods in a way that
   **** preserves lexical scope. */

/* called from methods_list_dispatch.c */
SEXP R_execMethod(SEXP op, SEXP rho)
{
    Closure* func = SEXP_downcast<Closure*>(op);
    Environment* callenv = SEXP_downcast<Environment*>(rho);
    const Frame* fromf = callenv->frame();

    // create a new environment frame enclosed by the lexical
    // environment of the method
    GCStackRoot<Frame> newframe(new ListFrame);
    GCStackRoot<Environment>
	newrho(new Environment(func->environment(), newframe));
    Frame* tof = newrho->frame();

    // Propagate bindings of the formal arguments of the generic to
    // newrho, but replace defaulted arguments with those appropriate
    // to the method:
    func->matcher()->propagateFormalBindings(callenv, newrho);

    /* copy the bindings of the special dispatch variables in the top
       frame of the generic call to the new frame */
    {
	static const Symbol* syms[]
	    = {DotdefinedSymbol, DotMethodSymbol, DottargetSymbol, nullptr};
	for (const Symbol** symp = syms; *symp; ++symp) {
	    tof->importBinding(fromf->binding(*symp));
	}
    }

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    {
	static const Symbol* syms[]
	    = {DotGenericSymbol, DotMethodsSymbol, nullptr};
	for (const Symbol** symp = syms; *symp; ++symp) {
	    const Frame::Binding* frombdg = callenv->findBinding(*symp);
	    tof->importBinding(frombdg);
	}
    }

    /* Find the calling context. */
    ClosureContext* cptr = ClosureContext::innermost();

    /* The calling environment should either be the environment of the
       generic, rho, or the environment of the caller of the generic,
       the current sysparent. */
    Environment* callerenv = cptr->callEnvironment(); /* or rho? */

    // Set up context and perform evaluation.  Note that ans needs to
    // be protected in case the destructor of ClosureContext executes
    // an on.exit function.
    GCStackRoot<> ans;
    {
	ClosureContext ctxt(cptr->call(), callerenv, func,
			    newrho, cptr->promiseArgs());
	ans = func->execute(newrho);
    }
    return ans;
}

static SEXP EnsureLocal(SEXP symbol, SEXP rho)
{
    GCStackRoot<> vl;

    if ((vl = Rf_findVarInFrame3(rho, symbol, TRUE)) != R_UnboundValue) {
	vl = Rf_eval(symbol, rho);	/* for promises */
	if(MAYBE_SHARED(vl)) {
	    PROTECT(vl = shallow_duplicate(vl));
	    Rf_defineVar(symbol, vl, rho);
	    UNPROTECT(1);
	    SET_NAMED(vl, 1);
	}
	return vl;
    }

    vl = Rf_eval(symbol, ENCLOS(rho));
    if (vl == R_UnboundValue)
	Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(symbol)));

    PROTECT(vl = shallow_duplicate(vl));
    Rf_defineVar(symbol, vl, rho);
    UNPROTECT(1);
    SET_NAMED(vl, 1);
    return vl;
}


/* Note: If val is a language object it must be protected */
/* to prevent evaluation.  As an example consider */
/* e <- quote(f(x=1,y=2); names(e) <- c("","a","b") */

static SEXP R_valueSym = NULL; /* initialized in R_initAsignSymbols below */

static SEXP replaceCall(SEXP fun, SEXP val, SEXP args, SEXP rhs)
{
    static Symbol* valuesym = Symbol::obtain("value");
    SEXP tmp, ptmp;
    PROTECT(fun);
    PROTECT(args);
    PROTECT(rhs);
    PROTECT(val);
    GCStackRoot<PairList> tl(PairList::make(length(args) + 2));
    ptmp = tmp = new Expression(nullptr, tl);
    UNPROTECT(4);
    SETCAR(ptmp, fun); ptmp = CDR(ptmp);
    SETCAR(ptmp, val); ptmp = CDR(ptmp);
    while(args != R_NilValue) {
	SETCAR(ptmp, CAR(args));
	SET_TAG(ptmp, TAG(args));
	ptmp = CDR(ptmp);
	args = CDR(args);
    }
    SETCAR(ptmp, rhs);
    SET_TAG(ptmp, valuesym);
    return tmp;
}


static SEXP assignCall(SEXP op, SEXP symbol, SEXP fun,
		       SEXP val, SEXP args, SEXP rhs)
{
    PROTECT(op);
    PROTECT(symbol);
    val = replaceCall(fun, val, args, rhs);
    UNPROTECT(2);
    return Rf_lang3(op, symbol, val);
}


Rboolean asLogicalNoNA(SEXP s, SEXP call)
{
    Rboolean cond = CXXRCONSTRUCT(Rboolean, NA_LOGICAL);

    if (length(s) > 1)
    {
	GCStackRoot<> gc_protect(s);
	Rf_warningcall(call,
		       _("the condition has length > 1 and only the first element will be used"));
    }
    if (length(s) > 0) {
	/* inline common cases for efficiency */
	switch(TYPEOF(s)) {
	case LGLSXP:
	    cond = CXXRCONSTRUCT(Rboolean, LOGICAL(s)[0]);
	    break;
	case INTSXP:
	    cond = CXXRCONSTRUCT(Rboolean, INTEGER(s)[0]); /* relies on NA_INTEGER == NA_LOGICAL */
	    break;
	default:
	    cond = CXXRCONSTRUCT(Rboolean, Rf_asLogical(s));
	}
    }

    if (cond == NA_LOGICAL) {
	char *msg = length(s) ? (Rf_isLogical(s) ?
				 _("missing value where TRUE/FALSE needed") :
				 _("argument is not interpretable as logical")) :
	    _("argument is of length zero");
	Rf_errorcall(call, msg);
    }
    return cond;
}


namespace {
    inline int BodyHasBraces(SEXP body)
    {
	return (Rf_isLanguage(body) && CAR(body) == R_BraceSymbol) ? 1 : 0;
    }

<<<<<<< eval.cpp
    inline void DO_LOOP_RDEBUG(SEXP call, SEXP op, SEXP args, SEXP rho, int bgn)
    {
	if (bgn && ENV_DEBUG(rho)) {
	    Rf_SrcrefPrompt("debug", R_Srcref);
	    Rf_PrintValue(CAR(args));
	    do_browser(call, op, nullptr, rho);
	}
    }

    /* Allocate space for the loop variable value the first time through
       (when v == R_NilValue) and when the value has been assigned to
       another variable (NAMED(v) == 2). This should be safe and avoid
       allocation in many cases. */
    inline RObject* ALLOC_LOOP_VAR(RObject* v, SEXPTYPE val_type)
    {
	if (!v || NAMED(v) == 2)
	    v = Rf_allocVector(val_type, 1);
	return v;
    }

    RObject* propagateBailout(RObject* bailout)
    {
	Evaluator::Context* callctxt
	    = Evaluator::Context::innermost()->nextOut();
	if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT) {
	    static_cast<Bailout*>(bailout)->throwException();
	}
	return bailout;
    }
}
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define DO_LOOP_RDEBUG(call, op, args, rho, bgn) do { \
    if (bgn && RDEBUG(rho)) { \
	SrcrefPrompt("debug", R_Srcref); \
	PrintValue(CAR(args)); \
	do_browser(call, op, R_NilValue, rho); \
    } } while (0)

/* Allocate space for the loop variable value the first time through
   (when v == R_NilValue) and when the value has been assigned to
   another variable (NAMED(v) == 2). This should be safe and avoid
   allocation in many cases. */
#define ALLOC_LOOP_VAR(v, val_type, vpi) do { \
	if (v == R_NilValue || NAMED(v) == 2) { \
	    REPROTECT(v = allocVector(val_type, 1), vpi); \
	    SET_NAMED(v, 1); \
	} \
    } while(0)
=======
/* Allocate space for the loop variable value the first time through
   (when v == R_NilValue) and when the value has been assigned to
   another variable (NAMED(v) > 1). This should be safe and avoid
   allocation in many cases. */
#define ALLOC_LOOP_VAR(v, val_type, vpi) do { \
	if (v == R_NilValue || MAYBE_SHARED(v)) { \
	    REPROTECT(v = allocVector(val_type, 1), vpi); \
	    SET_NAMED(v, 1); \
	} \
    } while(0)
>>>>>>> eval.c

SEXP attribute_hidden do_if(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP Cond, Stmt=R_NilValue;
    int vis=0;

    PROTECT(Cond = Rf_eval(CAR(args), rho));
    if (asLogicalNoNA(Cond, call))
	Stmt = CAR(CDR(args));
    else {
	if (length(args) > 2)
	    Stmt = CAR(CDR(CDR(args)));
	else
	    vis = 1;
    }
<<<<<<< eval.cpp
    if( ENV_DEBUG(rho) && !BodyHasBraces(Stmt)) {
	Rf_SrcrefPrompt("debug", R_Srcref);
	Rf_PrintValue(Stmt);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if( RDEBUG(rho) && !BodyHasBraces(Stmt)) {
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(Stmt);
=======
    if( !vis && ENV_DEBUG(rho) && !BodyHasBraces(Stmt) && !R_GlobalContext->browserfinish) {
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(Stmt);
>>>>>>> eval.c
	do_browser(call, op, R_NilValue, rho);
    }
    UNPROTECT(1);
    if( vis ) {
	R_Visible = FALSE; /* case of no 'else' so return invisible NULL */
	return Stmt;
    }

    {
	RObject* ans;
	{
	    BailoutContext bcntxt;
	    ans = Rf_eval(Stmt, rho);
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    return propagateBailout(ans);
	}
	return ans;
    }
}

static R_INLINE SEXP GET_BINDING_CELL(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return R_NilValue;
    else {
	SEXP loc = (SEXP) R_findVarLocInFrame(rho, symbol);
	return (loc != NULL) ? loc : R_NilValue;
    }
}

static R_INLINE Rboolean SET_BINDING_VALUE(SEXP loc, SEXP value) {
    /* This depends on the current implementation of bindings */
    if (loc != R_NilValue &&
	! BINDING_IS_LOCKED(loc) && ! IS_ACTIVE_BINDING(loc)) {
	if (CAR(loc) != value) {
	    SETCAR(loc, value);
	    if (MISSING(loc))
		SET_MISSING(loc, 0);
	}
	return TRUE;
    }
    else
	return FALSE;
}

SEXP attribute_hidden do_for_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> argsrt(args), rhort(rho);

    Rboolean dbg;
    SEXPTYPE val_type;
    GCStackRoot<> ans, v, val;
    SEXP sym, body;

    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !Rf_isSymbol(sym) ) Rf_errorcall(call, _("non-symbol loop variable"));

    /* CXXR FIXME
    if (R_jit_enabled > 2) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
    */

    val = Rf_eval(val, rho);
    Rf_defineVar(sym, R_NilValue, rho);
    PROTECT(cell = GET_BINDING_CELL(sym, rho));

    /* deal with the case where we are iterating over a factor
       we need to coerce to character - then iterate */

    if( Rf_inherits(val, "factor") ) {
	ans = Rf_asCharacterFactor(val);
	val = ans;
    }

    if (Rf_isList(val) || Rf_isNull(val))
	n = length(val);
    else
	n = LENGTH(val);

    val_type = TYPEOF(val);

    dbg = ENV_DEBUG(rho);
    bgn = BodyHasBraces(body);

    /* bump up NAMED count of sequence to avoid modification by loop code */
    INCREMENT_NAMED(val);
    INCREMENT_REFCNT(val);

    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);
    for (i = 0; i < n; i++) {
	Evaluator::maybeCheckForUserInterrupts();
<<<<<<< eval.cpp
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);
=======
>>>>>>> eval.c

	switch (val_type) {

	case EXPRSXP:
	    /* make sure loop variable is not modified via other vars */
	    SET_NAMED(XVECTOR_ELT(val, i), 2);
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    Rf_defineVar(sym, XVECTOR_ELT(val, i), rho);
	    break;
	case VECSXP:
	    /* make sure loop variable is not modified via other vars */
	    SET_NAMED(VECTOR_ELT(val, i), 2);
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    Rf_defineVar(sym, VECTOR_ELT(val, i), rho);
	    break;

	case LISTSXP:
	    /* make sure loop variable is not modified via other vars */
	    SET_NAMED(CAR(val), 2);
	    Rf_defineVar(sym, CAR(val), rho);
	    val = CDR(val);
	    break;

	default:

	    switch (val_type) {
	    case LGLSXP:
		v = ALLOC_LOOP_VAR(v, val_type);
		LOGICAL(v)[0] = LOGICAL(val)[i];
		break;
	    case INTSXP:
		v = ALLOC_LOOP_VAR(v, val_type);
		INTEGER(v)[0] = INTEGER(val)[i];
		break;
	    case REALSXP:
		v = ALLOC_LOOP_VAR(v, val_type);
		REAL(v)[0] = REAL(val)[i];
		break;
	    case CPLXSXP:
		v = ALLOC_LOOP_VAR(v, val_type);
		COMPLEX(v)[0] = COMPLEX(val)[i];
		break;
	    case STRSXP:
		v = ALLOC_LOOP_VAR(v, val_type);
		SET_STRING_ELT(v, 0, STRING_ELT(val, i));
		break;
	    case RAWSXP:
		v = ALLOC_LOOP_VAR(v, val_type);
		RAW(v)[0] = RAW(val)[i];
		break;
	    default:
		Rf_errorcall(call, _("invalid for() loop sequence"));
	    }
	    Rf_defineVar(sym, v, rho);
	}
	try {
	    BailoutContext bcntxt;
	    ans = Rf_eval(body, rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != env)
		throw;
	    if (lx.next())
		continue;
	    else break;
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    LoopBailout* lbo = dynamic_cast<LoopBailout*>(ans.get());
	    if (lbo) {
		if (lbo->environment() != rho)
		    abort();
		if (lbo->next())
		    continue;
		else break;
	    } else {  // This must be a ReturnBailout:
		SET_ENV_DEBUG(rho, dbg);
		return propagateBailout(ans.get());
	    }
<<<<<<< eval.cpp
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	    defineVar(sym, v, rho);
=======
	    if (CAR(cell) == R_UnboundValue || ! SET_BINDING_VALUE(cell, v))
		defineVar(sym, v, rho);
	}
	if (!bgn && RDEBUG(rho) && !R_GlobalContext->browserfinish) {
	    SrcrefPrompt("debug", R_Srcref);
	    PrintValue(body);
	    do_browser(call, op, R_NilValue, rho);
>>>>>>> eval.c
	}
<<<<<<< eval.cpp
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

	eval(body, rho);

    for_next:
	; /* needed for strict ISO C compliance, according to gcc 2.95.2 */
=======
	eval(body, rho);

    for_next:
	; /* needed for strict ISO C compliance, according to gcc 2.95.2 */
>>>>>>> eval.c
    }
<<<<<<< eval.cpp
    SET_ENV_DEBUG(rho, dbg);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
 for_break:
    endcontext(&cntxt);
    UNPROTECT(4);
    SET_RDEBUG(rho, dbg);
=======
 for_break:
    endcontext(&cntxt);
    DECREMENT_REFCNT(val);
    UNPROTECT(5);
    SET_RDEBUG(rho, dbg);
>>>>>>> eval.c
    return R_NilValue;
}

SEXP attribute_hidden do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() { return do_for_impl(call, op, args, rho); });
}

static SEXP do_while_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    checkArity(op, args);

    /* CXXR FIXME
    if (R_jit_enabled > 2) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
    */

<<<<<<< eval.cpp
    dbg = ENV_DEBUG(rho);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

    dbg = RDEBUG(rho);
=======
    dbg = RDEBUG(rho);
>>>>>>> eval.c
    body = CADR(args);
    bgn = BodyHasBraces(body);

<<<<<<< eval.cpp
    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);

    while (asLogicalNoNA(Rf_eval(CAR(args), rho), call)) {
	Evaluator::maybeCheckForUserInterrupts();
	RObject* ans;
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);
	try {
	    BailoutContext bcntxt;
	    ans = Rf_eval(body, rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != env)
		throw;
	    if (lx.next())
		continue;
	    else break;
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    LoopBailout* lbo = dynamic_cast<LoopBailout*>(ans);
	    if (lbo) {
		if (lbo->environment() != rho)
		    abort();
		if (lbo->next())
		    continue;
		else break;
	    } else {  // This must be a ReturnBailout:
		SET_ENV_DEBUG(rho, dbg);
		return propagateBailout(ans);
	    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
	while (asLogicalNoNA(eval(CAR(args), rho), call)) {
	    DO_LOOP_RDEBUG(call, op, args, rho, bgn);
	    eval(body, rho);
=======
    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
        while (asLogicalNoNA(eval(CAR(args), rho), call)) {
	    if (RDEBUG(rho) && !bgn && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		PrintValue(body);
		do_browser(call, op, R_NilValue, rho);
	    }
	    eval(body, rho);
	    if (RDEBUG(rho) && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		Rprintf("(while) ");
		PrintValue(CAR(args));
		do_browser(call, op, R_NilValue, rho);
	    }
>>>>>>> eval.c
	}
    }
    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}

SEXP attribute_hidden do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() {  return do_while_impl(call, op, args, rho); });
}

static SEXP do_repeat_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
<<<<<<< eval.cpp
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    int dbg;
    volatile int bgn;
=======
    int dbg;
>>>>>>> eval.c
    volatile SEXP body;

    checkArity(op, args);

    /* CXXR FIXME
    if (R_jit_enabled > 2) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
    */

    dbg = ENV_DEBUG(rho);
    body = CAR(args);

<<<<<<< eval.cpp
    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);
    for (;;) {
	Evaluator::maybeCheckForUserInterrupts();
	RObject* ans;
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);
	try {
	    BailoutContext bcntxt;
	    ans = Rf_eval(body, rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != env)
		throw;
	    if (lx.next())
		continue;
	    else break;
	}
	if (ans && ans->sexptype() == BAILSXP) {
	    LoopBailout* lbo = dynamic_cast<LoopBailout*>(ans);
	    if (lbo) {
		if (lbo->environment() != rho)
		    abort();
		if (lbo->next())
		    continue;
		else break;
	    } else {  // This must be a ReturnBailout:
		SET_ENV_DEBUG(rho, dbg);
		return propagateBailout(ans);
	    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
	for (;;) {
	    DO_LOOP_RDEBUG(call, op, args, rho, bgn);
	    eval(body, rho);
=======
    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
	for (;;) {
	    eval(body, rho);
>>>>>>> eval.c
	}
    }

    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}

SEXP attribute_hidden do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() { return do_repeat_impl(call, op, args, rho); });
}

SEXP attribute_hidden do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Environment* env = SEXP_downcast<Environment*>(rho);
    if (!env->loopActive())
	Rf_error(_("no loop to break from"));
    LoopBailout* lbo = new LoopBailout(env, PRIMVAL(op) == 1);
    return propagateBailout(lbo);
}

||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    findcontext(PRIMVAL(op), rho, R_NilValue);
    return R_NilValue;
}

=======
    findcontext(PRIMVAL(op), rho, R_NilValue);
}

>>>>>>> eval.c
RObject* attribute_hidden do_paren(/*const*/ Expression* call,
				   const BuiltInFunction* op,
				   Environment* env,
				   RObject* const* args,
				   int num_args,
				   const PairList* tags)
{
    op->checkNumArgs(num_args, call);
    return args[0];
}

SEXP attribute_hidden do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;
    if (args != R_NilValue) {
	GCStackRoot<> srcrefs(getBlockSrcrefs(call));
	int i = 1;
	while (args != R_NilValue) {
	    R_Srcref = getSrcref(srcrefs, i++);
	    if (ENV_DEBUG(rho) && !R_GlobalContext->browserfinish) {
		Rf_SrcrefPrompt("debug", R_Srcref);
		Rf_PrintValue(CAR(args));
		do_browser(call, op, R_NilValue, rho);
	    }
	    {
		BailoutContext bcntxt;
		s = Rf_eval(CAR(args), rho);
	    }
	    if (s && s->sexptype() == BAILSXP) {
		R_Srcref = nullptr;
		return propagateBailout(s);
	    }
	    args = CDR(args);
	}
	R_Srcref = R_NilValue;
    }
    return s;
}


SEXP attribute_hidden do_return(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> v;

    if (args == R_NilValue) /* zero arguments provided */
	v = R_NilValue;
    else if (CDR(args) == R_NilValue) /* one argument */
	v = Rf_eval(CAR(args), rho);
    else {
	v = R_NilValue; /* to avoid compiler warnings */
	Rf_errorcall(call, _("multi-argument returns are not permitted"));
    }

    Environment* envir = SEXP_downcast<Environment*>(rho);
    if (!envir->canReturn())
	Rf_error(_("no function to return from, jumping to top level"));
    ReturnBailout* rbo = new ReturnBailout(envir, v);
    return propagateBailout(rbo);
}

/* Declared with a variable number of args in names.c */
SEXP attribute_hidden do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> rval;

    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	SET_NAMED(op, 2);
    }
    if (length(args) < 2) WrongArgCount("function");
    SEXP formals = CAR(args);
    if (formals && formals->sexptype() != LISTSXP)
	Rf_error(_("invalid formal argument list for 'function'"));
    rval = Rf_mkCLOSXP(formals, CADR(args), rho);
    SEXP srcref = CADDR(args);
    if (!Rf_isNull(srcref)) Rf_setAttrib(rval, R_SrcrefSymbol, srcref);
    return rval;
}


/*
 *  Assignments for complex LVAL specifications. This is the stuff that
 *  nightmares are made of ...	Note that "evalseq" preprocesses the LHS
 *  of an assignment.  Given an expression, it builds a list of partial
 *  values for the exression.  For example, the assignment x$a[3] <- 10
 *  with LHS x$a[3] yields the (improper) list:
 *
 *	 (Rf_eval(x$a[3])	Rf_eval(x$a)  Rf_eval(x)  .  x)
 *
 *  (Note the terminating symbol).  The partial evaluations are carried
 *  out efficiently using previously computed components.
 */

// CXXR here (necessarily) uses a proper list, with x as the CAR of
// the last element.

/*
  For complex superassignment  x[y==z]<<-w
  we want x required to be nonlocal, y,z, and w permitted to be local or
  nonlocal.
*/

static PairList* evalseq(SEXP expr, SEXP rho, int forcelocal,  R_varloc_t tmploc)
{
    GCStackRoot<> exprrt(expr);
    if (Rf_isNull(expr))
	Rf_error(_("invalid (NULL) left side of assignment"));
    if (Rf_isSymbol(expr)) {
	GCStackRoot<> nval;
	if(forcelocal) {
	    nval = EnsureLocal(expr, rho);
	}
	else {/* now we are down to the target symbol */
	  nval = Rf_eval(expr, ENCLOS(rho));
	}
<<<<<<< eval.cpp
	return PairList::cons(nval, PairList::cons(expr));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	UNPROTECT(1);
	return CONS(nval, expr);
=======
	if (MAYBE_SHARED(nval))
	    nval = shallow_duplicate(nval);
	UNPROTECT(1);
	return CONS_NR(nval, expr);
>>>>>>> eval.c
    }
<<<<<<< eval.cpp
    else if (Rf_isLanguage(expr)) {
	Expression* exprn = static_cast<Expression*>(expr);
	GCStackRoot<PairList> val(evalseq(exprn->tail()->car(), rho, forcelocal, tmploc));
	R_SetVarLocValue(tmploc, val->car());
	GCStackRoot<PairList> nexprargs(PairList::cons(R_GetVarLocSymbol(tmploc),
						       exprn->tail()->tail()));
	GCStackRoot<Expression> nexpr(new Expression(exprn->car(), nexprargs));
	GCStackRoot<> nval(Rf_eval(nexpr, rho));
	return PairList::cons(nval, val);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    else if (isLanguage(expr)) {
	PROTECT(expr);
	PROTECT(val = evalseq(CADR(expr), rho, forcelocal, tmploc));
	R_SetVarLocValue(tmploc, CAR(val));
	PROTECT(nexpr = LCONS(R_GetVarLocSymbol(tmploc), CDDR(expr)));
	PROTECT(nexpr = LCONS(CAR(expr), nexpr));
	nval = eval(nexpr, rho);
	UNPROTECT(4);
	return CONS(nval, val);
=======
    else if (isLanguage(expr)) {
	PROTECT(expr);
	PROTECT(val = evalseq(CADR(expr), rho, forcelocal, tmploc));
	R_SetVarLocValue(tmploc, CAR(val));
	PROTECT(nexpr = LCONS(R_GetVarLocSymbol(tmploc), CDDR(expr)));
	PROTECT(nexpr = LCONS(CAR(expr), nexpr));
	nval = eval(nexpr, rho);
	/* duplicate nval if it might be shared _or_ if the container,
	   CAR(val), has become possibly shared by going through a
	   closure.  This is taken to indicate that the corresponding
	   replacement function might be a closure and will need to
	   see an unmodified LHS value. This heuristic fails if the
	   accessor function called here is not a closure but the
	   replacement function is. */
	if (MAYBE_REFERENCED(nval) &&
	    (MAYBE_SHARED(nval) || MAYBE_SHARED(CAR(val))))
	    nval = shallow_duplicate(nval);
	UNPROTECT(4);
	return CONS_NR(nval, val);
>>>>>>> eval.c
    }
    else Rf_error(_("target of assignment expands to non-language object"));
    return R_NilValue;	/*NOTREACHED*/
}

/* Main entry point for complex assignments */
/* We have checked to see that CAR(args) is a LANGSXP */

static const char * const asym[] = {":=", "<-", "<<-", "="};
#define NUM_ASYM (sizeof(asym) / sizeof(char *))
static SEXP asymSymbol[NUM_ASYM];

static SEXP R_ReplaceFunsTable = NULL;
static SEXP R_SubsetSym = NULL;
static SEXP R_SubassignSym = NULL;
static SEXP R_Subset2Sym = NULL;
static SEXP R_Subassign2Sym = NULL;
static SEXP R_DollarGetsSymbol = NULL;

void attribute_hidden R_initAsignSymbols(void)
{
    for (int i = 0; i < NUM_ASYM; i++)
	asymSymbol[i] = install(asym[i]);

    R_ReplaceFunsTable = R_NewHashedEnv(R_EmptyEnv, ScalarInteger(1099));
    R_PreserveObject(R_ReplaceFunsTable);
}

static R_INLINE SEXP lookupAssignFcnSymbol(SEXP fun)
{
    return findVarInFrame(R_ReplaceFunsTable, fun);
}

static void enterAssignFcnSymbol(SEXP fun, SEXP val)
{
    defineVar(fun, val, R_ReplaceFunsTable);
}

/* This macro stores the current assignment target in the saved
   binding location. It duplicates if necessary to make sure
   replacement functions are always called with a target with NAMED ==
   1. The SET_CAR is intended to protect against possible GC in
   R_SetVarLocValue; this might occur it the binding is an active
   binding. */
#define SET_TEMPVARLOC_FROM_CAR(loc, lhs) do { \
	SEXP __lhs__ = (lhs); \
	SEXP __v__ = CAR(__lhs__); \
<<<<<<< eval.cpp
	if (NAMED(__v__) == 2) { \
	    __v__ = Rf_duplicate(__v__); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (NAMED(__v__) == 2) { \
	    __v__ = duplicate(__v__); \
=======
	if (MAYBE_SHARED(__v__)) { \
	    __v__ = shallow_duplicate(__v__); \
>>>>>>> eval.c
	    SET_NAMED(__v__, 1); \
	    SETCAR(__lhs__, __v__); \
	} \
	R_SetVarLocValue(loc, __v__); \
    } while(0)

/* This macro makes sure the RHS NAMED value is 0 or 2. This is
   necessary to make sure the RHS value returned by the assignment
   expression is correct when the RHS value is part of the LHS
   object. */
#define FIXUP_RHS_NAMED(r) do { \
	SEXP __rhs__ = (r); \
	if (NAMED(__rhs__) && NAMED(__rhs__) <= 1) \
	    SET_NAMED(__rhs__, 2); \
    } while (0)

<<<<<<< eval.cpp
// Functions and data auxiliary to applydefine():
namespace {
    std::map<const Symbol*, Symbol*> sym2replac;

    // Given a Symbol "foo", this function returns the Symbol "foo<-":
    Symbol* func2ReplacementFunc(const Symbol* fsym)
    {
	Symbol* ans = sym2replac[fsym];
	if (!ans) {
	    std::string replacname = fsym->name()->stdstring() + "<-";
	    ans = sym2replac[fsym] = Symbol::obtain(replacname);
	}
	return ans;
    }

    std::vector<Symbol*> obtainAssignSyms()
    {
	std::vector<Symbol*> ans(4);
	for (size_t i = 0; i < 4; ++i)
	    ans[i] = Symbol::obtain(asym[i]);
	return ans;
    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define ASSIGNBUFSIZ 32
static R_INLINE SEXP installAssignFcnName(SEXP fun)
{
    char buf[ASSIGNBUFSIZ];
    if(strlen(CHAR(PRINTNAME(fun))) + 3 > ASSIGNBUFSIZ)
	error(_("overlong name in '%s'"), CHAR(PRINTNAME(fun)));
    sprintf(buf, "%s<-", CHAR(PRINTNAME(fun)));
    return install(buf);
=======
#define ASSIGNBUFSIZ 32
static SEXP installAssignFcnSymbol(SEXP fun)
{
    char buf[ASSIGNBUFSIZ];

    /* install the symbol */
    if(strlen(CHAR(PRINTNAME(fun))) + 3 > ASSIGNBUFSIZ)
	error(_("overlong name in '%s'"), EncodeChar(PRINTNAME(fun)));
    sprintf(buf, "%s<-", CHAR(PRINTNAME(fun)));
    SEXP val = install(buf);

    enterAssignFcnSymbol(fun, val);
    return val;
}

static R_INLINE SEXP getAssignFcnSymbol(SEXP fun)
{
    /* handle [<-, [[<-, and $<- efficiently */
    if (fun == R_SubsetSym)
	return R_SubassignSym;
    else if (fun == R_Subset2Sym)
	return R_Subassign2Sym;
    else if (fun == R_DollarSymbol)
	return R_DollarGetsSymbol;

    /* look up in the replacement functions table */
    SEXP val = lookupAssignFcnSymbol(fun);
    if (val != R_UnboundValue)
	return val;

    /* instal symbol, entern in table,  and return */
    return installAssignFcnSymbol(fun);
}

static R_INLINE SEXP mkRHSPROMISE(SEXP expr, SEXP rhs)
{
    return R_mkEVPROMISE_NR(expr, rhs);
>>>>>>> eval.c
}

// applydefine() handles the case where the left-hand side of an
// assignment is not a symbol: specifically the cases where it is an
// Expression (LANGSXP), or null (which gives an error).
//
// Section 13.5 of the 'yellow book' (Chambers' Software for Data
// Analysis) gives useful background.

static SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> expr(CAR(args));
    SEXP functor = CAR(expr);

    /*  It's important that the rhs get evaluated first because
	assignment is right associative i.e.  a <- b <- c is parsed as
	a <- (b <- c).  */

<<<<<<< eval.cpp
    GCStackRoot<> rhs, saverhs;
    saverhs = rhs = Rf_eval(CADR(args), rho);

    if (PRIMVAL(op) == 2)
	Environment::monitorLeaks(rhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    PROTECT(saverhs = rhs = eval(CADR(args), rho));
=======
    PROTECT(saverhs = rhs = eval(CADR(args), rho));
    INCREMENT_REFCNT(saverhs);
>>>>>>> eval.c

    /*  FIXME: We need to ensure that this works for hashed
	environments.  This code only works for unhashed ones.  the
	syntax error here is a deliberate marker so I don't forget that
	this needs to be done.  The code used in "missing" will help
	here.  */

    /*  FIXME: This strategy will not work when we are working in the
	data frame defined by the system hash table.  The structure there
	is different.  Should we special case here?  */

    /*  We need a temporary variable to hold the intermediate values
	in the computation.  For efficiency reasons we record the
	location where this variable is stored.  We need to protect
	the location in case the biding is removed from its
	environment by user code or an assignment within the
	assignment arguments */

    /*  There are two issues with the approach here:

	    A complex assignment within a complex assignment, like
	    f(x, y[] <- 1) <- 3, can cause the value temporary
	    variable for the outer assignment to be overwritten and
	    then removed by the inner one.  This could be addressed by
	    using multiple temporaries or using a promise for this
	    variable as is done for the RHS.  Printing of the
	    replacement function call in error messages might then need
	    to be adjusted.

	    With assignments of the form f(g(x, z), y) <- w the value
	    of 'z' will be computed twice, once for a call to g(x, z)
	    and once for the call to the replacement function g<-.  It
	    might be possible to address this by using promises.
	    Using more temporaries would not work as it would mess up
	    replacement functions that use substitute and/or
	    nonstandard evaluation (and there are packages that do
	    that -- igraph is one).

	    LT */

    FIXUP_RHS_NAMED(rhs);

    if (rho == R_BaseNamespace)
	Rf_errorcall(call, _("cannot do complex assignments in base namespace"));
    if (rho == R_BaseEnv)
<<<<<<< eval.cpp
	Rf_errorcall(call, _("cannot do complex assignments in base environment"));
    Rf_defineVar(R_TmpvalSymbol, R_NilValue, rho);
    R_varloc_t tmploc = R_findVarLocInFrame(rho, R_TmpvalSymbol);
    /* Now use try-catch to remove it when we are done, even in the
     * case of an error.  This all helps Rf_error() provide a better call.
     */
    try {
	/*  Do a partial evaluation down through the LHS. */
	GCStackRoot<> lhs(evalseq(CADR(expr), rho,
				  PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc));

	GCStackRoot<> rhsprom(Rf_mkPROMISE(CADR(args), rho));
	SET_PRVALUE(rhsprom, rhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	errorcall(call, _("cannot do complex assignments in base environment"));
    defineVar(R_TmpvalSymbol, R_NilValue, rho);
    PROTECT((SEXP) (tmploc = R_findVarLocInFrame(rho, R_TmpvalSymbol)));
=======
	errorcall(call, _("cannot do complex assignments in base environment"));
    defineVar(R_TmpvalSymbol, R_NilValue, rho);
    PROTECT((SEXP) (tmploc = R_findVarLocInFrame(rho, R_TmpvalSymbol)));
    DISABLE_REFCNT((SEXP) tmploc);
    DECREMENT_REFCNT(CDR((SEXP) tmploc));
>>>>>>> eval.c

<<<<<<< eval.cpp
	SEXP firstarg = CADR(expr);
	while (Rf_isLanguage(firstarg)) {
	    GCStackRoot<> tmp;
	    if (TYPEOF(functor) == SYMSXP)
		tmp = func2ReplacementFunc(static_cast<Symbol*>(functor));
	    else {
		/* check for and handle assignments of the form
		   foo::bar(x) <- y or foo:::bar(x) <- y */
		if (TYPEOF(functor) == LANGSXP) {
		    SEXP funchead = CAR(functor);
		    if ((funchead == R_DoubleColonSymbol
			 || funchead == R_TripleColonSymbol)
			&& length(functor) == 3) {
			SEXP arg2 = CADDR(functor);
			if (TYPEOF(arg2) == SYMSXP) {
			    const Symbol* fsym = static_cast<Symbol*>(arg2);
			    tmp = func2ReplacementFunc(fsym);
			    tmp = Rf_lang3(funchead, CADR(functor), tmp);
			}
		    }
		}
		if (!tmp)
		    Rf_error(_("invalid function in complex assignment"));
	    }
	    SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
	    rhs = replaceCall(tmp, R_TmpvalSymbol, CDDR(expr), rhsprom);
	    rhs = Rf_eval(rhs, rho);
	    SET_PRVALUE(rhsprom, rhs);
	    // Try doing without this in CXXR:
	    // SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	    lhs = CDR(lhs);
	    expr = firstarg;
	    functor = CAR(expr);
	    firstarg = CADR(expr);
	}
	GCStackRoot<> afun;
	if (TYPEOF(functor) == SYMSXP)
	    afun = func2ReplacementFunc(static_cast<Symbol*>(functor));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    /* Now set up a context to remove it when we are done, even in the
     * case of an error.  This all helps error() provide a better call.
     */
    begincontext(&cntxt, CTXT_CCODE, call, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &tmp_cleanup;
    cntxt.cenddata = rho;

    /*  Do a partial evaluation down through the LHS. */
    lhs = evalseq(CADR(expr), rho,
		  PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc);

    PROTECT(lhs);
    PROTECT(rhsprom = mkPROMISE(CADR(args), rho));
    SET_PRVALUE(rhsprom, rhs);

    while (isLanguage(CADR(expr))) {
	nprot = 1; /* the PROTECT of rhs below from this iteration */
	if (TYPEOF(CAR(expr)) == SYMSXP)
	    tmp = installAssignFcnName(CAR(expr));
=======
    /* Now set up a context to remove it when we are done, even in the
     * case of an error.  This all helps error() provide a better call.
     */
    begincontext(&cntxt, CTXT_CCODE, call, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &tmp_cleanup;
    cntxt.cenddata = rho;

    /*  Do a partial evaluation down through the LHS. */
    lhs = evalseq(CADR(expr), rho,
		  PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc);

    PROTECT(lhs);
    PROTECT(rhsprom = mkRHSPROMISE(CADR(args), rhs));

    while (isLanguage(CADR(expr))) {
	nprot = 1; /* the PROTECT of rhs below from this iteration */
	if (TYPEOF(CAR(expr)) == SYMSXP)
	    tmp = getAssignFcnSymbol(CAR(expr));
>>>>>>> eval.c
	else {
	    /* check for and handle assignments of the form
	       foo::bar(x) <- y or foo:::bar(x) <- y */
<<<<<<< eval.cpp
	    if (TYPEOF(functor) == LANGSXP) {
		SEXP funchead = CAR(functor);
		if ((funchead == R_DoubleColonSymbol
		     || funchead == R_TripleColonSymbol)
		    && length(functor) == 3) {
		    SEXP arg2 = CADDR(functor);
		    if (TYPEOF(arg2) == SYMSXP) {
			const Symbol* fsym = static_cast<Symbol*>(arg2);
			afun = func2ReplacementFunc(fsym);
			afun = Rf_lang3(funchead, CADR(functor), afun);
		    }
		}
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	    tmp = R_NilValue; /* avoid uninitialized variable warnings */
	    if (TYPEOF(CAR(expr)) == LANGSXP &&
		(CAR(CAR(expr)) == R_DoubleColonSymbol ||
		 CAR(CAR(expr)) == R_TripleColonSymbol) &&
		length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
		tmp = installAssignFcnName(CADDR(CAR(expr)));
		PROTECT(tmp = lang3(CAAR(expr), CADR(CAR(expr)), tmp));
		nprot++;
=======
	    tmp = R_NilValue; /* avoid uninitialized variable warnings */
	    if (TYPEOF(CAR(expr)) == LANGSXP &&
		(CAR(CAR(expr)) == R_DoubleColonSymbol ||
		 CAR(CAR(expr)) == R_TripleColonSymbol) &&
		length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
		tmp = getAssignFcnSymbol(CADDR(CAR(expr)));
		PROTECT(tmp = lang3(CAAR(expr), CADR(CAR(expr)), tmp));
		nprot++;
>>>>>>> eval.c
	    }
	    if (!afun)
		Rf_error(_("invalid function in complex assignment"));
	}
	SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
<<<<<<< eval.cpp
	static std::vector<Symbol*> assignsyms = obtainAssignSyms();
	// Second arg in the foll. changed in CXXR at r253 (2008-03-18):
	expr = assignCall(assignsyms[PRIMVAL(op)], CADR(lhs),
			  afun, R_TmpvalSymbol, CDDR(expr), rhsprom);
	expr = Rf_eval(expr, rho);
    }
    catch (...) {
	Rf_unbindVar(R_TmpvalSymbol, rho);
	throw;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	PROTECT(rhs = replaceCall(tmp, R_TmpvalSymbol, CDDR(expr), rhsprom));
	rhs = eval(rhs, rho);
	SET_PRVALUE(rhsprom, rhs);
	SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	UNPROTECT(nprot);
	lhs = CDR(lhs);
	expr = CADR(expr);
    }
    nprot = 5; /* the commont case */
    if (TYPEOF(CAR(expr)) == SYMSXP)
	afun = installAssignFcnName(CAR(expr));
    else {
	/* check for and handle assignments of the form
	   foo::bar(x) <- y or foo:::bar(x) <- y */
	afun = R_NilValue; /* avoid uninitialized variable warnings */
	if (TYPEOF(CAR(expr)) == LANGSXP &&
	    (CAR(CAR(expr)) == R_DoubleColonSymbol ||
	     CAR(CAR(expr)) == R_TripleColonSymbol) &&
	    length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
	    afun = installAssignFcnName(CADDR(CAR(expr)));
	    PROTECT(afun = lang3(CAAR(expr), CADR(CAR(expr)), afun));
	    nprot++;
	}
	else
	    error(_("invalid function in complex assignment"));
=======
	PROTECT(rhs = replaceCall(tmp, R_TmpvalSymbol, CDDR(expr), rhsprom));
	rhs = eval(rhs, rho);
	SET_PRVALUE(rhsprom, rhs);
	SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	UNPROTECT(nprot);
	lhs = CDR(lhs);
	expr = CADR(expr);
    }
    nprot = 5; /* the commont case */
    if (TYPEOF(CAR(expr)) == SYMSXP)
	afun = getAssignFcnSymbol(CAR(expr));
    else {
	/* check for and handle assignments of the form
	   foo::bar(x) <- y or foo:::bar(x) <- y */
	afun = R_NilValue; /* avoid uninitialized variable warnings */
	if (TYPEOF(CAR(expr)) == LANGSXP &&
	    (CAR(CAR(expr)) == R_DoubleColonSymbol ||
	     CAR(CAR(expr)) == R_TripleColonSymbol) &&
	    length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
	    afun = getAssignFcnSymbol(CADDR(CAR(expr)));
	    PROTECT(afun = lang3(CAAR(expr), CADR(CAR(expr)), afun));
	    nprot++;
	}
	else
	    error(_("invalid function in complex assignment"));
>>>>>>> eval.c
    }
<<<<<<< eval.cpp

    Rf_unbindVar(R_TmpvalSymbol, rho);
#ifdef CONSERVATIVE_COPYING /* not default */
    return Rf_duplicate(saverhs);
#else
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
    PROTECT(expr = assignCall(install(asym[PRIMVAL(op)]), CDR(lhs),
			      afun, R_TmpvalSymbol, CDDR(expr), rhsprom));
    expr = eval(expr, rho);
    UNPROTECT(nprot);
    endcontext(&cntxt); /* which does not run the remove */
    unbindVar(R_TmpvalSymbol, rho);
#ifdef CONSERVATIVE_COPYING /* not default */
    return duplicate(saverhs);
#else
=======
    SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
    PROTECT(expr = assignCall(asymSymbol[PRIMVAL(op)], CDR(lhs),
			      afun, R_TmpvalSymbol, CDDR(expr), rhsprom));
    expr = eval(expr, rho);
    UNPROTECT(nprot);
    endcontext(&cntxt); /* which does not run the remove */
    unbindVar(R_TmpvalSymbol, rho);
#ifdef OLD_RHS_NAMED
>>>>>>> eval.c
    /* we do not duplicate the value, so to be conservative mark the
       value as NAMED = 2 */
    SET_NAMED(saverhs, 2);
#else
    INCREMENT_NAMED(saverhs);
#endif
    DECREMENT_REFCNT(saverhs);
    return saverhs;
}

/* Defunct in 1.5.0
SEXP attribute_hidden do_alias(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op,args);
    Rprintf(".Alias is deprecated; there is no replacement \n");
    SET_NAMED(CAR(args), 0);
    return CAR(args);
}
*/

/*  Assignment in its various forms  */

SEXP attribute_hidden do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP lhs, rhs;

    if (args == R_NilValue ||
	CDR(args) == R_NilValue ||
	CDDR(args) != R_NilValue)
	WrongArgCount(asym[PRIMVAL(op)]);
<<<<<<< eval.cpp
    if (Rf_isString(CAR(args))) {
	/* fix up a duplicate or args and recursively call do_set */
	SEXP val;
	PROTECT(args = Rf_duplicate(args));
	SETCAR(args, Rf_installTrChar(STRING_ELT(CAR(args), 0)));
	val = do_set(call, op, args, rho);
	UNPROTECT(1);
	return val;
    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (isString(CAR(args))) {
	/* fix up a duplicate or args and recursively call do_set */
	SEXP val;
	PROTECT(args = duplicate(args));
	SETCAR(args, installTrChar(STRING_ELT(CAR(args), 0)));
	val = do_set(call, op, args, rho);
	UNPROTECT(1);
	return val;
    }
=======
>>>>>>> eval.c

<<<<<<< eval.cpp
    switch (PRIMVAL(op)) {
    case 1: case 3:					/* <-, = */
	if (Rf_isSymbol(CAR(args))) {
	    s = Rf_eval(CADR(args), rho);
#ifdef CONSERVATIVE_COPYING /* not default */
	    if (NAMED(s))
	    {
		SEXP t;
		PROTECT(s);
		t = Rf_duplicate(s);
		UNPROTECT(1);
		s = t;
	    }
	    PROTECT(s);
	    Rf_defineVar(CAR(args), s, rho);
	    UNPROTECT(1);
	    SET_NAMED(s, 1);
#else
	    switch (NAMED(s)) {
	    case 0: SET_NAMED(s, 1); break;
	    case 1: SET_NAMED(s, 2); break;
	    }
	    Rf_defineVar(CAR(args), s, rho);
#endif
	    R_Visible = FALSE;
	    return (s);
	}
	else if (Rf_isLanguage(CAR(args))) {
	    R_Visible = FALSE;
	    return applydefine(call, op, args, rho);
	}
	else Rf_errorcall(call,
		       _("invalid (do_set) left-hand side to assignment"));
    case 2:						/* <<- */
	if (Rf_isSymbol(CAR(args))) {
	    s = Rf_eval(CADR(args), rho);
	    Environment::monitorLeaks(s);
	    if (NAMED(s))
		s = Rf_duplicate(s);
	    PROTECT(s);
	    Rf_setVar(CAR(args), s, ENCLOS(rho));
	    UNPROTECT(1);
	    SET_NAMED(s, 1);
	    R_Visible = FALSE;
	    return s;
	}
	else if (Rf_isLanguage(CAR(args)))
	    return applydefine(call, op, args, rho);
	else Rf_error(_("invalid assignment left-hand side"));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    switch (PRIMVAL(op)) {
    case 1: case 3:					/* <-, = */
	if (isSymbol(CAR(args))) {
	    s = eval(CADR(args), rho);
#ifdef CONSERVATIVE_COPYING /* not default */
	    if (NAMED(s))
	    {
		SEXP t;
		PROTECT(s);
		t = duplicate(s);
		UNPROTECT(1);
		s = t;
	    }
	    PROTECT(s);
	    defineVar(CAR(args), s, rho);
	    UNPROTECT(1);
	    SET_NAMED(s, 1);
#else
	    switch (NAMED(s)) {
	    case 0: SET_NAMED(s, 1); break;
	    case 1: SET_NAMED(s, 2); break;
	    }
	    defineVar(CAR(args), s, rho);
#endif
	    R_Visible = FALSE;
	    return (s);
	}
	else if (isLanguage(CAR(args))) {
	    R_Visible = FALSE;
	    return applydefine(call, op, args, rho);
	}
	else errorcall(call,
		       _("invalid (do_set) left-hand side to assignment"));
    case 2:						/* <<- */
	if (isSymbol(CAR(args))) {
	    s = eval(CADR(args), rho);
	    if (NAMED(s))
		s = duplicate(s);
	    PROTECT(s);
	    setVar(CAR(args), s, ENCLOS(rho));
	    UNPROTECT(1);
	    SET_NAMED(s, 1);
	    R_Visible = FALSE;
	    return s;
	}
	else if (isLanguage(CAR(args)))
	    return applydefine(call, op, args, rho);
	else error(_("invalid assignment left-hand side"));
=======
    lhs = CAR(args);
>>>>>>> eval.c

    switch (TYPEOF(lhs)) {
    case STRSXP:
	lhs = installTrChar(STRING_ELT(lhs, 0));
	/* fall through */
    case SYMSXP:
	rhs = eval(CADR(args), rho);
	INCREMENT_NAMED(rhs);
	if (PRIMVAL(op) == 2)                       /* <<- */
	    setVar(lhs, rhs, ENCLOS(rho));
	else                                        /* <-, = */
	    defineVar(lhs, rhs, rho);
	R_Visible = FALSE;
	return rhs;
    case LANGSXP:
	R_Visible = FALSE;
	return applydefine(call, op, args, rho);
    default:
	errorcall(call, _("invalid (do_set) left-hand side to assignment"));
    }

    return R_NilValue;/*NOTREACHED*/
}


/* Evaluate each expression in "el" in the environment "rho".  This is
   a naturally recursive algorithm, but we use the iterative form below
   because it is does not cause growth of the pointer protection stack,
   and because it is a little more efficient.
*/

/* used in arithmetic.c, seq.c */
SEXP attribute_hidden Rf_evalListKeepMissing(SEXP el, SEXP rho)
{
<<<<<<< eval.cpp
    ArgList arglist(SEXP_downcast<PairList*>(el), ArgList::RAW);
    arglist.evaluate(SEXP_downcast<Environment*>(rho), true);
    return const_cast<PairList*>(arglist.list());
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SEXP head, tail, ev, h;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {
	n++;

	if (CAR(el) == R_DotsSymbol) {
	    /* If we have a ... symbol, we look to see what it is bound to.
	     * If its binding is Null (i.e. zero length)
	     *	we just ignore it and return the cdr with all its expressions evaluated;
	     * if it is bound to a ... list of promises,
	     *	we force all the promises and then splice
	     *	the list of resulting values into the return value.
	     * Anything else bound to a ... symbol is an error
	     */
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    ev = CONS(eval(CAR(h), rho), R_NilValue);
		    if (head==R_NilValue)
			PROTECT(head = ev);
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	} else if (CAR(el) == R_MissingArg) {
	    /* It was an empty element: most likely get here from evalArgs
	       which may have been called on part of the args. */
	    errorcall(call, _("argument %d is empty"), n);
	} else if (isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)) {
	    /* It was missing */
	    errorcall(call, _("'%s' is missing"), CHAR(PRINTNAME(CAR(el))));
	} else {
	    ev = CONS(eval(CAR(el), rho), R_NilValue);
	    if (head==R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    if (head!=R_NilValue)
	UNPROTECT(1);

    return head;

} /* evalList() */


/* A slight variation of evaluating each expression in "el" in "rho". */

/* used in evalArgs, arithmetic.c, seq.c */
SEXP attribute_hidden evalListKeepMissing(SEXP el, SEXP rho)
{
    SEXP head, tail, ev, h;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we force all the promises and then splice
	 *	the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (CAR(h) == R_MissingArg)
			ev = CONS(R_MissingArg, R_NilValue);
		    else
			ev = CONS(eval(CAR(h), rho), R_NilValue);
		    if (head==R_NilValue)
			PROTECT(head = ev);
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if(h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	}
	else {
	    if (CAR(el) == R_MissingArg ||
		 (isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)))
		ev = CONS(R_MissingArg, R_NilValue);
	    else
		ev = CONS(eval(CAR(el), rho), R_NilValue);
	    if (head==R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    if (head!=R_NilValue)
	UNPROTECT(1);

    return head;
}


/* Create a promise to evaluate each argument.	Although this is most */
/* naturally attacked with a recursive algorithm, we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

SEXP attribute_hidden promiseArgs(SEXP el, SEXP rho)
{
    SEXP ans, h, tail;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while(el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 * we just ignore it and return the cdr with all its
	 * expressions promised; if it is bound to a ... list
	 * of promises, we repromise all the promises and then splice
	 * the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	 */

	/* Is this double promise mechanism really needed? */

	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    SETCDR(tail, CONS(mkPROMISE(CAR(h), rho), R_NilValue));
		    tail = CDR(tail);
		    COPY_TAG(tail, h);
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	}
	else if (CAR(el) == R_MissingArg) {
	    SETCDR(tail, CONS(R_MissingArg, R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	else {
	    SETCDR(tail, CONS(mkPROMISE(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    return CDR(ans);
}


/* Check that each formal is a symbol */

/* used in coerce.c */
void attribute_hidden CheckFormals(SEXP ls)
{
    if (isList(ls)) {
	for (; ls != R_NilValue; ls = CDR(ls))
	    if (TYPEOF(TAG(ls)) != SYMSXP)
		goto err;
	return;
    }
 err:
    error(_("invalid formal argument list for \"function\""));
=======
    SEXP head, tail, ev, h;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {
	n++;

	if (CAR(el) == R_DotsSymbol) {
	    /* If we have a ... symbol, we look to see what it is bound to.
	     * If its binding is Null (i.e. zero length)
	     *	we just ignore it and return the cdr with all its expressions evaluated;
	     * if it is bound to a ... list of promises,
	     *	we force all the promises and then splice
	     *	the list of resulting values into the return value.
	     * Anything else bound to a ... symbol is an error
	     */
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    ev = CONS_NR(eval(CAR(h), rho), R_NilValue);
		    if (head==R_NilValue)
			PROTECT(head = ev);
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	} else if (CAR(el) == R_MissingArg) {
	    /* It was an empty element: most likely get here from evalArgs
	       which may have been called on part of the args. */
	    errorcall(call, _("argument %d is empty"), n);
#ifdef CHECK_IS_MISSING_IN_evalList
	    /* Radford Newl drops this R_isMissing check in pqR in
	       03-zap-isMissing (but it seems to creep in again later
	       with helper thread stuff?)  as it takes quite a bit of
	       time (essentially the equivalent of evaluating the
	       symbol, but maybe not as efficiently as eval) and only
	       serves to change the error message, not always for the
	       better. Also, the byte code interpreter does not do
	       this, so dropping this makes compiled and interreted
	       cod emore consistent. */
	} else if (isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)) {
	    /* It was missing */
	    errorcall(call, _("'%s' is missing"), EncodeChar(PRINTNAME(CAR(el))));
#endif
	} else {
	    ev = CONS_NR(eval(CAR(el), rho), R_NilValue);
	    if (head==R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    if (head!=R_NilValue)
	UNPROTECT(1);

    return head;

} /* evalList() */


/* A slight variation of evaluating each expression in "el" in "rho". */

/* used in evalArgs, arithmetic.c, seq.c */
SEXP attribute_hidden evalListKeepMissing(SEXP el, SEXP rho)
{
    SEXP head, tail, ev, h;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we force all the promises and then splice
	 *	the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (CAR(h) == R_MissingArg)
			ev = CONS_NR(R_MissingArg, R_NilValue);
		    else
			ev = CONS_NR(eval(CAR(h), rho), R_NilValue);
		    if (head==R_NilValue)
			PROTECT(head = ev);
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if(h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	}
	else {
	    if (CAR(el) == R_MissingArg ||
		(isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)))
		ev = CONS_NR(R_MissingArg, R_NilValue);
	    else
		ev = CONS_NR(eval(CAR(el), rho), R_NilValue);
	    if (head==R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    if (head!=R_NilValue)
	UNPROTECT(1);

    return head;
}


/* Create a promise to evaluate each argument.	Although this is most */
/* naturally attacked with a recursive algorithm, we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

SEXP attribute_hidden promiseArgs(SEXP el, SEXP rho)
{
    SEXP ans, h, tail;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while(el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 * we just ignore it and return the cdr with all its
	 * expressions promised; if it is bound to a ... list
	 * of promises, we repromise all the promises and then splice
	 * the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	 */

	/* Is this double promise mechanism really needed? */

	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    SETCDR(tail, CONS(mkPROMISE(CAR(h), rho), R_NilValue));
		    tail = CDR(tail);
		    COPY_TAG(tail, h);
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	}
	else if (CAR(el) == R_MissingArg) {
	    SETCDR(tail, CONS(R_MissingArg, R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	else {
	    SETCDR(tail, CONS(mkPROMISE(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    return CDR(ans);
}


/* Check that each formal is a symbol */

/* used in coerce.c */
void attribute_hidden CheckFormals(SEXP ls)
{
    if (isList(ls)) {
	for (; ls != R_NilValue; ls = CDR(ls))
	    if (TYPEOF(TAG(ls)) != SYMSXP)
		goto err;
	return;
    }
 err:
    error(_("invalid formal argument list for \"function\""));
>>>>>>> eval.c
}


static SEXP VectorToPairListNamed(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len = 0, named;
    const void *vmax = vmaxget();

    PROTECT(x);
    PROTECT(xnames = Rf_getAttrib(x, R_NamesSymbol)); /* isn't this protected via x? */
    named = (xnames != R_NilValue);
    if(named)
	for (i = 0; i < length(x); i++)
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') len++;

    if(len) {
	PROTECT(xnew = Rf_allocList(len));
	xptr = xnew;
	for (i = 0; i < length(x); i++) {
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') {
		SETCAR(xptr, VECTOR_ELT(x, i));
		SET_TAG(xptr, Rf_installTrChar(STRING_ELT(xnames, i)));
		xptr = CDR(xptr);
	    }
	}
	UNPROTECT(1);
    } else xnew = Rf_allocList(0);
    UNPROTECT(2);
    vmaxset(vmax);
    return xnew;
}

#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)

/* "eval": Evaluate the first argument
   in the environment specified by the second argument. */

SEXP attribute_hidden do_eval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP encl, x, xptr;
    volatile SEXP expr, env;

    int frame;

    checkArity(op, args);
    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    SEXPTYPE tEncl = TYPEOF(encl);
    if (Rf_isNull(encl)) {
	/* This is supposed to be defunct, but has been kept here
	   (and documented as such) */
	encl = R_BaseEnv;
    } else if ( !Rf_isEnvironment(encl) &&
		!Rf_isEnvironment((encl = simple_as_environment(encl))) ) {
	Rf_error(_("invalid '%s' argument of type '%s'"),
	      "enclos", Rf_type2char(tEncl));
    }
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* usually an ENVSXP */
    switch(TYPEOF(env)) {
    case NILSXP:
	env = encl;     /* soeval(expr, NULL, encl) works */
	/* falls through */
    case ENVSXP:
	PROTECT(env);	/* so we can unprotect 2 at the end */
	break;
    case LISTSXP:
	/* This usage requires all the pairlist to be named */
	env = Rf_NewEnvironment(R_NilValue, Rf_duplicate(CADR(args)), encl);
	PROTECT(env);
	break;
    case VECSXP:
	/* PR#14035 */
	x = VectorToPairListNamed(CADR(args));
	for (xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr))
	    SET_NAMED(CAR(xptr) , 2);
	env = Rf_NewEnvironment(R_NilValue, x, encl);
	PROTECT(env);
	break;
    case INTSXP:
    case REALSXP:
	if (length(env) != 1)
	    Rf_error(_("numeric 'envir' arg not of length one"));
	frame = Rf_asInteger(env);
	if (frame == NA_INTEGER)
	    Rf_error(_("invalid '%s' argument of type '%s'"),
		  "envir", Rf_type2char(TYPEOF(env)));
	PROTECT(env = R_sysframe(frame, ClosureContext::innermost()));
	break;
    default:
	Rf_error(_("invalid '%s' argument of type '%s'"),
	      "envir", Rf_type2char(TYPEOF(env)));
    }

    /* Rf_isLanguage include NILSXP, and that does not need to be
       evaluated
    if (Rf_isLanguage(expr) || Rf_isSymbol(expr) || isByteCode(expr)) { */
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP || isByteCode(expr)) {
	PROTECT(expr);
	{
	    Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* call_env = SEXP_downcast<Environment*>(rho);
	    FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
	    Environment* working_env = SEXP_downcast<Environment*>(env);
	    PairList* promargs = SEXP_downcast<PairList*>(args);
	    ClosureContext cntxt(callx, call_env, func, working_env, promargs);
	    Environment::ReturnScope returnscope(working_env);
	    try {
		expr = Rf_eval(expr, env);
	    }
	    catch (ReturnException& rx) {
		if (rx.environment() != working_env)
		    throw;
		expr = rx.value();
	    }
	}
	UNPROTECT(1);
    }
    else if (TYPEOF(expr) == EXPRSXP) {
	int i, n;
	SEXP srcrefs = getBlockSrcrefs(expr);
	PROTECT(expr);
	n = LENGTH(expr);
	SEXP tmp = R_NilValue;
	{
	    Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* call_env = SEXP_downcast<Environment*>(rho);
	    FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
	    Environment* working_env = SEXP_downcast<Environment*>(env);
	    PairList* promargs = SEXP_downcast<PairList*>(args);
	    ClosureContext cntxt(callx, call_env, func, working_env, promargs);
	    Environment::ReturnScope returnscope(working_env);
	    try {
		for (i = 0 ; i < n ; i++) {
		    R_Srcref = getSrcref(srcrefs, i); 
		    tmp = Rf_eval(XVECTOR_ELT(expr, i), env);
		}
	    }
	    catch (ReturnException& rx) {
		if (rx.environment() != working_env)
		    throw;
		tmp = rx.value();
	    }
	}
	UNPROTECT(1);
	expr = tmp;
    }
    else if( TYPEOF(expr) == PROMSXP ) {
	expr = Rf_eval(expr, rho);
    } /* else expr is returned unchanged */
    UNPROTECT(1);
    return expr;
}

/* This is a special .Internal */
SEXP attribute_hidden do_withVisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, nm, ret;

    checkArity(op, args);
    x = CAR(args);
    x = Rf_eval(x, rho);
    PROTECT(x);
    PROTECT(ret = ListVector::create(2));
    PROTECT(nm = StringVector::create(2));
    SET_STRING_ELT(nm, 0, Rf_mkChar("value"));
    SET_STRING_ELT(nm, 1, Rf_mkChar("visible"));
    SET_VECTOR_ELT(ret, 0, x);
    SET_VECTOR_ELT(ret, 1, Rf_ScalarLogical(R_Visible));
    Rf_setAttrib(ret, R_NamesSymbol, nm);
    UNPROTECT(3);
    return ret;
}

/* This is a special .Internal */
SEXP attribute_hidden do_recall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    ClosureContext *cptr;
    SEXP s, ans ;
    cptr = ClosureContext::innermost();
    /* get the args supplied */
    while (cptr && cptr->workingEnvironment() != rho) {
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    const PairList* argspl = (cptr ? cptr->promiseArgs()
			      : SEXP_downcast<PairList*>(args));
    ArgList arglist(argspl, ArgList::PROMISED);
    /* get the env recall was called from */
    s = ClosureContext::innermost()->callEnvironment();
    while (cptr && cptr->workingEnvironment() != s) {
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    if (cptr == nullptr)
	Rf_error(_("'Recall' called from outside a closure"));

    /* If the function has been recorded in the context, use it
       otherwise search for it by name or evaluate the expression
       originally used to get it.
    */
    if (cptr->function() != R_NilValue)
	PROTECT(s = CXXRCCAST(FunctionBase*, cptr->function()));
    else if( TYPEOF(CAR(CXXRCCAST(Expression*, cptr->call()))) == SYMSXP)
	PROTECT(s = Rf_findFun(CAR(CXXRCCAST(Expression*, cptr->call())), cptr->callEnvironment()));
    else
	PROTECT(s = Rf_eval(CAR(CXXRCCAST(Expression*, cptr->call())), cptr->callEnvironment()));
    if (TYPEOF(s) != CLOSXP)
<<<<<<< eval.cpp
	Rf_error(_("'Recall' called from outside a closure"));
    Closure* closure = SEXP_downcast<Closure*>(s);
    ans = closure->invoke(cptr->callEnvironment(), &arglist, cptr->call());
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	error(_("'Recall' called from outside a closure"));
    ans = applyClosure(cptr->call, s, args, cptr->sysparent, R_BaseEnv);
=======
	error(_("'Recall' called from outside a closure"));
    ans = applyClosure(cptr->call, s, args, cptr->sysparent, R_NilValue);
>>>>>>> eval.c
    UNPROTECT(1);
    return ans;
}


/* Rf_DispatchOrEval is used in internal functions which dispatch to
 * object methods (e.g. "[" or "[[").  The code either builds promises
 * and dispatches to the appropriate method, or it evaluates the
 * (unevaluated) arguments it comes in with and returns them so that
 * the generic built-in C code can continue.

 * To call this an ugly hack would be to insult all existing ugly hacks
 * at large in the world.
 */
attribute_hidden
int Rf_DispatchOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		   SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
<<<<<<< eval.cpp
    Expression* callx = SEXP_downcast<Expression*>(call);
    FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
    ArgList arglist(SEXP_downcast<PairList*>(args),
		    (argsevald ? ArgList::EVALUATED : ArgList::RAW));
    Environment* callenv = SEXP_downcast<Environment*>(rho);

    // Rf_DispatchOrEval is called very frequently, most often in cases
    // where no dispatching is needed and the Rf_isObject or the
    // string-based pre-test fail.  To avoid degrading performance it
    // is therefore necessary to avoid creating promises in these
    // cases.  The pre-test does require that we look at the first
    // argument, so that needs to be evaluated.  The complicating
    // factor is that the first argument might come in with a "..."
    // and that there might be other arguments in the "..." as well.
    // LT

    GCStackRoot<> x(arglist.firstArg(callenv).second);

    // try to dispatch on the object
    if (x && x->hasClass()) {
	// Try for formal method.
	if (x->isS4Object() && R_has_methods(func)) {
	    // create a promise to pass down to applyClosure
	    if (arglist.status() == ArgList::RAW)
		arglist.wrapInPromises(callenv);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
/* DispatchOrEval is called very frequently, most often in cases where
   no dispatching is needed and the isObject or the string-based
   pre-test fail.  To avoid degrading performance it is therefore
   necessary to avoid creating promises in these cases.  The pre-test
   does require that we look at the first argument, so that needs to
   be evaluated.  The complicating factor is that the first argument
   might come in with a "..." and that there might be other arguments
   in the "..." as well.  LT */

    SEXP x = R_NilValue;
    int dots = FALSE, nprotect = 0;;

    if( argsevald )
	{PROTECT(x = CAR(args)); nprotect++;}
    else {
	/* Find the object to dispatch on, dropping any leading
	   ... arguments with missing or empty values.  If there are no
	   arguments, R_NilValue is used. */
	for (; args != R_NilValue; args = CDR(args)) {
	    if (CAR(args) == R_DotsSymbol) {
		SEXP h = findVar(R_DotsSymbol, rho);
		if (TYPEOF(h) == DOTSXP) {
#ifdef DODO
		    /**** any self-evaluating value should be OK; this
			  is used in byte compiled code. LT */
		    /* just a consistency check */
		    if (TYPEOF(CAR(h)) != PROMSXP)
			error(_("value in '...' is not a promise"));
#endif
		    dots = TRUE;
		    x = eval(CAR(h), rho);
		break;
		}
		else if (h != R_NilValue && h != R_MissingArg)
		    error(_("'...' used in an incorrect context"));
	    }
	    else {
		dots = FALSE;
	    x = eval(CAR(args), rho);
	    break;
	    }
	}
	PROTECT(x); nprotect++;
    }
	/* try to dispatch on the object */
    if( isObject(x) ) {
	char *pt;
	/* Try for formal method. */
	if(IS_S4_OBJECT(x) && R_has_methods(op)) {
	    SEXP value, argValue;
	    /* create a promise to pass down to applyClosure  */
	    if(!argsevald) {
		argValue = promiseArgs(args, rho);
		SET_PRVALUE(CAR(argValue), x);
	    } else argValue = args;
	    PROTECT(argValue); nprotect++;
=======
/* DispatchOrEval is called very frequently, most often in cases where
   no dispatching is needed and the isObject or the string-based
   pre-test fail.  To avoid degrading performance it is therefore
   necessary to avoid creating promises in these cases.  The pre-test
   does require that we look at the first argument, so that needs to
   be evaluated.  The complicating factor is that the first argument
   might come in with a "..." and that there might be other arguments
   in the "..." as well.  LT */

    SEXP x = R_NilValue;
    int dots = FALSE, nprotect = 0;;

    if( argsevald )
	{PROTECT(x = CAR(args)); nprotect++;}
    else {
	/* Find the object to dispatch on, dropping any leading
	   ... arguments with missing or empty values.  If there are no
	   arguments, R_NilValue is used. */
	for (; args != R_NilValue; args = CDR(args)) {
	    if (CAR(args) == R_DotsSymbol) {
		SEXP h = findVar(R_DotsSymbol, rho);
		if (TYPEOF(h) == DOTSXP) {
#ifdef DODO
		    /**** any self-evaluating value should be OK; this
			  is used in byte compiled code. LT */
		    /* just a consistency check */
		    if (TYPEOF(CAR(h)) != PROMSXP)
			error(_("value in '...' is not a promise"));
#endif
		    dots = TRUE;
		    x = eval(CAR(h), rho);
		    break;
		}
		else if (h != R_NilValue && h != R_MissingArg)
		    error(_("'...' used in an incorrect context"));
	    }
	    else {
		dots = FALSE;
		x = eval(CAR(args), rho);
		break;
	    }
	}
	PROTECT(x); nprotect++;
    }
	/* try to dispatch on the object */
    if( isObject(x) ) {
	char *pt;
	/* Try for formal method. */
	if(IS_S4_OBJECT(x) && R_has_methods(op)) {
	    SEXP value, argValue;
	    /* create a promise to pass down to applyClosure  */
	    if(!argsevald) {
		argValue = promiseArgs(args, rho);
		SET_PRVALUE(CAR(argValue), x);
	    } else argValue = args;
	    PROTECT(argValue); nprotect++;
>>>>>>> eval.c
	    /* This means S4 dispatch */
	    std::pair<bool, SEXP> pr
		= R_possible_dispatch(callx, func,
				      const_cast<PairList*>(arglist.list()),
				      callenv, TRUE);
	    if (pr.first) {
		*ans = pr.second;
		return 1;
	    }
<<<<<<< eval.cpp
	}
	char* suffix = nullptr;
	{
	    RObject* callcar = callx->car();
	    if (callcar->sexptype() == SYMSXP) {
		Symbol* sym = static_cast<Symbol*>(callcar);
		suffix = Rf_strrchr(sym->name()->c_str(), '.');
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	    else {
		/* go on, with the evaluated args.  Not guaranteed to have
		   the same semantics as if the arguments were not
		   evaluated, in special cases (e.g., arg values that are
		   LANGSXP).
		   The use of the promiseArgs is supposed to prevent
		   multiple evaluation after the call to possible_dispatch.
		*/
		if (dots)
		    PROTECT(argValue = evalArgs(argValue, rho, dropmissing,
						call, 0));
		else {
		    PROTECT(argValue = CONS(x, evalArgs(CDR(argValue), rho,
							dropmissing, call, 1)));
		    SET_TAG(argValue, CreateTag(TAG(args)));
		}
		nprotect++;
		args = argValue;
		argsevald = 1;
=======
	    else {
		/* go on, with the evaluated args.  Not guaranteed to have
		   the same semantics as if the arguments were not
		   evaluated, in special cases (e.g., arg values that are
		   LANGSXP).
		   The use of the promiseArgs is supposed to prevent
		   multiple evaluation after the call to possible_dispatch.
		*/
		if (dots)
		    PROTECT(argValue = evalArgs(argValue, rho, dropmissing,
						call, 0));
		else {
		    PROTECT(argValue = CONS_NR(x, evalArgs(CDR(argValue), rho,
							dropmissing, call, 1)));
		    SET_TAG(argValue, CreateTag(TAG(args)));
		}
		nprotect++;
		args = argValue;
		argsevald = 1;
>>>>>>> eval.c
	    }
	}
	if (!suffix || strcmp(suffix, ".default")) {
	    if (arglist.status() == ArgList::RAW)
		arglist.wrapInPromises(callenv);
	    /* The context set up here is needed because of the way
	       Rf_usemethod() is written.  Rf_DispatchGroup() repeats some
	       internal Rf_usemethod() code and avoids the need for a
	       context; perhaps the Rf_usemethod() code should be
	       refactored so the contexts around the Rf_usemethod() calls
	       in this file can be removed.

	       Using rho for current and calling environment can be
	       confusing for things like sys.parent() calls captured
	       in promises (Gabor G had an example of this).  Also,
	       since the context is established without a SETJMP using
	       an R-accessible environment allows a segfault to be
	       triggered (by something very obscure, but still).
	       Hence here and in the other Rf_usemethod() uses below a
	       new environment rho1 is created and used.  LT */
	    GCStackRoot<Frame> frame(new ListFrame);
	    Environment* working_env = new Environment(callenv, frame);
	    ClosureContext cntxt(callx, callenv, func,
				 working_env, arglist.list());
	    int um = Rf_usemethod(generic, x, call,
				  const_cast<PairList*>(arglist.list()),
				  working_env, callenv, R_BaseEnv, ans);
	    if (um)
		return 1;
<<<<<<< eval.cpp
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	    }
	    endcontext(&cntxt);
	}
    }
    if(!argsevald) {
	if (dots)
	    /* The first call argument was ... and may contain more than the
	       object, so it needs to be evaluated here.  The object should be
	       in a promise, so evaluating it again should be no problem. */
	    *ans = evalArgs(args, rho, dropmissing, call, 0);
	else {
	    PROTECT(*ans = CONS(x, evalArgs(CDR(args), rho, dropmissing, call, 1)));
	    SET_TAG(*ans, CreateTag(TAG(args)));
	    UNPROTECT(1);
=======
	    }
	    endcontext(&cntxt);
	    DECREMENT_REFCNT(x);
	}
    }
    if(!argsevald) {
	if (dots)
	    /* The first call argument was ... and may contain more than the
	       object, so it needs to be evaluated here.  The object should be
	       in a promise, so evaluating it again should be no problem. */
	    *ans = evalArgs(args, rho, dropmissing, call, 0);
	else {
	    PROTECT(*ans = CONS_NR(x, evalArgs(CDR(args), rho, dropmissing, call, 1)));
	    SET_TAG(*ans, CreateTag(TAG(args)));
	    UNPROTECT(1);
>>>>>>> eval.c
	}
    }
    if (arglist.status() != ArgList::EVALUATED)
	arglist.evaluate(callenv, !dropmissing);
    *ans = const_cast<PairList*>(arglist.list());
    return 0;
}

<<<<<<< eval.cpp
	
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

/* gr needs to be protected on return from this function */
static void findmethod(SEXP Class, const char *group, const char *generic,
		       SEXP *sxp,  SEXP *gr, SEXP *meth, int *which,
		       char *buf, SEXP rho)
{
    int len, whichclass;
    const void *vmax = vmaxget();

    len = length(Class);

    /* Need to interleave looking for group and generic methods
       e.g. if class(x) is c("foo", "bar)" then x > 3 should invoke
       "Ops.foo" rather than ">.bar"
    */
    for (whichclass = 0 ; whichclass < len ; whichclass++) {
	const char *ss = translateChar(STRING_ELT(Class, whichclass));
	if(strlen(generic) + strlen(ss) + 2 > 512)
	    error(_("class name too long in '%s'"), generic);
	sprintf(buf, "%s.%s", generic, ss);
	*meth = install(buf);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = mkString("");
	    break;
	}
	if(strlen(group) + strlen(ss) + 2 > 512)
	    error(_("class name too long in '%s'"), group);
	sprintf(buf, "%s.%s", group, ss);
	*meth = install(buf);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = mkString(group);
	    break;
	}
    }
    vmaxset(vmax);
    *which = whichclass;
}

=======
static R_INLINE void updateObjFromS4Slot(SEXP objSlot, const char *className) {
    SEXP obj = CAR(objSlot);

    if(IS_S4_OBJECT(obj) && isBasicClass(className)) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
	if(NAMED(obj)) SET_NAMED(obj, 2);
	obj = R_getS4DataSlot(obj, S4SXP); /* the .S3Class obj. or NULL*/
	if(obj != R_NilValue) /* use the S3Part as the inherited object */
	    SETCAR(objSlot, obj);
    }
}

/* gr needs to be protected on return from this function */
static void findmethod(SEXP Class, const char *group, const char *generic,
		       SEXP *sxp,  SEXP *gr, SEXP *meth, int *which,
		       SEXP objSlot, SEXP rho)
{
    int len, whichclass;
    const void *vmax = vmaxget();

    len = length(Class);

    /* Need to interleave looking for group and generic methods
       e.g. if class(x) is c("foo", "bar)" then x > 3 should invoke
       "Ops.foo" rather than ">.bar"
    */
    for (whichclass = 0 ; whichclass < len ; whichclass++) {
	const char *ss = translateChar(STRING_ELT(Class, whichclass));
	*meth = installS3Signature(generic, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = R_BlankScalarString;
	    if (whichclass > 0) updateObjFromS4Slot(objSlot, ss);
	    break;
	}
	*meth = installS3Signature(group, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = mkString(group);
	    if (whichclass > 0) updateObjFromS4Slot(objSlot, ss);
	    break;
	}
    }
    vmaxset(vmax);
    *which = whichclass;
}

static SEXP classForGroupDispatch(SEXP obj) {

    return IS_S4_OBJECT(obj) ? R_data_class2(obj)
            : getAttrib(obj, R_ClassSymbol);
}

>>>>>>> eval.c
attribute_hidden
int Rf_DispatchGroup(const char* group, SEXP call, SEXP op, SEXP args, SEXP rho,
		     SEXP *ans)
{
<<<<<<< eval.cpp
    Expression* callx = SEXP_downcast<Expression*>(call);
    BuiltInFunction* opfun = SEXP_downcast<BuiltInFunction*>(op);
    PairList* callargs = SEXP_downcast<PairList*>(args);
    Environment* callenv = SEXP_downcast<Environment*>(rho);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    int i, j, nargs, lwhich, rwhich, set;
    SEXP lclass, s, t, m, lmeth, lsxp, lgr, newrho;
    SEXP rclass, rmeth, rgr, rsxp, value;
    char lbuf[512], rbuf[512], generic[128], *pt;
    Rboolean useS4 = TRUE, isOps = FALSE;
=======
    int i, nargs, lwhich, rwhich;
    SEXP lclass, s, t, m, lmeth, lsxp, lgr, newvars;
    SEXP rclass, rmeth, rgr, rsxp, value;
    char *generic;
    Rboolean useS4 = TRUE, isOps = FALSE;
>>>>>>> eval.c

    if (!callargs)
	return 0;
    std::size_t numargs = listLength(callargs);
    RObject* arg1val = callargs->car();
    RObject* arg2val = (numargs > 1 ? callargs->tail()->car() : nullptr);
    
    /* pre-test to avoid string computations when there is nothing to
       dispatch on because either there is only one argument and it
       isn't an object or there are two or more arguments but neither
       of the first two is an object -- both of these cases would be
       rejected by the code following the string examination code
       below */
    if (!((arg1val && arg1val->hasClass())
	  || (arg2val && arg2val->hasClass())))
	return 0;

    bool isOps = (strcmp(group, "Ops") == 0);

    /* try for formal method */
    bool useS4 = ((arg1val && arg1val->isS4Object())
		  || (arg2val && arg2val->isS4Object()));
    if (useS4) {
	ArgList arglist(callargs, ArgList::EVALUATED);
	/* Remove argument names to ensure positional matching */
	if (isOps)
	    arglist.stripTags();
	if (R_has_methods(opfun)) {
	    std::pair<bool, SEXP> pr
		= R_possible_dispatch(callx, opfun,
				      const_cast<PairList*>(arglist.list()),
				      callenv, FALSE);
	    if (pr.first) {
		*ans = pr.second;
		return 1;
	    }
	}
	/* else go on to look for S3 methods */
    }

    /* check whether we are processing the default method */
<<<<<<< eval.cpp
    {
	RObject* callcar = callx->car();
	if (callcar->sexptype() == SYMSXP) {
	    string callname
		= static_cast<Symbol*>(callcar)->name()->stdstring();
	    string::size_type index = callname.find_last_of(".");
	    if (index != string::npos
		&& callname.substr(index) == ".default")
		return 0;
	}
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if ( isSymbol(CAR(call)) ) {
	if(strlen(CHAR(PRINTNAME(CAR(call)))) >= 512)
	   error(_("call name too long in '%s'"), CHAR(PRINTNAME(CAR(call))));
	sprintf(lbuf, "%s", CHAR(PRINTNAME(CAR(call))) );
	pt = strtok(lbuf, ".");
	pt = strtok(NULL, ".");

	if( pt != NULL && !strcmp(pt, "default") )
	    return 0;
=======
    if ( isSymbol(CAR(call)) ) {
	const char *cstr = strchr(CHAR(PRINTNAME(CAR(call))), '.');
	if (cstr && !strcmp(cstr + 1, "default"))
	    return 0;
>>>>>>> eval.c
    }

<<<<<<< eval.cpp
    std::size_t nargs = (isOps ? numargs : 1);
    string generic(opfun->name());
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if(isOps)
	nargs = length(args);
    else
	nargs = 1;

    if( nargs == 1 && !isObject(CAR(args)) )
	return 0;

    if(!isObject(CAR(args)) && !isObject(CADR(args)))
	return 0;

    if(strlen(PRIMNAME(op)) >= 128)
	error(_("generic name too long in '%s'"), PRIMNAME(op));
    sprintf(generic, "%s", PRIMNAME(op) );

    lclass = IS_S4_OBJECT(CAR(args)) ? R_data_class2(CAR(args))
      : getAttrib(CAR(args), R_ClassSymbol);

    if( nargs == 2 )
	rclass = IS_S4_OBJECT(CADR(args)) ? R_data_class2(CADR(args))
      : getAttrib(CADR(args), R_ClassSymbol);
    else
	rclass = R_NilValue;

    lsxp = R_NilValue; lgr = R_NilValue; lmeth = R_NilValue;
    rsxp = R_NilValue; rgr = R_NilValue; rmeth = R_NilValue;
=======
    if(isOps)
	nargs = length(args);
    else
	nargs = 1;

    if( nargs == 1 && !isObject(CAR(args)) )
	return 0;

    generic = PRIMNAME(op);

    lclass = classForGroupDispatch(CAR(args));

    if( nargs == 2 )
	rclass = classForGroupDispatch(CADR(args));
    else
	rclass = R_NilValue;

    lsxp = R_NilValue; lgr = R_NilValue; lmeth = R_NilValue;
    rsxp = R_NilValue; rgr = R_NilValue; rmeth = R_NilValue;
>>>>>>> eval.c

<<<<<<< eval.cpp
    GCStackRoot<S3Launcher>
	l(S3Launcher::create(arg1val, generic, group,
			     callenv, Environment::base(), false));
    if (l && arg1val->isS4Object() && l->locInClasses() > 0
	&& Rf_isBasicClass(Rf_translateChar(l->className()))) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
        RObject* value = arg1val;
	if (NAMED(value))
	    SET_NAMED(value, 2);
	value = R_getS4DataSlot(value, S4SXP); /* the .S3Class obj. or NULL*/
	if (value) { /* use the S3Part as the inherited object */
	    callargs->setCar(value);
	    arg1val = value;
	}
    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    findmethod(lclass, group, generic, &lsxp, &lgr, &lmeth, &lwhich,
	       lbuf, rho);
    PROTECT(lgr);
    const void *vmax = vmaxget();
    if(isFunction(lsxp) && IS_S4_OBJECT(CAR(args)) && lwhich > 0
       && isBasicClass(translateChar(STRING_ELT(lclass, lwhich)))) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
	value = CAR(args);
	if(NAMED(value)) SET_NAMED(value, 2);
	value = R_getS4DataSlot(value, S4SXP); /* the .S3Class obj. or NULL*/
	if(value != R_NilValue) /* use the S3Part as the inherited object */
	    SETCAR(args, value);
    }
=======
    findmethod(lclass, group, generic, &lsxp, &lgr, &lmeth, &lwhich,
	       args, rho);
    PROTECT(lgr);
>>>>>>> eval.c

<<<<<<< eval.cpp
    GCStackRoot<S3Launcher> r;
    if (nargs == 2)
	r = S3Launcher::create(arg2val, generic, group,
			       callenv, Environment::base(), false);
    if (r && arg2val->isS4Object() && r->locInClasses() > 0
	&& Rf_isBasicClass(Rf_translateChar(r->className()))) {
        RObject* value = arg2val;
	if(NAMED(value))
	    SET_NAMED(value, 2);
	value = R_getS4DataSlot(value, S4SXP);
	if (value) {
	    callargs->tail()->setCar(value);
	    arg2val = value;
	}
    }

    if (!l && !r)
	return 0; /* no generic or group method so use default*/ 

    if (l && r && l->function() != r->function()) {
	/* special-case some methods involving difftime */
	const String &lname = *l->symbol()->name();
	const String &rname = *r->symbol()->name();
	if (rname == "Ops.difftime"
	    && (lname == "+.POSIXt" || lname == "-.POSIXt"
		|| lname == "+.Date" || lname == "-.Date"))
	    r = nullptr;
	else if (lname == "Ops.difftime"
		 && (rname == "+.POSIXt" || rname == "+.Date"))
	    l = nullptr;
	else {
	    Rf_warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
		       lname.c_str(), rname.c_str(), generic.c_str());
	    return 0;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if( nargs == 2 )
	findmethod(rclass, group, generic, &rsxp, &rgr, &rmeth,
		   &rwhich, rbuf, rho);
    else
	rwhich = 0;

    if(isFunction(rsxp) && IS_S4_OBJECT(CADR(args)) && rwhich > 0
       && isBasicClass(translateChar(STRING_ELT(rclass, rwhich)))) {
	value = CADR(args);
	if(NAMED(value)) SET_NAMED(value, 2);
	value = R_getS4DataSlot(value, S4SXP);
	if(value != R_NilValue) SETCADR(args, value);
    }
    vmaxset(vmax);

    PROTECT(rgr);

    if( !isFunction(lsxp) && !isFunction(rsxp) ) {
	UNPROTECT(2);
	return 0; /* no generic or group method so use default*/
    }

    if( lsxp != rsxp ) {
	if ( isFunction(lsxp) && isFunction(rsxp) ) {
	    /* special-case some methods involving difftime */
	    const char *lname = CHAR(PRINTNAME(lmeth)),
		*rname = CHAR(PRINTNAME(rmeth));
	    if( streql(rname, "Ops.difftime") &&
		(streql(lname, "+.POSIXt") || streql(lname, "-.POSIXt") ||
		 streql(lname, "+.Date") || streql(lname, "-.Date")) )
		rsxp = R_NilValue;
	    else if (streql(lname, "Ops.difftime") &&
		     (streql(rname, "+.POSIXt") || streql(rname, "+.Date")) )
		lsxp = R_NilValue;
	    else {
		warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
			lname, rname, generic);
		UNPROTECT(2);
		return 0;
	    }
	}
	/* if the right hand side is the one */
	if( !isFunction(lsxp) ) { /* copy over the righthand stuff */
	    lsxp = rsxp;
	    lmeth = rmeth;
	    lgr = rgr;
	    lclass = rclass;
	    lwhich = rwhich;
	    strcpy(lbuf, rbuf);
=======
    if( nargs == 2 )
	findmethod(rclass, group, generic, &rsxp, &rgr, &rmeth,
		   &rwhich, CDR(args), rho);
    else
	rwhich = 0;

    PROTECT(rgr);

    if( !isFunction(lsxp) && !isFunction(rsxp) ) {
	UNPROTECT(2);
	return 0; /* no generic or group method so use default */
    }

    if( lsxp != rsxp ) {
	if ( isFunction(lsxp) && isFunction(rsxp) ) {
	    /* special-case some methods involving difftime */
	    const char *lname = CHAR(PRINTNAME(lmeth)),
		*rname = CHAR(PRINTNAME(rmeth));
	    if( streql(rname, "Ops.difftime") &&
		(streql(lname, "+.POSIXt") || streql(lname, "-.POSIXt") ||
		 streql(lname, "+.Date") || streql(lname, "-.Date")) )
		rsxp = R_NilValue;
	    else if (streql(lname, "Ops.difftime") &&
		     (streql(rname, "+.POSIXt") || streql(rname, "+.Date")) )
		lsxp = R_NilValue;
	    else {
		warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
			lname, rname, generic);
		UNPROTECT(2);
		return 0;
	    }
	}
	/* if the right hand side is the one */
	if( !isFunction(lsxp) ) { /* copy over the righthand stuff */
	    lsxp = rsxp;
	    lmeth = rmeth;
	    lgr = rgr;
	    lclass = rclass;
	    lwhich = rwhich;
>>>>>>> eval.c
	}
    }

    S3Launcher* m = (l ? l : r);  // m is the method that will be applied.

    /* we either have a group method or a class method */

<<<<<<< eval.cpp
    GCStackRoot<Frame> supp_frame(new ListFrame);
    // Set up special method bindings:
    m->addMethodBindings(supp_frame);

    if (isOps) {
	// Rebind .Method:
	GCStackRoot<StringVector> dotmethod(StringVector::create(2));
	(*dotmethod)[0] = (l 
			   ? const_cast<String*>(l->symbol()->name())
			   : String::blank());
	(*dotmethod)[1] = (r
			   ? const_cast<String*>(r->symbol()->name())
			   : String::blank());
	supp_frame->bind(DotMethodSymbol, dotmethod);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    PROTECT(newrho = allocSExp(ENVSXP));
    PROTECT(m = allocVector(STRSXP,nargs));
    vmax = vmaxget();
    s = args;
    for (i = 0 ; i < nargs ; i++) {
	t = IS_S4_OBJECT(CAR(s)) ? R_data_class2(CAR(s))
	  : getAttrib(CAR(s), R_ClassSymbol);
	set = 0;
	if (isString(t)) {
	    for (j = 0 ; j < length(t) ; j++) {
		if (!strcmp(translateChar(STRING_ELT(t, j)),
			    translateChar(STRING_ELT(lclass, lwhich)))) {
		    SET_STRING_ELT(m, i, mkChar(lbuf));
		    set = 1;
		    break;
		}
	    }
	}
	if( !set )
	    SET_STRING_ELT(m, i, R_BlankString);
	s = CDR(s);
=======
    PROTECT(m = allocVector(STRSXP,nargs));
    const void *vmax = vmaxget();
    s = args;
    const char *dispatchClassName = translateChar(STRING_ELT(lclass, lwhich));
    for (i = 0 ; i < nargs ; i++) {
	t = classForGroupDispatch(CAR(s));
	if (isString(t) && (stringPositionTr(t, dispatchClassName) >= 0))
	    SET_STRING_ELT(m, i, PRINTNAME(lmeth));
        else
	    SET_STRING_ELT(m, i, R_BlankString);
	s = CDR(s);
>>>>>>> eval.c
    }
<<<<<<< eval.cpp
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    vmaxset(vmax);

    defineVar(R_dot_Method, m, newrho);
    UNPROTECT(1);
    PROTECT(t = mkString(generic));
    defineVar(R_dot_Generic, t, newrho);
    UNPROTECT(1);
    defineVar(R_dot_Group, lgr, newrho);
    set = length(lclass) - lwhich;
    PROTECT(t = allocVector(STRSXP, set));
    for(j = 0 ; j < set ; j++ )
	SET_STRING_ELT(t, j, duplicate(STRING_ELT(lclass, lwhich++)));
    defineVar(R_dot_Class, t, newrho);
    UNPROTECT(1);
    defineVar(R_dot_GenericCallEnv, rho, newrho);
    defineVar(R_dot_GenericDefEnv, R_BaseEnv, newrho);

    PROTECT(t = LCONS(lmeth, CDR(call)));
=======
    vmaxset(vmax);

    newvars = PROTECT(createS3Vars(
        PROTECT(mkString(generic)),
        lgr,
        PROTECT(stringSuffix(lclass, lwhich)),
        m,
        rho,
        R_BaseEnv
    ));

    PROTECT(t = LCONS(lmeth, CDR(call)));
>>>>>>> eval.c

    /* the arguments have been evaluated; since we are passing them */
    /* out to a closure we need to wrap them in promises so that */
    /* they get duplicated and things like missing/substitute work. */
    {
	GCStackRoot<Expression>
	    newcall(new Expression(m->symbol(), callx->tail()));
	ArgList arglist(callargs, ArgList::EVALUATED);
	arglist.wrapInPromises(nullptr);
	// Ensure positional matching for operators:
	if (isOps)
	    arglist.stripTags();
	Closure* func = SEXP_downcast<Closure*>(m->function());
	*ans = func->invoke(callenv, &arglist, newcall, supp_frame);
    }
<<<<<<< eval.cpp
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

    *ans = applyClosure(t, lsxp, s, rho, newrho);
    UNPROTECT(5);
=======

    *ans = applyClosure(t, lsxp, s, rho, newvars);
    UNPROTECT(8);
>>>>>>> eval.c
    return 1;
}

/* start of bytecode section */
static int R_bcVersion = 8;
static int R_bcMinVersion = 6;

<<<<<<< eval.cpp
static GCRoot<> R_AddSym = nullptr;
static GCRoot<> R_SubSym = nullptr;
static GCRoot<> R_MulSym = nullptr;
static GCRoot<> R_DivSym = nullptr;
static GCRoot<> R_ExptSym = nullptr;
static GCRoot<> R_SqrtSym = nullptr;
static GCRoot<> R_ExpSym = nullptr;
static GCRoot<> R_EqSym = nullptr;
static GCRoot<> R_NeSym = nullptr;
static GCRoot<> R_LtSym = nullptr;
static GCRoot<> R_LeSym = nullptr;
static GCRoot<> R_GeSym = nullptr;
static GCRoot<> R_GtSym = nullptr;
static GCRoot<> R_AndSym = nullptr;
static GCRoot<> R_OrSym = nullptr;
static GCRoot<> R_NotSym = nullptr;
static GCRoot<> R_SubsetSym = nullptr;
static GCRoot<> R_SubassignSym = nullptr;
static GCRoot<> R_CSym = nullptr;
static GCRoot<> R_Subset2Sym = nullptr;
static GCRoot<> R_Subassign2Sym = nullptr;
static GCRoot<> R_valueSym = nullptr;
static GCRoot<> R_TrueValue = nullptr;
static GCRoot<> R_FalseValue = nullptr;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static SEXP R_AddSym = NULL;
static SEXP R_SubSym = NULL;
static SEXP R_MulSym = NULL;
static SEXP R_DivSym = NULL;
static SEXP R_ExptSym = NULL;
static SEXP R_SqrtSym = NULL;
static SEXP R_ExpSym = NULL;
static SEXP R_EqSym = NULL;
static SEXP R_NeSym = NULL;
static SEXP R_LtSym = NULL;
static SEXP R_LeSym = NULL;
static SEXP R_GeSym = NULL;
static SEXP R_GtSym = NULL;
static SEXP R_AndSym = NULL;
static SEXP R_OrSym = NULL;
static SEXP R_NotSym = NULL;
static SEXP R_SubsetSym = NULL;
static SEXP R_SubassignSym = NULL;
static SEXP R_CSym = NULL;
static SEXP R_Subset2Sym = NULL;
static SEXP R_Subassign2Sym = NULL;
static SEXP R_valueSym = NULL;
static SEXP R_TrueValue = NULL;
static SEXP R_FalseValue = NULL;

#if defined(__GNUC__) && ! defined(BC_PROFILING) && (! defined(NO_THREADED_CODE))
# define THREADED_CODE
#endif
=======
static SEXP R_AddSym = NULL;
static SEXP R_SubSym = NULL;
static SEXP R_MulSym = NULL;
static SEXP R_DivSym = NULL;
static SEXP R_ExptSym = NULL;
static SEXP R_SqrtSym = NULL;
static SEXP R_ExpSym = NULL;
static SEXP R_EqSym = NULL;
static SEXP R_NeSym = NULL;
static SEXP R_LtSym = NULL;
static SEXP R_LeSym = NULL;
static SEXP R_GeSym = NULL;
static SEXP R_GtSym = NULL;
static SEXP R_AndSym = NULL;
static SEXP R_OrSym = NULL;
static SEXP R_NotSym = NULL;
static SEXP R_CSym = NULL;
static SEXP R_LogSym = NULL;

#if defined(__GNUC__) && ! defined(BC_PROFILING) && (! defined(NO_THREADED_CODE))
# define THREADED_CODE
#endif
>>>>>>> eval.c

attribute_hidden
void R_initialize_bcode(void)
{
<<<<<<< eval.cpp
  R_AddSym = Rf_install("+");
  R_SubSym = Rf_install("-");
  R_MulSym = Rf_install("*");
  R_DivSym = Rf_install("/");
  R_ExptSym = Rf_install("^");
  R_SqrtSym = Rf_install("sqrt");
  R_ExpSym = Rf_install("exp");
  R_EqSym = Rf_install("==");
  R_NeSym = Rf_install("!=");
  R_LtSym = Rf_install("<");
  R_LeSym = Rf_install("<=");
  R_GeSym = Rf_install(">=");
  R_GtSym = Rf_install(">");
  R_AndSym = Rf_install("&");
  R_OrSym = Rf_install("|");
  R_NotSym = Rf_install("!");
  R_SubsetSym = R_BracketSymbol; /* "[" */
  R_SubassignSym = Rf_install("[<-");
  R_CSym = Rf_install("c");
  R_Subset2Sym = R_Bracket2Symbol; /* "[[" */
  R_Subassign2Sym = Rf_install("[[<-");
  R_valueSym = Rf_install("value");

  R_TrueValue = Rf_mkTrue();
  SET_NAMED(R_TrueValue, 2);
  R_PreserveObject(R_TrueValue);
  R_FalseValue = Rf_mkFalse();
  SET_NAMED(R_FalseValue, 2);
  R_PreserveObject(R_FalseValue);
  ByteCode::initialize();
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  R_AddSym = install("+");
  R_SubSym = install("-");
  R_MulSym = install("*");
  R_DivSym = install("/");
  R_ExptSym = install("^");
  R_SqrtSym = install("sqrt");
  R_ExpSym = install("exp");
  R_EqSym = install("==");
  R_NeSym = install("!=");
  R_LtSym = install("<");
  R_LeSym = install("<=");
  R_GeSym = install(">=");
  R_GtSym = install(">");
  R_AndSym = install("&");
  R_OrSym = install("|");
  R_NotSym = install("!");
  R_SubsetSym = R_BracketSymbol; /* "[" */
  R_SubassignSym = install("[<-");
  R_CSym = install("c");
  R_Subset2Sym = R_Bracket2Symbol; /* "[[" */
  R_Subassign2Sym = install("[[<-");
  R_valueSym = install("value");

  R_TrueValue = mkTrue();
  SET_NAMED(R_TrueValue, 2);
  R_PreserveObject(R_TrueValue);
  R_FalseValue = mkFalse();
  SET_NAMED(R_FalseValue, 2);
  R_PreserveObject(R_FalseValue);
#ifdef THREADED_CODE
  bcEval(NULL, NULL, FALSE);
#endif
=======
  R_AddSym = install("+");
  R_SubSym = install("-");
  R_MulSym = install("*");
  R_DivSym = install("/");
  R_ExptSym = install("^");
  R_SqrtSym = install("sqrt");
  R_ExpSym = install("exp");
  R_EqSym = install("==");
  R_NeSym = install("!=");
  R_LtSym = install("<");
  R_LeSym = install("<=");
  R_GeSym = install(">=");
  R_GtSym = install(">");
  R_AndSym = install("&");
  R_OrSym = install("|");
  R_NotSym = install("!");
  R_CSym = install("c");
  R_LogSym = install("log");

#ifdef THREADED_CODE
  bcEval(NULL, NULL, FALSE);
#endif
>>>>>>> eval.c
}

enum {
  BCMISMATCH_OP,
  RETURN_OP,
  GOTO_OP,
  BRIFNOT_OP,
  POP_OP,
  DUP_OP,
  PRINTVALUE_OP,
  STARTLOOPCNTXT_OP,
  ENDLOOPCNTXT_OP,
  DOLOOPNEXT_OP,
  DOLOOPBREAK_OP,
  STARTFOR_OP,
  STEPFOR_OP,
  ENDFOR_OP,
  SETLOOPVAL_OP,
  INVISIBLE_OP,
  LDCONST_OP,
  LDNULL_OP,
  LDTRUE_OP,
  LDFALSE_OP,
  GETVAR_OP,
  DDVAL_OP,
  SETVAR_OP,
  GETFUN_OP,
  GETGLOBFUN_OP,
  GETSYMFUN_OP,
  GETBUILTIN_OP,
  GETINTLBUILTIN_OP,
  CHECKFUN_OP,
  MAKEPROM_OP,
  DOMISSING_OP,
  SETTAG_OP,
  DODOTS_OP,
  PUSHARG_OP,
  PUSHCONSTARG_OP,
  PUSHNULLARG_OP,
  PUSHTRUEARG_OP,
  PUSHFALSEARG_OP,
  CALL_OP,
  CALLBUILTIN_OP,
  CALLSPECIAL_OP,
  MAKECLOSURE_OP,
  UMINUS_OP,
  UPLUS_OP,
  ADD_OP,
  SUB_OP,
  MUL_OP,
  DIV_OP,
  EXPT_OP,
  SQRT_OP,
  EXP_OP,
  EQ_OP,
  NE_OP,
  LT_OP,
  LE_OP,
  GE_OP,
  GT_OP,
  AND_OP,
  OR_OP,
  NOT_OP,
  DOTSERR_OP,
  STARTASSIGN_OP,
  ENDASSIGN_OP,
  STARTSUBSET_OP,
  DFLTSUBSET_OP,
  STARTSUBASSIGN_OP,
  DFLTSUBASSIGN_OP,
  STARTC_OP,
  DFLTC_OP,
  STARTSUBSET2_OP,
  DFLTSUBSET2_OP,
  STARTSUBASSIGN2_OP,
  DFLTSUBASSIGN2_OP,
  DOLLAR_OP,
  DOLLARGETS_OP,
  ISNULL_OP,
  ISLOGICAL_OP,
  ISINTEGER_OP,
  ISDOUBLE_OP,
  ISCOMPLEX_OP,
  ISCHARACTER_OP,
  ISSYMBOL_OP,
  ISOBJECT_OP,
  ISNUMERIC_OP,
  VECSUBSET_OP,
  MATSUBSET_OP,
  VECSUBASSIGN_OP,
  MATSUBASSIGN_OP,
  AND1ST_OP,
  AND2ND_OP,
  OR1ST_OP,
  OR2ND_OP,
  GETVAR_MISSOK_OP,
  DDVAL_MISSOK_OP,
  VISIBLE_OP,
  SETVAR2_OP,
  STARTASSIGN2_OP,
  ENDASSIGN2_OP,
  SETTER_CALL_OP,
  GETTER_CALL_OP,
  SWAP_OP,
  DUP2ND_OP,
  SWITCH_OP,
  RETURNJMP_OP,
  STARTSUBSET_N_OP,
  STARTSUBASSIGN_N_OP,
  VECSUBSET2_OP,
  MATSUBSET2_OP,
  VECSUBASSIGN2_OP,
  MATSUBASSIGN2_OP,
  STARTSUBSET2_N_OP,
  STARTSUBASSIGN2_N_OP,
  SUBSET_N_OP,
  SUBSET2_N_OP,
  SUBASSIGN_N_OP,
  SUBASSIGN2_N_OP,
  LOG_OP,
  LOGBASE_OP,
  MATH1_OP,
  DOTCALL_OP,
  COLON_OP,
  SEQALONG_OP,
  SEQLEN_OP,
  OPCOUNT
};


/* Use header files!  2007/06/11 arr
SEXP R_unary(SEXP, SEXP, SEXP);
SEXP R_binary(SEXP, SEXP, SEXP, SEXP);
SEXP do_math1(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2_dflt(SEXP, SEXP, SEXP, SEXP);
*/

<<<<<<< eval.cpp
// Note that this macro name uses END in the sense of the C++ standard
// library end(), i.e. one past the current top element of the stack,
// not in the way that CR uses R_BCNodeStackEnd, which relates to the
// end of allocated storage.
#define NSFROMEND(i) (s_nodestack->fromEnd(i))
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define GETSTACK_PTR(s) (*(s))
#define GETSTACK(i) GETSTACK_PTR(R_BCNodeStackTop + (i))
=======
static SEXP seq_int(int n1, int n2)
{
    int n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
    SEXP ans = allocVector(INTSXP, n);
    int *data = INTEGER(ans);
    if (n1 <= n2)
	for (int i = 0; i < n; i++)
	    data[i] = n1 + i;
    else
	for (int i = 0; i < n; i++)
	    data[i] = n1 - i;
    return ans;
}

#ifdef TYPED_STACK
# define COMPACT_INTSEQ
# ifdef COMPACT_INTSEQ
#  define INTSEQSXP 9999
# endif
static R_INLINE SEXP GETSTACK_PTR_TAG(R_bcstack_t *s)
{
    /* no error checking since only called with tag != 0 */
    SEXP value;
    switch (s->tag) {
    case REALSXP: 
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = s->u.dval;
	break;
    case INTSXP:
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = s->u.ival;
	break;
    case LGLSXP:
	value = ScalarLogical(s->u.ival);
	break;
#ifdef COMPACT_INTSEQ
    case INTSEQSXP:
	{
	    int *seqinfo = INTEGER(s->u.sxpval);
	    value = seq_int(seqinfo[0], seqinfo[1]);
	}
	break;
#endif
    default: /* not reached */
	value = NULL;
    }
    s->tag = 0;
    s->u.sxpval = value;
    return value;
}
#define GETSTACK_PTR(s) ((s)->tag ? GETSTACK_PTR_TAG(s) : (s)->u.sxpval)

#define GETSTACK_SXPVAL_PTR(s) ((s)->u.sxpval)
>>>>>>> eval.c

<<<<<<< eval.cpp
#define GETSTACK(i) NSFROMEND(-i)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define SETSTACK_PTR(s, v) do { \
    SEXP __v__ = (v); \
    *(s) = __v__; \
} while (0)

#define SETSTACK(i, v) SETSTACK_PTR(R_BCNodeStackTop + (i), v)
=======
#define SETSTACK_PTR(s, v) do { \
    SEXP __v__ = (v); \
    (s)->tag = 0; \
    (s)->u.sxpval = __v__; \
} while (0)

#define SETSTACK_REAL_PTR(s, v) do { \
    double __v__ = (v); \
    (s)->tag = REALSXP; \
    (s)->u.dval = __v__; \
} while (0)
>>>>>>> eval.c

<<<<<<< eval.cpp
#define SETSTACK(i, v) NSFROMEND(-i) = v
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define SETSTACK_REAL_PTR(s, v) SETSTACK_PTR(s, ScalarReal(v))
=======
#define SETSTACK_INTEGER_PTR(s, v) do { \
    int __v__ = (v); \
    (s)->tag = INTSXP; \
    (s)->u.ival = __v__; \
} while (0)
>>>>>>> eval.c

<<<<<<< eval.cpp
#define SETSTACK_REAL(i, v) (NSFROMEND(-i) = Rf_ScalarReal(v))
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define SETSTACK_REAL(i, v) SETSTACK_REAL_PTR(R_BCNodeStackTop + (i), v)
=======
#define SETSTACK_LOGICAL_PTR(s, v) do {		\
	int __v__ = (v);			\
	(s)->tag = LGLSXP;			\
	if (__v__ == NA_LOGICAL)		\
	    (s)->u.ival = NA_LOGICAL;		\
	else					\
	    (s)->u.ival = __v__ ? TRUE : FALSE;	\
    } while (0)
>>>>>>> eval.c

<<<<<<< eval.cpp
#define SETSTACK_INTEGER(i, v) (NSFROMEND(-i) = Rf_ScalarInteger(v))
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define SETSTACK_INTEGER_PTR(s, v) SETSTACK_PTR(s, ScalarInteger(v))
=======
#define IS_STACKVAL_BOXED(idx)	(R_BCNodeStackTop[idx].tag == 0)
#else
#define GETSTACK_PTR(s) (*(s))
>>>>>>> eval.c

<<<<<<< eval.cpp
#define SETSTACK_LOGICAL(i, v) do {		\
    int __ssl_v__ = (v); \
    if (__ssl_v__ == NA_LOGICAL) \
	NSFROMEND(-i) = Rf_ScalarLogical(NA_LOGICAL);	\
    else \
	NSFROMEND(-i) = ( __ssl_v__ ? R_TrueValue : R_FalseValue);	\
} while(0)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define SETSTACK_INTEGER(i, v) SETSTACK_INTEGER_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_LOGICAL_PTR(s, v) do { \
    int __ssl_v__ = (v); \
    if (__ssl_v__ == NA_LOGICAL) \
	SETSTACK_PTR(s, ScalarLogical(NA_LOGICAL)); \
    else \
	SETSTACK_PTR(s, __ssl_v__ ? R_TrueValue : R_FalseValue); \
} while(0)
=======
#define GETSTACK_SXPVAL_PTR(s) (*(s))

#define SETSTACK_PTR(s, v) do { \
    SEXP __v__ = (v); \
    *(s) = __v__; \
} while (0)
>>>>>>> eval.c

<<<<<<< eval.cpp
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define SETSTACK_LOGICAL(i, v) SETSTACK_LOGICAL_PTR(R_BCNodeStackTop + (i), v)
=======
#define SETSTACK_REAL_PTR(s, v) SETSTACK_PTR(s, ScalarReal(v))
#define SETSTACK_INTEGER_PTR(s, v) SETSTACK_PTR(s, ScalarInteger(v))
#define SETSTACK_LOGICAL_PTR(s, v) SETSTACK_PTR(s, ScalarLogical(v))
>>>>>>> eval.c

#define IS_STACKVAL_BOXED(idx)	(TRUE)
#endif

#if defined(TYPED_STACK) && defined(COMPACT_INTSEQ)
#define SETSTACK_INTSEQ(idx, rn1, rn2) do {	\
	SEXP info = allocVector(INTSXP, 2);	\
	INTEGER(info)[0] = (int) rn1;		\
	INTEGER(info)[1] = (int) rn2;		\
	R_BCNodeStackTop[idx].u.sxpval = info;	\
	R_BCNodeStackTop[idx].tag = INTSEQSXP;	\
    } while (0)
#else
#define SETSTACK_INTSEQ(idx, rn1, rn2) \
    SETSTACK(idx, seq_int((int) rn1, (int) rn2))
#endif

#define GETSTACK_SXPVAL(i) GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (i))

#define GETSTACK(i) GETSTACK_PTR(R_BCNodeStackTop + (i))

#define SETSTACK(i, v) SETSTACK_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_REAL(i, v) SETSTACK_REAL_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_INTEGER(i, v) SETSTACK_INTEGER_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_LOGICAL(i, v) SETSTACK_LOGICAL_PTR(R_BCNodeStackTop + (i), v)

/* The next two macros will reuse a provided scalar box, if
   provided. The box is assumed to be of the correct typa and size. */
#define SETSTACK_REAL_EX(idx, dval, ans) do { \
	if (ans) {			      \
	    REAL(ans)[0] = dval;	      \
	    SETSTACK(idx, ans);		      \
	}				      \
	else SETSTACK_REAL(idx, dval);	      \
    } while (0)

#define SETSTACK_INTEGER_EX(idx, ival, ans) do { \
	if (ans) {				 \
	    INTEGER(ans)[0] = ival;		 \
	    SETSTACK(idx, ans);			 \
	}					 \
	else SETSTACK_INTEGER(idx, ival);	 \
    } while (0)

typedef union { double dval; int ival; } scalar_value_t;

/* bcStackScalar() checks whether the object in the specified stack
   location is a simple real, integer, or logical scalar (i.e. length
   one and no attributes.  If so, the type is returned as the function
   value and the value is returned in the structure pointed to by the
   second argument; if not, then zero is returned as the function
<<<<<<< eval.cpp
   value. */
static R_INLINE int bcStackScalar(RObject* x, scalar_value_t *v)
{
    if (ATTRIB(x) == R_NilValue) {
	switch(TYPEOF(x)) {
	case REALSXP:
	    if (LENGTH(x) == 1) {
		v->dval = REAL(x)[0];
		return REALSXP;
	    }
	    else return 0;
	case INTSXP:
	    if (LENGTH(x) == 1) {
		v->ival = INTEGER(x)[0];
		return INTSXP;
	    }
	    else return 0;
	case LGLSXP:
	    if (LENGTH(x) == 1) {
		v->ival = LOGICAL(x)[0];
		return LGLSXP;
	    }
	    else return 0;
	default: return 0;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
   value. */
static R_INLINE int bcStackScalar(R_bcstack_t *s, scalar_value_t *v)
{
    SEXP x = *s;
    if (ATTRIB(x) == R_NilValue) {
	switch(TYPEOF(x)) {
	case REALSXP:
	    if (LENGTH(x) == 1) {
		v->dval = REAL(x)[0];
		return REALSXP;
	    }
	    else return 0;
	case INTSXP:
	    if (LENGTH(x) == 1) {
		v->ival = INTEGER(x)[0];
		return INTSXP;
	    }
	    else return 0;
	case LGLSXP:
	    if (LENGTH(x) == 1) {
		v->ival = LOGICAL(x)[0];
		return LGLSXP;
	    }
	    else return 0;
	default: return 0;
=======
   value. The boxed value can be returned through a pointer argument
   if it is suitable for re-use. */
static R_INLINE int bcStackScalarEx(R_bcstack_t *s, scalar_value_t *v,
				    SEXP *pv)
{
#ifdef TYPED_STACK
    int tag = s->tag;

    if (tag) 
	switch(tag) {
	case REALSXP: v->dval = s->u.dval; return tag;
	case INTSXP: v->ival = s->u.ival; return tag;
	case LGLSXP: v->ival = s->u.ival; return tag;
>>>>>>> eval.c
	}
#endif
    SEXP x = GETSTACK_SXPVAL_PTR(s);
    if (IS_SIMPLE_SCALAR(x, REALSXP)) {
#ifndef NO_SAVE_ALLOC
	if (pv && NO_REFERENCES(x)) *pv = x;
#endif
	v->dval = REAL(x)[0];
	return REALSXP;
    }
    else if (IS_SIMPLE_SCALAR(x, INTSXP)) {
#ifndef NO_SAVE_ALLOC
	if (pv && NO_REFERENCES(x)) *pv = x;
#endif
	v->ival = INTEGER(x)[0];
	return INTSXP;
    }
    else if (IS_SIMPLE_SCALAR(x, LGLSXP)) {
	v->ival = LOGICAL(x)[0];
	return LGLSXP;
    }
    else return 0;
}

#define bcStackScalar(s, v) bcStackScalarEx(s, v, NULL)

#define INTEGER_TO_LOGICAL(x) \
    ((x) == NA_INTEGER ? NA_LOGICAL : (x) ? TRUE : FALSE)
#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))
#define LOGICAL_TO_REAL(x) ((x) == NA_LOGICAL ? NA_REAL : (x))

static R_INLINE int bcStackScalarRealEx(R_bcstack_t *s, scalar_value_t *px,
					SEXP *pv)
{
    int typex = bcStackScalarEx(s, px, pv);
    if (typex == INTSXP) {
	typex = REALSXP;
	px->dval = INTEGER_TO_REAL(px->ival);
	if (pv) *pv = NULL;
    }
    return typex;
}

#define DO_FAST_RELOP2(op,a,b) do { \
    SKIP_OP(); \
    SETSTACK_LOGICAL(-2, ((a) op (b)) ? TRUE : FALSE);	\
    s_nodestack->pop();					\
    NEXT(); \
} while (0)

# define FastRelop2(op,opval,opsym) do { \
    scalar_value_t vx; \
    scalar_value_t vy; \
    int typex = bcStackScalar(NSFROMEND(2), &vx);	  \
    int typey = bcStackScalar(NSFROMEND(1), &vy);	  \
    if (typex == REALSXP && ! ISNAN(vx.dval)) { \
	if (typey == REALSXP && ! ISNAN(vy.dval)) \
	    DO_FAST_RELOP2(op, vx.dval, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) \
	    DO_FAST_RELOP2(op, vx.dval, vy.ival); \
    } \
    else if (typex == INTSXP && vx.ival != NA_INTEGER) { \
	if (typey == REALSXP && ! ISNAN(vy.dval)) \
	    DO_FAST_RELOP2(op, vx.ival, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) { \
	    DO_FAST_RELOP2(op, vx.ival, vy.ival); \
	} \
    } \
    Relop2(opval, opsym); \
} while (0)

static R_INLINE SEXP getPrimitive(SEXP symbol, SEXPTYPE type)
{
    SEXP value = SYMVALUE(symbol);
    if (TYPEOF(value) == PROMSXP) {
	value = forcePromise(value);
	SET_NAMED(value, 2);
    }
    if (TYPEOF(value) != type) {
	/* probably means a package redefined the base function so
	   try to get the real thing from the internal table of
	   primitives */
	value = R_Primitive(CHAR(PRINTNAME(symbol)));
	if (TYPEOF(value) != type)
	    /* if that doesn't work we signal an error */
	    Rf_error(_("\"%s\" is not a %s function"),
		  CHAR(PRINTNAME(symbol)),
		  type == BUILTINSXP ? "BUILTIN" : "SPECIAL");
    }
    return value;
}

static SEXP cmp_relop(SEXP call, int opval, SEXP opsym, SEXP x, SEXP y,
		      SEXP rho)
{
<<<<<<< eval.cpp
    const BuiltInFunction* op = SEXP_downcast<const BuiltInFunction*>(
	getPrimitive(opsym, BUILTINSXP));
    RObject* args[] = { x, y };
    return do_relop(SEXP_downcast<Expression*>(call),
		    op,
		    SEXP_downcast<Environment*>(rho),
		    args, 2,
		    SEXP_downcast<PairList*>(CDR(call)));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x) || isObject(y)) {
	SEXP args, ans;
	args = CONS(x, CONS(y, R_NilValue));
	PROTECT(args);
	if (DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return do_relop_dflt(call, op, x, y);
=======
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x) || isObject(y)) {
	SEXP args, ans;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	PROTECT(args);
	if (DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return do_relop_dflt(call, op, x, y);
>>>>>>> eval.c
}

static SEXP cmp_arith1(SEXP call, SEXP opsym, SEXP x, SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (Rf_isObject(x)) {
	SEXP args, ans;
<<<<<<< eval.cpp
	args = PairList::cons(x);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	args = CONS(x, R_NilValue);
=======
	args = CONS_NR(x, R_NilValue);
>>>>>>> eval.c
	PROTECT(args);
	if (Rf_DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return R_unary(call, op, x);
}

static SEXP cmp_arith2(SEXP call, int opval, SEXP opsym, SEXP x, SEXP y,
		       SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	SET_NAMED(op, 2);
    }
    if (Rf_isObject(x) || Rf_isObject(y)) {
	SEXP args, ans;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	PROTECT(args);
	if (Rf_DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return R_binary(call, op, x, y);
}

#define Builtin1(do_fun,which,rho) do { \
<<<<<<< eval.cpp
  SEXP call = (*constants)[GETOP()]; \
  SETSTACK(-1, CONS(GETSTACK(-1), R_NilValue));		     \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SETSTACK(-1, CONS(GETSTACK(-1), R_NilValue));		     \
=======
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SETSTACK(-1, CONS_NR(GETSTACK(-1), R_NilValue));		     \
>>>>>>> eval.c
  SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP), \
		      GETSTACK(-1), rho));		     \
  NEXT(); \
} while(0)

#define Builtin2(do_fun,which,rho) do {		     \
<<<<<<< eval.cpp
  SEXP call = (*constants)[GETOP()]; \
  PairList* tmp = PairList::cons(GETSTACK(-1));	\
  SETSTACK(-2, CONS(GETSTACK(-2), tmp));     \
  s_nodestack->pop(); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SEXP tmp = CONS(GETSTACK(-1), R_NilValue); \
  SETSTACK(-2, CONS(GETSTACK(-2), tmp));     \
  R_BCNodeStackTop--; \
=======
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SEXP tmp = CONS_NR(GETSTACK(-1), R_NilValue); \
  SETSTACK(-2, CONS_NR(GETSTACK(-2), tmp));     \
  R_BCNodeStackTop--; \
>>>>>>> eval.c
  SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP),	\
		      GETSTACK(-1), rho));			\
  NEXT(); \
} while(0)

#define NewBuiltin2(do_fun,opval,opsym,rho) do {        \
  SEXP call = (*constants)[GETOP()]; \
  SEXP x = GETSTACK(-2); \
  SEXP y = GETSTACK(-1); \
  SETSTACK(-2, do_fun(call, opval, opsym, x, y,rho));	\
  s_nodestack->pop(); \
  NEXT(); \
} while(0)

#define Arith1(opsym) do {		\
  SEXP call = (*constants)[GETOP()]; \
  SEXP x = GETSTACK(-1); \
  SETSTACK(-1, cmp_arith1(call, opsym, x, rho)); \
  NEXT(); \
} while(0)


#define Arith2(opval,opsym) NewBuiltin2(cmp_arith2,opval,opsym,rho)
#define Relop2(opval,opsym) NewBuiltin2(cmp_relop,opval,opsym,rho)

#define FastMath1(fun, sym) do {					\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarEx(R_BCNodeStackTop - 1, &vx, &sa);	\
	if (typex == REALSXP) {						\
	    SKIP_OP();							\
	    SETSTACK_REAL_EX(-1, fun(vx.dval), sa);			\
	    NEXT();							\
	}								\
	else if (typex == INTSXP && vx.ival != NA_INTEGER) {		\
	    SKIP_OP();							\
	    SETSTACK_REAL_EX(-1, fun(vx.ival), NULL);			\
	    NEXT();							\
	}								\
	Builtin1(do_math1,sym,rho);					\
    } while (0)

#ifdef NO_SAVE_ALLOC
# define DO_FAST_BINOP(fun,a,b,v) do {		\
    SKIP_OP(); \
<<<<<<< eval.cpp
    SETSTACK_REAL(-2, (a) op (b)); \
    s_nodestack->pop(); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SETSTACK_REAL(-2, (a) op (b)); \
    R_BCNodeStackTop--; \
=======
    SETSTACK_REAL(-2, fun(a, b));		\
    R_BCNodeStackTop--; \
>>>>>>> eval.c
    NEXT(); \
} while (0)

<<<<<<< eval.cpp
# define DO_FAST_BINOP_INT(op, a, b) do { \
    double dval = (double( (a))) op (double( (b)));	\
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
# define DO_FAST_BINOP_INT(op, a, b) do { \
    double dval = ((double) (a)) op ((double) (b)); \
=======
# define DO_FAST_BINOP_INT(fun, a, b, v) do {	    \
    double dval = fun((double) (a), (double) (b));	\
>>>>>>> eval.c
    if (dval <= INT_MAX && dval >= INT_MIN + 1) { \
	SKIP_OP();				  \
	SETSTACK_INTEGER(-2, int( dval));	\
	s_nodestack->pop();			\
	NEXT(); \
    } \
} while(0)
#else
/* these reuse one of the two values on the top of the stack if it is
   of the right type and has no references. It is known that both of
   these will have length one and have no attributes. */
# define DO_FAST_BINOP(fun, a, b, ans) do {				\
	SKIP_OP();							\
	double dval = fun(a, b);					\
	SETSTACK_REAL_EX(-2, dval, ans);				\
	R_BCNodeStackTop--;						\
	NEXT();								\
    } while (0)

# define DO_FAST_BINOP_INT(fun, a, b, ans) do {				\
	double dval = fun((double) (a), (double) (b));			\
	if (dval <= INT_MAX && dval >= INT_MIN + 1) {			\
	    int val = (int) dval;					\
	    SKIP_OP();							\
	    SETSTACK_INTEGER_EX(-2, val, ans);				\
	    R_BCNodeStackTop--;						\
	    NEXT();							\
	}								\
    } while(0)
#endif

#define FastUnary(op, opsym) do {					\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarEx(R_BCNodeStackTop - 1, &vx, &sa);	\
	if (typex == REALSXP) {						\
	    SKIP_OP();							\
	    SETSTACK_REAL_EX(-1, op vx.dval, sa);			\
	    NEXT();							\
	}								\
	else if (typex == INTSXP && vx.ival != NA_INTEGER) {		\
	    SKIP_OP();							\
	    SETSTACK_INTEGER_EX(-1, op vx.ival, sa);			\
	    NEXT();							\
	}								\
	Arith1(opsym);							\
    } while (0)

# define FastBinary(op,opval,opsym) do { \
    scalar_value_t vx; \
    scalar_value_t vy; \
<<<<<<< eval.cpp
    int typex = bcStackScalar(NSFROMEND(2), &vx);	  \
    int typey = bcStackScalar(NSFROMEND(1), &vy);	  \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    int typex = bcStackScalar(R_BCNodeStackTop - 2, &vx); \
    int typey = bcStackScalar(R_BCNodeStackTop - 1, &vy); \
=======
    SEXP sa = NULL; \
    SEXP sb = NULL; \
    int typex = bcStackScalarEx(R_BCNodeStackTop - 2, &vx, &sa);	\
    int typey = bcStackScalarEx(R_BCNodeStackTop - 1, &vy, &sb);	\
>>>>>>> eval.c
    if (typex == REALSXP) { \
<<<<<<< eval.cpp
	if (typey == REALSXP)			 \
	    DO_FAST_BINOP(op, vx.dval, vy.dval); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.dval, vy.dval); \
=======
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.dval, vy.dval, sa ? sa : sb);	\
>>>>>>> eval.c
	else if (typey == INTSXP && vy.ival != NA_INTEGER) \
	    DO_FAST_BINOP(op, vx.dval, vy.ival, sa);	   \
    } \
    else if (typex == INTSXP && vx.ival != NA_INTEGER) { \
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.ival, vy.dval, sb);	     \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) { \
<<<<<<< eval.cpp
	    if (opval == DIVOP) \
		DO_FAST_BINOP(op, double( vx.ival), double( vy.ival));	\
	    else							\
		DO_FAST_BINOP_INT(op, vx.ival, vy.ival); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	    if (opval == DIVOP) \
		DO_FAST_BINOP(op, (double) vx.ival, (double) vy.ival); \
	    else \
		DO_FAST_BINOP_INT(op, vx.ival, vy.ival); \
=======
	    if (opval == DIVOP || opval == POWOP) \
		DO_FAST_BINOP(op, (double) vx.ival, (double) vy.ival, NULL); \
	    else \
		DO_FAST_BINOP_INT(op, vx.ival, vy.ival, sa ? sa : sb);	\
>>>>>>> eval.c
	} \
    } \
    Arith2(opval, opsym); \
} while (0)

<<<<<<< eval.cpp
#define BCNPUSH(v) (s_nodestack->push(v))

#define BCNDUP() (s_nodestack->push(s_nodestack->fromEnd(1)))
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define BCNPUSH(v) do { \
  SEXP __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1] = __value__; \
  R_BCNodeStackTop = __ntop__; \
} while (0)
=======
#define R_ADD(x, y) ((x) + (y))
#define R_SUB(x, y) ((x) - (y))
#define R_MUL(x, y) ((x) * (y))
#define R_DIV(x, y) ((x) / (y))

#include "arithmetic.h"

/* The current (as of r67808) Windows toolchain compiles explicit sqrt
   calls in a way that returns a different NaN than NA_real_ when
   called with NA_real_. Not sure this is a bug in the Windows
   toolchain or in our expectations, but these defines attempt to work
   around this. */
#if (defined(_WIN32) || defined(_WIN64)) && defined(__GNUC__) && \
    __GNUC__ <= 4
# define R_sqrt(x) (ISNAN(x) ? x : sqrt(x))
#else
# define R_sqrt sqrt
#endif

#define DO_LOG() do {							\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vx, &sa); \
	if (typex == REALSXP) {						\
	    SKIP_OP();							\
	    SETSTACK_REAL_EX(-1, R_log(vx.dval), sa);			\
	    NEXT();							\
	}								\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	SEXP args = CONS_NR(GETSTACK(-1), R_NilValue);			\
	SEXP op = getPrimitive(R_LogSym, SPECIALSXP);			\
	SETSTACK(-1, args); /* to protect */				\
	SETSTACK(-1, do_log_builtin(call, op, args, rho));		\
	NEXT();								\
 } while (0)

#define DO_LOGBASE() do {						\
	scalar_value_t vx, vy;						\
	SEXP sa = NULL;							\
	SEXP sb = NULL;							\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 2, &vx, &sa); \
	int typey = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vy, &sb); \
	if (typex == REALSXP && typey == REALSXP) {			\
	    SKIP_OP();							\
	    R_BCNodeStackTop--;						\
	    SETSTACK_REAL_EX(-1, logbase(vx.dval, vy.dval), sa);	\
	    NEXT();							\
	}								\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	SEXP args = CONS_NR(GETSTACK(-2), CONS_NR(GETSTACK(-1), R_NilValue)); \
	SEXP op = getPrimitive(R_LogSym, SPECIALSXP);			\
	R_BCNodeStackTop--;						\
	SETSTACK(-1, args); /* to protect */				\
	SETSTACK(-1, do_log_builtin(call, op, args, rho));		\
	NEXT();								\
    } while (0)

#include <Rmath.h>
/* Keep the order consistent with the order in the byte code compiler! */
static struct { const char *name; SEXP sym; double (*fun)(double); }
    math1funs[] = {
	{"floor", NULL, floor},
	{"ceiling", NULL, ceil},
	{"sign", NULL, sign},

	{"expm1", NULL, expm1},
	{"log1p", NULL, log1p},
	
	{"cos", NULL, cos},
	{"sin", NULL, sin},
	{"tan", NULL, tan},
	{"acos", NULL, acos},
	{"asin", NULL, asin},
	{"atan", NULL, atan},
	
	{"cosh", NULL, cosh},
	{"sinh", NULL, sinh},
	{"tanh", NULL, tanh},
	{"acosh", NULL, acosh},
	{"asinh", NULL, asinh},
	{"atanh", NULL, atanh},
	
	{"lgamma", NULL, lgammafn},
	{"gamma", NULL, gammafn},
	{"digamma", NULL, digamma},
	{"trigamma", NULL, trigamma},
	
	{"cospi", NULL, cospi},
	{"sinpi", NULL, sinpi},
#ifndef HAVE_TANPI
	{"tanpi", NULL, tanpi}
#else
	{"tanpi", NULL, Rtanpi}
#endif
    };
    
static R_INLINE double (*getMath1Fun(int i, SEXP call))(double) {
    if (math1funs[i].sym == NULL)
	math1funs[i].sym = install(math1funs[i].name);
    if (CAR(call) != math1funs[i].sym)
	error("math1 compiler/interpreter mismatch");
    return math1funs[i].fun;
}
    
#define DO_MATH1() do {							\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	double (*fun)(double) = getMath1Fun(GETOP(), call);		\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vx, &sa); \
	if (typex == REALSXP) {						\
	    SETSTACK_REAL_EX(-1, fun(vx.dval), sa);			\
	    NEXT();							\
	}								\
	SEXP args = CONS_NR(GETSTACK(-1), R_NilValue);			\
	SEXP sym = CAR(call);						\
	SEXP op = getPrimitive(sym, BUILTINSXP);			\
	SETSTACK(-1, args); /* to protect */				\
	SETSTACK(-1, do_math1(call, op, args, rho));			\
	NEXT();								\
    } while (0)

#include <Rdynpriv.h>

#define DOTCALL_MAX 16
#define DO_DOTCALL() do {						\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	int nargs = GETOP();						\
	DL_FUNC ofun = R_dotCallFn(GETSTACK(- nargs - 1), call, nargs);	\
	if (ofun && nargs <= DOTCALL_MAX) {				\
	    SEXP cargs[DOTCALL_MAX];					\
	    for (int i = 0; i < nargs; i++)				\
		cargs[i] = GETSTACK(i - nargs);				\
	    SEXP val = R_doDotCall(ofun, nargs, cargs, call);		\
	    R_BCNodeStackTop -= nargs;					\
	    SETSTACK(-1, val);						\
	    NEXT();							\
	}								\
	SEXP args = R_NilValue;						\
	while (nargs-- > 0) {						\
	    args = CONS_NR(GETSTACK(-1), args);				\
	    BCNPOP_IGNORE_VALUE();					\
	}								\
	args = CONS_NR(GETSTACK(-1), args);				\
	SETSTACK(-1, args); /* to protect */				\
	SEXP sym = CAR(call);						\
	SEXP op = getPrimitive(sym, BUILTINSXP);			\
	SETSTACK(-1, do_dotcall(call, op, args, rho));			\
	NEXT();								\
    } while (0)

#define DO_COLON() do {							\
	scalar_value_t vx;						\
	scalar_value_t vy;						\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 2, &vx, NULL); \
	int typey = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vy, NULL); \
	if (typex == REALSXP && typey == REALSXP) {			\
	    double rn1 = vx.dval;					\
	    double rn2 = vy.dval;					\
	    if (INT_MIN <= rn1 && INT_MAX >= rn1 &&			\
		INT_MIN <= rn2 && INT_MAX >- rn2 &&			\
		rn1 == (int) rn1 && rn2 == (int) rn2) {			\
		SKIP_OP(); /* skip 'call' index */			\
		R_BCNodeStackTop--;					\
		SETSTACK_INTSEQ(-1, rn1, rn2);				\
		NEXT();							\
	    }								\
	}								\
	Builtin2(do_colon, R_ColonSymbol, rho);				\
    } while (0)

#define DO_SEQ_ALONG() do {					\
	SEXP x = GETSTACK(-1);					\
	if (! OBJECT(x)) {					\
	    R_xlen_t len = xlength(x);				\
	    if (len >= 1 && len <= INT_MAX) {			\
		SKIP_OP(); /* skip 'call' index */		\
		SETSTACK_INTSEQ(-1, 1, len);			\
		NEXT();						\
	    }							\
	}							\
	Builtin1(do_seq_along, install("seq_along"), rho);	\
    } while (0)

#define DO_SEQ_LEN() do {						\
	scalar_value_t vx;						\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vx, NULL); \
	if (typex == REALSXP) {						\
	    double rlen = vx.dval;					\
	    if (1 <= rlen && INT_MAX >= rlen &&				\
		rlen == (int) rlen) {					\
		SKIP_OP(); /* skip 'call' index */			\
		SETSTACK_INTSEQ(-1, 1, rlen);				\
		NEXT();							\
	    }								\
	}								\
	Builtin1(do_seq_len, install("seq_len"), rho);			\
    } while (0)

static R_INLINE SEXP getForLoopSeq(int offset, Rboolean *iscompact)
{
#if defined(TYPED_STACK) && defined(COMPACT_INTSEQ)
    R_bcstack_t *s = R_BCNodeStackTop + offset;
    if (s->tag == INTSEQSXP) {
	*iscompact = TRUE;
	return s->u.sxpval;
    }
#endif
    *iscompact = FALSE;
    return GETSTACK(offset);
}

#define BCNPUSH(v) do { \
  SEXP __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  SETSTACK(0, __value__); \
  R_BCNodeStackTop = __ntop__; \
} while (0)
>>>>>>> eval.c

<<<<<<< eval.cpp
#define BCNDUP2ND() (s_nodestack->push(s_nodestack->fromEnd(2)))
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define BCNDUP() do { \
    R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
    if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
    __ntop__[-1] = __ntop__[-2]; \
    R_BCNodeStackTop = __ntop__; \
} while(0)
=======
#ifdef TYPED_STACK
#define BCNPUSH_REAL(v) do { \
  double __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1].u.dval = __value__; \
  __ntop__[-1].tag = REALSXP; \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_INTEGER(v) do { \
  int __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1].u.ival = __value__; \
  __ntop__[-1].tag = INTSXP; \
  R_BCNodeStackTop = __ntop__; \
} while (0)
#endif

#define BCNDUP() do { \
    R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
    if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
    __ntop__[-1] = __ntop__[-2]; \
    R_BCNodeStackTop = __ntop__; \
} while(0)
>>>>>>> eval.c

#define BCNPOP(v) (s_nodestack->topnpop())

#define BCNPOP_IGNORE_VALUE() (s_nodestack->pop())

<<<<<<< eval.cpp
#define BCNSTACKCHECK(n)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define BCNSTACKCHECK(n)  do { \
  if (R_BCNodeStackTop + 1 > R_BCNodeStackEnd) nodeStackOverflow(); \
} while (0)
=======
#define BCNSTACKCHECK(n)  do {						\
	if (R_BCNodeStackTop + (n) > R_BCNodeStackEnd) nodeStackOverflow(); \
    } while (0)
>>>>>>> eval.c

#define BCIPUSHPTR(v)  do { \
  void *__value__ = (v); \
  IStackval *__ntop__ = R_BCIntStackTop + 1; \
  if (__ntop__ > R_BCIntStackEnd) intStackOverflow(); \
  *__ntop__[-1].p = __value__; \
  R_BCIntStackTop = __ntop__; \
} while (0)

#define BCIPUSHINT(v)  do { \
  int __value__ = (v); \
  IStackval *__ntop__ = R_BCIntStackTop + 1; \
  if (__ntop__ > R_BCIntStackEnd) intStackOverflow(); \
  __ntop__[-1].i = __value__; \
  R_BCIntStackTop = __ntop__; \
} while (0)

#define BCIPOPPTR() ((--R_BCIntStackTop)->p)
#define BCIPOPINT() ((--R_BCIntStackTop)->i)

<<<<<<< eval.cpp
#define BCCONSTS(e) (SEXP_downcast<ByteCode*>(e)->constants())
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define BCCONSTS(e) BCODE_CONSTS(e)

static void nodeStackOverflow()
{
    error(_("node stack overflow"));
}

#ifdef BC_INT_STACK
static void intStackOverflow()
{
    error(_("integer stack overflow"));
}
#endif
=======
#define BCCONSTS(e) BCODE_CONSTS(e)

static void NORET nodeStackOverflow()
{
    error(_("node stack overflow"));
}

#ifdef BC_INT_STACK
static void NORET intStackOverflow()
{
    error(_("integer stack overflow"));
}
#endif
>>>>>>> eval.c

static SEXP bytecodeExpr(SEXP e)
{
    if (isByteCode(e)) {
	if (LENGTH(BCCONSTS(e)) > 0)
	    return VECTOR_ELT(BCCONSTS(e), 0);
	else return R_NilValue;
    }
    else return e;
}

SEXP R_PromiseExpr(SEXP p)
{
    return bytecodeExpr(PRCODE(p));
}

SEXP R_ClosureExpr(SEXP p)
{
    return bytecodeExpr(BODY(p));
}

#ifdef THREADED_CODE
void* ByteCode::s_op_address[OPCOUNT];
#ifndef TOKEN_THREADING
int ByteCode::s_op_arity[OPCOUNT];
#endif

#ifdef TOKEN_THREADING
#define OP(name,n) \
  case name##_OP: s_op_address[name##_OP] = (__extension__ &&op_##name); \
    goto loop; \
    op_##name
#else
#define OP(name,n) \
  case name##_OP: s_op_address[name##_OP] = (__extension__ &&op_##name); \
    s_op_arity[name##_OP] = (n); \
    goto loop; \
    op_##name
#endif

#define BEGIN_MACHINE  NEXT(); init: { loop: switch(which++)
#define LASTOP } value = R_NilValue; goto done
#define INITIALIZE_MACHINE() if (body == NULL) goto init

#ifdef TOKEN_THREADING
#define NEXT() (__extension__ ({goto *s_op_address[*pc++];}))
#define GETOP() *pc++
#else
#define NEXT() (__extension__ ({goto *(*pc++).v;}))
#define GETOP() (*pc++).i
#endif

#else
// Not THREADED_CODE:

typedef int BCODE;

#define OP(name,argc) case name##_OP

#ifdef BC_PROFILING
#define BEGIN_MACHINE  loop: current_opcode = *pc; switch(*pc++)
#else
#define BEGIN_MACHINE  loop: switch(*pc++)
#endif
#define LASTOP  default: Rf_error(_("bad opcode"))
#define INITIALIZE_MACHINE()

#define NEXT() goto loop
#define GETOP() *pc++

#endif

<<<<<<< eval.cpp
#define SKIP_OP() (pc++)

static inline Frame::Binding* GET_BINDING_CELL(SEXP symbol, SEXP rho)
{
    return R_findVarLocInFrame(rho, symbol);
}

static inline bool SET_BINDING_VALUE(Frame::Binding* bdg, RObject* value)
{
    if (bdg && !bdg->isLocked() && !bdg->isActive()) {
	if (bdg->rawValue() != value)
	    bdg->setValue(value);
	return true;
    }
    return false;
}

static inline RObject* BINDING_VALUE(const Frame::Binding* bdg)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static R_INLINE SEXP GET_BINDING_CELL(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return R_NilValue;
    else {
	SEXP loc = (SEXP) R_findVarLocInFrame(rho, symbol);
	return (loc != NULL) ? loc : R_NilValue;
    }
}

static R_INLINE Rboolean SET_BINDING_VALUE(SEXP loc, SEXP value) {
    /* This depends on the current implementation of bindings */
    if (loc != R_NilValue &&
	! BINDING_IS_LOCKED(loc) && ! IS_ACTIVE_BINDING(loc)) {
	if (CAR(loc) != value) {
	    SETCAR(loc, value);
	    if (MISSING(loc))
		SET_MISSING(loc, 0);
	}
	return TRUE;
    }
    else
	return FALSE;
}

static R_INLINE SEXP BINDING_VALUE(SEXP loc)
=======
static R_INLINE SEXP BINDING_VALUE(SEXP loc)
>>>>>>> eval.c
{
    if (bdg && !bdg->isActive())
	return bdg->rawValue();
    return Symbol::unboundValue();
}

#define BINDING_SYMBOL(loc) const_cast<Symbol*>(loc->symbol())

/* Defining USE_BINDING_CACHE enables a cache for GETVAR, SETVAR, and
   others to more efficiently locate bindings in the top frame of the
   current environment.  The index into of the symbol in the constant
   table is used as the cache index.  Two options can be used to chose
   among implementation strategies:

       If CACHE_ON_STACK is defined the the cache is allocated on the
       byte code stack. Otherwise it is allocated on the heap as a
       VECSXP.  The stack-based approach is more efficient, but runs
       the risk of running out of stack space.

       If CACHE_MAX is defined, then a cache of at most that size is
       used. The value must be a power of 2 so a modulus computation x
       % CACHE_MAX can be done as x & (CACHE_MAX - 1). More than 90%
       of the closures in base have constant pools with fewer than 128
       entries when compiled, to that is a good value to use. But
       increasing to 256 handles some benchmark scripts a bit better.

   On average about 1/3 of constant pool entries are symbols, so this
   approach wastes some space.  This could be avoided by grouping the
   symbols at the beginning of the constant pool and recording the
   number.

   Bindings recorded may become invalid if user code removes a
   variable.  The code in envir.c has been modified to insert
   R_unboundValue as the value of a binding when it is removed, and
   code using cached bindings checks for this.

   It would be nice if we could also cache bindings for variables
   found in enclosing environments. These would become invalid if a
   new variable is defined in an intervening frame. Some mechanism for
   invalidating the cache would be needed. This is certainly possible,
   but finding an efficient mechanism does not seem to be easy.   LT */

/* Both mechanisms implemented here make use of the stack to hold
   cache information.  This is not a problem except for "safe" for()
   loops using the STARTLOOPCNTXT instruction to run the body in a
   separate bcEval call.  Since this approach expects loop setup
   information to be passed on the stack from the outer bcEval call to
   an inner one the inner one cannot put things on the stack. For now,
   bcEval takes an additional argument that disables the cache in
   calls via STARTLOOPCNTXT for all "safe" loops. It would be better
   to deal with this in some other way, for example by having a
   specific STARTFORLOOPCNTXT instruction that deals with transferring
   the information in some other way. For now disabling the cache is
   an expedient solution. LT */

// Not defined in CXXR for the time being:
//#define USE_BINDING_CACHE
# ifdef USE_BINDING_CACHE
/* CACHE_MAX must be a power of 2 for modulus using & CACHE_MASK to work*/
# define CACHE_MAX 256
# ifdef CACHE_MAX
#  define CACHE_MASK (CACHE_MAX - 1)
#  define CACHEIDX(i) ((i) & CACHE_MASK)
# else
#  define CACHEIDX(i) (i)
# endif

# define CACHE_ON_STACK
# ifdef CACHE_ON_STACK
typedef R_bcstack_t * R_binding_cache_t;
#  define VCACHE(i) GETSTACK_SXPVAL_PTR(vcache + (i))
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? VCACHE(CACHEIDX(sidx)) : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? VCACHE(sidx) : R_NilValue)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) VCACHE(CACHEIDX(sidx)) = (cell); } while (0)
# else
typedef SEXP R_binding_cache_t;
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, CACHEIDX(sidx)) : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, sidx) : R_NilValue)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) SET_VECTOR_ELT(vcache, CACHEIDX(sidx), cell); } while (0)
# endif
#else
typedef void *R_binding_cache_t;
# define GET_CACHED_BINDING_CELL(vcache, sidx) R_NilValue
# define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) R_NilValue

# define SET_CACHED_BINDING(vcache, sidx, cell)
#endif

static R_INLINE Frame::Binding* GET_BINDING_CELL_CACHE(SEXP symbol, SEXP rho,
					    R_binding_cache_t vcache, int idx)
{
    Frame::Binding* cell = GET_CACHED_BINDING_CELL(vcache, idx);
    if (cell && cell->symbol() == symbol && cell->rawValue() != R_UnboundValue)
	return cell;
    else {
	Frame::Binding* ncell = GET_BINDING_CELL(symbol, rho);
	if (ncell)
	    SET_CACHED_BINDING(vcache, idx, ncell);
	else if (cell && cell->rawValue() == R_UnboundValue)
	    SET_CACHED_BINDING(vcache, idx, R_NilValue);
	return ncell;
    }
}

static void NORET MISSING_ARGUMENT_ERROR(SEXP symbol)
{
    const char *n = CHAR(PRINTNAME(symbol));
    if(*n) Rf_error(_("argument \"%s\" is missing, with no default"), n);
    else Rf_error(_("argument is missing, with no default"));
}

#define MAYBE_MISSING_ARGUMENT_ERROR(symbol, keepmiss) \
    do { if (! keepmiss) MISSING_ARGUMENT_ERROR(symbol); } while (0)

static void NORET UNBOUND_VARIABLE_ERROR(SEXP symbol)
{
<<<<<<< eval.cpp
    Rf_error(_("object '%s' not found"), CHAR(PRINTNAME(symbol)));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    error(_("object '%s' not found"), CHAR(PRINTNAME(symbol)));
=======
    error(_("object '%s' not found"), EncodeChar(PRINTNAME(symbol)));
>>>>>>> eval.c
}

static R_INLINE SEXP FORCE_PROMISE(SEXP value, SEXP symbol, SEXP rho,
				   Rboolean keepmiss)
{
    if (PRVALUE(value) == R_UnboundValue) {
	/**** R_isMissing is inefficient */
	if (keepmiss && R_isMissing(symbol, rho))
	    value = R_MissingArg;
	else value = forcePromise(value);
    }
    else value = PRVALUE(value);
    SET_NAMED(value, 2);
    return value;
}

static R_INLINE SEXP FIND_VAR_NO_CACHE(SEXP symbol, SEXP rho, const Frame::Binding* cell)
{
    SEXP value;
    /* only need to search the current frame again if
       binding was special or frame is a base frame */
    if (cell)
	value =  Rf_findVar(symbol, rho);
    else
	value =  Rf_findVar(symbol, ENCLOS(rho));
    return value;
}

static R_INLINE SEXP getvar(SEXP symbol, SEXP rho,
			    Rboolean dd, Rboolean keepmiss,
			    R_binding_cache_t vcache, int sidx)
{
    SEXP value;
    if (dd)
	value = Rf_ddfindVar(symbol, rho);
    else if (vcache != nullptr) {
	Frame::Binding* cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = BINDING_VALUE(cell);
	if (value == R_UnboundValue)
	    value = FIND_VAR_NO_CACHE(symbol, rho, cell);
    }
    else
	value = Rf_findVar(symbol, rho);

    if (value == R_UnboundValue)
	UNBOUND_VARIABLE_ERROR(symbol);
    else if (value == R_MissingArg)
	MAYBE_MISSING_ARGUMENT_ERROR(symbol, keepmiss);
    else if (TYPEOF(value) == PROMSXP)
	value = FORCE_PROMISE(value, symbol, rho, keepmiss);
    else if (NAMED(value) == 0 && value != R_NilValue)
	SET_NAMED(value, 1);
    return value;
}

#define INLINE_GETVAR
#ifdef INLINE_GETVAR
/* Try to handle the most common case as efficiently as possible.  If
   smallcache is true then a modulus operation on the index is not
   needed, nor is a check that a non-null value corresponds to the
   requested symbol. The symbol from the constant pool is also usually
   not needed. The test TYPOF(value) != SYMBOL rules out R_MissingArg
   and R_UnboundValue as these are implemented s symbols.  It also
   rules other symbols, but as those are rare they are handled by the
   getvar() call. */
#define DO_GETVAR(dd,keepmiss) do { \
    int sidx = GETOP(); \
    if (!dd && smallcache) { \
	SEXP cell = GET_SMALLCACHE_BINDING_CELL(vcache, sidx); \
	/* try fast handling of REALSXP, INTSXP, LGLSXP */ \
	/* (cell won't be R_NilValue or an active binding) */ \
	value = CAR(cell); \
	int type = TYPEOF(value); \
	switch(type) { \
	case REALSXP: \
	case INTSXP: \
	case LGLSXP: \
	    /* may be ok to skip this test: */ \
	    if (NAMED(value) == 0) \
		SET_NAMED(value, 1); \
	    R_Visible = TRUE; \
	    BCNPUSH(value); \
	    NEXT(); \
	} \
	if (cell != R_NilValue && ! IS_ACTIVE_BINDING(cell)) { \
	    value = CAR(cell); \
	    if (TYPEOF(value) != SYMSXP) {	\
		if (TYPEOF(value) == PROMSXP) {		\
		    SEXP pv = PRVALUE(value);		\
		    if (pv == R_UnboundValue) {		\
			SEXP symbol = VECTOR_ELT(constants, sidx);	\
			value = FORCE_PROMISE(value, symbol, rho, keepmiss); \
		    }							\
		    else value = pv;					\
		}							\
		else if (NAMED(value) == 0)				\
		    SET_NAMED(value, 1);				\
		R_Visible = TRUE;					\
		BCNPUSH(value);						\
		NEXT();							\
	    }								\
	}								\
    }									\
    SEXP symbol = VECTOR_ELT(constants, sidx);				\
    R_Visible = TRUE;							\
    BCNPUSH(getvar(symbol, rho, dd, keepmiss, vcache, sidx));		\
    NEXT();								\
} while (0)
#else
#define DO_GETVAR(dd,keepmiss) do { \
  int sidx = GETOP(); \
  SEXP symbol = VECTOR_ELT(constants, sidx); \
  R_Visible = TRUE; \
  BCNPUSH(getvar(symbol, rho, dd, keepmiss, vcache, sidx));	\
  NEXT(); \
} while (0)
#endif

<<<<<<< eval.cpp
#define PUSHCALLARG(v) PUSHCALLARG_CELL(PairList::cons(v))
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define PUSHCALLARG(v) PUSHCALLARG_CELL(CONS(v, R_NilValue))
=======
/* call frame accessors */
#define CALL_FRAME_FUN() GETSTACK(-3)
#define CALL_FRAME_ARGS() GETSTACK(-2)
#define CALL_FRAME_FTYPE() TYPEOF(CALL_FRAME_FUN())
#define CALL_FRAME_SIZE() (3)

#define GETSTACK_BELOW_CALL_FRAME(n) GETSTACK((n) - CALL_FRAME_SIZE())
#define SETSTACK_BELOW_CALL_FRAME(n, v) SETSTACK((n) - CALL_FRAME_SIZE(), v)

/* create room for accumulating the arguments. */
#define INIT_CALL_FRAME_ARGS() do { \
	BCNSTACKCHECK(2);	  \
	SETSTACK(0, R_NilValue);  \
	SETSTACK(1, R_NilValue);  \
	R_BCNodeStackTop += 2;	  \
    } while (0)

/* push the function and create room for accumulating the arguments. */
#define INIT_CALL_FRAME(fun) do { \
	BCNPUSH(fun);		\
	INIT_CALL_FRAME_ARGS();	\
    } while (0)

/* remove the call frame from the stack and push the return value */
#define POP_CALL_FRAME(value) POP_CALL_FRAME_PLUS(0, value)
>>>>>>> eval.c

#define POP_CALL_FRAME_PLUS(n, value) do {	\
	R_BCNodeStackTop -= (2 + (n));		\
	SETSTACK(-1, value);			\
    } while (0)

#define PUSHCALLARG(v) do { \
  SEXP __cell__ = CONS_NR(v, R_NilValue); \
  if (GETSTACK(-2) == R_NilValue) SETSTACK(-2, __cell__); \
  else SETCDR(GETSTACK(-1), __cell__); \
  SETSTACK(-1, __cell__);	       \
} while (0)

<<<<<<< eval.cpp
static int tryDispatch(CXXRCONST char *generic, SEXP call, SEXP x, SEXP rho, SEXP *pv)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static int tryDispatch(char *generic, SEXP call, SEXP x, SEXP rho, SEXP *pv)
=======
/* place a tag on the most recently pushed call argument */
#define SETCALLARG_TAG(t) do {			\
	SEXP __tag__ = (t);			\
	if (__tag__ != R_NilValue) {		\
	    SEXP __cell__ = GETSTACK(-1);	\
	    if (__cell__ != R_NilValue)		   \
		SET_TAG(__cell__, CreateTag(__tag__));	\
	}						\
    } while (0)

/* same, but tag is known to be a symbol */
#define SETCALLARG_TAG_SYMBOL(t) do {			\
	SEXP __cell__ = GETSTACK(-1);			\
	if (__cell__ != R_NilValue)			\
	    SET_TAG(__cell__, t);			\
    } while (0)

static int tryDispatch(char *generic, SEXP call, SEXP x, SEXP rho, SEXP *pv)
>>>>>>> eval.c
{
  SEXP pargs, rho1;
  int dispatched = FALSE;
  SEXP op = SYMVALUE(Rf_install(generic)); /**** avoid this */

  PROTECT(pargs = Rf_promiseArgs(CDR(call), rho));
  SET_PRVALUE(CAR(pargs), x);

  /**** Minimal hack to try to handle the S4 case.  If we do the check
	and do not dispatch then some arguments beyond the first might
	have been evaluated; these will then be evaluated again by the
	compiled argument code. */
  if (IS_S4_OBJECT(x) && R_has_methods(op)) {
    pair<bool, RObject*> pr = R_possible_dispatch(call, op, pargs, rho, TRUE);
    if (pr.first) {
      *pv = pr.second;
      UNPROTECT(1);
      return TRUE;
    }
  }

  /* See comment at first Rf_usemethod() call in this file. LT */
  PROTECT(rho1 = Rf_NewEnvironment(nullptr, nullptr, rho));
  {
      Expression* callx = SEXP_downcast<Expression*>(call);
      Environment* call_env = SEXP_downcast<Environment*>(rho);
      FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
      Environment* working_env = SEXP_downcast<Environment*>(rho1);
      PairList* promargs = SEXP_downcast<PairList*>(pargs);
      ClosureContext cntxt(callx, call_env, func, working_env, promargs);
      if (Rf_usemethod(generic, x, call, pargs, rho1, rho, R_BaseEnv, pv))
	  dispatched = TRUE;
  }
  UNPROTECT(2);
  if (! dispatched) DECREMENT_REFCNT(x);
  return dispatched;
}

static int tryAssignDispatch(CXXRCONST char *generic, SEXP call, SEXP lhs, SEXP rhs,
			     SEXP rho, SEXP *pv)
{
    int result;
    SEXP ncall, last, prom;

    PROTECT(ncall = Rf_duplicate(call));
    last = ncall;
    while (CDR(last) != R_NilValue)
	last = CDR(last);
<<<<<<< eval.cpp
    prom = Rf_mkPROMISE(CAR(last), rho);
    SET_PRVALUE(prom, rhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    prom = mkPROMISE(CAR(last), rho);
    SET_PRVALUE(prom, rhs);
=======
    prom = mkRHSPROMISE(CAR(last), rhs);
>>>>>>> eval.c
    SETCAR(last, prom);
    result = tryDispatch(generic, ncall, lhs, rho, pv);
    UNPROTECT(1);
    return result;
}

#define DO_STARTDISPATCH(generic) do { \
  SEXP call = (*constants)[GETOP()]; \
  int label = GETOP(); \
  value = GETSTACK(-1); \
  if (Rf_isObject(value) && tryDispatch(generic, call, value, rho, &value)) {\
    SETSTACK(-1, value);						\
    BC_CHECK_SIGINT(); \
    pc = codebase + label; \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
<<<<<<< eval.cpp
    SEXP cell = PairList::cons(value);	\
    BCNSTACKCHECK(3); \
    s_nodestack->push(call); \
    s_nodestack->push(cell); \
    s_nodestack->push(cell); \
    if (tag != R_NilValue) \
      SET_TAG(cell, Rf_CreateTag(tag)); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SEXP cell = CONS(value, R_NilValue); \
    BCNSTACKCHECK(3); \
    SETSTACK(0, call); \
    SETSTACK(1, cell); \
    SETSTACK(2, cell); \
    R_BCNodeStackTop += 3; \
    if (tag != R_NilValue) \
      SET_TAG(cell, CreateTag(tag)); \
=======
    BCNPUSH(call); \
    INIT_CALL_FRAME(R_NilValue); \
    PUSHCALLARG(value); \
    SETCALLARG_TAG(tag);   \
>>>>>>> eval.c
  } \
  NEXT(); \
} while (0)

#define DO_DFLTDISPATCH(fun, symbol) do { \
  SEXP call = GETSTACK_BELOW_CALL_FRAME(-1); \
  SEXP args = CALL_FRAME_ARGS(); \
  value = fun(call, symbol, args, rho); \
<<<<<<< eval.cpp
  s_nodestack->pop(3);	\
  SETSTACK(-1, value); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  R_BCNodeStackTop -= 3; \
  SETSTACK(-1, value); \
=======
  POP_CALL_FRAME_PLUS(2, value); \
>>>>>>> eval.c
  NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH(generic) do { \
  SEXP call = (*constants)[GETOP()]; \
  int label = GETOP(); \
  SEXP lhs = GETSTACK(-2); \
  SEXP rhs = GETSTACK(-1); \
<<<<<<< eval.cpp
  if (NAMED(lhs) == 2) { \
    lhs = Rf_duplicate(lhs); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  if (NAMED(lhs) == 2) { \
    lhs = duplicate(lhs); \
=======
  if (MAYBE_SHARED(lhs)) { \
    lhs = shallow_duplicate(lhs); \
>>>>>>> eval.c
    SETSTACK(-2, lhs); \
    SET_NAMED(lhs, 1); \
  } \
  if (Rf_isObject(lhs) && \
      tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
    s_nodestack->pop();	\
    SETSTACK(-1, value); \
    BC_CHECK_SIGINT(); \
    pc = codebase + label; \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
<<<<<<< eval.cpp
    SEXP cell = PairList::cons(lhs);	\
    BCNSTACKCHECK(3); \
    s_nodestack->push(call); \
    s_nodestack->push(cell); \
    s_nodestack->push(cell); \
    if (tag != R_NilValue) \
      SET_TAG(cell, Rf_CreateTag(tag)); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SEXP cell = CONS(lhs, R_NilValue); \
    BCNSTACKCHECK(3); \
    SETSTACK(0, call); \
    SETSTACK(1, cell); \
    SETSTACK(2, cell); \
    R_BCNodeStackTop += 3; \
    if (tag != R_NilValue) \
      SET_TAG(cell, CreateTag(tag)); \
=======
    BCNPUSH(call); \
    INIT_CALL_FRAME(R_NilValue); \
    PUSHCALLARG(lhs); \
    SETCALLARG_TAG(tag);   \
>>>>>>> eval.c
  } \
  NEXT(); \
} while (0)

#define DO_DFLT_ASSIGN_DISPATCH(fun, symbol) do { \
  SEXP rhs = GETSTACK_BELOW_CALL_FRAME(-2); \
  SEXP call = GETSTACK_BELOW_CALL_FRAME(-1); \
  SEXP args = CALL_FRAME_ARGS(); \
  PUSHCALLARG(rhs); \
  value = fun(call, symbol, args, rho); \
<<<<<<< eval.cpp
  s_nodestack->pop(4);	\
  SETSTACK(-1, value);	 \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  R_BCNodeStackTop -= 4; \
  SETSTACK(-1, value);	 \
=======
  POP_CALL_FRAME_PLUS(3, value); \
>>>>>>> eval.c
  NEXT(); \
} while (0)

#define DO_STARTDISPATCH_N(generic) do { \
    int callidx = GETOP(); \
    int label = GETOP(); \
    value = GETSTACK(-1); \
    if (Rf_isObject(value)) { \
	SEXP call = VECTOR_ELT(constants, callidx); \
	if (tryDispatch(generic, call, value, rho, &value)) { \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    pc = codebase + label; \
	} \
    } \
    NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH_N(generic) do { \
    int callidx = GETOP(); \
    int label = GETOP(); \
    SEXP lhs = GETSTACK(-2); \
    if (Rf_isObject(lhs)) { \
	SEXP call = VECTOR_ELT(constants, callidx); \
	SEXP rhs = GETSTACK(-1); \
<<<<<<< eval.cpp
	if (NAMED(lhs) == 2) { \
	    lhs = Rf_duplicate(lhs); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (NAMED(lhs) == 2) { \
	    lhs = duplicate(lhs); \
=======
	if (MAYBE_SHARED(lhs)) { \
	    lhs = shallow_duplicate(lhs); \
>>>>>>> eval.c
	    SETSTACK(-2, lhs); \
	    SET_NAMED(lhs, 1); \
	} \
	if (tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
	    s_nodestack->pop();					       \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    pc = codebase + label; \
	} \
    } \
    NEXT(); \
} while (0)

#define DO_ISTEST(fun) do { \
  SETSTACK(-1, fun(GETSTACK(-1)) ? R_TrueValue : R_FalseValue);	\
  NEXT(); \
} while(0)
#define DO_ISTYPE(type) do { \
<<<<<<< eval.cpp
  SETSTACK(-1, TYPEOF(GETSTACK(-1)) == type ? Rf_mkTrue() : Rf_mkFalse()); \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  SETSTACK(-1, TYPEOF(GETSTACK(-1)) == type ? mkTrue() : mkFalse()); \
=======
  SETSTACK(-1, TYPEOF(GETSTACK(-1)) == type ? R_TrueValue : R_FalseValue); \
>>>>>>> eval.c
  NEXT(); \
} while (0)
#define isNumericOnly(x) (Rf_isNumeric(x) && ! Rf_isLogical(x))

#ifdef BC_PROFILING
#define NO_CURRENT_OPCODE -1
static int current_opcode = NO_CURRENT_OPCODE;
static int opcode_counts[OPCOUNT];
#endif

#define BC_COUNT_DELTA 1000

#ifndef IMMEDIATE_FINALIZERS
/* finalizers are run here since this should only be called at
   points where running arbitrary code should be safe */
#define BC_CHECK_SIGINT() do { \
  if (++evalcount > BC_COUNT_DELTA) { \
      R_CheckUserInterrupt(); \
      R_RunPendingFinalizers(); \
      evalcount = 0; \
  } \
} while (0)
#else
#define BC_CHECK_SIGINT() do { \
  if (++evalcount > BC_COUNT_DELTA) { \
      R_CheckUserInterrupt(); \
      evalcount = 0; \
  } \
} while (0)
#endif

static void loopWithContext(ByteCode* code, Environment* rho)
{
    Environment::LoopScope loopscope(rho);
    bool redo;
    do {
	redo = false;
	try {
	    // CXXR FIXME (maybe): at this point CR calls bcEval with
	    // its useCache arg set to FALSE:
	    code->evaluate(rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != rho)
		throw;
	    redo = lx.next();
	}
    } while (redo);
}

<<<<<<< eval.cpp
static R_INLINE int bcStackIndex(RObject* idx)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static R_INLINE int bcStackIndex(R_bcstack_t *s)
=======
static R_INLINE R_xlen_t bcStackIndex(R_bcstack_t *s)
>>>>>>> eval.c
{
<<<<<<< eval.cpp
    switch(TYPEOF(idx)) {
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SEXP idx = *s;
    switch(TYPEOF(idx)) {
=======
#ifdef TYPED_STACK
    switch(s->tag) {
>>>>>>> eval.c
    case INTSXP:
	if (s->u.ival != NA_INTEGER)
	    return s->u.ival;
	else return -1;
    case REALSXP:
<<<<<<< eval.cpp
	if (LENGTH(idx) == 1) {
	    double val = REAL(idx)[0];
	    if (! ISNAN(val) && val <= INT_MAX && val > INT_MIN)
		return int( val);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (LENGTH(idx) == 1) {
	    double val = REAL(idx)[0];
	    if (! ISNAN(val) && val <= INT_MAX && val > INT_MIN)
		return (int) val;
=======
	{
	    double val = s->u.dval;
	    if (! ISNAN(val) && val <= R_XLEN_T_MAX && val > 0)
		return (R_xlen_t) s->u.dval;
>>>>>>> eval.c
	    else return -1;
	}
    case LGLSXP: return -1;
    default: break;
    }
#endif
    SEXP idx = GETSTACK_SXPVAL_PTR(s);
    if (IS_SCALAR(idx, INTSXP)) {
	if (INTEGER(idx)[0] != NA_INTEGER)
	    return INTEGER(idx)[0];
	else return -1;
    }
    else if (IS_SCALAR(idx, REALSXP)) {
	double val = REAL(idx)[0];
	if (! ISNAN(val) && val <= R_XLEN_T_MAX && val > 0)
	    return (R_xlen_t) val;
	else return -1;
    }
    else return -1;
}

<<<<<<< eval.cpp
static inline RObject* VECSUBSET_PTR(RObject* vec, RObject* idx,
				     SEXP rho)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static R_INLINE void VECSUBSET_PTR(R_bcstack_t *sx, R_bcstack_t *si,
				   R_bcstack_t *sv, SEXP rho)
=======
static R_INLINE SEXP mkVector1(SEXP s)
{
    SEXP t = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(t, 0, s);
    return t;
}

#define DO_FAST_VECELT(sv, vec,  i, subset2) do {		\
	switch (TYPEOF(vec)) {					\
	case REALSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_REAL_PTR(sv, REAL(vec)[i]);		\
	    return;						\
	case INTSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_INTEGER_PTR(sv, INTEGER(vec)[i]);		\
	    return;						\
	case LGLSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_LOGICAL_PTR(sv, LOGICAL(vec)[i]);		\
	    return;						\
	case CPLXSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_PTR(sv, ScalarComplex(COMPLEX(vec)[i]));	\
	    return;						\
	case RAWSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_PTR(sv, ScalarRaw(RAW(vec)[i]));		\
	    return;						\
	case VECSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SEXP elt = VECTOR_ELT(vec, i);			\
	    if (NAMED(vec) > NAMED(elt))			\
		SET_NAMED(elt, NAMED(vec));			\
	    if (subset2)					\
		SETSTACK_PTR(sv, elt);				\
	    else						\
		SETSTACK_PTR(sv, mkVector1(elt));		\
	    return;						\
	}							\
    } while (0)

#define FAST_VECELT_OK(vec) \
    (ATTRIB(vec) == R_NilValue ||		\
     (TAG(ATTRIB(vec)) == R_DimSymbol &&	\
      CDR(ATTRIB(vec)) == R_NilValue))

static R_INLINE void VECSUBSET_PTR(R_bcstack_t *sx, R_bcstack_t *si,
				   R_bcstack_t *sv, SEXP rho,
				   SEXP consts, int callidx,
				   Rboolean subset2)
>>>>>>> eval.c
{
<<<<<<< eval.cpp
    SEXP args, value;
    int i = bcStackIndex(idx) - 1;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    SEXP idx, args, value;
    SEXP vec = GETSTACK_PTR(sx);
    int i = bcStackIndex(si) - 1;
=======
    SEXP idx, args, value;
    SEXP vec = GETSTACK_PTR(sx);
    R_xlen_t i = bcStackIndex(si) - 1;
>>>>>>> eval.c

<<<<<<< eval.cpp
    if (ATTRIB(vec) == R_NilValue && i >= 0) {
	switch (TYPEOF(vec)) {
	case REALSXP:
	    if (LENGTH(vec) <= i) break;
	    return Rf_ScalarReal(REAL(vec)[i]);
	case INTSXP:
	    if (LENGTH(vec) <= i) break;
	    return Rf_ScalarInteger(INTEGER(vec)[i]);
	case LGLSXP:
	    if (LENGTH(vec) <= i) break;
	    // NB: CR's SETSTACK_LOGICAL_PTR includes special handling
	    // of NA_LOGICAL; I'm not sure why.
	    return Rf_ScalarLogical(LOGICAL(vec)[i]);
	case CPLXSXP:
	    if (LENGTH(vec) <= i) break;
	    return Rf_ScalarComplex(COMPLEX(vec)[i]);
	case RAWSXP:
	    if (LENGTH(vec) <= i) break;
	    return Rf_ScalarRaw(RAW(vec)[i]);
	default:
	    Rf_error(_("Internal error: unexpected SEXPTYPE"));
	}
    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (ATTRIB(vec) == R_NilValue && i >= 0) {
	switch (TYPEOF(vec)) {
	case REALSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_REAL_PTR(sv, REAL(vec)[i]);
	    return;
	case INTSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_INTEGER_PTR(sv, INTEGER(vec)[i]);
	    return;
	case LGLSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_LOGICAL_PTR(sv, LOGICAL(vec)[i]);
	    return;
	case CPLXSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_PTR(sv, ScalarComplex(COMPLEX(vec)[i]));
	    return;
	case RAWSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_PTR(sv, ScalarRaw(RAW(vec)[i]));
	    return;
	}
    }
=======
    if (i >= 0 && (subset2 || FAST_VECELT_OK(vec)))
	DO_FAST_VECELT(sv, vec, i, subset2);
>>>>>>> eval.c

    /* fall through to the standard default handler */
<<<<<<< eval.cpp
    args = CONS(idx, R_NilValue);
    args = CONS(vec, args);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    idx = GETSTACK_PTR(si);
    args = CONS(idx, R_NilValue);
    args = CONS(vec, args);
=======
    idx = GETSTACK_PTR(si);
    args = CONS_NR(idx, R_NilValue);
    args = CONS_NR(vec, args);
>>>>>>> eval.c
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    return value;
}

<<<<<<< eval.cpp
#define DO_VECSUBSET(rho) do { \
    NSFROMEND(2) = VECSUBSET_PTR(NSFROMEND(2), NSFROMEND(1), rho); \
    s_nodestack->pop(); \
} while(0)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
#define DO_VECSUBSET(rho) do { \
    VECSUBSET_PTR(R_BCNodeStackTop - 2, R_BCNodeStackTop - 1, \
		  R_BCNodeStackTop - 2, rho); \
    R_BCNodeStackTop--; \
} while(0)
=======
#define DO_VECSUBSET(rho, sub2) do {					\
	int callidx = GETOP();						\
	VECSUBSET_PTR(R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
		      R_BCNodeStackTop - 2, rho,			\
		      constants, callidx, sub2);			\
	R_BCNodeStackTop--;						\
    } while(0)
>>>>>>> eval.c

static R_INLINE SEXP getMatrixDim(SEXP mat)
{
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim = TAG(attr) == R_DimSymbol ? CAR(attr) :
	getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2)
	return dim;
    else return R_NilValue;
}

static R_INLINE SEXP getArrayDim(SEXP mat)
{
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim = TAG(attr) == R_DimSymbol ? CAR(attr) :
	getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) > 0)
	return dim;
    else return R_NilValue;
}

static R_INLINE R_xlen_t colMajorStackIndex(SEXP dim, int rank, R_bcstack_t *si)
{
    if (rank != LENGTH(dim))
    return -1;

    int *idim = INTEGER(dim);

    R_xlen_t mul = idim[0];
    R_xlen_t idx = bcStackIndex(si);

    if (idx < 1 || idx > idim[0])
	return -1;

    R_xlen_t k = idx - 1;
    for (int i = 1; i < rank; i++) {
	idx = bcStackIndex(si + i);
	if (idx < 1 || idx > idim[i])
	    return -1;
	k = k + mul * (idx - 1);
	mul = mul * idim[i];
    }
    return k;
}

<<<<<<< eval.cpp
R_INLINE void ByteCode::DO_MATSUBSET(SEXP rho)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static R_INLINE void DO_MATSUBSET(SEXP rho)
=======
static R_INLINE void MATSUBSET_PTR(R_bcstack_t *sx,
				   R_bcstack_t *si, R_bcstack_t *sj,
				   R_bcstack_t *sv, SEXP rho,
				   SEXP consts, int callidx,
				   Rboolean subset2)
>>>>>>> eval.c
{
    SEXP idx, jdx, args, value;
    SEXP mat = GETSTACK_PTR(sx);

<<<<<<< eval.cpp
    if (dim != R_NilValue) {
	int i = bcStackIndex(NSFROMEND(2));
	int j = bcStackIndex(NSFROMEND(1));
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
	    int k = i - 1 + nrow * (j - 1);
	    switch (TYPEOF(mat)) {
	    case REALSXP:
		if (LENGTH(mat) <= k) break;
		s_nodestack->pop(2);
		SETSTACK_REAL(-1, REAL(mat)[k]);
		return;
	    case INTSXP:
		if (LENGTH(mat) <= k) break;
		s_nodestack->pop(2);
		SETSTACK_INTEGER(-1, INTEGER(mat)[k]);
		return;
	    case LGLSXP:
		if (LENGTH(mat) <= k) break;
		s_nodestack->pop(2);
		SETSTACK_LOGICAL(-1, LOGICAL(mat)[k]);
		return;
	    case CPLXSXP:
		if (LENGTH(mat) <= k) break;
		s_nodestack->pop(2);
		SETSTACK(-1, Rf_ScalarComplex(COMPLEX(mat)[k]));
		return;
	    default:
		Rf_error(_("Internal error: unexpected SEXPTYPE"));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (dim != R_NilValue) {
	int i = bcStackIndex(R_BCNodeStackTop - 2);
	int j = bcStackIndex(R_BCNodeStackTop - 1);
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
	    int k = i - 1 + nrow * (j - 1);
	    switch (TYPEOF(mat)) {
	    case REALSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK_REAL(-1, REAL(mat)[k]);
		return;
	    case INTSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK_INTEGER(-1, INTEGER(mat)[k]);
		return;
	    case LGLSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK_LOGICAL(-1, LOGICAL(mat)[k]);
		return;
	    case CPLXSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK(-1, ScalarComplex(COMPLEX(mat)[k]));
		return;
=======
    if (subset2 || FAST_VECELT_OK(mat)) {
	SEXP dim = getMatrixDim(mat);
	if (dim != R_NilValue) {
	    R_xlen_t i = bcStackIndex(si);
	    R_xlen_t j = bcStackIndex(sj);
	    R_xlen_t nrow = INTEGER(dim)[0];
	    R_xlen_t ncol = INTEGER(dim)[1];
	    if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
		R_xlen_t k = i - 1 + nrow * (j - 1);
		DO_FAST_VECELT(sv, mat, k, subset2);
>>>>>>> eval.c
	    }
	}
    }

    /* fall through to the standard default handler */
<<<<<<< eval.cpp
    idx = GETSTACK(-2);
    jdx = GETSTACK(-1);
    args = CONS(jdx, R_NilValue);
    args = CONS(idx, args);
    args = CONS(mat, args);
    SETSTACK(-1, args); /* for GC protection */
    value = do_subset_dflt(R_NilValue, R_SubsetSym, args, rho);
    s_nodestack->pop(2);
    SETSTACK(-1, value);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    idx = GETSTACK(-2);
    jdx = GETSTACK(-1);
    args = CONS(jdx, R_NilValue);
    args = CONS(idx, args);
    args = CONS(mat, args);
    SETSTACK(-1, args); /* for GC protection */
    value = do_subset_dflt(R_NilValue, R_SubsetSym, args, rho);
    R_BCNodeStackTop -= 2;
    SETSTACK(-1, value);
=======
    idx = GETSTACK_PTR(si);
    jdx = GETSTACK_PTR(sj);
    args = CONS_NR(jdx, R_NilValue);
    args = CONS_NR(idx, args);
    args = CONS_NR(mat, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
}

#define DO_MATSUBSET(rho, sub2) do {					\
	int callidx = GETOP();						\
	MATSUBSET_PTR(R_BCNodeStackTop - 3,				\
		      R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
		      R_BCNodeStackTop - 3, rho,			\
		      constants, callidx, sub2);			\
	R_BCNodeStackTop -= 2;						\
    } while (0)

static R_INLINE SEXP addStackArgsList(int n, R_bcstack_t *start, SEXP val)
{
    R_bcstack_t *p = start + n - 1;
    for (int i = 0; i < n; i++, p--)
	val = CONS(GETSTACK_PTR(p), val);
    return val;
}

static R_INLINE SEXP getStackArgsList(int n, R_bcstack_t *start)
{
    return addStackArgsList(n, start, R_NilValue);
}

static R_INLINE void SUBSET_N_PTR(R_bcstack_t *sx, int rank,
				  R_bcstack_t *si, R_bcstack_t *sv,
				  SEXP rho, SEXP consts, int callidx,
				  Rboolean subset2)
{
    SEXP args, value;
    SEXP x = GETSTACK_PTR(sx);

    if (subset2 || FAST_VECELT_OK(x)) {
	SEXP dim = getArrayDim(x);
	if (dim != R_NilValue) {
	    R_xlen_t k = colMajorStackIndex(dim, rank, si);
	    if (k >= 0)
		DO_FAST_VECELT(sv, x, k, subset2);
	}
    }

    /* fall through to the standard default handler */
    PROTECT(args = CONS(x, getStackArgsList(rank, si)));
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
>>>>>>> eval.c
}

#define DO_SUBSET_N(rho, sub2) do {					\
	int callidx = GETOP();						\
	int rank = GETOP();						\
	SUBSET_N_PTR(R_BCNodeStackTop - rank - 1, rank,			\
		     R_BCNodeStackTop - rank,				\
		     R_BCNodeStackTop - rank - 1, rho,			\
		     constants, callidx, sub2);				\
	R_BCNodeStackTop -= rank;					\
    } while (0)

static R_INLINE Rboolean setElementFromScalar(SEXP vec, R_xlen_t i, int typev,
					      scalar_value_t *v)
{
    if (i < 0) return FALSE;

    if (TYPEOF(vec) == REALSXP) {
	if (XLENGTH(vec) <= i) return FALSE;
	switch(typev) {
	case REALSXP: REAL(vec)[i] = v->dval; return TRUE;
	case INTSXP: REAL(vec)[i] = INTEGER_TO_REAL(v->ival); return TRUE;
	case LGLSXP: REAL(vec)[i] = LOGICAL_TO_REAL(v->ival); return TRUE;
	}
    }
    else if (typev == TYPEOF(vec)) {
	if (XLENGTH(vec) <= i) return FALSE;
	switch(typev) {
	case INTSXP: INTEGER(vec)[i] = v->ival; return TRUE;
	case LGLSXP: LOGICAL(vec)[i] = INTEGER_TO_LOGICAL(v->ival); return TRUE;
	}
    }
    return FALSE;
}

<<<<<<< eval.cpp
static R_INLINE RObject* SETVECSUBSET_PTR(RObject* vec, RObject* value,
					  RObject* idx,
					  SEXP rho)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static R_INLINE void SETVECSUBSET_PTR(R_bcstack_t *sx, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sv,
				      SEXP rho)
=======
#define DO_FAST_SETVECELT(sv, srhs, vec,  i, subset2) do {		\
	scalar_value_t v;						\
	int typev = bcStackScalar(srhs, &v);				\
	if (setElementFromScalar(vec, i, typev, &v)) {			\
	    SETSTACK_PTR(sv, vec);					\
	    return;							\
	}								\
	else if (subassign2 && TYPEOF(vec) == VECSXP && i < XLENGTH(vec)) { \
	    SEXP rhs = R_FixupRHS(vec, GETSTACK_PTR(srhs));		\
	    if (rhs != R_NilValue) {					\
		SET_VECTOR_ELT(vec, i, rhs);				\
		SETSTACK_PTR(sv, vec);					\
		return;							\
	    }								\
	}								\
    } while (0)

static R_INLINE void VECSUBASSIGN_PTR(R_bcstack_t *sx, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sv,
				      SEXP rho, SEXP consts, int callidx,
				      Rboolean subassign2)
>>>>>>> eval.c
{
    SEXP args;

<<<<<<< eval.cpp
    if (NAMED(vec) == 2) {
	vec = Rf_duplicate(vec);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (NAMED(vec) == 2) {
	vec = duplicate(vec);
	SETSTACK_PTR(sx, vec);
=======
    if (MAYBE_SHARED(vec)) {
	vec = duplicate(vec);
	SETSTACK_PTR(sx, vec);
>>>>>>> eval.c
    }
    else if (NAMED(vec) == 1)
	SET_NAMED(vec, 0);

<<<<<<< eval.cpp
    if (ATTRIB(vec) == R_NilValue) {
	int i = bcStackIndex(idx);
	if (i > 0) {
	    scalar_value_t v;
	    int typev = bcStackScalar(value, &v);
	    if (setElementFromScalar(vec, i - 1, typev, &v)) {
		return vec;
	    }
	}
    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (ATTRIB(vec) == R_NilValue) {
	int i = bcStackIndex(si);
	if (i > 0) {
	    scalar_value_t v;
	    int typev = bcStackScalar(srhs, &v);
	    if (setElementFromScalar(vec, i - 1, typev, &v)) {
		SETSTACK_PTR(sv, vec);
		return;
	    }
	}
    }
=======
    R_xlen_t i = bcStackIndex(si) - 1;
    if (i >= 0)
	DO_FAST_SETVECELT(sv, srhs, vec,  i, subset2);
>>>>>>> eval.c

    /* fall through to the standard default handler */
<<<<<<< eval.cpp
    args = CONS(value, R_NilValue);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    args = CONS(value, R_NilValue);
=======
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    args = CONS_NR(value, R_NilValue);
>>>>>>> eval.c
    SET_TAG(args, R_valueSym);
    args = CONS_NR(idx, args);
    args = CONS_NR(vec, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subassign2)
	vec = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	vec = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    return vec;
}

<<<<<<< eval.cpp
R_INLINE void ByteCode::DO_SETVECSUBSET(SEXP rho)
{
    NSFROMEND(3) = SETVECSUBSET_PTR(NSFROMEND(3), NSFROMEND(2),
				    NSFROMEND(1), rho);
    s_nodestack->pop(2);
}
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static R_INLINE void DO_SETVECSUBSET(SEXP rho)
{
    SETVECSUBSET_PTR(R_BCNodeStackTop - 3, R_BCNodeStackTop - 2,
		     R_BCNodeStackTop - 1, R_BCNodeStackTop - 3, rho);
    R_BCNodeStackTop -= 2;
}
=======
#define DO_VECSUBASSIGN(rho, sub2) do {					\
	int callidx = GETOP();						\
	VECSUBASSIGN_PTR(R_BCNodeStackTop - 3, R_BCNodeStackTop - 2,	\
			 R_BCNodeStackTop - 1, R_BCNodeStackTop - 3,	\
			 rho, constants, callidx, sub2);		\
	R_BCNodeStackTop -= 2;						\
    } while (0)
>>>>>>> eval.c

<<<<<<< eval.cpp
R_INLINE void ByteCode::DO_SETMATSUBSET(SEXP rho)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
static R_INLINE void DO_SETMATSUBSET(SEXP rho)
=======
static R_INLINE void MATSUBASSIGN_PTR(R_bcstack_t *sx, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sj,
				      R_bcstack_t *sv, 
				      SEXP rho, SEXP consts, int callidx,
				      Rboolean subassign2)
>>>>>>> eval.c
{
    SEXP dim, idx, jdx, args, value;
    SEXP mat = GETSTACK_PTR(sx);

<<<<<<< eval.cpp
    if (NAMED(mat) > 1) {
	mat = Rf_duplicate(mat);
	SETSTACK(-4, mat);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (NAMED(mat) > 1) {
	mat = duplicate(mat);
	SETSTACK(-4, mat);
=======
    if (MAYBE_SHARED(mat)) {
	mat = duplicate(mat);
	SETSTACK_PTR(sx, mat);
>>>>>>> eval.c
    }
    else if (NAMED(mat) == 1)
	SET_NAMED(mat, 0);

    dim = getMatrixDim(mat);

    if (dim != R_NilValue) {
<<<<<<< eval.cpp
	int i = bcStackIndex(NSFROMEND(2));
	int j = bcStackIndex(NSFROMEND(1));
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	int i = bcStackIndex(R_BCNodeStackTop - 2);
	int j = bcStackIndex(R_BCNodeStackTop - 1);
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
=======
	R_xlen_t i = bcStackIndex(si);
	R_xlen_t j = bcStackIndex(sj);
	R_xlen_t nrow = INTEGER(dim)[0];
	R_xlen_t ncol = INTEGER(dim)[1];
>>>>>>> eval.c
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
<<<<<<< eval.cpp
	    scalar_value_t v;
	    int typev = bcStackScalar(NSFROMEND(3), &v);
	    int k = i - 1 + nrow * (j - 1);
	    if (setElementFromScalar(mat, k, typev, &v)) {
		s_nodestack->pop(3);
		SETSTACK(-1, mat);
		return;
	    }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	    scalar_value_t v;
	    int typev = bcStackScalar(R_BCNodeStackTop - 3, &v);
	    int k = i - 1 + nrow * (j - 1);
	    if (setElementFromScalar(mat, k, typev, &v)) {
		R_BCNodeStackTop -= 3;
		SETSTACK(-1, mat);
		return;
	    }
=======
	    R_xlen_t k = i - 1 + nrow * (j - 1);
	    DO_FAST_SETVECELT(sv, srhs, mat,  k, subset2);
>>>>>>> eval.c
	}
    }

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    jdx = GETSTACK_PTR(sj);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
<<<<<<< eval.cpp
    args = CONS(jdx, args);
    args = CONS(idx, args);
    args = CONS(mat, args);
    SETSTACK(-1, args); /* for GC protection */
    mat = do_subassign_dflt(R_NilValue, R_SubassignSym, args, rho);
    s_nodestack->pop(3);
    SETSTACK(-1, mat);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    args = CONS(jdx, args);
    args = CONS(idx, args);
    args = CONS(mat, args);
    SETSTACK(-1, args); /* for GC protection */
    mat = do_subassign_dflt(R_NilValue, R_SubassignSym, args, rho);
    R_BCNodeStackTop -= 3;
    SETSTACK(-1, mat);
=======
    args = CONS_NR(jdx, args);
    args = CONS_NR(idx, args);
    args = CONS_NR(mat, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subassign2)
	mat = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	mat = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, mat);
>>>>>>> eval.c
}

#define DO_MATSUBASSIGN(rho, sub2) do {					\
	int callidx = GETOP();						\
	MATSUBASSIGN_PTR(R_BCNodeStackTop - 4, R_BCNodeStackTop - 3,	\
			 R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
			 R_BCNodeStackTop - 4,				\
			 rho, constants, callidx, sub2);		\
	R_BCNodeStackTop -= 3;						\
    } while (0)

static R_INLINE void SUBASSIGN_N_PTR(R_bcstack_t *sx, int rank,
				     R_bcstack_t *srhs,
				     R_bcstack_t *si, R_bcstack_t *sv, 
				     SEXP rho, SEXP consts, int callidx,
				     Rboolean subassign2)
{
    SEXP dim, args, value;
    SEXP x = GETSTACK_PTR(sx);

    if (MAYBE_SHARED(x)) {
	x = duplicate(x);
	SETSTACK_PTR(sx, x);
    }
    else if (NAMED(x) == 1)
	SET_NAMED(x, 0);

    dim = getArrayDim(x);

    if (dim != R_NilValue) {
	R_xlen_t k = colMajorStackIndex(dim, rank, si);
	if (k >= 0)
	    DO_FAST_SETVECELT(sv, srhs, x,  k, subset2);
    }

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    PROTECT(args = CONS(x, addStackArgsList(rank, si, args)));
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subassign2)
	x = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	x = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, x);
}

#define DO_SUBASSIGN_N(rho, sub2) do {					\
	int callidx = GETOP();						\
	int rank = GETOP();						\
	SUBASSIGN_N_PTR(R_BCNodeStackTop - rank - 2, rank,		\
			R_BCNodeStackTop - rank - 1,			\
			R_BCNodeStackTop - rank,			\
			R_BCNodeStackTop - rank - 2, rho,		\
			constants, callidx, sub2);			\
	R_BCNodeStackTop -= rank + 1;					\
    } while (0)

#define FIXUP_SCALAR_LOGICAL(callidx, arg, op) do { \
	SEXP val = GETSTACK(-1); \
<<<<<<< eval.cpp
	if (TYPEOF(val) != LGLSXP || LENGTH(val) != 1) { \
	    if (!Rf_isNumber(val))	\
		Rf_errorcall(VECTOR_ELT(constants, callidx), \
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (TYPEOF(val) != LGLSXP || LENGTH(val) != 1) { \
	    if (!isNumber(val))	\
		errorcall(VECTOR_ELT(constants, callidx), \
=======
	if (TYPEOF(val) != LGLSXP || XLENGTH(val) != 1) { \
	    if (!isNumber(val))	\
		errorcall(VECTOR_ELT(constants, callidx), \
>>>>>>> eval.c
			  _("invalid %s type in 'x %s y'"), arg, op);	\
	    SETSTACK(-1, Rf_ScalarLogical(Rf_asLogical(val))); \
	} \
    } while(0)

static void signalMissingArgError(SEXP args, SEXP call)
{
    SEXP a, c;
    int n, k;
    for (a = args, n = 1; a != R_NilValue; a = CDR(a), n++)
	if (CAR(a) == R_MissingArg) {
	    /* check for an empty argument in the call -- start from
	       the beginning in case of ... arguments */
	    if (call != R_NilValue) {
		for (k = 1, c = CDR(call); c != R_NilValue; c = CDR(c), k++)
		    if (CAR(c) == R_MissingArg)
			Rf_errorcall(call, "argument %d is empty", k);
	    }
	    /* An error from evaluating a symbol will already have
	       been signaled.  The interpreter, in evalList, does
	       _not_ signal an error for a call expression that
	       produces an R_MissingArg value; for example

	           c(alist(a=)$a)

	       does not signal an error. If we decide we do want an
	       error in this case we can modify evalList for the
	       interpreter and here use the code below. */
#ifdef NO_COMPUTED_MISSINGS
	    /* otherwise signal a 'missing argument' error */
	    errorcall(call, "argument %d is missing", n);
#endif
	}
}

static R_INLINE void checkForMissings(SEXP args, SEXP call)
{
    Rboolean found = FALSE;
    for (SEXP a = args; a != R_NilValue; a = CDR(a))
	if (CAR(a) == R_MissingArg) {
	    found = TRUE;
	    break;
	}
    if (found)
	signalMissingArgError(args, call);
}

#define GET_VEC_LOOP_VALUE(var, pos) do {		\
    (var) = GETSTACK(pos);				\
<<<<<<< eval.cpp
    if (NAMED(var) == 2) {				\
	(var) = Rf_allocVector(TYPEOF(seq), 1);		\
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    if (NAMED(var) == 2) {				\
	(var) = allocVector(TYPEOF(seq), 1);		\
=======
    if (MAYBE_SHARED(var)) {				\
	(var) = allocVector(TYPEOF(seq), 1);		\
>>>>>>> eval.c
	SETSTACK(pos, var);				\
	SET_NAMED(var, 1);				\
    }							\
} while (0)

<<<<<<< eval.cpp
RObject* ByteCode::interpret(ByteCode* bcode, Environment* rho)
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
/* The CALLBUILTIN instruction handles calls to both true BUILTINs and
   to .Internals of type BUILTIN. To handle profiling in a way that is
   consistent with this instruction needs to be able to distinguish a
   true BUILTIN from a .Internal. LT */
#define IS_TRUE_BUILTIN(x) ((R_FunTab[PRIMOFFSET(x)].eval % 100 )/10 == 0)

static SEXP bcEval(SEXP body, SEXP rho, Rboolean useCache)
=======
/* The CALLBUILTIN instruction handles calls to both true BUILTINs and
   to .Internals of type BUILTIN. To handle profiling in a way that is
   consistent with this instruction needs to be able to distinguish a
   true BUILTIN from a .Internal. LT */
#define IS_TRUE_BUILTIN(x) ((R_FunTab[PRIMOFFSET(x)].eval % 100 )/10 == 0)

static R_INLINE Rboolean GETSTACK_LOGICAL_NO_NA_PTR(R_bcstack_t *s, int callidx,
						    SEXP constants) 
{
#ifdef TYPED_STACK
    if (s->tag == LGLSXP && s->u.ival != NA_LOGICAL)
	return s->u.ival;
#endif
    SEXP value = GETSTACK_PTR(s); 
    if (IS_SCALAR(value, LGLSXP) && LOGICAL(value)[0] != NA_LOGICAL)
	return LOGICAL(value)[0];
    else {
	SEXP call = VECTOR_ELT(constants, callidx);
	return asLogicalNoNA(value, call);
    }
}

static SEXP bcEval(SEXP body, SEXP rho, Rboolean useCache)
>>>>>>> eval.c
{
<<<<<<< eval.cpp
  ByteCode::Scope scope;
  GCStackRoot<> bcode_reachable(bcode);
  SEXP body = bcode;
  SEXP value;
  ListVector* constants;
  const BCODE *pc, *codebase;
  int ftype = 0;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
  SEXP value, constants;
  BCODE *pc, *codebase;
  int ftype = 0;
  R_bcstack_t *oldntop = R_BCNodeStackTop;
=======
  SEXP value, constants;
  BCODE *pc, *codebase;
  R_bcstack_t *oldntop = R_BCNodeStackTop;
>>>>>>> eval.c
  static int evalcount = 0;
#ifdef BC_PROFILING
  int old_current_opcode = current_opcode;
#endif
#ifdef THREADED_CODE
  int which = 0;
#endif

  BC_CHECK_SIGINT();

  INITIALIZE_MACHINE();
  codebase = &*(bcode->code().begin());
  pc = codebase;
  constants = bcode->m_constants;

  /* allow bytecode to be disabled for testing */
  if (R_disable_bytecode)
      return Rf_eval(bytecodeExpr(body), rho);

  /* check version */
  {
      int version = GETOP();
      if (version < R_bcMinVersion || version > R_bcVersion) {
	  if (version >= 2) {
	      static Rboolean warned = FALSE;
	      if (! warned) {
		  warned = TRUE;
		  Rf_warning(_("bytecode version mismatch; using eval"));
	      }
	      return Rf_eval(bytecodeExpr(body), rho);
	  }
	  else if (version < R_bcMinVersion)
	      Rf_error(_("bytecode version is too old"));
	  else Rf_error(_("bytecode version is too new"));
      }
  }

  R_binding_cache_t vcache = nullptr;
  Rboolean smallcache = TRUE;
#ifdef USE_BINDING_CACHE
  if (useCache) {
      R_len_t n = LENGTH(constants);
# ifdef CACHE_MAX
      if (n > CACHE_MAX) {
	  n = CACHE_MAX;
	  smallcache = FALSE;
      }
# endif
# ifdef CACHE_ON_STACK
      /* initialize binding cache on the stack */
      vcache = R_BCNodeStackTop;
      if (R_BCNodeStackTop + n > R_BCNodeStackEnd)
	  nodeStackOverflow();
      while (n > 0) {
	  SETSTACK(0, R_NilValue);
	  R_BCNodeStackTop++;
	  n--;
      }
# else
      /* allocate binding cache and protect on stack */
      vcache = Rf_allocVector(VECSXP, n);
      BCNPUSH(vcache);
# endif
  }
#endif

  BEGIN_MACHINE {
    OP(BCMISMATCH, 0): Rf_error(_("byte code version mismatch"));
    OP(RETURN, 0): value = GETSTACK(-1); goto done;
    OP(GOTO, 1):
      {
	int label = GETOP();
	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(BRIFNOT, 2):
      {
	int callidx = GETOP();
	int label = GETOP();
<<<<<<< eval.cpp
	int cond;
	SEXP call = (*constants)[callidx];
	value = BCNPOP();
	cond = asLogicalNoNA(value, call);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	int cond;
	SEXP call = VECTOR_ELT(constants, callidx);
	value = BCNPOP();
	cond = asLogicalNoNA(value, call);
=======
	Rboolean cond = GETSTACK_LOGICAL_NO_NA_PTR(R_BCNodeStackTop - 1,
						   callidx, constants);
	BCNPOP_IGNORE_VALUE();
>>>>>>> eval.c
	if (! cond) {
	    BC_CHECK_SIGINT(); /**** only on back branch?*/
	    pc = codebase + label;
	}
	NEXT();
      }
    OP(POP, 0):
	BCNPOP_IGNORE_VALUE();
	NEXT();
    OP(DUP, 0):
	value = NSFROMEND(1);
	BCNPUSH(value);
	NEXT();
    OP(PRINTVALUE, 0):
	Rf_PrintValue(BCNPOP());
	NEXT();
    OP(STARTLOOPCNTXT, 1):
	{
	    ByteCode* code = SEXP_downcast<ByteCode*>((*constants)[GETOP()].get());
	    loopWithContext(code, rho);
	    NEXT();
	}
    OP(ENDLOOPCNTXT, 0):
	value = R_NilValue;
	goto done;
    OP(DOLOOPNEXT, 0):
	{
	    throw LoopException(rho, true);
	}
    OP(DOLOOPBREAK, 0):
	{
	    throw LoopException(rho, false);
	}
    OP(STARTFOR, 3):
      {
	Rboolean iscompact = FALSE;
	SEXP seq = getForLoopSeq(-1, &iscompact);
	int callidx = GETOP();
	SEXP symbol = (*constants)[GETOP()];
	int label = GETOP();

	/* if we are iterating over a factor, coerce to character first */
	if (Rf_inherits(seq, "factor")) {
	    seq = Rf_asCharacterFactor(seq);
	    SETSTACK(-1, seq);
	}

	Rf_defineVar(symbol, R_NilValue, rho);

	// Here CR casts the return value of GET_BINDING_CELL (which
	// in CR is of type R_varloc_t) into a SEXP and pushes it onto
	// the node stack.  In CXXR we use a separate stack for
	// bindings, but push a null pointer onto the node stack to
	// maintain CR's stack alignment.
	BCNPUSH(nullptr);
	s_loopvar_stack->push_back(GET_BINDING_CELL(symbol, rho));

	value = IntVector::create(2);
	INTEGER(value)[0] = -1;
<<<<<<< eval.cpp
	if (Rf_isVector(seq))
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (isVector(seq))
=======
#ifdef COMPACT_INTSEQ
	if (iscompact) {
	    int n1 = INTEGER(seq)[0];
	    int n2 = INTEGER(seq)[1];
	    INTEGER(value)[1] = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
	}
	else
#endif
	if (isVector(seq))
>>>>>>> eval.c
	  INTEGER(value)[1] = LENGTH(seq);
	else if (Rf_isList(seq) || Rf_isNull(seq))
	  INTEGER(value)[1] = length(seq);
	else Rf_errorcall(VECTOR_ELT(constants, callidx),
		       _("invalid for() loop sequence"));
	BCNPUSH(value);

	/* bump up NAMED count of seq to avoid modification by loop code */
	INCREMENT_NAMED(seq);
	INCREMENT_REFCNT(seq);

	/* place initial loop variable value object on stack */
	switch(TYPEOF(seq)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    value = Rf_allocVector(TYPEOF(seq), 1);
	    BCNPUSH(value);
	    break;
	default: BCNPUSH(nullptr);
	}

	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(STEPFOR, 1):
      {
	int label = GETOP();
	int *loopinfo = INTEGER(GETSTACK_SXPVAL(-2));
	int i = ++loopinfo[0];
	int n = loopinfo[1];
	if (i < n) {
<<<<<<< eval.cpp
	  SEXP seq = GETSTACK(-4);
	  Frame::Binding* cell = s_loopvar_stack->back();
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  SEXP seq = GETSTACK(-4);
	  SEXP cell = GETSTACK(-3);
=======
	  Rboolean iscompact = FALSE;
	  SEXP seq = getForLoopSeq(-4, &iscompact);
	  SEXP cell = GETSTACK(-3);
>>>>>>> eval.c
	  switch (TYPEOF(seq)) {
	  case LGLSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    LOGICAL(value)[0] = LOGICAL(seq)[i];
	    break;
	  case INTSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
#ifdef COMPACT_INTSEQ
	    if (iscompact) {
		int *info = INTEGER(seq);
		int n1 = info[0];
		int n2 = info[1];
		int val = n1 <= n2 ? n1 + i : n1 - i;
		INTEGER(value)[0] = val;
	    }
	    else
#endif
	    INTEGER(value)[0] = INTEGER(seq)[i];
	    break;
	  case REALSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    REAL(value)[0] = REAL(seq)[i];
	    break;
	  case CPLXSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    COMPLEX(value)[0] = COMPLEX(seq)[i];
	    break;
	  case STRSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    SET_STRING_ELT(value, 0, STRING_ELT(seq, i));
	    break;
	  case RAWSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    RAW(value)[0] = RAW(seq)[i];
	    break;
	  case EXPRSXP:
	      value = (*static_cast<ExpressionVector*>(seq))[i];
	      SET_NAMED(value, 2);
	      break;
	  case VECSXP:
	      value = (*static_cast<ListVector*>(seq))[i];
	      SET_NAMED(value, 2);
	      break;
	  case LISTSXP:
	    value = CAR(seq);
	    SETSTACK(-4, CDR(seq));
	    SET_NAMED(value, 2);
	    break;
	  default:
	    Rf_error(_("invalid sequence argument in for loop"));
	  }
<<<<<<< eval.cpp
	  if (! SET_BINDING_VALUE(cell, value))
	      Rf_defineVar(BINDING_SYMBOL(cell), value, rho);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  if (! SET_BINDING_VALUE(cell, value))
	      defineVar(BINDING_SYMBOL(cell), value, rho);
=======
	  if (CAR(cell) == R_UnboundValue || ! SET_BINDING_VALUE(cell, value))
	      defineVar(BINDING_SYMBOL(cell), value, rho);
>>>>>>> eval.c
	  BC_CHECK_SIGINT();
	  pc = codebase + label;
	}
	NEXT();
      }
    OP(ENDFOR, 0):
      {
<<<<<<< eval.cpp
	s_nodestack->pop(3);
	s_loopvar_stack->pop_back();
	SETSTACK(-1, nullptr);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	R_BCNodeStackTop -= 3;
	SETSTACK(-1, R_NilValue);
=======
#ifdef COMPUTE_REFCNT_VALUES
	Rboolean iscompact = FALSE;
	SEXP seq = getForLoopSeq(-4, &iscompact);
	DECREMENT_REFCNT(seq);
#endif
	R_BCNodeStackTop -= 3;
	SETSTACK(-1, R_NilValue);
>>>>>>> eval.c
	NEXT();
      }
    OP(SETLOOPVAL, 0):
      BCNPOP_IGNORE_VALUE(); SETSTACK(-1, nullptr); NEXT();
    OP(INVISIBLE,0):
	R_Visible = FALSE;
	NEXT();
    /**** for now LDCONST, LDTRUE, and LDFALSE duplicate/allocate to
	  be defensive against bad package C code */
    OP(LDCONST, 1):
<<<<<<< eval.cpp
	R_Visible = TRUE;
	value = (*constants)[GETOP()];
	/* make sure NAMED = 2 -- lower values might be safe in some cases but
	   not in general, especially if the constant pool was created by
	   unserializing a compiled expression. */
	/*if (NAMED(value) < 2) SET_NAMED(value, 2);*/
	BCNPUSH(Rf_duplicate(value));
	NEXT();
    OP(LDNULL, 0):
	R_Visible = TRUE;
	BCNPUSH(nullptr);
	NEXT();
    OP(LDTRUE, 0):
	R_Visible = TRUE;
	BCNPUSH(Rf_mkTrue());
	NEXT();
    OP(LDFALSE, 0):
	R_Visible = TRUE;
	BCNPUSH(Rf_mkFalse());
	NEXT();
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
      R_Visible = TRUE;
      value = VECTOR_ELT(constants, GETOP());
      /* make sure NAMED = 2 -- lower values might be safe in some cases but
	 not in general, especially if the constant pool was created by
	 unserializing a compiled expression. */
      /*if (NAMED(value) < 2) SET_NAMED(value, 2);*/
      BCNPUSH(duplicate(value));
      NEXT();
    OP(LDNULL, 0): R_Visible = TRUE; BCNPUSH(R_NilValue); NEXT();
    OP(LDTRUE, 0): R_Visible = TRUE; BCNPUSH(mkTrue()); NEXT();
    OP(LDFALSE, 0): R_Visible = TRUE; BCNPUSH(mkFalse()); NEXT();
=======
      R_Visible = TRUE;
      value = VECTOR_ELT(constants, GETOP());
      MARK_NOT_MUTABLE(value);
      BCNPUSH(value);
      NEXT();
    OP(LDNULL, 0): R_Visible = TRUE; BCNPUSH(R_NilValue); NEXT();
    OP(LDTRUE, 0): R_Visible = TRUE; BCNPUSH(R_TrueValue); NEXT();
    OP(LDFALSE, 0): R_Visible = TRUE; BCNPUSH(R_FalseValue); NEXT();
>>>>>>> eval.c
    OP(GETVAR, 1): DO_GETVAR(FALSE, FALSE);
    OP(DDVAL, 1): DO_GETVAR(TRUE, FALSE);
    OP(SETVAR, 1):
      {
	int sidx = GETOP();
	Frame::Binding* loc;
	if (smallcache)
	    loc = GET_SMALLCACHE_BINDING_CELL(vcache, sidx);
	else {
	    SEXP symbol = (*constants)[sidx];
	    loc = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	}
#ifdef TYPED_STACK
	R_bcstack_t *s = R_BCNodeStackTop - 1;
	/* reading the locked bit is OK even if cell is R_NilValue */
	if (s->tag && ! BINDING_IS_LOCKED(loc)) {
	    /* if cell is R_NilValue or an active binding, or if the value
	       is R_UnboundValue, then TYPEOF(CAR(cell)) will not match the
	       immediate value tag. */
	    SEXP x = CAR(loc);  /* fast, but assumes binding is a CONS */
	    if (NOT_SHARED(x) && IS_SIMPLE_SCALAR(x, s->tag)) {
		/* if the binding value is not shared and is a simple
		   scaler of the same type as the immediate value,
		   then we can copy the stack value into the binding
		   value */
		switch (s->tag) {
		case REALSXP: REAL(x)[0] = s->u.dval; NEXT();
		case INTSXP: INTEGER(x)[0] = s->u.ival; NEXT();
		case LGLSXP: LOGICAL(x)[0] = s->u.ival; NEXT();
		}
	    }
	}
#endif
	value = GETSTACK(-1);
	INCREMENT_NAMED(value);
	if (! SET_BINDING_VALUE(loc, value)) {
	    SEXP symbol = VECTOR_ELT(constants, sidx);
	    PROTECT(value);
	    Rf_defineVar(symbol, value, rho);
	    UNPROTECT(1);
	}
	NEXT();
      }
    OP(GETFUN, 1):
      {
	/* get the function */
	SEXP symbol = (*constants)[GETOP()];
	value = Rf_findFun(symbol, rho);
	if(RTRACE(value)) {
	  Rprintf("trace: ");
	  Rf_PrintValue(symbol);
	}
<<<<<<< eval.cpp

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	SETSTACK(0, value);
	SETSTACK(1, R_NilValue);
	SETSTACK(2, R_NilValue);
	R_BCNodeStackTop += 3;
=======
	INIT_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(GETGLOBFUN, 1):
      {
	/* get the function */
	SEXP symbol = (*constants)[GETOP()];
	value = Rf_findFun(symbol, R_GlobalEnv);
	if(RTRACE(value)) {
	  Rprintf("trace: ");
	  Rf_PrintValue(symbol);
	}
<<<<<<< eval.cpp

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	SETSTACK(0, value);
	SETSTACK(1, R_NilValue);
	SETSTACK(2, R_NilValue);
	R_BCNodeStackTop += 3;
=======
	INIT_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(GETSYMFUN, 1):
      {
	/* get the function */
	SEXP symbol = (*constants)[GETOP()];
	value = SYMVALUE(symbol);
	if (TYPEOF(value) == PROMSXP) {
	    value = forcePromise(value);
	    SET_NAMED(value, 2);
	}
	if(RTRACE(value)) {
	  Rprintf("trace: ");
	  Rf_PrintValue(symbol);
	}
<<<<<<< eval.cpp

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	SETSTACK(0, value);
	SETSTACK(1, R_NilValue);
	SETSTACK(2, R_NilValue);
	R_BCNodeStackTop += 3;
=======
	INIT_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(GETBUILTIN, 1):
      {
	/* get the function */
	SEXP symbol = (*constants)[GETOP()];
	value = getPrimitive(symbol, BUILTINSXP);
	if (RTRACE(value)) {
	  Rprintf("trace: ");
	  Rf_PrintValue(symbol);
	}
<<<<<<< eval.cpp

	/* push the function and push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

	/* push the function and push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	SETSTACK(0, value);
	SETSTACK(1, R_NilValue);
	SETSTACK(2, R_NilValue);
	R_BCNodeStackTop += 3;
=======
	INIT_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(GETINTLBUILTIN, 1):
      {
	/* get the function */
	SEXP symbol = (*constants)[GETOP()];
	value = INTERNAL(symbol);
	if (TYPEOF(value) != BUILTINSXP)
	  Rf_error(_("there is no .Internal function '%s'"),
		CHAR(PRINTNAME(symbol)));
<<<<<<< eval.cpp

	/* push the function and push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp

	/* push the function and push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	SETSTACK(0, value);
	SETSTACK(1, R_NilValue);
	SETSTACK(2, R_NilValue);
	R_BCNodeStackTop += 3;
=======
	INIT_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(CHECKFUN, 0):
      {
	/* check then the value on the stack is a function */
	value = GETSTACK(-1);
	if (TYPEOF(value) != CLOSXP && TYPEOF(value) != BUILTINSXP &&
	    TYPEOF(value) != SPECIALSXP)
<<<<<<< eval.cpp
	  Rf_error(_("attempt to apply non-function"));

	/* initialize the function type register, and push space for
	   creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(2);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  error(_("attempt to apply non-function"));

	/* initialize the function type register, and push space for
	   creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(2);
	SETSTACK(0, R_NilValue);
	SETSTACK(1, R_NilValue);
	R_BCNodeStackTop += 2;
=======
	  error(_("attempt to apply non-function"));
	INIT_CALL_FRAME_ARGS();
>>>>>>> eval.c
	NEXT();
      }
    OP(MAKEPROM, 1):
      {
<<<<<<< eval.cpp
	  ByteCode* code = SEXP_downcast<ByteCode*>((*constants)[GETOP()].get());
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	SEXP code = VECTOR_ELT(constants, GETOP());
=======
	SEXP code = VECTOR_ELT(constants, GETOP());
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
>>>>>>> eval.c
	if (ftype != SPECIALSXP) {
	  if (ftype == BUILTINSXP)
	      value = code->evaluate(rho);
	  else
	    value = Rf_mkPROMISE(code, rho);
	  PUSHCALLARG(value);
	}
	NEXT();
      }
    OP(DOMISSING, 0):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	if (ftype != SPECIALSXP)
	  PUSHCALLARG(R_MissingArg);
	NEXT();
      }
    OP(SETTAG, 1):
      {
<<<<<<< eval.cpp
	SEXP tag = (*constants)[GETOP()];
	SEXP cell = GETSTACK(-1);
	if (ftype != SPECIALSXP && cell != R_NilValue)
	  SET_TAG(cell, Rf_CreateTag(tag));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	SEXP tag = VECTOR_ELT(constants, GETOP());
	SEXP cell = GETSTACK(-1);
	if (ftype != SPECIALSXP && cell != R_NilValue)
	  SET_TAG(cell, CreateTag(tag));
=======
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	int tagidx = GETOP();
	if (ftype != SPECIALSXP) {
	    SEXP tag = VECTOR_ELT(constants, tagidx);
	    SETCALLARG_TAG(tag);
	}
>>>>>>> eval.c
	NEXT();
      }
    OP(DODOTS, 0):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	if (ftype != SPECIALSXP) {
	  SEXP h = Rf_findVar(R_DotsSymbol, rho);
	  if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
	    for (; h != R_NilValue; h = CDR(h)) {
<<<<<<< eval.cpp
	      SEXP val, cell;
	      if (ftype == BUILTINSXP) val = Rf_eval(CAR(h), rho);
	      else val = Rf_mkPROMISE(CAR(h), rho);
	      cell = PairList::cons(val);
	      PUSHCALLARG_CELL(cell);
	      if (TAG(h) != R_NilValue) SET_TAG(cell, Rf_CreateTag(TAG(h)));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	      SEXP val, cell;
	      if (ftype == BUILTINSXP) val = eval(CAR(h), rho);
	      else val = mkPROMISE(CAR(h), rho);
	      cell = CONS(val, R_NilValue);
	      PUSHCALLARG_CELL(cell);
	      if (TAG(h) != R_NilValue) SET_TAG(cell, CreateTag(TAG(h)));
=======
	      SEXP val;
	      if (ftype == BUILTINSXP) val = eval(CAR(h), rho);
	      else val = mkPROMISE(CAR(h), rho);
	      PUSHCALLARG(val);
	      SETCALLARG_TAG(TAG(h));
>>>>>>> eval.c
	    }
	  }
	  else if (h != R_MissingArg)
	    Rf_error(_("'...' used in an incorrect context"));
	}
	NEXT();
      }
    OP(PUSHARG, 0):
	PUSHCALLARG(BCNPOP());
	NEXT();
    /**** for now PUSHCONST, PUSHTRUE, and PUSHFALSE duplicate/allocate to
	  be defensive against bad package C code */
    OP(PUSHCONSTARG, 1):
<<<<<<< eval.cpp
      value = (*constants)[GETOP()];
      PUSHCALLARG(Rf_duplicate(value));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
      value = VECTOR_ELT(constants, GETOP());
      PUSHCALLARG(duplicate(value));
=======
      value = VECTOR_ELT(constants, GETOP());
      MARK_NOT_MUTABLE(value);
      PUSHCALLARG(value);
>>>>>>> eval.c
      NEXT();
<<<<<<< eval.cpp
    OP(PUSHNULLARG, 0):
	PUSHCALLARG(nullptr);
	NEXT();
    OP(PUSHTRUEARG, 0):
	PUSHCALLARG(Rf_mkTrue());
	NEXT();
    OP(PUSHFALSEARG, 0):
	PUSHCALLARG(Rf_mkFalse());
	NEXT();
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
    OP(PUSHNULLARG, 0): PUSHCALLARG(R_NilValue); NEXT();
    OP(PUSHTRUEARG, 0): PUSHCALLARG(mkTrue()); NEXT();
    OP(PUSHFALSEARG, 0): PUSHCALLARG(mkFalse()); NEXT();
=======
    OP(PUSHNULLARG, 0): PUSHCALLARG(R_NilValue); NEXT();
    OP(PUSHTRUEARG, 0): PUSHCALLARG(R_TrueValue); NEXT();
    OP(PUSHFALSEARG, 0): PUSHCALLARG(R_FalseValue); NEXT();
>>>>>>> eval.c
    OP(CALL, 1):
      {
<<<<<<< eval.cpp
	FunctionBase* fun = SEXP_downcast<FunctionBase*>(
	    static_cast<RObject*>(GETSTACK(-3)));
	Expression* call = SEXP_downcast<Expression*>(
	    static_cast<RObject*>((*constants)[GETOP()]));
	PairList* args = SEXP_downcast<PairList*>(
	    static_cast<RObject*>(GETSTACK(-2)));

	switch (ftype) {
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	SEXP fun = GETSTACK(-3);
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP args = GETSTACK(-2);
	int flag;
	switch (ftype) {
=======
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP args = CALL_FRAME_ARGS();
	int flag;
	switch (TYPEOF(fun)) {
>>>>>>> eval.c
	case BUILTINSXP:
	{
	    checkForMissings(args, call);
	    ArgList arglist(args, ArgList::EVALUATED);
	    value = fun->apply(&arglist, rho, call);
	    break;
	}
	case SPECIALSXP:
	{
	    ArgList arglist(call->tail(), ArgList::RAW);
	    value = fun->apply(&arglist, rho, call);
	    break;
	}
	case CLOSXP:
<<<<<<< eval.cpp
	{
	    ArgList arglist(args, ArgList::PROMISED);
	    value = fun->apply(&arglist, rho, call);
	    break;
	}
	default: Rf_error(_("bad function"));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  value = applyClosure(call, fun, args, rho, R_BaseEnv);
	  break;
	default: error(_("bad function"));
=======
	  value = applyClosure(call, fun, args, rho, R_NilValue);
	  break;
	default: error(_("bad function"));
>>>>>>> eval.c
	}
<<<<<<< eval.cpp
	s_nodestack->pop(2);
	SETSTACK(-1, value);
	ftype = 0;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	R_BCNodeStackTop -= 2;
	SETSTACK(-1, value);
	ftype = 0;
=======
	POP_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(CALLBUILTIN, 1):
      {
<<<<<<< eval.cpp
	SEXP fun = GETSTACK(-3);
	SEXP call = (*constants)[GETOP()];
	SEXP args = GETSTACK(-2);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	SEXP fun = GETSTACK(-3);
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP args = GETSTACK(-2);
	int flag;
	const void *vmax = vmaxget();
=======
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP args = CALL_FRAME_ARGS();
	int flag;
	const void *vmax = vmaxget();
>>>>>>> eval.c
	if (TYPEOF(fun) != BUILTINSXP)
<<<<<<< eval.cpp
	  Rf_error(_("not a BUILTIN function"));
	// Inner block because destructor of ArgList must be called
	// before NEXT():
	{
	    const BuiltInFunction* func = static_cast<BuiltInFunction*>(fun);
	    const Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* env = SEXP_downcast<Environment*>(rho);
	    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
	    value = func->apply(&arglist, env, callx);
	}
	s_nodestack->pop(2);
	SETSTACK(-1, value);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  error(_("not a BUILTIN function"));
	flag = PRIMPRINT(fun);
	R_Visible = flag != 1;
	if (R_Profiling && IS_TRUE_BUILTIN(fun)) {
	    RCNTXT cntxt;
	    SEXP oldref = R_Srcref;
	    begincontext(&cntxt, CTXT_BUILTIN, call,
			 R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    R_Srcref = NULL;
	    value = PRIMFUN(fun) (call, fun, args, rho);
	    R_Srcref = oldref;
	    endcontext(&cntxt);
	} else {
	    value = PRIMFUN(fun) (call, fun, args, rho);
	}
	if (flag < 2) R_Visible = flag != 1;
	vmaxset(vmax);
	R_BCNodeStackTop -= 2;
	SETSTACK(-1, value);
=======
	  error(_("not a BUILTIN function"));
	flag = PRIMPRINT(fun);
	R_Visible = flag != 1;
	if (R_Profiling && IS_TRUE_BUILTIN(fun)) {
	    RCNTXT cntxt;
	    SEXP oldref = R_Srcref;
	    begincontext(&cntxt, CTXT_BUILTIN, call,
			 R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    R_Srcref = NULL;
	    value = PRIMFUN(fun) (call, fun, args, rho);
	    R_Srcref = oldref;
	    endcontext(&cntxt);
	} else {
	    value = PRIMFUN(fun) (call, fun, args, rho);
	}
	if (flag < 2) R_Visible = flag != 1;
	vmaxset(vmax);
	POP_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(CALLSPECIAL, 1):
      {
	SEXP call = (*constants)[GETOP()];
	SEXP symbol = CAR(call);
	SEXP fun = getPrimitive(symbol, SPECIALSXP);
	if (RTRACE(fun)) {
	  Rprintf("trace: ");
	  Rf_PrintValue(symbol);
	}
	BCNPUSH(fun);  /* for GC protection */
	// Inner block because destructor of ArgList must be called
	// before NEXT():
	{
	    const BuiltInFunction* func = static_cast<BuiltInFunction*>(fun);
	    const Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* env = SEXP_downcast<Environment*>(rho);
	    ArgList arglist(callx->tail(), ArgList::RAW);
	    value = func->apply(&arglist, env, callx);
	}
	SETSTACK(-1, value); /* replaces fun on stack */
	NEXT();
      }
    OP(MAKECLOSURE, 1):
      {
	SEXP fb = (*constants)[GETOP()];
	SEXP forms = VECTOR_ELT(fb, 0);
	SEXP body = VECTOR_ELT(fb, 1);
	value = Rf_mkCLOSXP(forms, body, rho);
	BCNPUSH(value);
	NEXT();
      }
    OP(UMINUS, 1): FastUnary(-, R_SubSym);
    OP(UPLUS, 1): FastUnary(+, R_AddSym);
    OP(ADD, 1): FastBinary(R_ADD, PLUSOP, R_AddSym);
    OP(SUB, 1): FastBinary(R_SUB, MINUSOP, R_SubSym);
    OP(MUL, 1): FastBinary(R_MUL, TIMESOP, R_MulSym);
    OP(DIV, 1): FastBinary(R_DIV, DIVOP, R_DivSym);
    OP(EXPT, 1): FastBinary(R_POW, POWOP, R_ExptSym);
    OP(SQRT, 1): FastMath1(R_sqrt, R_SqrtSym);
    OP(EXP, 1): FastMath1(exp, R_ExpSym);
    OP(EQ, 1): FastRelop2(==, EQOP, R_EqSym);
    OP(NE, 1): FastRelop2(!=, NEOP, R_NeSym);
    OP(LT, 1): FastRelop2(<, LTOP, R_LtSym);
    OP(LE, 1): FastRelop2(<=, LEOP, R_LeSym);
    OP(GE, 1): FastRelop2(>=, GEOP, R_GeSym);
    OP(GT, 1): FastRelop2(>, GTOP, R_GtSym);
    OP(AND, 1): Builtin2(do_logic_slow, R_AndSym, rho);
    OP(OR, 1): Builtin2(do_logic_slow, R_OrSym, rho);
    OP(NOT, 1): Builtin1(do_logic_slow, R_NotSym, rho);
    OP(DOTSERR, 0): Rf_error(_("'...' used in an incorrect context"));
    OP(STARTASSIGN, 1):
      {
	int sidx = GETOP();
	SEXP symbol = (*constants)[sidx];
	Frame::Binding* cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = BINDING_VALUE(cell);
	if (value == R_UnboundValue ||
	    TYPEOF(value) == PROMSXP ||
#ifdef SWITCH_TO_REFCNT
	    REFCNT(value) != 1
#else
	    NAMED(value) != 1
#endif
	    )
	    value = EnsureLocal(symbol, rho);
	BCNPUSH(value);
	BCNDUP2ND();
	/* top three stack entries are now RHS value, LHS value, RHS value */
	if (IS_STACKVAL_BOXED(-1)) {
	    FIXUP_RHS_NAMED(GETSTACK(-1));
	    INCREMENT_REFCNT(GETSTACK(-1));
	}
	NEXT();
      }
    OP(ENDASSIGN, 1):
      {
	int sidx = GETOP();
	SEXP symbol = (*constants)[sidx];
	Frame::Binding* cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = GETSTACK(-1); /* leave on stack for GC protection */
	INCREMENT_NAMED(value);
	if (! SET_BINDING_VALUE(cell, value))
	    Rf_defineVar(symbol, value, rho);
	s_nodestack->pop(); /* now pop LHS value off the stack */
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = 2 */
	SET_NAMED(GETSTACK(-1), 2);
#else
	if (IS_STACKVAL_BOXED(-1)) {
	    INCREMENT_NAMED(GETSTACK(-1));
	    DECREMENT_REFCNT(GETSTACK(-1));
	}
#endif
	NEXT();
      }
    OP(STARTSUBSET, 2): DO_STARTDISPATCH("[");
    OP(DFLTSUBSET, 0): DO_DFLTDISPATCH(do_subset_dflt, R_SubsetSym);
    OP(STARTSUBASSIGN, 2): DO_START_ASSIGN_DISPATCH(CXXRCCAST(char*, "[<-"));
    OP(DFLTSUBASSIGN, 0):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign_dflt, R_SubassignSym);
    OP(STARTC, 2): DO_STARTDISPATCH("c");
    OP(DFLTC, 0): DO_DFLTDISPATCH(do_c_dflt, R_CSym);
    OP(STARTSUBSET2, 2): DO_STARTDISPATCH("[[");
    OP(DFLTSUBSET2, 0): DO_DFLTDISPATCH(do_subset2_dflt, R_Subset2Sym);
    OP(STARTSUBASSIGN2, 2): DO_START_ASSIGN_DISPATCH(CXXRCCAST(char*, "[[<-"));
    OP(DFLTSUBASSIGN2, 0):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign2_dflt, R_Subassign2Sym);
    OP(DOLLAR, 2):
      {
	int dispatched = FALSE;
	SEXP call = (*constants)[GETOP()];
	SEXP symbol = (*constants)[GETOP()];
	SEXP x = GETSTACK(-1);
	if (Rf_isObject(x)) {
	    SEXP ncall;
	    PROTECT(ncall = Rf_duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), Rf_ScalarString(PRINTNAME(symbol)));
	    dispatched = tryDispatch("$", ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (dispatched)
	    SETSTACK(-1, value);
	else
	    SETSTACK(-1, R_subset3_dflt(x, PRINTNAME(symbol), nullptr));
	NEXT();
      }
    OP(DOLLARGETS, 2):
      {
	int dispatched = FALSE;
	SEXP call = (*constants)[GETOP()];
	SEXP symbol = (*constants)[GETOP()];
	SEXP x = GETSTACK(-2);
	SEXP rhs = GETSTACK(-1);
<<<<<<< eval.cpp
	if (NAMED(x) == 2) {
	    x = Rf_duplicate(x);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (NAMED(x) == 2) {
	    x = duplicate(x);
=======
	if (MAYBE_SHARED(x)) {
	    x = shallow_duplicate(x);
>>>>>>> eval.c
	    SETSTACK(-2, x);
	    SET_NAMED(x, 1);
	}
	if (Rf_isObject(x)) {
	    SEXP ncall, prom;
	    PROTECT(ncall = Rf_duplicate(call));
	    /**** hack to avoid evaluating the symbol */
<<<<<<< eval.cpp
	    SETCAR(CDDR(ncall), Rf_ScalarString(PRINTNAME(symbol)));
	    prom = Rf_mkPROMISE(CADDDR(ncall), rho);
	    SET_PRVALUE(prom, rhs);
	    SETCAR(CDR(CDDR(ncall)), prom);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    prom = mkPROMISE(CADDDR(ncall), rho);
	    SET_PRVALUE(prom, rhs);
	    SETCAR(CDR(CDDR(ncall)), prom);
=======
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    prom = mkRHSPROMISE(CADDDR(ncall), rhs);
	    SETCAR(CDDDR(ncall), prom);
>>>>>>> eval.c
	    dispatched = tryDispatch("$<-", ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (! dispatched)
	  value = R_subassign3_dflt(call, x, symbol, rhs);
	s_nodestack->pop();
	SETSTACK(-1, value);
	NEXT();
      }
    OP(ISNULL, 0): DO_ISTEST(Rf_isNull);
    OP(ISLOGICAL, 0): DO_ISTYPE(LGLSXP);
    OP(ISINTEGER, 0): {
	SEXP arg = GETSTACK(-1);
<<<<<<< eval.cpp
	bool test = (TYPEOF(arg) == INTSXP) && ! Rf_inherits(arg, "factor");
	SETSTACK(-1, test ? Rf_mkTrue() : Rf_mkFalse());
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	Rboolean test = (TYPEOF(arg) == INTSXP) && ! inherits(arg, "factor");
	SETSTACK(-1, test ? mkTrue() : mkFalse());
=======
	Rboolean test = (TYPEOF(arg) == INTSXP) && ! inherits(arg, "factor");
	SETSTACK(-1, test ? R_TrueValue : R_FalseValue);
>>>>>>> eval.c
	NEXT();
      }
    OP(ISDOUBLE, 0): DO_ISTYPE(REALSXP);
    OP(ISCOMPLEX, 0): DO_ISTYPE(CPLXSXP);
    OP(ISCHARACTER, 0): DO_ISTYPE(STRSXP);
    OP(ISSYMBOL, 0): DO_ISTYPE(SYMSXP); /**** S4 thingy allowed now???*/
    OP(ISOBJECT, 0): DO_ISTEST(OBJECT);
    OP(ISNUMERIC, 0): DO_ISTEST(isNumericOnly);
    OP(VECSUBSET, 1): DO_VECSUBSET(rho, FALSE); NEXT();
    OP(MATSUBSET, 1): DO_MATSUBSET(rho, FALSE); NEXT();
    OP(VECSUBASSIGN, 1): DO_VECSUBASSIGN(rho, FALSE); NEXT();
    OP(MATSUBASSIGN, 1): DO_MATSUBASSIGN(rho, FALSE); NEXT();
    OP(AND1ST, 2): {
	int callidx = GETOP();
	int label = GETOP();
	FIXUP_SCALAR_LOGICAL(callidx, "'x'", "&&");
	value = GETSTACK(-1);
	if (LOGICAL(value)[0] == FALSE)
	    pc = codebase + label;
	NEXT();
    }
    OP(AND2ND, 1): {
	int callidx = GETOP();
	FIXUP_SCALAR_LOGICAL(callidx, "'y'", "&&");
	value = GETSTACK(-1);
	/* The first argument is TRUE or NA. If the second argument is
	   not TRUE then its value is the result. If the second
	   argument is TRUE, then the first argument's value is the
	   result. */
	if (LOGICAL(value)[0] != TRUE)
	    SETSTACK(-2, value);
	s_nodestack->pop();
	NEXT();
    }
    OP(OR1ST, 2):  {
	int callidx = GETOP();
	int label = GETOP();
	FIXUP_SCALAR_LOGICAL(callidx, "'x'", "||");
	value = GETSTACK(-1);
	if (LOGICAL(value)[0] != NA_LOGICAL && LOGICAL(value)[0]) /* is true */
	    pc = codebase + label;
	NEXT();
    }
    OP(OR2ND, 1):  {
	int callidx = GETOP();
	FIXUP_SCALAR_LOGICAL(callidx, "'y'", "||");
	value = GETSTACK(-1);
	/* The first argument is FALSE or NA. If the second argument is
	   not FALSE then its value is the result. If the second
	   argument is FALSE, then the first argument's value is the
	   result. */
	if (LOGICAL(value)[0] != FALSE)
	    SETSTACK(-2, value);
	s_nodestack->pop();
	NEXT();
    }
    OP(GETVAR_MISSOK, 1): DO_GETVAR(FALSE, TRUE);
    OP(DDVAL_MISSOK, 1): DO_GETVAR(TRUE, TRUE);
    OP(VISIBLE,0):
	R_Visible = TRUE;
	NEXT();
    OP(SETVAR2, 1):
      {
	SEXP symbol = (*constants)[GETOP()];
	value = GETSTACK(-1);
<<<<<<< eval.cpp
	if (NAMED(value)) {
	    value = Rf_duplicate(value);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (NAMED(value)) {
	    value = duplicate(value);
=======
	if (MAYBE_REFERENCED(value)) {
	    value = duplicate(value);
>>>>>>> eval.c
	    SETSTACK(-1, value);
	}
	Rf_setVar(symbol, value, ENCLOS(rho));
	NEXT();
      }
    OP(STARTASSIGN2, 1):
      {
	SEXP symbol = (*constants)[GETOP()];
	value = GETSTACK(-1);
	BCNPUSH(getvar(symbol, ENCLOS(rho), FALSE, FALSE, nullptr, 0));
	BCNPUSH(value);
	/* top three stack entries are now RHS value, LHS value, RHS value */
	FIXUP_RHS_NAMED(value);
	INCREMENT_REFCNT(value);
	NEXT();
      }
    OP(ENDASSIGN2, 1):
      {
	SEXP symbol = (*constants)[GETOP()];
	value = BCNPOP();
<<<<<<< eval.cpp
	switch (NAMED(value)) {
	case 0: SET_NAMED(value, 1); break;
	case 1: SET_NAMED(value, 2); break;
	}
	Rf_setVar(symbol, value, ENCLOS(rho));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	switch (NAMED(value)) {
	case 0: SET_NAMED(value, 1); break;
	case 1: SET_NAMED(value, 2); break;
	}
	setVar(symbol, value, ENCLOS(rho));
=======
	INCREMENT_NAMED(value);
	setVar(symbol, value, ENCLOS(rho));
>>>>>>> eval.c
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = 2 */
	SET_NAMED(GETSTACK(-1), 2);
#else
	INCREMENT_NAMED(GETSTACK(-1));
#endif
	DECREMENT_REFCNT(GETSTACK(-1));
	NEXT();
      }
    OP(SETTER_CALL, 2):
      {
<<<<<<< eval.cpp
	SEXP lhs = GETSTACK(-5);
	SEXP rhs = GETSTACK(-4);
	FunctionBase* fun = SEXP_downcast<FunctionBase*>(
	    static_cast<RObject*>(GETSTACK(-3)));
	Expression* call = SEXP_downcast<Expression*>(
	    static_cast<RObject*>((*constants)[GETOP()]));
	SEXP vexpr = (*constants)[GETOP()];
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	SEXP lhs = GETSTACK(-5);
	SEXP rhs = GETSTACK(-4);
	SEXP fun = GETSTACK(-3);
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP vexpr = VECTOR_ELT(constants, GETOP());
=======
	SEXP lhs = GETSTACK_BELOW_CALL_FRAME(-2);
	SEXP rhs = GETSTACK_BELOW_CALL_FRAME(-1);
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP vexpr = VECTOR_ELT(constants, GETOP());
>>>>>>> eval.c
	SEXP args, prom, last;
<<<<<<< eval.cpp
	if (NAMED(lhs) == 2) {
	  lhs = Rf_duplicate(lhs);
	  SETSTACK(-5, lhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	if (NAMED(lhs) == 2) {
	  lhs = duplicate(lhs);
	  SETSTACK(-5, lhs);
=======
	if (MAYBE_SHARED(lhs)) {
	  lhs = shallow_duplicate(lhs);
	  SETSTACK_BELOW_CALL_FRAME(-2, lhs);
>>>>>>> eval.c
	  SET_NAMED(lhs, 1);
	}
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  /* push RHS value onto arguments with 'value' tag */
	  PUSHCALLARG(rhs);
	  SETCALLARG_TAG_SYMBOL(R_valueSym);
	  /* replace first argument with LHS value */
	  args = CALL_FRAME_ARGS();
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  {
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
	      value = fun->apply(&arglist, rho, call);
	  }
	  break;
	case SPECIALSXP:
<<<<<<< eval.cpp
	  /* duplicate arguments and put into stack for GC protection */
	  args = Rf_duplicate(CDR(call));
	  SETSTACK(-2, args);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  /* duplicate arguments and put into stack for GC protection */
	  args = duplicate(CDR(call));
	  SETSTACK(-2, args);
=======
	  /* duplicate arguments and protect */
	  PROTECT(args = duplicate(CDR(call)));
>>>>>>> eval.c
	  /* insert evaluated promise for LHS as first argument */
<<<<<<< eval.cpp
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  prom = mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
=======
	  /* promise won't be captured so don't track refrences */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
>>>>>>> eval.c
	  SETCAR(args, prom);
	  /* insert evaluated promise for RHS as last argument */
	  last = args;
	  while (CDR(last) != R_NilValue)
	      last = CDR(last);
<<<<<<< eval.cpp
	  prom = Rf_mkPROMISE(vexpr, rho);
	  SET_PRVALUE(prom, rhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  prom = mkPROMISE(vexpr, rho);
	  SET_PRVALUE(prom, rhs);
=======
	  prom = mkRHSPROMISE(vexpr, rhs);
>>>>>>> eval.c
	  SETCAR(last, prom);
	  /* make the call */
<<<<<<< eval.cpp
	  {
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
	      value = fun->apply(&arglist, rho, call);
	  }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  value = PRIMFUN(fun) (call, fun, args, rho);
=======
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  UNPROTECT(1);
>>>>>>> eval.c
	  break;
	case CLOSXP:
	  /* push evaluated promise for RHS onto arguments with 'value' tag */
<<<<<<< eval.cpp
	  prom = Rf_mkPROMISE(vexpr, rho);
	  SET_PRVALUE(prom, rhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  prom = mkPROMISE(vexpr, rho);
	  SET_PRVALUE(prom, rhs);
=======
	  prom = mkRHSPROMISE(vexpr, rhs);
>>>>>>> eval.c
	  PUSHCALLARG(prom);
	  SETCALLARG_TAG_SYMBOL(R_valueSym);
	  /* replace first argument with evaluated promise for LHS */
<<<<<<< eval.cpp
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  args = GETSTACK(-2);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  prom = mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  args = GETSTACK(-2);
=======
	  /* promise might be captured, so track references */
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  args = CALL_FRAME_ARGS();
>>>>>>> eval.c
	  SETCAR(args, prom);
	  /* make the call */
<<<<<<< eval.cpp
	  {
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::PROMISED);
	      value = fun->apply(&arglist, rho, call);
	  }    
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  value = applyClosure(call, fun, args, rho, R_BaseEnv);
=======
	  value = applyClosure(call, fun, args, rho, R_NilValue);
>>>>>>> eval.c
	  break;
	default: Rf_error(_("bad function"));
	}
<<<<<<< eval.cpp
	s_nodestack->pop(4);
	SETSTACK(-1, value);
	ftype = 0;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	R_BCNodeStackTop -= 4;
	SETSTACK(-1, value);
	ftype = 0;
=======
	POP_CALL_FRAME_PLUS(2, value);
>>>>>>> eval.c
	NEXT();
      }
    OP(GETTER_CALL, 1):
      {
<<<<<<< eval.cpp
	SEXP lhs = GETSTACK(-5);
	FunctionBase* fun = SEXP_downcast<FunctionBase*>(
	    static_cast<RObject*>(GETSTACK(-3)));
	Expression* call = SEXP_downcast<Expression*>(
	    static_cast<RObject*>((*constants)[GETOP()]));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	SEXP lhs = GETSTACK(-5);
	SEXP fun = GETSTACK(-3);
	SEXP call = VECTOR_ELT(constants, GETOP());
=======
	SEXP lhs = GETSTACK_BELOW_CALL_FRAME(-2);
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
>>>>>>> eval.c
	SEXP args, prom;
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  /* replace first argument with LHS value */
	  args = CALL_FRAME_ARGS();
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  {
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
	      value = fun->apply(&arglist, rho, call);
	  }
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = Rf_duplicate(CDR(call));
	  SETSTACK(-2, args);
	  /* insert evaluated promise for LHS as first argument */
<<<<<<< eval.cpp
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  prom = mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
=======
	  /* promise won't be captured so don't track refrences */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
>>>>>>> eval.c
	  SETCAR(args, prom);
	  /* make the call */
	  {
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
	      value = fun->apply(&arglist, rho, call);
	  }
	  break;
	case CLOSXP:
	  /* replace first argument with evaluated promise for LHS */
<<<<<<< eval.cpp
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  args = GETSTACK(-2);
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  prom = mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  args = GETSTACK(-2);
=======
	  /* promise might be captured, so track references */
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  args = CALL_FRAME_ARGS();
>>>>>>> eval.c
	  SETCAR(args, prom);
	  /* make the call */
<<<<<<< eval.cpp
	  {
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::PROMISED);
	      value = fun->apply(&arglist, rho, call);
	  }
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	  value = applyClosure(call, fun, args, rho, R_BaseEnv);
=======
	  value = applyClosure(call, fun, args, rho, R_NilValue);
>>>>>>> eval.c
	  break;
	default: Rf_error(_("bad function"));
	}
<<<<<<< eval.cpp
	s_nodestack->pop(2);
	SETSTACK(-1, value);
	ftype = 0;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	R_BCNodeStackTop -= 2;
	SETSTACK(-1, value);
	ftype = 0;
=======
	POP_CALL_FRAME(value);
>>>>>>> eval.c
	NEXT();
      }
    OP(SWAP, 0): {
<<<<<<< eval.cpp
	R_bcstack_t tmp = NSFROMEND(1);
	NSFROMEND(1) = NSFROMEND(2);
	NSFROMEND(2) = tmp;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	R_bcstack_t tmp = R_BCNodeStackTop[-1];
	R_BCNodeStackTop[-1] = R_BCNodeStackTop[-2];
	R_BCNodeStackTop[-2] = tmp;
=======
	R_bcstack_t tmp = R_BCNodeStackTop[-1];
	/* This instruction only occurs between accessor calls in
	   complex assignments. [It should probably be renamed to
	   reflect this.] It needs to make sure intermediate LHS
	   values in complex assignments are not shared by duplicating
	   the extracted value in tmp when necessary. Duplicating is
	   necessary if the value might be shared _or_ if the
	   container, which is in R_BCNodeStackTop[-3], has become
	   possibly shared by going through a closure in the preceding
	   accessor call.  This is taken to indicate that the
	   corresponding replacement function might be a closure and
	   will need to see an unmodified LHS value. This heuristic
	   fails if the accessor function called here is not a closure
	   but the replacement function is. */

	/* For the typed stack it might be OK just to force boxing at
	   this point, but for now this code tries to avoid doing
	   that. The macros make the code a little more reabable. */
#define STACKVAL_MAYBE_REFERENCED(idx)				\
	(IS_STACKVAL_BOXED(idx) &&				\
	 MAYBE_REFERENCED(GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (idx))))
#define STACKVAL_MAYBE_SHARED(idx)				\
	(IS_STACKVAL_BOXED(idx) &&				\
	 MAYBE_SHARED(GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (idx))))

	if (STACKVAL_MAYBE_REFERENCED(-1) &&
	    (STACKVAL_MAYBE_SHARED(-1) || STACKVAL_MAYBE_SHARED(-3)))
	    GETSTACK_SXPVAL_PTR(&tmp) =
		shallow_duplicate(GETSTACK_SXPVAL_PTR(&tmp));

	R_BCNodeStackTop[-1] = R_BCNodeStackTop[-2];
	R_BCNodeStackTop[-2] = tmp;
>>>>>>> eval.c
	NEXT();
    }
    OP(DUP2ND, 0): BCNDUP2ND(); NEXT();
    OP(SWITCH, 4): {
       SEXP call = (*constants)[GETOP()];
       SEXP names = (*constants)[GETOP()];
       SEXP coffsets = (*constants)[GETOP()];
       SEXP ioffsets = (*constants)[GETOP()];
       value = BCNPOP();
<<<<<<< eval.cpp
       if (!Rf_isVector(value) || length(value) != 1)
	   Rf_errorcall(call, _("EXPR must be a length 1 vector"));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
       if (!isVector(value) || length(value) != 1)
	   errorcall(call, _("EXPR must be a length 1 vector"));
=======
       if (!isVector(value) || length(value) != 1)
	   errorcall(call, _("EXPR must be a length 1 vector"));
       if (isFactor(value))
	   warningcall(call,
		       _("EXPR is a \"factor\", treated as integer.\n"
			 " Consider using '%s' instead."),
		       "switch(as.character( * ), ...)");
>>>>>>> eval.c
       if (TYPEOF(value) == STRSXP) {
	   int i, n, which;
	   if (names == R_NilValue)
	       Rf_errorcall(call, _("numeric EXPR required for switch() without named alternatives"));
	   if (TYPEOF(coffsets) != INTSXP)
	       Rf_errorcall(call, _("bad character 'switch' offsets"));
	   if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(coffsets))
	       Rf_errorcall(call, _("bad 'switch' names"));
	   n = LENGTH(names);
	   which = n - 1;
	   for (i = 0; i < n - 1; i++)
	       if (Rf_pmatch(STRING_ELT(value, 0),
			  STRING_ELT(names, i), CXXRTRUE /* exact */)) {
		   which = i;
		   break;
	       }
	   pc = codebase + INTEGER(coffsets)[which];
       }
       else {
<<<<<<< eval.cpp
	   int which = Rf_asInteger(value) - 1;
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	   int which = asInteger(value) - 1;
=======
>>>>>>> eval.c
	   if (TYPEOF(ioffsets) != INTSXP)
<<<<<<< eval.cpp
	       Rf_errorcall(call, _("bad numeric 'switch' offsets"));
||||||| /home/kmillar/git/r-prev-merge-point/src/main/eval.cpp
	       errorcall(call, "bad numeric 'switch' offsets");
=======
	       errorcall(call, "bad numeric 'switch' offsets");
	   int which = asInteger(value);
	   if (which != NA_INTEGER) which--;
>>>>>>> eval.c
	   if (which < 0 || which >= LENGTH(ioffsets))
	       which = LENGTH(ioffsets) - 1;
	   pc = codebase + INTEGER(ioffsets)[which];
       }
       NEXT();
    }
    OP(RETURNJMP, 0): {
      value = BCNPOP();
      if (!rho->canReturn())
	  Rf_error(_("no function to return from, jumping to top level"));
      throw ReturnException(rho, value);
    }
    OP(STARTSUBSET_N, 2): DO_STARTDISPATCH_N("[");
    OP(STARTSUBASSIGN_N, 2): DO_START_ASSIGN_DISPATCH_N("[<-");
    OP(VECSUBSET2, 1): DO_VECSUBSET(rho, TRUE); NEXT();
    OP(MATSUBSET2, 1): DO_MATSUBSET(rho, TRUE); NEXT();
    OP(VECSUBASSIGN2, 1): DO_VECSUBASSIGN(rho, TRUE); NEXT();
    OP(MATSUBASSIGN2, 1): DO_MATSUBASSIGN(rho, TRUE); NEXT();
    OP(STARTSUBSET2_N, 2): DO_STARTDISPATCH_N("[[");
    OP(STARTSUBASSIGN2_N, 2): DO_START_ASSIGN_DISPATCH_N("[[<-");
    OP(SUBSET_N, 2): DO_SUBSET_N(rho, FALSE); NEXT();
    OP(SUBSET2_N, 2): DO_SUBSET_N(rho, TRUE); NEXT();
    OP(SUBASSIGN_N, 2): DO_SUBASSIGN_N(rho, FALSE); NEXT();
    OP(SUBASSIGN2_N, 2): DO_SUBASSIGN_N(rho, TRUE); NEXT();
    OP(LOG, 1): DO_LOG(); NEXT();
    OP(LOGBASE, 1): DO_LOGBASE(); NEXT();
    OP(MATH1, 2): DO_MATH1(); NEXT();
    OP(DOTCALL, 2): DO_DOTCALL(); NEXT();
    OP(COLON, 1): DO_COLON(); NEXT();
    OP(SEQALONG, 1): DO_SEQ_ALONG(); NEXT();
    OP(SEQLEN, 1): DO_SEQ_LEN(); NEXT();
    LASTOP;
  }

 done:
#ifdef BC_PROFILING
  current_opcode = old_current_opcode;
#endif
  return value;
}

#ifdef ENCODED_BCODE
vector<BCODE> ByteCode::encode(IntVector* bytes)
{
    vector<BCODE> encoded;

    IntVector& ipc = *bytes;
    size_t n = encoded->size();
    int version = ipc[0];
    // Check version:
    if (version < R_bcMinVersion || version > R_bcVersion) {
	encoded.resize(2);
	encoded[0].i = version;
	encoded[1].v = s_op_address[BCMISMATCH_OP];
	return encoded;
    }
    encoded.resize(n);
    // Insert the current version number:
    encoded[0].i = R_bcVersion;
    // First do a straight copy:
    for (size_t i = 1; i < n; ++i)
	encoded[i].i = ipc[i];
    // Now replace the opcodes with the appropriate code addresses:
    {
	size_t i = 1;
	while (i < n) {
	    BCODE& cell = encoded[i];
	    int op = cell.i;
	    if (op < 0 || op >= OPCOUNT)
		error("unknown instruction code");
	    cell.v = s_op_address[op];
	    i += s_op_arity[op] + 1;
	}
    }
    return encoded;
}

static int findOp(void *addr)
{
    int i;

    for (i = 0; i < OPCOUNT; i++)
	if (opinfo[i].addr == addr)
	    return i;
    error(_("cannot find index for threaded code address"));
    return 0; /* not reached */
}

IntVector* ByteCode::decode()
{
    GCStackRoot<> ensure_this_is_reachable(this);

    int  i, j;
    int n = m_code.size();
    const vector<BCODE>& pc = m_code;

    IntVector* bytes = IntVector::create(n);
    IntVector& ipc = *bytes;

    /* copy the version number */
    ipc[0] = pc[0].i;

    for (i = 1; i < n;) {
	int op = findOp(pc[i].v);
	int argc = opinfo[op].argc;
	ipc[i] = op;
	i++;
	for (j = 0; j < argc; j++, i++)
	    ipc[i] = pc[i].i;
    }

    return bytes;
}
#else
IntVector* ByteCode::decode() const {
    return IntVector::create(m_code.begin(), m_code.end());
}

vector<BCODE> ByteCode::encode(IntVector* bytes) {
    return vector<BCODE>(bytes->begin(), bytes->end());
}
#endif

SEXP attribute_hidden do_mkcode(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP bytes, consts;

    op->checkNumArgs(num_args, call);
    bytes = args[0];
    consts = args[1];
    GCStackRoot<IntVector> enc(SEXP_downcast<IntVector*>(bytes));
    GCStackRoot<ListVector> pl(SEXP_downcast<ListVector*>(consts));
    return new ByteCode(enc, pl);
}

SEXP attribute_hidden do_bcclose(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP forms, body, env;

    op->checkNumArgs(num_args, call);
    forms = args[0];
    body = args[1];
    env = args[2];

    if (! isByteCode(body))
	Rf_errorcall(call, _("invalid body"));

    if (Rf_isNull(env)) {
	Rf_error(_("use of NULL environment is defunct"));
	env = R_BaseEnv;
    } else
    if (!Rf_isEnvironment(env))
	Rf_errorcall(call, _("invalid environment"));

    return Rf_mkCLOSXP(forms, body, env);
}

SEXP attribute_hidden do_is_builtin_internal(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP symbol, i;

    op->checkNumArgs(num_args, call);
    symbol = args[0];

    if (!Rf_isSymbol(symbol))
	Rf_errorcall(call, _("invalid symbol"));

    if ((i = INTERNAL(symbol)) != R_NilValue && TYPEOF(i) == BUILTINSXP)
	return R_TrueValue;
    else
	return R_FalseValue;
}

static SEXP disassemble(SEXP bc)
{
  SEXP ans, dconsts;
  int i;
  ByteCode* bcode = SEXP_downcast<ByteCode*>(bc);
  SEXP code = bcode->decode();
  SEXP consts = bcode->constants();
  SEXP expr = nullptr;  // Set to BCODE_EXPR(bc) in CR
  int nc = LENGTH(consts);

  PROTECT(ans = Rf_allocVector(VECSXP, expr != nullptr ? 4 : 3));
  SET_VECTOR_ELT(ans, 0, Rf_install(".Code"));
  SET_VECTOR_ELT(ans, 1, code);
  SET_VECTOR_ELT(ans, 2, Rf_allocVector(VECSXP, nc));
  if (expr != R_NilValue)
      SET_VECTOR_ELT(ans, 3, Rf_duplicate(expr));

  dconsts = VECTOR_ELT(ans, 2);
  for (i = 0; i < nc; i++) {
    SEXP c = VECTOR_ELT(consts, i);
    if (isByteCode(c))
      SET_VECTOR_ELT(dconsts, i, disassemble(c));
    else
      SET_VECTOR_ELT(dconsts, i, Rf_duplicate(c));
  }

  UNPROTECT(1);
  return ans;
}

SEXP attribute_hidden do_disassemble(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
  SEXP code;

  op->checkNumArgs(num_args, call);
  code = args[0];
  if (! isByteCode(code))
    Rf_errorcall(call, _("argument is not a byte code object"));
  return disassemble(code);
}

SEXP attribute_hidden do_bcversion(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
  SEXP ans = Rf_allocVector(INTSXP, 1);
  INTEGER(ans)[0] = R_bcVersion;
  return ans;
}

SEXP attribute_hidden do_loadfile(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP file, s;
    FILE *fp;

    op->checkNumArgs(num_args, call);

    PROTECT(file = Rf_coerceVector(args[0], STRSXP));

    if (! Rf_isValidStringF(file))
	Rf_errorcall(call, _("bad file name"));

    fp = RC_fopen(STRING_ELT(file, 0), "rb", TRUE);
    if (!fp)
	Rf_errorcall(call, _("unable to open 'file'"));
    s = R_LoadFromFile(fp, 0);
    fclose(fp);

    UNPROTECT(1);
    return s;
}

SEXP attribute_hidden do_savefile(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    FILE *fp;

    op->checkNumArgs(num_args, call);

    if (!Rf_isValidStringF(args[1]))
	Rf_errorcall(call, _("'file' must be non-empty string"));
    if (TYPEOF(args[2]) != LGLSXP)
	Rf_errorcall(call, _("'ascii' must be logical"));

    fp = RC_fopen(STRING_ELT(args[1], 0), "wb", TRUE);
    if (!fp)
	Rf_errorcall(call, _("unable to open 'file'"));

    R_SaveToFileV(args[0], fp, INTEGER(args[2])[0], 0);

    fclose(fp);
    return R_NilValue;
}

#ifdef UNUSED
#define R_COMPILED_EXTENSION ".Rc"

/* neither of these functions call R_ExpandFileName -- the caller
   should do that if it wants to */
char *R_CompiledFileName(char *fname, char *buf, std::size_t bsize)
{
    char *basename, *ext;

    /* find the base name and the extension */
    basename = Rf_strrchr(fname, FILESEP[0]);
    if (basename == NULL) basename = fname;
    ext = Rf_strrchr(basename, '.');

    if (ext != NULL && strcmp(ext, R_COMPILED_EXTENSION) == 0) {
	/* the supplied file name has the compiled file extension, so
	   just copy it to the buffer and return the buffer pointer */
	if (snprintf(buf, bsize, "%s", fname) < 0)
	    Rf_error("R_CompiledFileName: buffer too small");
	return buf;
    }
    else if (ext == NULL) {
	/* if the requested file has no extention, make a name that
	   has the extenrion added on to the expanded name */
	if (snprintf(buf, bsize, "%s%s", fname, R_COMPILED_EXTENSION) < 0)
	    Rf_error("R_CompiledFileName: buffer too small");
	return buf;
    }
    else {
	/* the supplied file already has an extension, so there is no
	   corresponding compiled file name */
	return NULL;
    }
}

FILE *R_OpenCompiledFile(char *fname, char *buf, std::size_t bsize)
{
    char *cname = R_CompiledFileName(fname, buf, bsize);

    if (cname != NULL && R_FileExists(cname) &&
	(strcmp(fname, cname) == 0 ||
	 ! R_FileExists(fname) ||
	 R_FileMtime(cname) > R_FileMtime(fname)))
	/* the compiled file cname exists, and either fname does not
	   exist, or it is the same as cname, or both exist and cname
	   is newer */
	return R_fopen(buf, "rb");
    else return NULL;
}
#endif

SEXP attribute_hidden do_growconst(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP constBuf, ans;
    int i, n;

    op->checkNumArgs(num_args, call);
    constBuf = args[0];
    if (TYPEOF(constBuf) != VECSXP)
	Rf_error(_("constant buffer must be a generic vector"));

    n = LENGTH(constBuf);
    ans = Rf_allocVector(VECSXP, 2 * n);
    for (i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

SEXP attribute_hidden do_putconst(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP constBuf, x;
    int i, constCount;

    op->checkNumArgs(num_args, call);

    constBuf = args[0];
    if (TYPEOF(constBuf) != VECSXP)
	Rf_error(_("constant_buffer must be a generic vector"));

    constCount = Rf_asInteger(args[1]);
    if (constCount < 0 || constCount >= LENGTH(constBuf))
	Rf_error("bad constCount value");

    x = args[2];

    /* check for a match and return index if one is found */
    for (i = 0; i < constCount; i++) {
	SEXP y = VECTOR_ELT(constBuf, i);
	if (x == y || R_compute_identical(x, y, 0))
	    return Rf_ScalarInteger(i);
    }

    /* otherwise insert the constant and return index */
    SET_VECTOR_ELT(constBuf, constCount, x);
    return Rf_ScalarInteger(constCount);
}

SEXP attribute_hidden do_getconst(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP constBuf, ans;
    int i, n;

    op->checkNumArgs(num_args, call);
    constBuf = args[0];
    n = Rf_asInteger(args[1]);

    if (TYPEOF(constBuf) != VECSXP)
	Rf_error(_("constant buffer must be a generic vector"));
    if (n < 0 || n > LENGTH(constBuf))
	Rf_error(_("bad constant count"));

    ans = Rf_allocVector(VECSXP, n);
    for (i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

#ifdef BC_PROFILING
SEXP do_bcprofcounts(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP val;
    int i;

    val = Rf_allocVector(INTSXP, OPCOUNT);
    for (i = 0; i < OPCOUNT; i++)
	INTEGER(val)[i] = opcode_counts[i];
    return val;
}

static void dobcprof(int sig)
{
    if (current_opcode >= 0 && current_opcode < OPCOUNT)
	opcode_counts[current_opcode]++;
    signal(SIGPROF, dobcprof);
}

SEXP do_bcprofstart(SEXP call, SEXP op, SEXP args, SEXP env)
{
    struct itimerval itv;
    int interval;
    double dinterval = 0.02;
    int i;

    if (Evaluator::profiling())
	Rf_error(_("profile timer in use"));
    if (bc_profiling)
	Rf_error(_("already byte code profiling"));

    /* according to man setitimer, it waits until the next clock
       tick, usually 10ms, so avoid too small intervals here */
    interval = 1e6 * dinterval + 0.5;

    /* initialize the profile data */
    current_opcode = NO_CURRENT_OPCODE;
    for (i = 0; i < OPCOUNT; i++)
	opcode_counts[i] = 0;

    signal(SIGPROF, dobcprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	Rf_error(_("setting profile timer failed"));

    bc_profiling = TRUE;

    return R_NilValue;
}

static void dobcprof_null(int sig)
{
    signal(SIGPROF, dobcprof_null);
}

SEXP do_bcprofstop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    struct itimerval itv;

    if (! bc_profiling)
	Rf_error(_("not byte code profiling"));

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, dobcprof_null);

    bc_profiling = FALSE;

    return R_NilValue;
}
#else
SEXP NORET do_bcprofcounts(SEXP call, SEXP op, SEXP args, SEXP env) {
    error(_("byte code profiling is not supported in this build"));
}
SEXP NORET do_bcprofstart(SEXP call, SEXP op, SEXP args, SEXP env) {
    error(_("byte code profiling is not supported in this build"));
}
SEXP NORET do_bcprofstop(SEXP call, SEXP op, SEXP args, SEXP env) {
    error(_("byte code profiling is not supported in this build"));
}
#endif

/* end of byte code section */

SEXP attribute_hidden do_setnumthreads(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    int old = R_num_math_threads, newi;
    op->checkNumArgs(num_args, call);
    newi = Rf_asInteger(args[0]);
    if (newi >= 0 && newi <= R_max_num_math_threads)
	R_num_math_threads = newi;
    return Rf_ScalarInteger(old);
}

SEXP attribute_hidden do_setmaxnumthreads(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    int old = R_max_num_math_threads, newi;
    op->checkNumArgs(num_args, call);
    newi = Rf_asInteger(args[0]);
    if (newi >= 0) {
	R_max_num_math_threads = newi;
	if (R_num_math_threads > R_max_num_math_threads)
	    R_num_math_threads = R_max_num_math_threads;
    }
    return Rf_ScalarInteger(old);
}

#include "CXXR/ArgMatcher.hpp"

/* Create a promise to evaluate each argument.	Although this is most */
/* naturally attacked with a recursive algorithm, we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

SEXP attribute_hidden Rf_promiseArgs(SEXP el, SEXP rho)
{
    ArgList arglist(SEXP_downcast<PairList*>(el), ArgList::RAW);
    arglist.wrapInPromises(SEXP_downcast<Environment*>(rho));
    return const_cast<PairList*>(arglist.list());
}

SEXP attribute_hidden do_returnValue(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP val;
    checkArity(op, args);
    if (R_ExitContext && (val = R_ExitContext->returnValue)){
        MARK_NOT_MUTABLE(val);
        return val;
    }
    return CAR(args); /* default */
}
