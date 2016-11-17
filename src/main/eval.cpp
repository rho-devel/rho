/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2012	The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
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

#include "rho/ArgList.hpp"
#include "rho/BailoutContext.hpp"
#include "rho/Closure.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/DottedArgs.hpp"
#include "rho/ExpressionVector.hpp"
#include "rho/GCStackFrameBoundary.hpp"
#include "rho/Frame.hpp"
#include "rho/LoopBailout.hpp"
#include "rho/LoopException.hpp"
#include "rho/Promise.hpp"
#include "rho/ProvenanceTracker.hpp"
#include "rho/ReturnBailout.hpp"
#include "rho/ReturnException.hpp"
#include "rho/S3Launcher.hpp"

using namespace std;
using namespace rho;

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
extern void get_current_mem(unsigned long *,unsigned long *,unsigned long *); /* in memory.c */
extern unsigned long get_duplicate_counter(void);  /* in duplicate.c */
extern void reset_duplicate_counter(void);         /* in duplicate.c */
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
	if (fnum >= RHOCONSTRUCT(int, R_Srcfile_bufcount)) { /* too many files */
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
    unsigned long bigv, smallv, nodes;
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
			 ":%ld:%ld:%ld:%ld:", smallv, bigv,
			 nodes, get_duplicate_counter());
	    reset_duplicate_counter();
    }

// R_gc_running() not (yet) implemented in rho:
#if 0
    if (R_GC_Profiling && R_gc_running())
	strcat(buf, "\"<GC>\" ");
#endif

    if (R_Line_Profiling)
	lineprof(buf, R_Srcref);

// rho FIXME: not yet adapted for rho:
#if 0
    for (Evaluator::Context* cptr = Evaluator::Context::innermost();
	 cptr; cptr = cptr->nextOut()) {
	if (TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < PROFLINEMAX) {
		strcat(buf, "\"");

		char itembuf[PROFITEMMAX];

		if (TYPEOF(fun) == SYMSXP) {
		    snprintf(itembuf, PROFITEMMAX-1, "%s", CHAR(PRINTNAME(fun)));

		} else if ((CAR(fun) == R_DoubleColonSymbol ||
			    CAR(fun) == R_TripleColonSymbol ||
			    CAR(fun) == R_DollarSymbol) &&
			   TYPEOF(CADR(fun)) == SYMSXP &&
			   TYPEOF(CADDR(fun)) == SYMSXP) {
		    /* Function accessed via ::, :::, or $. Both args must be
		       symbols. It is possible to use strings with these
		       functions, as in "base"::"list", but that's a very rare
		       case so we won't bother handling it. */
		    snprintf(itembuf, PROFITEMMAX-1, "%s%s%s",
			     CHAR(PRINTNAME(CADR(fun))),
			     CHAR(PRINTNAME(CAR(fun))),
			     CHAR(PRINTNAME(CADDR(fun))));

		} else if (CAR(fun) == R_Bracket2Symbol &&
			   TYPEOF(CADR(fun)) == SYMSXP &&
			   ((TYPEOF(CADDR(fun)) == SYMSXP ||
			     TYPEOF(CADDR(fun)) == STRSXP ||
			     TYPEOF(CADDR(fun)) == INTSXP ||
			     TYPEOF(CADDR(fun)) == REALSXP) &&
			    length(CADDR(fun)) > 0)) {
		    /* Function accessed via [[. The first arg must be a symbol
		       and the second can be a symbol, string, integer, or
		       real. */
		    SEXP arg1 = CADR(fun);
		    SEXP arg2 = CADDR(fun);
		    char arg2buf[PROFITEMMAX];

		    if (TYPEOF(arg2) == SYMSXP) {
			snprintf(arg2buf, PROFITEMMAX-1, "%s", CHAR(PRINTNAME(arg2)));

		    } else if (TYPEOF(arg2) == STRSXP) {
			snprintf(arg2buf, PROFITEMMAX-1, "\"%s\"", CHAR(STRING_ELT(arg2, 0)));

		    } else if (TYPEOF(arg2) == INTSXP) {
			snprintf(arg2buf, PROFITEMMAX-1, "%d", INTEGER(arg2)[0]);

		    } else if (TYPEOF(arg2) == REALSXP) {
			snprintf(arg2buf, PROFITEMMAX-1, "%.0f", REAL(arg2)[0]);

		    } else {
			/* Shouldn't get here, but just in case. */
			arg2buf[0] = '\0';
		    }

		    snprintf(itembuf, PROFITEMMAX-1, "%s[[%s]]",
			     CHAR(PRINTNAME(arg1)),
			     arg2buf);

		} else {
		    sprintf(itembuf, "<Anonymous>");
		}

		strcat(buf, itembuf);
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
    Promise* prom = SEXP_downcast<Promise*>(e);
    return prom->force();
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
		Rprintf(_("%s at %s#%d: "), prefix, CHAR(STRING_ELT(filename, 0)), 
			                    Rf_asInteger(srcref));
		return;
	    }
	}
    }
    /* default: */
    Rprintf("%s: ", prefix);
}

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

/* There's another copy of this in main.c */
static void PrintCall(SEXP call, SEXP rho)
{
    int old_bl = R_BrowseLines,
        blines = Rf_asInteger(Rf_GetOption1(Rf_install("deparse.max.lines")));
    if(blines != NA_INTEGER && blines > 0)
	R_BrowseLines = blines;
    Rf_PrintValueRec(call, rho);
    R_BrowseLines = old_bl;
}

void Closure::DebugScope::startDebugging() const
{
    const ClosureContext* ctxt = ClosureContext::innermost();
    const Expression* call = ctxt->call();
    Environment* working_env = ctxt->workingEnvironment();
    working_env->setSingleStepping(true);
    Rprintf("debugging in: ");
    PrintCall(const_cast<Expression*>(call), working_env);

    Rprintf("debug: ");
    Rf_PrintValue(m_closure->m_body);
    do_browser(nullptr, nullptr, nullptr, working_env);
}

void Closure::DebugScope::endDebugging() const
{
    const ClosureContext* ctxt = ClosureContext::innermost();
    try {
	Rprintf("exiting from: ");
	PrintCall(const_cast<Expression*>(ctxt->call()), nullptr);
    }
    // Don't allow exceptions to escape destructor:
    catch (...) {}
}


SEXP R_forceAndCall(SEXP e, int n, SEXP rho)
{
    Expression* call = SEXP_downcast<Expression*>(e);
    Environment* env = SEXP_downcast<Environment*>(rho);

    SEXP fun, result;
    if (TYPEOF(CAR(e)) == SYMSXP)
       /* This will throw an error if the function is not found */
       PROTECT(fun = Rf_findFun(CAR(e), rho));
    else
       PROTECT(fun = Rf_eval(CAR(e), rho));

    ArgList arglist(call->tail(), ArgList::RAW);

    BuiltInFunction* builtin = dynamic_cast<BuiltInFunction*>(fun);
    if (builtin) {
	result = call->applyBuiltIn(builtin, env, &arglist);
    } else if (TYPEOF(fun) == CLOSXP) {
	Closure* closure = SEXP_downcast<Closure*>(fun);

	arglist.wrapInPromises(env);

	// Force the promises.
	int i = 0;
	auto args = arglist.getArgs();
	for (auto cell = args.begin(); i < n && cell != args.end(); ++cell, ++i)
	{
	    SEXP p = cell->car();
	    if (TYPEOF(p) == PROMSXP)
		Rf_eval(p, rho);
	    else if (p == R_MissingArg)
		Rf_errorcall(e, _("argument %d is empty"), i + 1);
	    else
		Rf_error("something weird happened");
	}
	result = call->invokeClosure(closure, env, &arglist);
    }
    else {
       Rf_error(_("attempt to apply non-function"));
    }

    UNPROTECT(1);
    return result;
}

SEXP attribute_hidden do_forceAndCall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n = Rf_asInteger(Rf_eval(CADR(call), rho));
    SEXP e = CDDR(call);

    /* this would not be needed if CDDR(call) was a LANGSXP */
    Expression* expr = new Expression(CAR(e), SEXP_downcast<PairList*>(CDR(e)));
    SEXP val = R_forceAndCall(expr, n, rho);
    UNPROTECT(1);
    return val;
}

/* **** FIXME: Temporary code to execute S4 methods in a way that
   **** preserves lexical scope. */

/* called from methods_list_dispatch.c */
SEXP R_execMethod(SEXP op, SEXP rho)
{
    Closure* func = SEXP_downcast<Closure*>(op);
    Environment* callenv = SEXP_downcast<Environment*>(rho);
    const Frame* fromf = callenv->frame();

    /* Find the calling context. */
    ClosureContext* cptr = ClosureContext::innermost();

    /* The calling environment should either be the environment of the
       generic, rho, or the environment of the caller of the generic,
       the current sysparent. */
    Environment* callerenv = cptr->callEnvironment(); /* or rho? */

    // create a new environment frame enclosed by the lexical
    // environment of the method
    const ArgList& args = cptr->promiseArgs();
    GCStackRoot<Environment> newrho(func->createExecutionEnv(args));
    Frame* newframe = newrho->frame();

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
	    newframe->importBinding(fromf->binding(*symp));
	}
    }

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    {
	static const Symbol* syms[]
	    = {DotGenericSymbol, DotMethodsSymbol, nullptr};
	for (const Symbol** symp = syms; *symp; ++symp) {
	    const Frame::Binding* frombdg = callenv->findBinding(*symp);
	    newframe->importBinding(frombdg);
	}
    }

    // Set up context and perform evaluation.  Note that ans needs to
    // be protected in case the destructor of ClosureContext executes
    // an on.exit function.
    GCStackRoot<> ans;
    {
	ClosureContext ctxt(cptr->call(), callerenv, func, newrho);
	ans = func->execute(newrho);
    }
    return ans;
}

static SEXP EnsureLocal(SEXP symbol, SEXP rho)
{
    GCStackRoot<> vl;

    if ((vl = Rf_findVarInFrame3(rho, symbol, TRUE)) != R_UnboundValue) {
	vl = Rf_eval(symbol, rho);	/* for promises */
	if(NAMED(vl) == 2) {
	    vl = Rf_duplicate(vl);
	    Rf_defineVar(symbol, vl, rho);
	    SET_NAMED(vl, 1);
	}
	return vl;
    }

    vl = Rf_eval(symbol, ENCLOS(rho));
    if (vl == R_UnboundValue)
	Rf_error(_("object '%s' not found"), CHAR(PRINTNAME(symbol)));

    vl = Rf_duplicate(vl);
    Rf_defineVar(symbol, vl, rho);
    SET_NAMED(vl, 1);
    return vl;
}


/* Note: If val is a language object it must be protected */
/* to prevent evaluation.  As an example consider */
/* e <- quote(f(x=1,y=2); names(e) <- c("","a","b") */

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
    int cond = NA_LOGICAL;

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
	    cond = LOGICAL(s)[0];
	    break;
	case INTSXP:
	    cond = INTEGER(s)[0]; /* relies on NA_INTEGER == NA_LOGICAL */
	    break;
	default:
	    cond = Rf_asLogical(s);
	}
    }

    if (cond == NA_LOGICAL) {
	char *msg = length(s) ? (Rf_isLogical(s) ?
				 _("missing value where TRUE/FALSE needed") :
				 _("argument is not interpretable as logical")) :
	    _("argument is of length zero");
	Rf_errorcall(call, msg);
    }
    return static_cast<Rboolean>(cond);
}


namespace {
    inline int BodyHasBraces(SEXP body)
    {
	return (Rf_isLanguage(body) && CAR(body) == R_BraceSymbol) ? 1 : 0;
    }

    inline void DO_LOOP_RDEBUG(SEXP call, SEXP op, SEXP args, SEXP rho, int bgn)
    {
	if (bgn && ENV_DEBUG(rho)) {
	    Rf_SrcrefPrompt("debug", R_Srcref);
	    Rf_PrintValue(CAR(args));
	    do_browser(nullptr, nullptr, nullptr, rho);
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
    if( ENV_DEBUG(rho) && !BodyHasBraces(Stmt)) {
	Rf_SrcrefPrompt("debug", R_Srcref);
	Rf_PrintValue(Stmt);
	do_browser(nullptr, nullptr, nullptr, rho);
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

SEXP attribute_hidden do_for_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> argsrt(args), rhort(rho);

    /* Need to declare volatile variables whose values are relied on
       after for_next or for_break longjmps and might change between
       the setjmp and longjmp calls. Theoretically this does not
       include n and bgn, but gcc -O2 -Wclobbered warns about these so
       to be safe we declare them volatile as well. */
    volatile int i = 0, n, bgn;
    Rboolean dbg;
    SEXPTYPE val_type;
    GCStackRoot<> ans, v, val;
    SEXP sym, body;

    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !Rf_isSymbol(sym) ) Rf_errorcall(call, _("non-symbol loop variable"));

    /* rho FIXME
    if (R_jit_enabled > 2 && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
    */

    val = Rf_eval(val, rho);
    Rf_defineVar(sym, R_NilValue, rho);

    /* deal with the case where we are iterating over a factor
       we need to coerce to character - then iterate */

    if( Rf_inherits(val, "factor") ) {
	ans = Rf_asCharacterFactor(val);
	val = ans;
    }

    if (Rf_isList(val) || Rf_isNull(val)) {
	n = length(val);
    } else {
	n = LENGTH(val);
    }

    val_type = TYPEOF(val);

    dbg = ENV_DEBUG(rho);
    bgn = BodyHasBraces(body);

    /* bump up NAMED count of sequence to avoid modification by loop code */
    if (NAMED(val) < 2) SET_NAMED(val, NAMED(val) + 1);

    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);
    for (i = 0; i < n; i++) {
	Evaluator::maybeCheckForUserInterrupts();
	DO_LOOP_RDEBUG(call, op, args, rho, bgn);

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
	}
    }
    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}

SEXP attribute_hidden do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    
    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() { return do_for_impl(call, op, args, rho); });
}

static SEXP do_while_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    /* rho FIXME
    if (R_jit_enabled > 2 && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
    */

    dbg = ENV_DEBUG(rho);
    body = CADR(args);
    bgn = BodyHasBraces(body);

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
	}
    }
    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}

SEXP attribute_hidden do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() {  return do_while_impl(call, op, args, rho); });
}

static SEXP do_repeat_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    /* rho FIXME
    if (R_jit_enabled > 2 && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
    */

    dbg = ENV_DEBUG(rho);
    body = CAR(args);
    bgn = BodyHasBraces(body);

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
	}
    }

    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}

SEXP attribute_hidden do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() { return do_repeat_impl(call, op, args, rho); });
}

SEXP attribute_hidden do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    Environment* env = SEXP_downcast<Environment*>(rho);
    if (!env->loopActive())
	Rf_error(_("no loop to break from"));
    LoopBailout* lbo = new LoopBailout(env, PRIMVAL(op) == 1);
    return propagateBailout(lbo);
}

RObject* attribute_hidden do_paren(Expression* call,
				   const BuiltInFunction* op,
				   RObject* x_)
{
    return x_;
}

SEXP attribute_hidden do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;
    if (args != R_NilValue) {
	GCStackRoot<> srcrefs(getBlockSrcrefs(call));
	int i = 1;
	while (args != R_NilValue) {
	    R_Srcref = getSrcref(srcrefs, i++);
	    if (ENV_DEBUG(rho)) {
		Rf_SrcrefPrompt("debug", R_Srcref);
		Rf_PrintValue(CAR(args));
		do_browser(nullptr, nullptr, nullptr, rho);
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

// rho here (necessarily) uses a proper list, with x as the CAR of
// the last element.

/*
  For complex superassignment  x[y==z]<<-w
  we want x required to be nonlocal, y,z, and w permitted to be local or
  nonlocal.
*/

static PairList* evalseq(SEXP expr, SEXP rho, int forcelocal,
			 Frame::Binding* tmploc)
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
	return PairList::cons(nval, PairList::cons(expr));
    }
    else if (Rf_isLanguage(expr)) {
	Expression* exprn = static_cast<Expression*>(expr);
	GCStackRoot<PairList> val(evalseq(exprn->tail()->car(), rho, forcelocal, tmploc));
	tmploc->assign(val->car());
	GCStackRoot<PairList> nexprargs(
	    PairList::cons(const_cast<Symbol*>(tmploc->symbol()),
			   exprn->tail()->tail()));
	GCStackRoot<Expression> nexpr(new Expression(exprn->car(), nexprargs));
	GCStackRoot<> nval(Rf_eval(nexpr, rho));
	return PairList::cons(nval, val);
    }
    else Rf_error(_("target of assignment expands to non-language object"));
    return R_NilValue;	/*NOTREACHED*/
}

/* Main entry point for complex assignments */
/* We have checked to see that CAR(args) is a LANGSXP */

static const char * const asym[] = {":=", "<-", "<<-", "="};

/* This function stores the current assignment target in the saved
   binding location. It duplicates if necessary to make sure
   replacement functions are always called with a target with NAMED ==
   1. The SET_CAR is intended to protect against possible GC in
   R_SetVarLocValue; this might occur it the binding is an active
   binding. */
static void SET_TEMPVARLOC_FROM_CAR(Frame::Binding* loc, PairList* lhs) {
    SEXP v = lhs->car();
    if (NAMED(v) == 2) {
	v = Rf_duplicate(v);
	SET_NAMED(v, 1);
	lhs->setCar(v);
    }
    loc->assign(v);
}

/* This macro makes sure the RHS NAMED value is 0 or 2. This is
   necessary to make sure the RHS value returned by the assignment
   expression is correct when the RHS value is part of the LHS
   object. */
#define FIXUP_RHS_NAMED(r) do { \
	SEXP __rhs__ = (r); \
	if (NAMED(__rhs__)) \
	    SET_NAMED(__rhs__, 2); \
    } while (0)

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
}

// applydefine() handles the case where the left-hand side of an
// assignment is not a symbol: specifically the cases where it is an
// Expression (LANGSXP), or null (which gives an error).
//
// Section 13.5 of the 'yellow book' (Chambers' Software for Data
// Analysis) gives useful background.

SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> expr(CAR(args));
    SEXP functor = CAR(expr);

    /*  It's important that the rhs get evaluated first because
	assignment is right associative i.e.  a <- b <- c is parsed as
	a <- (b <- c).  */

    GCStackRoot<> rhs, saverhs;
    saverhs = rhs = Rf_eval(CADR(args), rho);

    if (PRIMVAL(op) == 2)
	Environment::monitorLeaks(rhs);

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
	Rf_errorcall(call, _("cannot do complex assignments in base environment"));
    Rf_defineVar(R_TmpvalSymbol, R_NilValue, rho);
    Frame::Binding* tmploc
	= SEXP_downcast<Environment*>(rho)->frame()->binding(TmpvalSymbol);
    /* Now use try-catch to remove it when we are done, even in the
     * case of an error.  This all helps Rf_error() provide a better call.
     */
    try {
	/*  Do a partial evaluation down through the LHS. */
	GCStackRoot<PairList> lhs(
	    evalseq(CADR(expr), rho,
		    PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc));

	GCStackRoot<> rhsprom(Promise::createEvaluatedPromise(CADR(args), rhs));

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
	    // Try doing without this in rho:
	    // SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	    lhs = lhs->tail();
	    expr = firstarg;
	    functor = CAR(expr);
	    firstarg = CADR(expr);
	}
	GCStackRoot<> afun;
	if (TYPEOF(functor) == SYMSXP)
	    afun = func2ReplacementFunc(static_cast<Symbol*>(functor));
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
			afun = func2ReplacementFunc(fsym);
			afun = Rf_lang3(funchead, CADR(functor), afun);
		    }
		}
	    }
	    if (!afun)
		Rf_error(_("invalid function in complex assignment"));
	}
	SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
	static std::vector<Symbol*> assignsyms = obtainAssignSyms();
	// Second arg in the foll. changed in rho at r253 (2008-03-18):
	expr = assignCall(assignsyms[PRIMVAL(op)], CADR(lhs),
			  afun, R_TmpvalSymbol, CDDR(expr), rhsprom);
	expr = Rf_eval(expr, rho);
    }
    catch (...) {
	Rf_unbindVar(R_TmpvalSymbol, rho);
	throw;
    }

    Rf_unbindVar(R_TmpvalSymbol, rho);
#ifdef CONSERVATIVE_COPYING /* not default */
    return Rf_duplicate(saverhs);
#else
    /* we do not duplicate the value, so to be conservative mark the
       value as NAMED = 2 */
    SET_NAMED(saverhs, 2);
    return saverhs;
#endif
}

/*  Assignment in its various forms  */

SEXP attribute_hidden do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s;
    if (length(args) != 2)
	WrongArgCount(asym[PRIMVAL(op)]);
    if (Rf_isString(CAR(args))) {
	/* fix up a duplicate or args and recursively call do_set */
	SEXP val;
	PROTECT(args = Rf_duplicate(args));
	SETCAR(args, Rf_installTrChar(STRING_ELT(CAR(args), 0)));
	val = do_set(call, op, args, rho);
	UNPROTECT(1);
	return val;
    }

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

    default:
	UNIMPLEMENTED("do_set");

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
    ArgList arglist(SEXP_downcast<PairList*>(el), ArgList::RAW);
    arglist.evaluate(SEXP_downcast<Environment*>(rho),
		     MissingArgHandling::Keep);
    return const_cast<PairList*>(arglist.list());
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
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP) {
	PROTECT(expr);
	{
	    Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* call_env = SEXP_downcast<Environment*>(rho);
	    FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
	    Environment* working_env = SEXP_downcast<Environment*>(env);
	    ClosureContext cntxt(callx, call_env, func, working_env);
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
	    ClosureContext cntxt(callx, call_env, func, working_env);
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
    ClosureContext* args_cptr = cptr;

    /* get the env recall was called from */
    s = ClosureContext::innermost()->callEnvironment();
    while (cptr && cptr->workingEnvironment() != s) {
	cptr = ClosureContext::innermost(cptr->nextOut());
    }
    if (cptr == nullptr)
	Rf_error(_("'Recall' called from outside a closure"));
    const ArgList& arglist = args_cptr->promiseArgs();

    /* If the function has been recorded in the context, use it
       otherwise search for it by name or evaluate the expression
       originally used to get it.
    */
    if (cptr->function() != R_NilValue)
	PROTECT(s = RHO_C_CAST(FunctionBase*, cptr->function()));
    else if( TYPEOF(CAR(RHO_C_CAST(Expression*, cptr->call()))) == SYMSXP)
	PROTECT(s = Rf_findFun(CAR(RHO_C_CAST(Expression*, cptr->call())), cptr->callEnvironment()));
    else
	PROTECT(s = Rf_eval(CAR(RHO_C_CAST(Expression*, cptr->call())), cptr->callEnvironment()));
    if (TYPEOF(s) != CLOSXP)
	Rf_error(_("'Recall' called from outside a closure"));
    Closure* closure = SEXP_downcast<Closure*>(s);
    ans = cptr->call()->invokeClosure(closure, cptr->callEnvironment(),
                                      const_cast<ArgList*>(&arglist));
    UNPROTECT(1);
    return ans;
}

static bool isDefaultMethod(const Expression* call) {
    RObject* callcar = call->car();
    if (callcar->sexptype() != SYMSXP) {
	return false;
    }
    const String* symbol_name = static_cast<Symbol*>(callcar)->name();

    static const size_t suffix_length = strlen(".default");
    if (symbol_name->size() < suffix_length)
	return false;
    return strcmp(symbol_name->c_str() + symbol_name->size() - suffix_length,
		  ".default") == 0;
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
int Rf_DispatchOrEval(SEXP call, SEXP op, SEXP args,
		      SEXP rho, SEXP *ans, MissingArgHandling dropmissing,
		      int argsevald)
{
    Expression* callx = SEXP_downcast<Expression*>(call);
    BuiltInFunction* func = SEXP_downcast<BuiltInFunction*>(op);
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
	    arglist.wrapInPromises(callenv, callx);

	    /* This means S4 dispatch */
	    std::pair<bool, SEXP> pr
		= R_possible_dispatch(callx, func, arglist, callenv);
	    if (pr.first) {
		*ans = pr.second;
		return 1;
	    }
	}
	if (!isDefaultMethod(callx)) {
	    if (arglist.status() != ArgList::PROMISED)
		arglist.wrapInPromises(callenv, callx);
	    /* The context set up here is needed because of the way
	       Rf_usemethod() is written.  Rf_DispatchGroup() repeats some
	       internal Rf_usemethod() code and avoids the need for a
	       context; perhaps the Rf_usemethod() code should be
	       refactored so the contexts around the Rf_usemethod() calls
	       in this file can be removed.

	       Using callenv for current and calling environment can be
	       confusing for things like sys.parent() calls captured
	       in promises (Gabor G had an example of this).  Also,
	       since the context is established without a SETJMP using
	       an R-accessible environment allows a segfault to be
	       triggered (by something very obscure, but still).
	       Hence here and in the other Rf_usemethod() uses below a
	       new environment working_env is created and used.  LT */
	    GCStackRoot<Frame> frame(Frame::closureWorkingFrame(arglist));
	    Environment* working_env = new Environment(callenv, frame);
	    ClosureContext cntxt(callx, callenv, func, working_env);
	    const char* generic = func->name();
	    int um = Rf_usemethod(generic, x, call,
				  working_env, callenv, R_BaseEnv, ans);
	    if (um)
		return 1;
	}
    }
    if (arglist.status() != ArgList::EVALUATED)
	arglist.evaluate(callenv, dropmissing);
    *ans = const_cast<PairList*>(arglist.list());
    return 0;
}

	
attribute_hidden
int Rf_DispatchGroup(const char* group, SEXP call, SEXP op, SEXP args, SEXP rho,
		     SEXP *ans)
{
    Expression* callx = SEXP_downcast<Expression*>(call);
    BuiltInFunction* opfun = SEXP_downcast<BuiltInFunction*>(op);
    PairList* callargs = SEXP_downcast<PairList*>(args);
    Environment* callenv = SEXP_downcast<Environment*>(rho);
    assert(group != nullptr);

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
		= R_possible_dispatch(callx, opfun, arglist, callenv);
	    if (pr.first) {
		*ans = pr.second;
		return 1;
	    }
	}
	/* else go on to look for S3 methods */
    }

    /* check whether we are processing the default method */
    if (isDefaultMethod(callx)) {
	return 0;
    }

    std::size_t nargs = (isOps ? numargs : 1);
    string generic(opfun->name());

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
	}
    }

    S3Launcher* m = (l ? l : r);  // m is the method that will be applied.

    /* we either have a group method or a class method */

    GCStackRoot<Frame> supp_frame(Frame::normalFrame(6));
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
    }

    {
	GCStackRoot<Expression>
	    newcall(new Expression(m->symbol(), callx->tail()));
	ArgList arglist(callargs, ArgList::EVALUATED);
	// Ensure positional matching for operators:
	if (isOps)
	    arglist.stripTags();
	Closure* func = SEXP_downcast<Closure*>(m->function());
	*ans = newcall->invokeClosure(func, callenv, &arglist, supp_frame);
    }
    return 1;
}

SEXP R_PromiseExpr(SEXP p)
{
    return PRCODE(p);
}

SEXP R_ClosureExpr(SEXP p)
{
    return BODY(p);
}

SEXP attribute_hidden do_loadfile(/*const*/ Expression* call, const BuiltInFunction* op, Environment* env, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP file, s;
    FILE *fp;

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

SEXP attribute_hidden do_savefile(/*const*/ Expression* call, const BuiltInFunction* op, Environment* env, RObject* const* args, int num_args, const PairList* tags)
{
    FILE *fp;

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

SEXP attribute_hidden do_setnumthreads(/*const*/ Expression* call, const BuiltInFunction* op, RObject* num_threads_)
{
    int old = R_num_math_threads, newi;
    newi = Rf_asInteger(num_threads_);
    if (newi >= 0 && newi <= R_max_num_math_threads)
	R_num_math_threads = newi;
    return Rf_ScalarInteger(old);
}

SEXP attribute_hidden do_setmaxnumthreads(/*const*/ Expression* call, const BuiltInFunction* op, RObject* num_threads_)
{
    int old = R_max_num_math_threads, newi;
    newi = Rf_asInteger(num_threads_);
    if (newi >= 0) {
	R_max_num_math_threads = newi;
	if (R_num_math_threads > R_max_num_math_threads)
	    R_num_math_threads = R_max_num_math_threads;
    }
    return Rf_ScalarInteger(old);
}
