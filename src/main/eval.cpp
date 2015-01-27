/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2012	The R Core Team.
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

/* BC_PROILFING needs to be defined here and in registration.c */
/*#define BC_PROFILING*/
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

SEXP attribute_hidden do_enablejit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_jit_enabled, newi;
    checkArity(op, args);
    newi = Rf_asInteger(CAR(args));
    if (newi > 0)
	loadCompilerNamespace();
    R_jit_enabled = newi;
    return Rf_ScalarInteger(old);
}

SEXP attribute_hidden do_compilepkgs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_compile_pkgs, newi;
    checkArity(op, args);
    newi = Rf_asLogical(CAR(args));
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

void Closure::DebugScope::startDebugging() const
{
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
	if(blines != NA_INTEGER && blines > 0)
	    R_BrowseLines = blines;
	Rf_PrintValueRec(const_cast<Expression*>(call), nullptr);
	R_BrowseLines = old_bl;
    }
    Rprintf("debug: ");
    Rf_PrintValue(m_closure->m_body);
    do_browser(const_cast<Expression*>(call), const_cast<Closure*>(m_closure),
	       const_cast<PairList*>(ctxt->promiseArgs()), working_env);
}

void Closure::DebugScope::endDebugging() const
{
    const ClosureContext* ctxt = ClosureContext::innermost();
    try {
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

    /* CXXR FIXME
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

    if (Rf_isList(val) || Rf_isNull(val))
	n = length(val);
    else
	n = LENGTH(val);

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
    return GCStackFrameBoundary::withStackFrameBoundary(
	[=]() {  return do_while_impl(call, op, args, rho); });
}

static SEXP do_repeat_impl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    checkArity(op, args);

    /* CXXR FIXME
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

SEXP attribute_hidden do_paren(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return CAR(args);
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
	return PairList::cons(nval, PairList::cons(expr));
    }
    else if (Rf_isLanguage(expr)) {
	Expression* exprn = static_cast<Expression*>(expr);
	GCStackRoot<PairList> val(evalseq(exprn->tail()->car(), rho, forcelocal, tmploc));
	R_SetVarLocValue(tmploc, val->car());
	GCStackRoot<PairList> nexprargs(PairList::cons(R_GetVarLocSymbol(tmploc),
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

/* This macro stores the current assignment target in the saved
   binding location. It duplicates if necessary to make sure
   replacement functions are always called with a target with NAMED ==
   1. The SET_CAR is intended to protect against possible GC in
   R_SetVarLocValue; this might occur it the binding is an active
   binding. */
#define SET_TEMPVARLOC_FROM_CAR(loc, lhs) do { \
	SEXP __lhs__ = (lhs); \
	SEXP __v__ = CAR(__lhs__); \
	if (NAMED(__v__) == 2) { \
	    __v__ = Rf_duplicate(__v__); \
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
	if (NAMED(__rhs__) && NAMED(__rhs__) != 2) \
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

static SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
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
	// Second arg in the foll. changed in CXXR at r253 (2008-03-18):
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
    arglist.evaluate(SEXP_downcast<Environment*>(rho), true);
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
	Rf_error(_("'Recall' called from outside a closure"));
    Closure* closure = SEXP_downcast<Closure*>(s);
    ans = closure->invoke(cptr->callEnvironment(), &arglist, cptr->call());
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
	    /* This means S4 dispatch */
	    std::pair<bool, SEXP> pr
		= R_possible_dispatch(callx, func,
				      const_cast<PairList*>(arglist.list()),
				      callenv, TRUE);
	    if (pr.first) {
		*ans = pr.second;
		return 1;
	    }
	}
	char* suffix = nullptr;
	{
	    RObject* callcar = callx->car();
	    if (callcar->sexptype() == SYMSXP) {
		Symbol* sym = static_cast<Symbol*>(callcar);
		suffix = Rf_strrchr(sym->name()->c_str(), '.');
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
	}
    }
    if (arglist.status() != ArgList::EVALUATED)
	arglist.evaluate(callenv, !dropmissing);
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
    }

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
    return 1;
}

/* start of bytecode section */
static int R_bcVersion = 7;
static int R_bcMinVersion = 6;

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

attribute_hidden
void R_initialize_bcode(void)
{
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
  SETVECSUBSET_OP,
  SETMATSUBSET_OP,
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
  STARTVECSUBSET_OP,
  STARTMATSUBSET_OP,
  STARTSETVECSUBSET_OP,
  STARTSETMATSUBSET_OP,
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

// Note that this macro name uses END in the sense of the C++ standard
// library end(), i.e. one past the current top element of the stack,
// not in the way that CR uses R_BCNodeStackEnd, which relates to the
// end of allocated storage.
#define NSFROMEND(i) (s_nodestack->fromEnd(i))

#define GETSTACK(i) NSFROMEND(-i)

#define SETSTACK(i, v) NSFROMEND(-i) = v

#define SETSTACK_REAL(i, v) (NSFROMEND(-i) = Rf_ScalarReal(v))

#define SETSTACK_INTEGER(i, v) (NSFROMEND(-i) = Rf_ScalarInteger(v))

#define SETSTACK_LOGICAL(i, v) do {		\
    int __ssl_v__ = (v); \
    if (__ssl_v__ == NA_LOGICAL) \
	NSFROMEND(-i) = Rf_ScalarLogical(NA_LOGICAL);	\
    else \
	NSFROMEND(-i) = ( __ssl_v__ ? R_TrueValue : R_FalseValue);	\
} while(0)


typedef union { double dval; int ival; } scalar_value_t;

/* bcStackScalar() checks whether the object in the specified stack
   location is a simple real, integer, or logical scalar (i.e. length
   one and no attributes.  If so, the type is returned as the function
   value and the value is returned in the structure pointed to by the
   second argument; if not, then zero is returned as the function
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
	}
    }
    else return 0;
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
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (Rf_isObject(x) || Rf_isObject(y)) {
	SEXP args, ans;
	args = PairList::cons(x, PairList::cons(y));
	PROTECT(args);
	if (Rf_DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return do_relop_dflt(call, op, x, y);
}

static SEXP cmp_arith1(SEXP call, SEXP opsym, SEXP x, SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (Rf_isObject(x)) {
	SEXP args, ans;
	args = PairList::cons(x);
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
	args = CONS(x, CONS(y, R_NilValue));
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
  SEXP call = (*constants)[GETOP()]; \
  SETSTACK(-1, CONS(GETSTACK(-1), R_NilValue));		     \
  SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP), \
		      GETSTACK(-1), rho));		     \
  NEXT(); \
} while(0)

#define Builtin2(do_fun,which,rho) do {		     \
  SEXP call = (*constants)[GETOP()]; \
  PairList* tmp = PairList::cons(GETSTACK(-1));	\
  SETSTACK(-2, CONS(GETSTACK(-2), tmp));     \
  s_nodestack->pop(); \
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
#define Math1(which) Builtin1(do_math1,which,rho)
#define Relop2(opval,opsym) NewBuiltin2(cmp_relop,opval,opsym,rho)

# define DO_FAST_BINOP(op,a,b) do { \
    SKIP_OP(); \
    SETSTACK_REAL(-2, (a) op (b)); \
    s_nodestack->pop(); \
    NEXT(); \
} while (0)

# define DO_FAST_BINOP_INT(op, a, b) do { \
    double dval = (double( (a))) op (double( (b)));	\
    if (dval <= INT_MAX && dval >= INT_MIN + 1) { \
	SKIP_OP();				  \
	SETSTACK_INTEGER(-2, int( dval));	\
	s_nodestack->pop();			\
	NEXT(); \
    } \
} while(0)

# define FastBinary(op,opval,opsym) do { \
    scalar_value_t vx; \
    scalar_value_t vy; \
    int typex = bcStackScalar(NSFROMEND(2), &vx);	  \
    int typey = bcStackScalar(NSFROMEND(1), &vy);	  \
    if (typex == REALSXP) { \
	if (typey == REALSXP)			 \
	    DO_FAST_BINOP(op, vx.dval, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) \
	    DO_FAST_BINOP(op, vx.dval, vy.ival); \
    } \
    else if (typex == INTSXP && vx.ival != NA_INTEGER) { \
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.ival, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) { \
	    if (opval == DIVOP) \
		DO_FAST_BINOP(op, double( vx.ival), double( vy.ival));	\
	    else							\
		DO_FAST_BINOP_INT(op, vx.ival, vy.ival); \
	} \
    } \
    Arith2(opval, opsym); \
} while (0)

#define BCNPUSH(v) (s_nodestack->push(v))

#define BCNDUP() (s_nodestack->push(s_nodestack->fromEnd(1)))

#define BCNDUP2ND() (s_nodestack->push(s_nodestack->fromEnd(2)))

#define BCNPOP(v) (s_nodestack->topnpop())

#define BCNPOP_IGNORE_VALUE() (s_nodestack->pop())

#define BCNSTACKCHECK(n)

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

#define BCCONSTS(e) (SEXP_downcast<ByteCode*>(e)->constants())

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
       entries when compiled, to that is a good value to use.

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
# define CACHE_MAX 128
# ifdef CACHE_MAX
#  define CACHE_MASK (CACHE_MAX - 1)
#  define CACHEIDX(i) ((i) & CACHE_MASK)
# else
#  define CACHEIDX(i) (i)
# endif

# define CACHE_ON_STACK
# ifdef CACHE_ON_STACK
typedef R_bcstack_t * R_binding_cache_t;
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? vcache[CACHEIDX(sidx)] : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? vcache[sidx] : R_NilValue)

#  define SET_CACHED_BINDING(cvache, sidx, cell) \
    do { if (vcache) vcache[CACHEIDX(sidx)] = (cell); } while (0)
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

static void MISSING_ARGUMENT_ERROR(SEXP symbol)
{
    const char *n = CHAR(PRINTNAME(symbol));
    if(*n) Rf_error(_("argument \"%s\" is missing, with no default"), n);
    else Rf_error(_("argument is missing, with no default"));
}

#define MAYBE_MISSING_ARGUMENT_ERROR(symbol, keepmiss) \
    do { if (! keepmiss) MISSING_ARGUMENT_ERROR(symbol); } while (0)

static void UNBOUND_VARIABLE_ERROR(SEXP symbol)
{
    Rf_error(_("object '%s' not found"), CHAR(PRINTNAME(symbol)));
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

#define PUSHCALLARG(v) PUSHCALLARG_CELL(PairList::cons(v))

#define PUSHCALLARG_CELL(c) do { \
  SEXP __cell__ = (c); \
  if (GETSTACK(-2) == R_NilValue) SETSTACK(-2, __cell__); \
  else SETCDR(GETSTACK(-1), __cell__); \
  SETSTACK(-1, __cell__);	       \
} while (0)

static int tryDispatch(CXXRCONST char *generic, SEXP call, SEXP x, SEXP rho, SEXP *pv)
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
    prom = Rf_mkPROMISE(CAR(last), rho);
    SET_PRVALUE(prom, rhs);
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
    SEXP cell = PairList::cons(value);	\
    BCNSTACKCHECK(3); \
    s_nodestack->push(call); \
    s_nodestack->push(cell); \
    s_nodestack->push(cell); \
    if (tag != R_NilValue) \
      SET_TAG(cell, Rf_CreateTag(tag)); \
  } \
  NEXT(); \
} while (0)

#define DO_DFLTDISPATCH(fun, symbol) do { \
  SEXP call = GETSTACK(-3); \
  SEXP args = GETSTACK(-2); \
  value = fun(call, symbol, args, rho); \
  s_nodestack->pop(3);	\
  SETSTACK(-1, value); \
  NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH(generic) do { \
  SEXP call = (*constants)[GETOP()]; \
  int label = GETOP(); \
  SEXP lhs = GETSTACK(-2); \
  SEXP rhs = GETSTACK(-1); \
  if (NAMED(lhs) == 2) { \
    lhs = Rf_duplicate(lhs); \
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
    SEXP cell = PairList::cons(lhs);	\
    BCNSTACKCHECK(3); \
    s_nodestack->push(call); \
    s_nodestack->push(cell); \
    s_nodestack->push(cell); \
    if (tag != R_NilValue) \
      SET_TAG(cell, Rf_CreateTag(tag)); \
  } \
  NEXT(); \
} while (0)

#define DO_DFLT_ASSIGN_DISPATCH(fun, symbol) do { \
  SEXP rhs = GETSTACK(-4); \
  SEXP call = GETSTACK(-3); \
  SEXP args = GETSTACK(-2); \
  PUSHCALLARG(rhs); \
  value = fun(call, symbol, args, rho); \
  s_nodestack->pop(4);	\
  SETSTACK(-1, value);	 \
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
	if (NAMED(lhs) == 2) { \
	    lhs = Rf_duplicate(lhs); \
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
  SETSTACK(-1, TYPEOF(GETSTACK(-1)) == type ? Rf_mkTrue() : Rf_mkFalse()); \
  NEXT(); \
} while (0)
#define isNumericOnly(x) (Rf_isNumeric(x) && ! Rf_isLogical(x))

#ifdef BC_PROFILING
#define NO_CURRENT_OPCODE -1
static int current_opcode = NO_CURRENT_OPCODE;
static int opcode_counts[OPCOUNT];
#endif

#define BC_COUNT_DELTA 1000

#define BC_CHECK_SIGINT() do { \
  if (++evalcount > BC_COUNT_DELTA) { \
      R_CheckUserInterrupt(); \
      evalcount = 0; \
  } \
} while (0)

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

static R_INLINE int bcStackIndex(RObject* idx)
{
    switch(TYPEOF(idx)) {
    case INTSXP:
	if (LENGTH(idx) == 1 && INTEGER(idx)[0] != NA_INTEGER)
	    return INTEGER(idx)[0];
	else return -1;
    case REALSXP:
	if (LENGTH(idx) == 1) {
	    double val = REAL(idx)[0];
	    if (! ISNAN(val) && val <= INT_MAX && val > INT_MIN)
		return int( val);
	    else return -1;
	}
	else return -1;
    default: return -1;
    }
}

static inline RObject* VECSUBSET_PTR(RObject* vec, RObject* idx,
				     SEXP rho)
{
    SEXP args, value;
    int i = bcStackIndex(idx) - 1;

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

    /* fall through to the standard default handler */
    args = CONS(idx, R_NilValue);
    args = CONS(vec, args);
    PROTECT(args);
    value = do_subset_dflt(R_NilValue, R_SubsetSym, args, rho);
    UNPROTECT(1);
    return value;
}

#define DO_VECSUBSET(rho) do { \
    NSFROMEND(2) = VECSUBSET_PTR(NSFROMEND(2), NSFROMEND(1), rho); \
    s_nodestack->pop(); \
} while(0)

static R_INLINE SEXP getMatrixDim(SEXP mat)
{
    if (! OBJECT(mat) &&
	TAG(ATTRIB(mat)) == R_DimSymbol &&
	CDR(ATTRIB(mat)) == R_NilValue) {
	SEXP dim = CAR(ATTRIB(mat));
	if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2)
	    return dim;
	else return R_NilValue;
    }
    else return R_NilValue;
}

R_INLINE void ByteCode::DO_MATSUBSET(SEXP rho)
{
    SEXP idx, jdx, args, value;
    SEXP mat = GETSTACK(-3);
    SEXP dim = getMatrixDim(mat);

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
	    }
	}
    }

    /* fall through to the standard default handler */
    idx = GETSTACK(-2);
    jdx = GETSTACK(-1);
    args = CONS(jdx, R_NilValue);
    args = CONS(idx, args);
    args = CONS(mat, args);
    SETSTACK(-1, args); /* for GC protection */
    value = do_subset_dflt(R_NilValue, R_SubsetSym, args, rho);
    s_nodestack->pop(2);
    SETSTACK(-1, value);
}

#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))
#define LOGICAL_TO_REAL(x) ((x) == NA_LOGICAL ? NA_REAL : (x))

static R_INLINE Rboolean setElementFromScalar(SEXP vec, int i, int typev,
					      scalar_value_t *v)
{
    if (i < 0) return FALSE;

    if (TYPEOF(vec) == REALSXP) {
	if (LENGTH(vec) <= i) return FALSE;
	switch(typev) {
	case REALSXP: REAL(vec)[i] = v->dval; return TRUE;
	case INTSXP: REAL(vec)[i] = INTEGER_TO_REAL(v->ival); return TRUE;
	case LGLSXP: REAL(vec)[i] = LOGICAL_TO_REAL(v->ival); return TRUE;
	}
    }
    else if (typev == TYPEOF(vec)) {
	if (LENGTH(vec) <= i) return FALSE;
	switch (typev) {
	case INTSXP: INTEGER(vec)[i] = v->ival; return TRUE;
	case LGLSXP: LOGICAL(vec)[i] = v->ival; return TRUE;
	}
    }
    return FALSE;
}

static R_INLINE RObject* SETVECSUBSET_PTR(RObject* vec, RObject* value,
					  RObject* idx,
					  SEXP rho)
{
    SEXP args;

    if (NAMED(vec) == 2) {
	vec = Rf_duplicate(vec);
    }
    else if (NAMED(vec) == 1)
	SET_NAMED(vec, 0);

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

    /* fall through to the standard default handler */
    args = CONS(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    args = CONS(idx, args);
    args = CONS(vec, args);
    PROTECT(args);
    vec = do_subassign_dflt(R_NilValue, R_SubassignSym, args, rho);
    UNPROTECT(1);
    return vec;
}

R_INLINE void ByteCode::DO_SETVECSUBSET(SEXP rho)
{
    NSFROMEND(3) = SETVECSUBSET_PTR(NSFROMEND(3), NSFROMEND(2),
				    NSFROMEND(1), rho);
    s_nodestack->pop(2);
}

R_INLINE void ByteCode::DO_SETMATSUBSET(SEXP rho)
{
    SEXP dim, idx, jdx, args, value;
    SEXP mat = GETSTACK(-4);

    if (NAMED(mat) > 1) {
	mat = Rf_duplicate(mat);
	SETSTACK(-4, mat);
    }
    else if (NAMED(mat) == 1)
	SET_NAMED(mat, 0);

    dim = getMatrixDim(mat);

    if (dim != R_NilValue) {
	int i = bcStackIndex(NSFROMEND(2));
	int j = bcStackIndex(NSFROMEND(1));
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
	    scalar_value_t v;
	    int typev = bcStackScalar(NSFROMEND(3), &v);
	    int k = i - 1 + nrow * (j - 1);
	    if (setElementFromScalar(mat, k, typev, &v)) {
		s_nodestack->pop(3);
		SETSTACK(-1, mat);
		return;
	    }
	}
    }

    /* fall through to the standard default handler */
    value = GETSTACK(-3);
    idx = GETSTACK(-2);
    jdx = GETSTACK(-1);
    args = CONS(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    args = CONS(jdx, args);
    args = CONS(idx, args);
    args = CONS(mat, args);
    SETSTACK(-1, args); /* for GC protection */
    mat = do_subassign_dflt(R_NilValue, R_SubassignSym, args, rho);
    s_nodestack->pop(3);
    SETSTACK(-1, mat);
}

#define FIXUP_SCALAR_LOGICAL(callidx, arg, op) do { \
	SEXP val = GETSTACK(-1); \
	if (TYPEOF(val) != LGLSXP || LENGTH(val) != 1) { \
	    if (!Rf_isNumber(val))	\
		Rf_errorcall(VECTOR_ELT(constants, callidx), \
			  _("invalid %s type in 'x %s y'"), arg, op);	\
	    SETSTACK(-1, Rf_ScalarLogical(Rf_asLogical(val))); \
	} \
    } while(0)

static R_INLINE void checkForMissings(SEXP args, SEXP call)
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

#define GET_VEC_LOOP_VALUE(var, pos) do {		\
    (var) = GETSTACK(pos);				\
    if (NAMED(var) == 2) {				\
	(var) = Rf_allocVector(TYPEOF(seq), 1);		\
	SETSTACK(pos, var);				\
	SET_NAMED(var, 1);				\
    }							\
} while (0)

/* The CALLBUILTIN instruction handles calls to both true BUILTINs and
   to .Internals of type BUILTIN. To handle profiling in a way that is
   consistent with this instruction needs to be able to distinguish a
   true BUILTIN from a .Internal. LT */
#define IS_TRUE_BUILTIN(x) ((R_FunTab[PRIMOFFSET(x)].eval % 100 )/10 == 0)

RObject* ByteCode::interpret(ByteCode* bcode, Environment* rho)
{
  ByteCode::Scope scope;
  SEXP body = bcode;
  SEXP value;
  ListVector* constants;
  BCODE *pc, *codebase;
  int ftype = 0;
  static int evalcount = 0;
#ifdef BC_PROFILING
  int old_current_opcode = current_opcode;
#endif
#ifdef THREADED_CODE
  int which = 0;
#endif

  BC_CHECK_SIGINT();

  INITIALIZE_MACHINE();
#ifdef ENCODED_BCODE
  codebase = &bcode->m_threaded_code[0];
#else
  codebase = &(*bcode->m_code)[0];
#endif
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
	  *R_BCNodeStackTop = R_NilValue;
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
	int cond;
	SEXP call = (*constants)[callidx];
	value = BCNPOP();
	cond = asLogicalNoNA(value, call);
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
	SEXP seq = GETSTACK(-1);
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
	if (Rf_isVector(seq))
	  INTEGER(value)[1] = LENGTH(seq);
	else if (Rf_isList(seq) || Rf_isNull(seq))
	  INTEGER(value)[1] = length(seq);
	else Rf_errorcall(VECTOR_ELT(constants, callidx),
		       _("invalid for() loop sequence"));
	BCNPUSH(value);

	/* bump up NAMED count of seq to avoid modification by loop code */
	if (NAMED(seq) < 2) SET_NAMED(seq, NAMED(seq) + 1);

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
	int i = ++(INTEGER(GETSTACK(-2))[0]);
	int n = INTEGER(GETSTACK(-2))[1];
	if (i < n) {
	  SEXP seq = GETSTACK(-4);
	  Frame::Binding* cell = s_loopvar_stack->back();
	  switch (TYPEOF(seq)) {
	  case LGLSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    LOGICAL(value)[0] = LOGICAL(seq)[i];
	    break;
	  case INTSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
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
	  if (! SET_BINDING_VALUE(cell, value))
	      Rf_defineVar(BINDING_SYMBOL(cell), value, rho);
	  BC_CHECK_SIGINT();
	  pc = codebase + label;
	}
	NEXT();
      }
    OP(ENDFOR, 0):
      {
	s_nodestack->pop(3);
	s_loopvar_stack->pop_back();
	SETSTACK(-1, nullptr);
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
	value = GETSTACK(-1);
	switch (NAMED(value)) {
	case 0: SET_NAMED(value, 1); break;
	case 1: SET_NAMED(value, 2); break;
	}
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

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
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

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
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

	/* initialize the function type register, push the function, and
	   push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
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

	/* push the function and push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
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

	/* push the function and push space for creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(3);
	s_nodestack->push(value);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
	NEXT();
      }
    OP(CHECKFUN, 0):
      {
	/* check then the value on the stack is a function */
	value = GETSTACK(-1);
	if (TYPEOF(value) != CLOSXP && TYPEOF(value) != BUILTINSXP &&
	    TYPEOF(value) != SPECIALSXP)
	  Rf_error(_("attempt to apply non-function"));

	/* initialize the function type register, and push space for
	   creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(2);
	s_nodestack->push(R_NilValue);
	s_nodestack->push(R_NilValue);
	NEXT();
      }
    OP(MAKEPROM, 1):
      {
	  ByteCode* code = SEXP_downcast<ByteCode*>((*constants)[GETOP()].get());
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
	if (ftype != SPECIALSXP)
	  PUSHCALLARG(R_MissingArg);
	NEXT();
      }
    OP(SETTAG, 1):
      {
	SEXP tag = (*constants)[GETOP()];
	SEXP cell = GETSTACK(-1);
	if (ftype != SPECIALSXP && cell != R_NilValue)
	  SET_TAG(cell, Rf_CreateTag(tag));
	NEXT();
      }
    OP(DODOTS, 0):
      {
	if (ftype != SPECIALSXP) {
	  SEXP h = Rf_findVar(R_DotsSymbol, rho);
	  if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
	    for (; h != R_NilValue; h = CDR(h)) {
	      SEXP val, cell;
	      if (ftype == BUILTINSXP) val = Rf_eval(CAR(h), rho);
	      else val = Rf_mkPROMISE(CAR(h), rho);
	      cell = PairList::cons(val);
	      PUSHCALLARG_CELL(cell);
	      if (TAG(h) != R_NilValue) SET_TAG(cell, Rf_CreateTag(TAG(h)));
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
      value = (*constants)[GETOP()];
      PUSHCALLARG(Rf_duplicate(value));
      NEXT();
    OP(PUSHNULLARG, 0):
	PUSHCALLARG(nullptr);
	NEXT();
    OP(PUSHTRUEARG, 0):
	PUSHCALLARG(Rf_mkTrue());
	NEXT();
    OP(PUSHFALSEARG, 0):
	PUSHCALLARG(Rf_mkFalse());
	NEXT();
    OP(CALL, 1):
      {
	SEXP fun = GETSTACK(-3);
	SEXP call = (*constants)[GETOP()];
	SEXP args = GETSTACK(-2);
	int flag;
	switch (ftype) {
	case BUILTINSXP:
	  checkForMissings(args, call);
	  flag = PRIMPRINT(fun);
	  R_Visible = CXXRCONSTRUCT(Rboolean, flag != 1);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  if (flag < 2) R_Visible = CXXRCONSTRUCT(Rboolean, flag != 1);
	  break;
	case SPECIALSXP:
	  flag = PRIMPRINT(fun);
	  R_Visible = CXXRCONSTRUCT(Rboolean, flag != 1);
	  value = PRIMFUN(fun) (call, fun, CDR(call), rho);
	  if (flag < 2) R_Visible = CXXRCONSTRUCT(Rboolean, flag != 1);
	  break;
	case CLOSXP: {
	    Closure* closure = SEXP_downcast<Closure*>(fun);
	    Expression* callx = SEXP_downcast<Expression*>(call);
	    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::PROMISED);
	    value = closure->invoke(rho, &arglist, callx);
	    break;
	}
	default: Rf_error(_("bad function"));
	}
	s_nodestack->pop(2);
	SETSTACK(-1, value);
	ftype = 0;
	NEXT();
      }
    OP(CALLBUILTIN, 1):
      {
	SEXP fun = GETSTACK(-3);
	SEXP call = (*constants)[GETOP()];
	SEXP args = GETSTACK(-2);
	if (TYPEOF(fun) != BUILTINSXP)
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
    OP(UMINUS, 1): Arith1(R_SubSym);
    OP(UPLUS, 1): Arith1(R_AddSym);
    OP(ADD, 1): FastBinary(+, PLUSOP, R_AddSym);
    OP(SUB, 1): FastBinary(-, MINUSOP, R_SubSym);
    OP(MUL, 1): FastBinary(*, TIMESOP, R_MulSym);
    OP(DIV, 1): FastBinary(/, DIVOP, R_DivSym);
    OP(EXPT, 1): Arith2(POWOP, R_ExptSym);
    OP(SQRT, 1): Math1(R_SqrtSym);
    OP(EXP, 1): Math1(R_ExpSym);
    OP(EQ, 1): FastRelop2(==, EQOP, R_EqSym);
    OP(NE, 1): FastRelop2(!=, NEOP, R_NeSym);
    OP(LT, 1): FastRelop2(<, LTOP, R_LtSym);
    OP(LE, 1): FastRelop2(<=, LEOP, R_LeSym);
    OP(GE, 1): FastRelop2(>=, GEOP, R_GeSym);
    OP(GT, 1): FastRelop2(>, GTOP, R_GtSym);
    OP(AND, 1): Builtin2(do_logic, R_AndSym, rho);
    OP(OR, 1): Builtin2(do_logic, R_OrSym, rho);
    OP(NOT, 1): Builtin1(do_logic, R_NotSym, rho);
    OP(DOTSERR, 0): Rf_error(_("'...' used in an incorrect context"));
    OP(STARTASSIGN, 1):
      {
	int sidx = GETOP();
	SEXP symbol = (*constants)[sidx];
	Frame::Binding* cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = BINDING_VALUE(cell);
	if (value == R_UnboundValue || NAMED(value) != 1)
	    value = EnsureLocal(symbol, rho);
	BCNPUSH(value);
	BCNDUP2ND();
	/* top three stack entries are now RHS value, LHS value, RHS value */
	FIXUP_RHS_NAMED(GETSTACK(-1));
	NEXT();
      }
    OP(ENDASSIGN, 1):
      {
	int sidx = GETOP();
	SEXP symbol = (*constants)[sidx];
	Frame::Binding* cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = GETSTACK(-1); /* leave on stack for GC protection */
	switch (NAMED(value)) {
	case 0: SET_NAMED(value, 1); break;
	case 1: SET_NAMED(value, 2); break;
	}
	if (! SET_BINDING_VALUE(cell, value))
	    Rf_defineVar(symbol, value, rho);
	s_nodestack->pop(); /* now pop LHS value off the stack */
	/* original right-hand side value is now on top of stack again */
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = 2 */
	SET_NAMED(GETSTACK(-1), 2);
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
	if (NAMED(x) == 2) {
	    x = Rf_duplicate(x);
	    SETSTACK(-2, x);
	    SET_NAMED(x, 1);
	}
	if (Rf_isObject(x)) {
	    SEXP ncall, prom;
	    PROTECT(ncall = Rf_duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), Rf_ScalarString(PRINTNAME(symbol)));
	    prom = Rf_mkPROMISE(CADDDR(ncall), rho);
	    SET_PRVALUE(prom, rhs);
	    SETCAR(CDR(CDDR(ncall)), prom);
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
	bool test = (TYPEOF(arg) == INTSXP) && ! Rf_inherits(arg, "factor");
	SETSTACK(-1, test ? Rf_mkTrue() : Rf_mkFalse());
	NEXT();
      }
    OP(ISDOUBLE, 0): DO_ISTYPE(REALSXP);
    OP(ISCOMPLEX, 0): DO_ISTYPE(CPLXSXP);
    OP(ISCHARACTER, 0): DO_ISTYPE(STRSXP);
    OP(ISSYMBOL, 0): DO_ISTYPE(SYMSXP); /**** S4 thingy allowed now???*/
    OP(ISOBJECT, 0): DO_ISTEST(OBJECT);
    OP(ISNUMERIC, 0): DO_ISTEST(isNumericOnly);
    OP(VECSUBSET, 0): DO_VECSUBSET(rho); NEXT();
    OP(MATSUBSET, 0): DO_MATSUBSET(rho); NEXT();
    OP(SETVECSUBSET, 0): DO_SETVECSUBSET(rho); NEXT();
    OP(SETMATSUBSET, 0): DO_SETMATSUBSET(rho); NEXT();
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
	if (NAMED(value)) {
	    value = Rf_duplicate(value);
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
	NEXT();
      }
    OP(ENDASSIGN2, 1):
      {
	SEXP symbol = (*constants)[GETOP()];
	value = BCNPOP();
	switch (NAMED(value)) {
	case 0: SET_NAMED(value, 1); break;
	case 1: SET_NAMED(value, 2); break;
	}
	Rf_setVar(symbol, value, ENCLOS(rho));
	/* original right-hand side value is now on top of stack again */
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = 2 */
	SET_NAMED(GETSTACK(-1), 2);
	NEXT();
      }
    OP(SETTER_CALL, 2):
      {
	SEXP lhs = GETSTACK(-5);
	SEXP rhs = GETSTACK(-4);
	SEXP fun = GETSTACK(-3);
	SEXP call = (*constants)[GETOP()];
	SEXP vexpr = (*constants)[GETOP()];
	SEXP args, prom, last;
	if (NAMED(lhs) == 2) {
	  lhs = Rf_duplicate(lhs);
	  SETSTACK(-5, lhs);
	  SET_NAMED(lhs, 1);
	}
	switch (ftype) {
	case BUILTINSXP:
	  /* push RHS value onto arguments with 'value' tag */
	  PUSHCALLARG(rhs);
	  SET_TAG(GETSTACK(-1), R_valueSym);
	  /* replace first argument with LHS value */
	  args = GETSTACK(-2);
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = Rf_duplicate(CDR(call));
	  SETSTACK(-2, args);
	  /* insert evaluated promise for LHS as first argument */
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  SETCAR(args, prom);
	  /* insert evaluated promise for RHS as last argument */
	  last = args;
	  while (CDR(last) != R_NilValue)
	      last = CDR(last);
	  prom = Rf_mkPROMISE(vexpr, rho);
	  SET_PRVALUE(prom, rhs);
	  SETCAR(last, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case CLOSXP:
	  /* push evaluated promise for RHS onto arguments with 'value' tag */
	  prom = Rf_mkPROMISE(vexpr, rho);
	  SET_PRVALUE(prom, rhs);
	  PUSHCALLARG(prom);
	  SET_TAG(GETSTACK(-1), R_valueSym);
	  /* replace first argument with evaluated promise for LHS */
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  args = GETSTACK(-2);
	  SETCAR(args, prom);
	  /* make the call */
	  {
	      Closure* closure = SEXP_downcast<Closure*>(fun);
	      Expression* callx = SEXP_downcast<Expression*>(call);
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::PROMISED);
	      value = closure->invoke(rho, &arglist, callx);
	  }    
	  break;
	default: Rf_error(_("bad function"));
	}
	s_nodestack->pop(4);
	SETSTACK(-1, value);
	ftype = 0;
	NEXT();
      }
    OP(GETTER_CALL, 1):
      {
	SEXP lhs = GETSTACK(-5);
	SEXP fun = GETSTACK(-3);
	SEXP call = (*constants)[GETOP()];
	SEXP args, prom;
	switch (ftype) {
	case BUILTINSXP:
	  /* replace first argument with LHS value */
	  args = GETSTACK(-2);
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = Rf_duplicate(CDR(call));
	  SETSTACK(-2, args);
	  /* insert evaluated promise for LHS as first argument */
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case CLOSXP:
	  /* replace first argument with evaluated promise for LHS */
	  prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  args = GETSTACK(-2);
	  SETCAR(args, prom);
	  /* make the call */
	  {
	      Closure* closure = SEXP_downcast<Closure*>(fun);
	      Expression* callx = SEXP_downcast<Expression*>(call);
	      ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::PROMISED);
	      value = closure->invoke(rho, &arglist, callx);
	  }
	  break;
	default: Rf_error(_("bad function"));
	}
	s_nodestack->pop(2);
	SETSTACK(-1, value);
	ftype = 0;
	NEXT();
      }
    OP(SWAP, 0): {
	R_bcstack_t tmp = NSFROMEND(1);
	NSFROMEND(1) = NSFROMEND(2);
	NSFROMEND(2) = tmp;
	NEXT();
    }
    OP(DUP2ND, 0): BCNDUP2ND(); NEXT();
    OP(SWITCH, 4): {
       SEXP call = (*constants)[GETOP()];
       SEXP names = (*constants)[GETOP()];
       SEXP coffsets = (*constants)[GETOP()];
       SEXP ioffsets = (*constants)[GETOP()];
       value = BCNPOP();
       if (!Rf_isVector(value) || length(value) != 1)
	   Rf_errorcall(call, _("EXPR must be a length 1 vector"));
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
	   int which = Rf_asInteger(value) - 1;
	   if (TYPEOF(ioffsets) != INTSXP)
	       Rf_errorcall(call, _("bad numeric 'switch' offsets"));
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
    OP(STARTVECSUBSET, 2): DO_STARTDISPATCH_N("[");
    OP(STARTMATSUBSET, 2): DO_STARTDISPATCH_N("[");
    OP(STARTSETVECSUBSET, 2): DO_START_ASSIGN_DISPATCH_N("[<-");
    OP(STARTSETMATSUBSET, 2): DO_START_ASSIGN_DISPATCH_N("[<-");
    LASTOP;
  }

 done:
#ifdef BC_PROFILING
  current_opcode = old_current_opcode;
#endif
  return value;
}

#ifdef THREADED_CODE
#ifndef TOKEN_THREADING
void ByteCode::thread()
{
    size_t n = m_code->size();
    int version = (*m_code)[0];
    // Check version:
    if (version < R_bcMinVersion || version > R_bcVersion) {
	m_threaded_code.resize(2);
	m_threaded_code[0].i = version;
	m_threaded_code[1].v = s_op_address[BCMISMATCH_OP];
	return;
    }
    m_threaded_code.resize(n);
    // Insert the current version number:
    m_threaded_code[0].i = R_bcVersion;
    // First do a straight copy:
    for (size_t i = 1; i < n; ++i)
	m_threaded_code[i].i = (*m_code)[i];
    // Now replace the opcodes with the appropriate code addresses:
    {
	size_t i = 1;
	while (i < n) {
	    BCODE& cell = m_threaded_code[i];
	    int op = cell.i;
	    if (op < 0 || op >= OPCOUNT)
		error("unknown instruction code");
	    cell.v = s_op_address[op];
	    i += s_op_arity[op] + 1;
	}
    }
}
#endif 
#endif

SEXP attribute_hidden do_mkcode(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP bytes, consts;

    checkArity(op, args);
    bytes = CAR(args);
    consts = CADR(args);
    GCStackRoot<IntVector> enc(SEXP_downcast<IntVector*>(bytes));
    GCStackRoot<ListVector> pl(SEXP_downcast<ListVector*>(consts));
    return new ByteCode(enc, pl);
}

SEXP attribute_hidden do_bcclose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP forms, body, env;

    checkArity(op, args);
    forms = CAR(args);
    body = CADR(args);
    env = CADDR(args);

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

SEXP attribute_hidden do_is_builtin_internal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP symbol, i;

    checkArity(op, args);
    symbol = CAR(args);

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
  SEXP code = bcode->code();
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

SEXP attribute_hidden do_disassemble(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  SEXP code;

  checkArity(op, args);
  code = CAR(args);
  if (! isByteCode(code))
    Rf_errorcall(call, _("argument is not a byte code object"));
  return disassemble(code);
}

SEXP attribute_hidden do_bcversion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  SEXP ans = Rf_allocVector(INTSXP, 1);
  INTEGER(ans)[0] = R_bcVersion;
  return ans;
}

SEXP attribute_hidden do_loadfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file, s;
    FILE *fp;

    checkArity(op, args);

    PROTECT(file = Rf_coerceVector(CAR(args), STRSXP));

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

SEXP attribute_hidden do_savefile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    FILE *fp;

    checkArity(op, args);

    if (!Rf_isValidStringF(CADR(args)))
	Rf_errorcall(call, _("'file' must be non-empty string"));
    if (TYPEOF(CADDR(args)) != LGLSXP)
	Rf_errorcall(call, _("'ascii' must be logical"));

    fp = RC_fopen(STRING_ELT(CADR(args), 0), "wb", TRUE);
    if (!fp)
	Rf_errorcall(call, _("unable to open 'file'"));

    R_SaveToFileV(CAR(args), fp, INTEGER(CADDR(args))[0], 0);

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

SEXP attribute_hidden do_growconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, ans;
    int i, n;

    checkArity(op, args);
    constBuf = CAR(args);
    if (TYPEOF(constBuf) != VECSXP)
	Rf_error(_("constant buffer must be a generic vector"));

    n = LENGTH(constBuf);
    ans = Rf_allocVector(VECSXP, 2 * n);
    for (i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

SEXP attribute_hidden do_putconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, x;
    int i, constCount;

    checkArity(op, args);

    constBuf = CAR(args);
    if (TYPEOF(constBuf) != VECSXP)
	Rf_error(_("constant_buffer must be a generic vector"));

    constCount = Rf_asInteger(CADR(args));
    if (constCount < 0 || constCount >= LENGTH(constBuf))
	Rf_error("bad constCount value");

    x = CADDR(args);

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

SEXP attribute_hidden do_getconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, ans;
    int i, n;

    checkArity(op, args);
    constBuf = CAR(args);
    n = Rf_asInteger(CADR(args));

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
SEXP R_getbcprofcounts()
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

SEXP R_startbcprof()
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

SEXP R_stopbcprof()
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
#endif

/* end of byte code section */

SEXP attribute_hidden do_setnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_num_math_threads, newi;
    checkArity(op, args);
    newi = Rf_asInteger(CAR(args));
    if (newi >= 0 && newi <= R_max_num_math_threads)
	R_num_math_threads = newi;
    return Rf_ScalarInteger(old);
}

SEXP attribute_hidden do_setmaxnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_max_num_math_threads, newi;
    checkArity(op, args);
    newi = Rf_asInteger(CAR(args));
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
