/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-11 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	The R Development Core Team.
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
#include <Rinterface.h>
#include <Fileio.h>
#include "arithmetic.h"
#include "basedecl.h"

#include "CXXR/ArgList.hpp"
#include "CXXR/BailoutContext.hpp"
#include "CXXR/ByteCode.hpp"
#include "CXXR/ClosureContext.hpp"
#include "CXXR/DottedArgs.hpp"
#include "CXXR/LoopBailout.hpp"
#include "CXXR/LoopException.hpp"
#include "CXXR/PlainContext.hpp"
#include "CXXR/ReturnBailout.hpp"
#include "CXXR/ReturnException.hpp"
#include "CXXR/S3Launcher.hpp"
#include "CXXR/VectorFrame.hpp"

using namespace std;
using namespace CXXR;

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

static FILE *R_ProfileOutfile = NULL;
static int R_Mem_Profiling=0;
extern void get_current_mem(unsigned long *,unsigned long *,unsigned long *); /* in memory.c */
extern unsigned long get_duplicate_counter(void);  /* in duplicate.c */
extern void reset_duplicate_counter(void);         /* in duplicate.c */

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;

static void doprof(void)
{
    Evaluator::Context *cptr;
    char buf[1100];
    unsigned long bigv, smallv, nodes;
    int len;

    buf[0] = '\0';
    SuspendThread(MainThread);
    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if((len = strlen(buf)) < 1000) {
		sprintf(buf+len, ":%ld:%ld:%ld:%ld:", smallv, bigv,
		     nodes, get_duplicate_counter());
	    }
	    reset_duplicate_counter();
    }
    for (cptr = Evaluator::Context::innermost(); cptr; cptr = cptr->nextOut()) {
	if (TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < 1000) {
		strcat(buf, TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		       "<Anonymous>");
		strcat(buf, " ");
	    }
	}
    }
    ResumeThread(MainThread);
    if(strlen(buf))
	fprintf(R_ProfileOutfile, "%s\n", buf);
}

/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait);

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof();
    }
}
#else /* not Win32 */
static void doprof(int sig)
{
    FunctionContext *cptr;
    int newline = 0;
    unsigned long bigv, smallv, nodes;
    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if (!newline) newline = 1;
	    fprintf(R_ProfileOutfile, ":%ld:%ld:%ld:%ld:", smallv, bigv,
		     nodes, get_duplicate_counter());
	    reset_duplicate_counter();
    }
    for (cptr = FunctionContext::innermost();
	 cptr;
	 cptr = FunctionContext::innermost(cptr->nextOut())) {
	if (TYPEOF(CXXRCCAST(Expression*, cptr->call())) == LANGSXP) {
	    SEXP fun = CAR(CXXRCCAST(Expression*, cptr->call()));
	    if (!newline) newline = 1;
	    fprintf(R_ProfileOutfile, "\"%s\" ",
		    TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    if (newline) fprintf(R_ProfileOutfile, "\n");
    signal(SIGPROF, doprof);
}

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
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, doprof_null);
#endif /* not Win32 */
    if(R_ProfileOutfile) fclose(R_ProfileOutfile);
    R_ProfileOutfile = NULL;
    Evaluator::enableProfiling(false);
}

#if !defined(Win32) && defined(_R_HAVE_TIMING_)
// defined in unix/sys-unix.c
//double R_getClockIncrement(void);  // Use header files! 2007/06/11 arr
#endif

static void R_InitProfiling(SEXP filename, int append, double dinterval, int mem_profiling)
{
#ifndef Win32
    struct itimerval itv;
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();
#endif
    int interval;

#if !defined(Win32) && defined(_R_HAVE_TIMING_)
    /* according to man setitimer, it waits until the next clock
       tick, usually 10ms, so avoid too small intervals here
    double clock_incr = R_getClockIncrement();
    int nclock = CXXRCONSTRUCT(int, floor(dinterval/clock_incr + 0.5));
    interval = 1e6 * ((nclock > 1)?nclock:1) * clock_incr + 0.5; */
    interval = CXXRCONSTRUCT(int, 1e6 * dinterval + 0.5);
#else
    interval = CXXRCONSTRUCT(int, 1e6 * dinterval + 0.5);
#endif
    if(R_ProfileOutfile != NULL) R_EndProfiling();
    R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_ProfileOutfile == NULL)
	Rf_error(_("Rprof: cannot open profile file '%s'"),
	      Rf_translateChar(filename));
    if(mem_profiling)
	fprintf(R_ProfileOutfile, "memory profiling: sample.interval=%d\n", interval);
    else
	fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);

    R_Mem_Profiling=mem_profiling;
    if (mem_profiling)
	reset_duplicate_counter();

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
    signal(SIGPROF, doprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	R_Suicide("setting profile timer failed");
#endif /* not Win32 */
    Evaluator::enableProfiling(true);
}

SEXP attribute_hidden do_Rprof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP filename;
    int append_mode, mem_profiling;
    double dinterval;

#ifdef BC_PROFILING
    if (bc_profiling) {
	Rf_warning(_("can't use R profiling while byte code profiling"));
	return R_NilValue;
    }
#endif
    checkArity(op, args);
    if (!Rf_isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
	Rf_error(_("invalid '%s' argument"), "filename");
    append_mode = Rf_asLogical(CADR(args));
    dinterval = Rf_asReal(CADDR(args));
    mem_profiling = Rf_asLogical(CADDDR(args));
    filename = STRING_ELT(CAR(args), 0);
    if (LENGTH(filename))
	R_InitProfiling(filename, append_mode, dinterval, mem_profiling);
    else
	R_EndProfiling();
    return R_NilValue;
}
#else /* not R_PROFILING */
SEXP attribute_hidden do_Rprof(SEXP call, SEXP op, SEXP args, SEXP rho)
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

#ifdef BYTECODE
static void loadCompilerNamespace(void)
{
    SEXP fun, arg, expr;

    PROTECT(fun = Rf_install("getNamespace"));
    PROTECT(arg = Rf_mkString("compiler"));
    PROTECT(expr = Rf_lang2(fun, arg));
    Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(3);
}

void attribute_hidden R_init_jit_enabled(void)
{
    if (R_jit_enabled <= 0) {
	char *enable = getenv("R_ENABLE_JIT");
	if (enable != NULL) {
	    int val = atoi(enable);
	    if (val > 0)
		loadCompilerNamespace();
	    R_jit_enabled = val;
	}
    }

    if (R_compile_pkgs <= 0) {
	char *compile = getenv("R_COMPILE_PKGS");
	if (compile != NULL) {
	    int val = atoi(compile);
	    if (val > 0)
		R_compile_pkgs = TRUE;
	    else
		R_compile_pkgs = FALSE;
	}
    }
}
    
SEXP attribute_hidden R_cmpfun(SEXP fun)
{
    SEXP packsym, funsym, call, fcall, val;

    packsym = Rf_install("compiler");
    funsym = Rf_install("cmpfun");

    PROTECT(fcall = Rf_lang3(R_DoubleColonSymbol, packsym, funsym));
    PROTECT(call = Rf_lang2(fcall, fun));
    val = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

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
#endif

void Closure::DebugScope::startDebugging() const
{
    const ClosureContext* ctxt = ClosureContext::innermost();
    const Expression* call = ctxt->call();
    Environment* working_env = ctxt->workingEnvironment();
    working_env->setSingleStepping(true);
#ifdef BYTECODE
    /* switch to interpreted version when debugging compiled code */
    // CXXR FIXME:
#if 0
    if (TYPEOF(body) == BCODESXP)
	body = bytecodeExpr(body);
#endif
#endif
    Rprintf("debugging in: ");
    // Print call:
    {
	int old_bl = R_BrowseLines;
	int blines = Rf_asInteger(Rf_GetOption(Rf_install("deparse.max.lines"),
					       R_BaseEnv));
	if(blines != NA_INTEGER && blines > 0)
	    R_BrowseLines = blines;
	Rf_PrintValueRec(const_cast<Expression*>(call), 0);
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
	Rf_PrintValueRec(const_cast<Expression*>(ctxt->call()), 0);
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
    GCStackRoot<Frame> newframe(CXXR_NEW(VectorFrame));
    GCStackRoot<Environment>
	newrho(CXXR_NEW(Environment(func->environment(), newframe)));
    Frame* tof = newrho->frame();

    // Propagate bindings of the formal arguments of the generic to
    // newrho, but replace defaulted arguments with those appropriate
    // to the method:
    func->matcher()->propagateFormalBindings(callenv, newrho);

    /* copy the bindings of the special dispatch variables in the top
       frame of the generic call to the new frame */
    {
	static const Symbol* syms[]
	    = {DotdefinedSymbol, DotMethodSymbol, DottargetSymbol, 0};
	for (const Symbol** symp = syms; *symp; ++symp) {
	    const Frame::Binding* frombdg = fromf->binding(*symp);
	    Frame::Binding* tobdg = tof->obtainBinding(*symp);
	    tobdg->setValue(frombdg->value());
	}
    }

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    {
	static const Symbol* syms[]
	    = {DotGenericSymbol, DotMethodsSymbol, 0};
	for (const Symbol** symp = syms; *symp; ++symp) {
	    const Frame::Binding* frombdg
		= callenv->findBinding(*symp).second;
	    Frame::Binding* tobdg = tof->obtainBinding(*symp);
	    tobdg->setValue(frombdg->value());
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
    ptmp = tmp = CXXR_NEW(Expression(0, tl));
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


static R_INLINE Rboolean asLogicalNoNA(SEXP s, SEXP call)
{
    Rboolean cond = CXXRCONSTRUCT(Rboolean, NA_LOGICAL);

    if (length(s) > 1)
	Rf_warningcall(call,
		    _("the condition has length > 1 and only the first element will be used"));
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
    if( ENV_DEBUG(rho) ) {
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
	    Evaluator::Context* callctxt
		= Evaluator::Context::innermost()->nextOut();
	    if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT)
		static_cast<Bailout*>(ans)->throwException();
	}
	return ans;
    }
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
	    do_browser(call, op, 0, rho);
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
}

SEXP attribute_hidden do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    volatile int i, n, bgn;
    Rboolean dbg;
    SEXPTYPE val_type;
    GCStackRoot<> ans, v, val;
    GCStackRoot<> argsrt(args), rhort(rho);
    SEXP sym, body;

    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !Rf_isSymbol(sym) ) Rf_errorcall(call, _("non-symbol loop variable"));

#ifdef BYTECODE
    if (R_jit_enabled > 2) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
#endif

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
		Evaluator::Context* callctxt
		    = Evaluator::Context::innermost()->nextOut();
		if (!callctxt
		    || callctxt->type() != Evaluator::Context::BAILOUT)
		    static_cast<Bailout*>(ans.get())->throwException();
		return ans;
	    }
	}
    }
    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    checkArity(op, args);

#ifdef BYTECODE
    if (R_jit_enabled > 2) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
#endif

    dbg = ENV_DEBUG(rho);
    body = CADR(args);
    bgn = BodyHasBraces(body);

    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);

    while (asLogicalNoNA(Rf_eval(CAR(args), rho), call)) {
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
		Evaluator::Context* callctxt
		    = Evaluator::Context::innermost()->nextOut();
		if (!callctxt
		    || callctxt->type() != Evaluator::Context::BAILOUT)
		    static_cast<Bailout*>(ans)->throwException();
		return ans;
	    }
	}
    }
    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean dbg;
    volatile int bgn;
    GCStackRoot<> t;
    volatile SEXP body;

    checkArity(op, args);

#ifdef BYTECODE
    if (R_jit_enabled > 2) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }
#endif

    dbg = ENV_DEBUG(rho);
    body = CAR(args);
    bgn = BodyHasBraces(body);

    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment::LoopScope loopscope(env);
    for (;;) {
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
		Evaluator::Context* callctxt
		    = Evaluator::Context::innermost()->nextOut();
		if (!callctxt
		    || callctxt->type() != Evaluator::Context::BAILOUT)
		    static_cast<Bailout*>(ans)->throwException();
		return ans;
	    }
	}
    }

    SET_ENV_DEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Environment* env = SEXP_downcast<Environment*>(rho);
    if (!env->loopActive())
	Rf_error(_("no loop to break from"));
    LoopBailout* lbo = CXXR_NEW(LoopBailout(env, PRIMVAL(op) == 1));
    Evaluator::Context* callctxt = Evaluator::Context::innermost()->nextOut();
    if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT)
	lbo->throwException();
    return lbo;
}


SEXP attribute_hidden do_paren(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return CAR(args);
}

/* this function gets the srcref attribute from a statement block, 
   and confirms it's in the expected format */
   
static R_INLINE SEXP getSrcrefs(SEXP call, SEXP args)
{
    SEXP srcrefs = Rf_getAttrib(call, R_SrcrefSymbol);
    if (   TYPEOF(srcrefs) == VECSXP
        && length(srcrefs) == length(args)+1 ) return srcrefs;
    return R_NilValue;
}

SEXP attribute_hidden do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;
    if (args != R_NilValue) {
    	GCStackRoot<> srcrefs(getSrcrefs(call, args));
    	int i = 1;
    	R_Srcref = R_NilValue;
	while (args != R_NilValue) {
	    if (srcrefs != R_NilValue) {
	    	R_Srcref = VECTOR_ELT(srcrefs, i++);
	    	if (  TYPEOF(R_Srcref) != INTSXP
	    	    || length(R_Srcref) < 6) {  /* old code will have length 6; new code length 8 */
	    	    srcrefs = R_Srcref = R_NilValue;
	    	}
	    }
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
		Evaluator::Context* callctxt
		    = Evaluator::Context::innermost()->nextOut();
		if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT)
		    static_cast<Bailout*>(s)->throwException();
		R_Srcref = 0;
		return s;
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
    ReturnBailout* rbo = CXXR_NEW(ReturnBailout(envir, v));
    Evaluator::Context* callctxt = Evaluator::Context::innermost()->nextOut();
    if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT)
	rbo->throwException();
    return rbo;
}


SEXP attribute_hidden do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> rval;

    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	SET_NAMED(op, 2);
    }
    if (length(args) < 2)
	WrongArgCount("lambda");
    SEXP formals = CAR(args);
    if (formals && formals->sexptype() != LISTSXP)
	Rf_error(_("invalid formal argument list for 'function'"));
    rval = Rf_mkCLOSXP(formals, CADR(args), rho);
    Rf_setAttrib(rval, R_SourceSymbol, CADDR(args));
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
	GCStackRoot<Expression> nexpr(CXXR_NEW(Expression(exprn->car(), nexprargs)));
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

// Functions and data auxiliary to applydefine():
namespace {
    std::map<const Symbol*, Symbol*> sym2replac;

    // Given a Symbol "foo", this function returns the Symbol "foo<-":
    Symbol* func2ReplacementFunc(const Symbol* fsym)
    {
	typedef std::map<const Symbol*, Symbol*> Fmap;
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
	SETCAR(args, Rf_install(Rf_translateChar(STRING_ELT(CAR(args), 0))));
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
		SET_TAG(xptr, Rf_install(Rf_translateChar(STRING_ELT(xnames, i))));
		xptr = CDR(xptr);
	    }
	}
	UNPROTECT(1);
    } else xnew = Rf_allocList(0);
    UNPROTECT(2);
    return xnew;
}

#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)

/* "eval" and "eval.with.vis" : Evaluate the first argument */
/* in the environment specified by the second argument. */

SEXP attribute_hidden do_eval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP encl, x, xptr;
    volatile SEXP expr, env, tmp;

    int frame;

    checkArity(op, args);
    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    if (Rf_isNull(encl)) {
	/* This is supposed to be defunct, but has been kept here
	   (and documented as such) */
	encl = R_BaseEnv;
    } else if ( !Rf_isEnvironment(encl) &&
		!Rf_isEnvironment((encl = simple_as_environment(encl))) )
	Rf_error(_("invalid '%s' argument"), "enclos");
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* usually an ENVSXP */
    switch(TYPEOF(env)) {
    case NILSXP:
	env = encl;     /* soeval(expr, NULL, encl) works */
	/* falls through */
    case ENVSXP:
	/* This usage requires all the pairlist to be named */
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
	    Rf_error(_("invalid '%s' argument"), "envir");
	PROTECT(env = R_sysframe(frame, ClosureContext::innermost()));
	break;
    default:
	Rf_error(_("invalid '%s' argument"), "envir");
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
		tmp = rx.value();
	    }
	}
	UNPROTECT(1);
    }
    else if (TYPEOF(expr) == EXPRSXP) {
	int i, n;
	PROTECT(expr);
	n = LENGTH(expr);
	tmp = R_NilValue;
	{
	    Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* call_env = SEXP_downcast<Environment*>(rho);
	    FunctionBase* func = SEXP_downcast<FunctionBase*>(op);
	    Environment* working_env = SEXP_downcast<Environment*>(env);
	    PairList* promargs = SEXP_downcast<PairList*>(args);
	    ClosureContext cntxt(callx, call_env, func, working_env, promargs);
	    Environment::ReturnScope returnscope(working_env);
	    try {
		for (i = 0 ; i < n ; i++)
		    tmp = Rf_eval(XVECTOR_ELT(expr, i), env);
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
    if (PRIMVAL(op)) { /* eval.with.vis(*) : */
	PROTECT(expr);
	PROTECT(env = CXXR_NEW(ListVector(2)));
	PROTECT(encl = CXXR_NEW(StringVector(2)));
	SET_STRING_ELT(encl, 0, Rf_mkChar("value"));
	SET_STRING_ELT(encl, 1, Rf_mkChar("visible"));
	SET_VECTOR_ELT(env, 0, expr);
	SET_VECTOR_ELT(env, 1, Rf_ScalarLogical(R_Visible));
	Rf_setAttrib(env, R_NamesSymbol, encl);
	expr = env;
	UNPROTECT(3);
    }
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
    PROTECT(ret = CXXR_NEW(ListVector(2)));
    PROTECT(nm = CXXR_NEW(StringVector(2)));
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
    if (cptr == NULL)
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
	char* suffix = 0;
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
	    GCStackRoot<Frame> frame(CXXR_NEW(VectorFrame));
	    Environment* working_env = CXXR_NEW(Environment(callenv, frame));
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
    RObject* arg2val = (numargs > 1 ? callargs->tail()->car() : 0);
    
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

    unsigned int nargs = (isOps ? numargs : 1);
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
	string lname = l->symbol()->name()->stdstring();
	string rname = r->symbol()->name()->stdstring();
	if (rname == "Ops.difftime"
	    && (lname == "+.POSIXt" || lname == "-.POSIXt"
		|| lname == "+.Date" || lname == "-.Date"))
	    r = 0;
	else if (lname == "Ops.difftime"
		 && (rname == "+.POSIXt" || rname == "+.Date"))
	    l = 0;
	else {
	    Rf_warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
		       lname.c_str(), rname.c_str(), generic.c_str());
	    return 0;
	}
    }

    S3Launcher* m = (l ? l : r);  // m is the method that will be applied.

    /* we either have a group method or a class method */

    GCStackRoot<Frame> supp_frame(CXXR_NEW(VectorFrame));
    // Set up special method bindings:
    m->addMethodBindings(supp_frame);

    if (isOps) {
	// Rebind .Method:
	GCStackRoot<StringVector> dotmethod(CXXR_NEW(StringVector(2)));
	(*dotmethod)[0] = (l 
			   ? const_cast<CachedString*>(l->symbol()->name())
			   : CachedString::blank());
	(*dotmethod)[1] = (r
			   ? const_cast<CachedString*>(r->symbol()->name())
			   : CachedString::blank());
	supp_frame->bind(DotMethodSymbol, dotmethod);
    }

    /* the arguments have been evaluated; since we are passing them */
    /* out to a closure we need to wrap them in promises so that */
    /* they get duplicated and things like missing/substitute work. */
    {
	GCStackRoot<Expression>
	    newcall(CXXR_NEW(Expression(m->symbol(), callx->tail())));
	ArgList arglist(callargs, ArgList::EVALUATED);
	arglist.wrapInPromises(0);
	// Ensure positional matching for operators:
	if (isOps)
	    arglist.stripTags();
	Closure* func = SEXP_downcast<Closure*>(m->function());
	*ans = func->invoke(callenv, &arglist, newcall, supp_frame);
    }
    return 1;
}

#ifdef BYTECODE
static int R_bcVersion = 6;
static int R_bcMinVersion = 6;

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
static SEXP R_TrueValue = NULL;
static SEXP R_FalseValue = NULL;

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
  NVECELT_OP,
  NMATELT_OP,
  SETNVECELT_OP,
  SETNMATELT_OP,
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
#define NODESTACKEND (s_nodestack->end())

#define DO_FAST_RELOP2(op,a,b) do { \
    double __a__ = (a), __b__ = (b); \
    SEXP val; \
    SKIP_OP(); \
    if (ISNAN(__a__) || ISNAN(__b__)) val = Rf_ScalarLogical(NA_LOGICAL); \
    else val = (__a__ op __b__) ? R_TrueValue : R_FalseValue; \
    NODESTACKEND[-2] = val; \
    s_nodestack->pop(); \
    NEXT(); \
} while (0)

# define FastRelop2(op,opval,opsym) do { \
    SEXP x = NODESTACKEND[-2]; \
    SEXP y = NODESTACKEND[-1]; \
    if (ATTRIB(x) == R_NilValue && ATTRIB(y) == R_NilValue) { \
	if (TYPEOF(x) == REALSXP && LENGTH(x) == 1 && \
	    TYPEOF(y) == REALSXP && LENGTH(y) == 1) \
	    DO_FAST_RELOP2(op, REAL(x)[0], REAL(y)[0]); \
	else if (TYPEOF(x) == INTSXP && LENGTH(x) == 1 && \
		 TYPEOF(y) == REALSXP && LENGTH(y) == 1) { \
	    double xd = INTEGER(x)[0] == NA_INTEGER ? NA_REAL : INTEGER(x)[0];\
	    DO_FAST_RELOP2(op, xd, REAL(y)[0]); \
	} \
	else if (TYPEOF(x) == REALSXP && LENGTH(x) == 1 && \
		 TYPEOF(y) == INTSXP && LENGTH(y) == 1) { \
	    double yd = INTEGER(y)[0] == NA_INTEGER ? NA_REAL : INTEGER(y)[0];\
	    DO_FAST_RELOP2(op, REAL(x)[0], yd); \
	} \
	else if (TYPEOF(x) == INTSXP && LENGTH(x) == 1 && \
		 TYPEOF(y) == INTSXP && LENGTH(y) == 1) { \
	    double xd = INTEGER(x)[0] == NA_INTEGER ? NA_REAL : INTEGER(x)[0];\
	    double yd = INTEGER(y)[0] == NA_INTEGER ? NA_REAL : INTEGER(y)[0];\
	    DO_FAST_RELOP2(op, xd, yd); \
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

#define Builtin1(do_fun,which,rho) do {				 \
  SEXP call = (*constants)[GETOP()]; \
  NODESTACKEND[-1] = PairList::cons(NODESTACKEND[-1]);		\
  NODESTACKEND[-1] = do_fun(call, getPrimitive(which, BUILTINSXP),	 \
				NODESTACKEND[-1], rho); \
  NEXT(); \
} while(0)

#define Builtin2(do_fun,which,rho) do {		     \
  SEXP call = (*constants)[GETOP()]; \
  PairList* tmp = PairList::cons(NODESTACKEND[-1]);	\
  NODESTACKEND[-2] = PairList::cons(NODESTACKEND[-2], tmp);	\
  s_nodestack->pop(); \
  NODESTACKEND[-1] = do_fun(call, getPrimitive(which, BUILTINSXP), \
				NODESTACKEND[-1], rho); \
  NEXT(); \
} while(0)

#define NewBuiltin2(do_fun,opval,opsym,rho) do {        \
  SEXP call = (*constants)[GETOP()]; \
  SEXP x = NODESTACKEND[-2]; \
  SEXP y = NODESTACKEND[-1]; \
  NODESTACKEND[-2] = do_fun(call, opval, opsym, x, y,rho);  \
  s_nodestack->pop(); \
  NEXT(); \
} while(0)

#define Arith1(opsym) do {		\
  SEXP call = (*constants)[GETOP()]; \
  SEXP x = NODESTACKEND[-1]; \
  NODESTACKEND[-1] = cmp_arith1(call, opsym, x, rho);	\
  NEXT(); \
} while(0)


#define Arith2(opval,opsym) NewBuiltin2(cmp_arith2,opval,opsym,rho)
#define Math1(which) Builtin1(do_math1,which,rho)
#define Relop2(opval,opsym) NewBuiltin2(cmp_relop,opval,opsym,rho)

# define DO_FAST_BINOP(op,a,b) do { \
    RealVector* val = CXXR_NEW(RealVector(1)); \
    SKIP_OP(); \
    (*val)[0] = (a) op (b); \
    NODESTACKEND[-2] = val; \
    s_nodestack->pop(); \
    NEXT(); \
} while (0)
# define FastBinary(op,opval,opsym) do { \
    SEXP x = NODESTACKEND[-2]; \
    SEXP y = NODESTACKEND[-1]; \
    if (ATTRIB(x) == R_NilValue && ATTRIB(y) == R_NilValue) { \
	if (TYPEOF(x) == REALSXP && LENGTH(x) == 1 && \
	    TYPEOF(y) == REALSXP && LENGTH(y) == 1) \
	    DO_FAST_BINOP(op, REAL(x)[0], REAL(y)[0]); \
	else if (TYPEOF(x) == INTSXP && LENGTH(x) == 1 && \
		 INTEGER(x)[0] != NA_INTEGER && \
		 TYPEOF(y) == REALSXP && LENGTH(y) == 1) \
	    DO_FAST_BINOP(op, INTEGER(x)[0], REAL(y)[0]); \
	else if (TYPEOF(x) == REALSXP && LENGTH(x) == 1 && \
		 TYPEOF(y) == INTSXP && LENGTH(y) == 1 && \
		 INTEGER(y)[0] != NA_INTEGER) \
	    DO_FAST_BINOP(op, REAL(x)[0], INTEGER(y)[0]); \
    } \
    Arith2(opval, opsym); \
} while (0)

#define BCNPUSH(v) (s_nodestack->push(v))

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
#define LASTOP  default: Rf_error(_("Bad opcode"))
#define INITIALIZE_MACHINE()

#define NEXT() goto loop
#define GETOP() *pc++

#endif

#define SKIP_OP() (pc++)

static R_INLINE SEXP getvar(SEXP symbol, SEXP rho,
			    Rboolean dd, Rboolean keepmiss)
{
    SEXP value = (dd) ? Rf_ddfindVar(symbol, rho) : Rf_findVar(symbol, rho);
    if (value == R_UnboundValue)
	Rf_error(_("object '%s' not found"), CHAR(PRINTNAME(symbol)));
    else if (value == R_MissingArg) {
	if (! keepmiss) {
	    const char *n = CHAR(PRINTNAME(symbol));
	    if(*n) Rf_error(_("argument \"%s\" is missing, with no default"), n);
	    else Rf_error(_("argument is missing, with no default"));
	}
    }
    else if (TYPEOF(value) == PROMSXP) {
	if (PRVALUE(value) == R_UnboundValue) {
	    /**** R_isMissing is inefficient */
	    if (keepmiss && R_isMissing(symbol, rho))
		value = R_MissingArg;
	    else value = forcePromise(value);
	}
	else value = PRVALUE(value);
	SET_NAMED(value, 2);
    }
    else if (!Rf_isNull(value) && NAMED(value) < 1)
	SET_NAMED(value, 1);
    return value;
}

#define DO_GETVAR(dd,keepmiss) do { \
  SEXP symbol = (*constants)[GETOP()]; \
  R_Visible = TRUE; \
  BCNPUSH(getvar(symbol, rho, dd, keepmiss));	\
  NEXT(); \
} while (0)

#define PUSHCALLARG(v) PUSHCALLARG_CELL(PairList::cons(v))

#define PUSHCALLARG_CELL(c) do { \
  SEXP __cell__ = (c); \
  if (NODESTACKEND[-2] == R_NilValue) NODESTACKEND[-2] = __cell__; \
  else SETCDR(NODESTACKEND[-1], __cell__); \
  NODESTACKEND[-1] = __cell__; \
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
  PROTECT(rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, rho));
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

static int tryAssignDispatch(char *generic, SEXP call, SEXP lhs, SEXP rhs,
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
  value = NODESTACKEND[-1]; \
  if (Rf_isObject(value) && tryDispatch(generic, call, value, rho, &value)) {\
    NODESTACKEND[-1] = value; \
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
  SEXP call = NODESTACKEND[-3]; \
  SEXP args = NODESTACKEND[-2]; \
  value = fun(call, symbol, args, rho); \
  s_nodestack->pop(3);	\
  NODESTACKEND[-1] = value; \
  NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH(generic) do { \
  SEXP call = (*constants)[GETOP()]; \
  int label = GETOP(); \
  SEXP lhs = NODESTACKEND[-2]; \
  SEXP rhs = NODESTACKEND[-1]; \
  if (NAMED(lhs) == 2) { \
    lhs = NODESTACKEND[-2] = Rf_duplicate(lhs); \
    SET_NAMED(lhs, 1); \
  } \
  if (Rf_isObject(lhs) && \
      tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
    s_nodestack->pop();	\
    NODESTACKEND[-1] = value; \
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
  SEXP rhs = NODESTACKEND[-4]; \
  SEXP call = NODESTACKEND[-3]; \
  SEXP args = NODESTACKEND[-2]; \
  PUSHCALLARG(rhs); \
  value = fun(call, symbol, args, rho); \
  s_nodestack->pop(4);	\
  NODESTACKEND[-1] = value; \
  NEXT(); \
} while (0)

#define DO_ISTEST(fun) do { \
  NODESTACKEND[-1] = fun(NODESTACKEND[-1]) ? \
			 R_TrueValue : R_FalseValue; \
  NEXT(); \
} while(0)
#define DO_ISTYPE(type) do { \
  NODESTACKEND[-1] = TYPEOF(NODESTACKEND[-1]) == type ? \
			 Rf_mkTrue() : Rf_mkFalse(); \
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
	    code->evaluate(rho);
	}
	catch (LoopException& lx) {
	    if (lx.environment() != rho)
		throw;
	    redo = lx.next();
	}
    } while (redo);
}

static void checkVectorSubscript(SEXP vec, int k)
{
    switch (TYPEOF(vec)) {
    case REALSXP:
    case INTSXP:
    case LGLSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	if (k < 0 || k >= LENGTH(vec))
	    Rf_error(_("subscript out of bounds"));
	break;
    default: Rf_error(_("not a vector object"));
    }
}

static SEXP numVecElt(SEXP vec, SEXP idx)
{
    int i = Rf_asInteger(idx) - 1;
    if (OBJECT(vec))
	Rf_error(_("can only handle simple real vectors"));
    checkVectorSubscript(vec, i);
    switch (TYPEOF(vec)) {
    case REALSXP: return Rf_ScalarReal(REAL(vec)[i]);
    case INTSXP: return Rf_ScalarInteger(INTEGER(vec)[i]);
    case LGLSXP: return Rf_ScalarLogical(LOGICAL(vec)[i]);
    case CPLXSXP: return Rf_ScalarComplex(COMPLEX(vec)[i]);
    case RAWSXP: return Rf_ScalarRaw(RAW(vec)[i]);
    default:
	Rf_error(_("not a simple vector"));
	return R_NilValue; /* keep -Wall happy */
    }
}

static SEXP numMatElt(SEXP mat, SEXP idx, SEXP jdx)
{
    SEXP dim;
    int k, nrow;
    int i = Rf_asInteger(idx);
    int j = Rf_asInteger(jdx);

    if (OBJECT(mat))
	Rf_error(_("can only handle simple real vectors"));

    dim = Rf_getAttrib(mat, R_DimSymbol);
    if (mat == R_NilValue || TYPEOF(dim) != INTSXP || LENGTH(dim) != 2)
	Rf_error(_("incorrect number of subscripts"));
    nrow = INTEGER(dim)[0];
    k = i - 1 + nrow * (j - 1);
    checkVectorSubscript(mat, k);

    switch (TYPEOF(mat)) {
    case REALSXP: return Rf_ScalarReal(REAL(mat)[k]);
    case INTSXP: return Rf_ScalarInteger(INTEGER(mat)[k]);
    case LGLSXP: return Rf_ScalarLogical(LOGICAL(mat)[k]);
    case CPLXSXP: return Rf_ScalarComplex(COMPLEX(mat)[k]);
    default:
	Rf_error(_("not a simple matrix"));
	return R_NilValue; /* keep -Wall happy */
    }
}

static SEXP setNumVecElt(SEXP vec, SEXP idx, SEXP value)
{
    int i = Rf_asInteger(idx) - 1;
    if (OBJECT(vec))
	Rf_error(_("can only handle simple real vectors"));
    checkVectorSubscript(vec, i);
    if (NAMED(vec) > 1)
	vec = Rf_duplicate(vec);
    PROTECT(vec);
    switch (TYPEOF(vec)) {
    case REALSXP: REAL(vec)[i] = Rf_asReal(value); break;
    case INTSXP: INTEGER(vec)[i] = Rf_asInteger(value); break;
    case LGLSXP: LOGICAL(vec)[i] = Rf_asLogical(value); break;
    case CPLXSXP: COMPLEX(vec)[i] = Rf_asComplex(value); break;
    default: Rf_error(_("not a simple vector"));
    }
    UNPROTECT(1);
    return vec;
}

static SEXP setNumMatElt(SEXP mat, SEXP idx, SEXP jdx, SEXP value)
{
    SEXP dim;
    int k, nrow;
    int i = Rf_asInteger(idx);
    int j = Rf_asInteger(jdx);

    if (OBJECT(mat))
	Rf_error(_("can only handle simple real vectors"));

    dim = Rf_getAttrib(mat, R_DimSymbol);
    if (mat == R_NilValue || TYPEOF(dim) != INTSXP || LENGTH(dim) != 2)
	Rf_error(_("incorrect number of subscripts"));
    nrow = INTEGER(dim)[0];
    k = i - 1 + nrow * (j - 1);
    checkVectorSubscript(mat, k);

    if (NAMED(mat) > 1)
	mat = Rf_duplicate(mat);

    PROTECT(mat);
    switch (TYPEOF(mat)) {
    case REALSXP: REAL(mat)[k] = Rf_asReal(value); break;
    case INTSXP: INTEGER(mat)[k] = Rf_asInteger(value); break;
    case LGLSXP: LOGICAL(mat)[k] = Rf_asLogical(value); break;
    case CPLXSXP: COMPLEX(mat)[k] = Rf_asComplex(value); break;
    default: Rf_error(_("not a simple matrix"));
    }
    UNPROTECT(1);
    return mat;
}

#define FIXUP_SCALAR_LOGICAL(callidx, arg, op) do { \
	SEXP val = NODESTACKEND[-1]; \
	if (TYPEOF(val) != LGLSXP || LENGTH(val) != 1) { \
	    if (!Rf_isNumber(val))	\
		Rf_errorcall(VECTOR_ELT(constants, callidx), \
			  _("invalid %s type in 'x %s y'"), arg, op);	\
	    NODESTACKEND[-1] = Rf_ScalarLogical(Rf_asLogical(val)); \
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

RObject* ByteCode::interpret(ByteCode* bcode, Environment* rho)
{
  Scope scope;
  std::vector<Frame::Binding*> binding_stack;
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

  BEGIN_MACHINE {
    OP(BCMISMATCH, 0): Rf_error(_("byte code version mismatch"));
    OP(RETURN, 0): value = NODESTACKEND[-1]; goto done;
    OP(GOTO, 1):
      {
	int label = GETOP();
	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(BRIFNOT, 2):
      {
	SEXP call = (*constants)[GETOP()];
	int label = GETOP();
	int cond;
	value = BCNPOP();
	/**** should probably inline this and avoid looking up call
	      unless it is needed */
	cond = asLogicalNoNA(value, call);
	if (! cond) {
	    BC_CHECK_SIGINT();
	    pc = codebase + label;
	}
	NEXT();
      }
    OP(POP, 0):
	BCNPOP_IGNORE_VALUE();
	NEXT();
    OP(DUP, 0):
	value = NODESTACKEND[-1];
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
	SEXP seq = NODESTACKEND[-1];
	int callidx = GETOP();
	SEXP symbol = (*constants)[GETOP()];
	int label = GETOP();

	/* if we are iterating over a factor, coerce to character first */
	if (Rf_inherits(seq, "factor")) {
	    seq = Rf_asCharacterFactor(seq);
	    NODESTACKEND[-1] = seq;
	}

	Rf_defineVar(symbol, R_NilValue, rho);

	// Here CR casts the return value of R_findVarLocInFrame (of
	// type R_varloc_t, which is Frame::Binding* in CXXR) into
	// a SEXP and pushes it onto the node stack.  In CXXR we use a
	// separate stack for binding, but push a null pointer onto
	// the node stack to maintain CR's stack alignment.
	BCNPUSH(0);
	binding_stack.push_back(R_findVarLocInFrame(rho, symbol));

	value = CXXR_NEW(IntVector(2));
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

	BCNPUSH(R_NilValue);

	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(STEPFOR, 1):
      {
	int label = GETOP();
	int i = ++(INTEGER(NODESTACKEND[-2])[0]);
	int n = INTEGER(NODESTACKEND[-2])[1];
	if (i < n) {
	  SEXP seq = NODESTACKEND[-4];
	  Frame::Binding* cell = binding_stack.back();
	  switch (TYPEOF(seq)) {
	  case LGLSXP:
	      {
		  LogicalVector* lv = CXXR_NEW(LogicalVector(1));
		  (*lv)[0] = (*static_cast<LogicalVector*>(seq))[i];
		  value = lv;
	      }
	      break;
	  case INTSXP:
	      {
		  IntVector* iv = CXXR_NEW(IntVector(1));
		  (*iv)[0] = (*static_cast<IntVector*>(seq))[i];
		  value = iv;
	      }
	      break;
	  case REALSXP:
	      {
		  RealVector* rv = CXXR_NEW(RealVector(1));
		  (*rv)[0] = (*static_cast<RealVector*>(seq))[i];
		  value = rv;
	      }
	      break;
	  case CPLXSXP:
	      {
		  ComplexVector* cv = CXXR_NEW(ComplexVector(1));
		  (*cv)[0] = (*static_cast<ComplexVector*>(seq))[i];
		  value = cv;
	      }
	      break;
	  case STRSXP:
	      {
		  StringVector* sv = CXXR_NEW(StringVector(1));
		  (*sv)[0] = (*static_cast<StringVector*>(seq))[i];
		  value = sv;
	      }
	      break;
	  case RAWSXP:
	      {
		  RawVector* rv = CXXR_NEW(RawVector(1));
		  (*rv)[0] = (*static_cast<RawVector*>(seq))[i];
		  value = rv;
	      }
	      break;
	  case EXPRSXP:
	      value = (*static_cast<ExpressionVector*>(seq))[i];
	      break;
	  case VECSXP:
	      value = (*static_cast<ListVector*>(seq))[i];
	      break;
	  case LISTSXP:
	    value = CAR(seq);
	    NODESTACKEND[-4] = CDR(seq);
	    break;
	  default:
	    Rf_error(_("invalid sequence argument in for loop"));
	  }
	  R_SetVarLocValue(cell, value);
	  BC_CHECK_SIGINT();
	  pc = codebase + label;
	}
	NEXT();
      }
    OP(ENDFOR, 0):
      {
	value = NODESTACKEND[-1];
	s_nodestack->pop(3);
	binding_stack.pop_back();
	NODESTACKEND[-1] = value;
	NEXT();
      }
    OP(SETLOOPVAL, 0):
	BCNPOP_IGNORE_VALUE();
	NODESTACKEND[-1] = R_NilValue;
	NEXT();
    OP(INVISIBLE,0):
	R_Visible = FALSE;
	NEXT();
    /**** for now LDCONST, LDTRUE, and LDFALSE duplicate/allocate to
	  be defensive against bad package C code */
    OP(LDCONST, 1):
	R_Visible = TRUE;
	value = (*constants)[GETOP()];
	/* make sure NAMED = 2 -- lower values might be safe in some
	   cases but not ingeneral, especially if the ocnstant pool
	   was created by unserializing a compiled expression. */
	if (NAMED(value) < 2) SET_NAMED(value, 2);
	BCNPUSH(Rf_duplicate(value));
	NEXT();
    OP(LDNULL, 0):
	R_Visible = TRUE;
	BCNPUSH(R_NilValue);
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
	SEXP symbol = (*constants)[GETOP()];
	value = NODESTACKEND[-1];
	switch (NAMED(value)) {
	case 0: SET_NAMED(value, 1); break;
	case 1: SET_NAMED(value, 2); break;
	}
	Rf_defineVar(symbol, value, rho);
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
	  Rf_error(_("not a BUILTIN function"));

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
	value = NODESTACKEND[-1];
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
	SEXP cell = NODESTACKEND[-1];
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
	PUSHCALLARG(R_NilValue);
	NEXT();
    OP(PUSHTRUEARG, 0):
	PUSHCALLARG(Rf_mkTrue());
	NEXT();
    OP(PUSHFALSEARG, 0):
	PUSHCALLARG(Rf_mkFalse());
	NEXT();
    OP(CALL, 1):
      {
	SEXP fun = NODESTACKEND[-3];
	SEXP call = (*constants)[GETOP()];
	SEXP args = NODESTACKEND[-2];
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
	NODESTACKEND[-1] = value;
	ftype = 0;
	NEXT();
      }
    OP(CALLBUILTIN, 1):
      {
	SEXP fun = NODESTACKEND[-3];
	SEXP call = (*constants)[GETOP()];
	SEXP args = NODESTACKEND[-2];
	if (TYPEOF(fun) != BUILTINSXP)
	  Rf_error(_("not a BUILTIN function"));
	// Inner block because destructor of ArgList must be called
	// before NEXT():
	{
	    const BuiltInFunction* func = static_cast<BuiltInFunction*>(fun);
	    const Expression* callx = SEXP_downcast<Expression*>(call);
	    Environment* env = SEXP_downcast<Environment*>(rho);
	    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
	    value = func->apply(&arglist, env, callx);
	}
	s_nodestack->pop(2);
	NODESTACKEND[-1] = value;
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
	NODESTACKEND[-1] = value; /* replaces fun on stack */
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
	SEXP symbol = (*constants)[GETOP()];
	value = NODESTACKEND[-1];
	BCNPUSH(EnsureLocal(symbol, rho));
	BCNPUSH(value);
	/* top three stack entries are now RHS value, LHS value, RHS value */
	NEXT();
      }
    OP(ENDASSIGN, 1):
      {
	SEXP symbol = (*constants)[GETOP()];
	value = BCNPOP();
	switch (NAMED(value)) {
	case 0: SET_NAMED(value, 1); break;
	case 1: SET_NAMED(value, 2); break;
	}
	Rf_defineVar(symbol, value, rho);
	/* original right-hand side value is now on top of stack again */
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = 2 */
	SET_NAMED(NODESTACKEND[-1], 2);
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
	SEXP x = NODESTACKEND[-1];
	if (Rf_isObject(x)) {
	    SEXP ncall;
	    PROTECT(ncall = Rf_duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), Rf_ScalarString(PRINTNAME(symbol)));
	    dispatched = tryDispatch("$", ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (dispatched)
	  NODESTACKEND[-1] = value;
	else
	    NODESTACKEND[-1] = R_subset3_dflt(x, PRINTNAME(symbol), R_NilValue);
	NEXT();
      }
    OP(DOLLARGETS, 2):
      {
	int dispatched = FALSE;
	SEXP call = (*constants)[GETOP()];
	SEXP symbol = (*constants)[GETOP()];
	SEXP x = NODESTACKEND[-2];
	SEXP rhs = NODESTACKEND[-1];
	if (NAMED(x) == 2) {
	  x = NODESTACKEND[-2] = Rf_duplicate(x);
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
	NODESTACKEND[-1] = value;
	NEXT();
      }
    OP(ISNULL, 0): DO_ISTEST(Rf_isNull);
    OP(ISLOGICAL, 0): DO_ISTYPE(LGLSXP);
    OP(ISINTEGER, 0): {
	SEXP arg = NODESTACKEND[-1];
	bool test = (TYPEOF(arg) == INTSXP) && ! Rf_inherits(arg, "factor");
	NODESTACKEND[-1] = test ? Rf_mkTrue() : Rf_mkFalse();
	NEXT();
      }
    OP(ISDOUBLE, 0): DO_ISTYPE(REALSXP);
    OP(ISCOMPLEX, 0): DO_ISTYPE(CPLXSXP);
    OP(ISCHARACTER, 0): DO_ISTYPE(STRSXP);
    OP(ISSYMBOL, 0): DO_ISTYPE(SYMSXP); /**** S4 thingy allowed now???*/
    OP(ISOBJECT, 0): DO_ISTEST(OBJECT);
    OP(ISNUMERIC, 0): DO_ISTEST(isNumericOnly);
    OP(NVECELT, 0): {
	SEXP vec = NODESTACKEND[-2];
	SEXP idx = NODESTACKEND[-1];
	value = numVecElt(vec, idx);
	s_nodestack->pop();
	NODESTACKEND[-1] = value;
	NEXT();
    }
    OP(NMATELT, 0): {
	SEXP mat = NODESTACKEND[-3];
	SEXP idx = NODESTACKEND[-2];
	SEXP jdx = NODESTACKEND[-1];
	value = numMatElt(mat, idx, jdx);
	s_nodestack->pop(2);
	NODESTACKEND[-1] = value;
	NEXT();
    }
    OP(SETNVECELT, 0): {
	SEXP vec = NODESTACKEND[-3];
	SEXP idx = NODESTACKEND[-2];
	value = NODESTACKEND[-1];
	value = setNumVecElt(vec, idx, value);
	s_nodestack->pop(2);
	NODESTACKEND[-1] = value;
	NEXT();
    }
    OP(SETNMATELT, 0): {
	SEXP mat = NODESTACKEND[-4];
	SEXP idx = NODESTACKEND[-3];
	SEXP jdx = NODESTACKEND[-2];
	value = NODESTACKEND[-1];
	value = setNumMatElt(mat, idx, jdx, value);
	s_nodestack->pop(3);
	NODESTACKEND[-1] = value;
	NEXT();
    }
    OP(AND1ST, 2): {
	int callidx = GETOP();
	int label = GETOP();
        FIXUP_SCALAR_LOGICAL(callidx, "'x'", "&&");
        value = NODESTACKEND[-1];
	if (LOGICAL(value)[0] == FALSE)
	    pc = codebase + label;
	NEXT();
    }
    OP(AND2ND, 1): {
	int callidx = GETOP();
	FIXUP_SCALAR_LOGICAL(callidx, "'y'", "&&");
        value = NODESTACKEND[-1];
	/* The first argument is TRUE or NA. If the second argument is
	   not TRUE then its value is the result. If the second
	   argument is TRUE, then the first argument's value is the
	   result. */
	if (LOGICAL(value)[0] != TRUE)
	    NODESTACKEND[-2] = value;
	s_nodestack->pop();
	NEXT();
    }
    OP(OR1ST, 2):  {
	int callidx = GETOP();
	int label = GETOP();
        FIXUP_SCALAR_LOGICAL(callidx, "'x'", "||");
        value = NODESTACKEND[-1];
	if (LOGICAL(value)[0] != NA_LOGICAL && LOGICAL(value)[0]) /* is true */
	    pc = codebase + label;
	NEXT();
    }
    OP(OR2ND, 1):  {
	int callidx = GETOP();
	FIXUP_SCALAR_LOGICAL(callidx, "'y'", "||");
        value = NODESTACKEND[-1];
	/* The first argument is FALSE or NA. If the second argument is
	   not FALSE then its value is the result. If the second
	   argument is FALSE, then the first argument's value is the
	   result. */
	if (LOGICAL(value)[0] != FALSE)
	    NODESTACKEND[-2] = value;
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
	value = NODESTACKEND[-1];
	if (NAMED(value)) {
	    value = Rf_duplicate(value);
	    NODESTACKEND[-1] = value;
	}
	Rf_setVar(symbol, value, ENCLOS(rho));
	NEXT();
      }
    OP(STARTASSIGN2, 1):
      {
	SEXP symbol = (*constants)[GETOP()];
	value = NODESTACKEND[-1];
	BCNPUSH(getvar(symbol, ENCLOS(rho), FALSE, FALSE));
	BCNPUSH(value);
	/* top three stack entries are now RHS value, LHS value, RHS value */
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
	SET_NAMED(NODESTACKEND[-1], 2);
	NEXT();
      }
    OP(SETTER_CALL, 2):
      {
        SEXP lhs = NODESTACKEND[-5];
        SEXP rhs = NODESTACKEND[-4];
	SEXP fun = NODESTACKEND[-3];
	SEXP call = (*constants)[GETOP()];
	SEXP vexpr = (*constants)[GETOP()];
	SEXP args, prom, last;
	if (NAMED(lhs) == 2) {
	  lhs = NODESTACKEND[-5] = Rf_duplicate(lhs);
	  SET_NAMED(lhs, 1);
	}
	switch (ftype) {
	case BUILTINSXP:
	  /* push RHS value onto arguments with 'value' tag */
	  PUSHCALLARG(rhs);
	  SET_TAG(NODESTACKEND[-1], Rf_install("value"));
	  /* replace first argument with LHS value */
	  args = NODESTACKEND[-2];
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = NODESTACKEND[-2] = Rf_duplicate(CDR(call));
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
	  SET_TAG(NODESTACKEND[-1], Rf_install("value"));
	  /* replace first argument with evaluated promise for LHS */
          prom = Rf_mkPROMISE(R_TmpvalSymbol, rho);
	  SET_PRVALUE(prom, lhs);
	  args = NODESTACKEND[-2];
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
	NODESTACKEND[-1] = value;
	ftype = 0;
	NEXT();
      }
    OP(GETTER_CALL, 1):
      {
        SEXP lhs = NODESTACKEND[-5];
	SEXP fun = NODESTACKEND[-3];
	SEXP call = (*constants)[GETOP()];
	SEXP args, prom;
	switch (ftype) {
	case BUILTINSXP:
	  /* replace first argument with LHS value */
	  args = NODESTACKEND[-2];
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = NODESTACKEND[-2] = Rf_duplicate(CDR(call));
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
	  args = NODESTACKEND[-2];
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
	NODESTACKEND[-1] = value;
	ftype = 0;
	NEXT();
      }
    OP(SWAP, 0): {
	value = NODESTACKEND[-1];
	NODESTACKEND[-1] = NODESTACKEND[-2];
	NODESTACKEND[-2] = value;
	NEXT();
    }
    OP(DUP2ND, 0): {
	value = NODESTACKEND[-2];
	BCNPUSH(value);
	NEXT();
    }
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
	       Rf_errorcall(call, _("numeric EXPR required for switch() "
				 "without named alternatives"));
	   if (TYPEOF(coffsets) != INTSXP)
	       Rf_errorcall(call, _("bad character switch offsets"));
	   if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(coffsets))
	       Rf_errorcall(call, _("bad switch names"));
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
	       Rf_errorcall(call, _("bad numeric switch offsets"));
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
    return CXXR_NEW(ByteCode(enc, pl));
}

SEXP attribute_hidden do_bcclose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP forms, body, env;

    checkArity(op, args);
    forms = CAR(args);
    body = CADR(args);
    env = CADDR(args);

    if (! isByteCode(body))
	Rf_errorcall(call, _("invalid environment"));

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
  SEXP expr = 0;  // Set to BCODE_EXPR(bc) in CR
  int nc = LENGTH(consts);

  PROTECT(ans = Rf_allocVector(VECSXP, expr != R_NilValue ? 4 : 3));
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
	    Rf_error(_("R_CompiledFileName: buffer too small"));
	return buf;
    }
    else if (ext == NULL) {
	/* if the requested file has no extention, make a name that
	   has the extenrion added on to the expanded name */
	if (snprintf(buf, bsize, "%s%s", fname, R_COMPILED_EXTENSION) < 0)
	    Rf_error(_("R_CompiledFileName: buffer too small"));
	return buf;
    }
    else {
	/* the supplied file already has an extention, so there is no
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
	Rf_error(_("constBuf must be a generic vector"));

    constCount = Rf_asInteger(CADR(args));
    if (constCount < 0 || constCount >= LENGTH(constBuf))
	Rf_error(_("bad constCount value"));

    x = CADDR(args);

    /* check for a match and return index if one is found */
    for (i = 0; i < constCount; i++) {
	SEXP y = VECTOR_ELT(constBuf, i);
	if (x == y || R_compute_identical(x, y, TRUE, TRUE, TRUE))
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
#else
SEXP R_getbcprofcounts() { return R_NilValue; }
SEXP R_startbcprof() { return R_NilValue; }
SEXP R_stopbcprof() { return R_NilValue; }
#endif
#endif

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
