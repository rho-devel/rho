/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* <UTF8> char here is handled as a whole */

/** @file memory.cpp
 *
 * Memory management, garbage collection, and memory profiling.
 */

#define USE_RINTERNALS

#include "Rvalgrind.h"

// For debugging:
#include <iostream>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/RS.h> /* for S4 allocation */
#include "CXXR/GCManager.hpp"
#include "CXXR/MemoryBank.hpp"
#include "CXXR/JMPException.hpp"

using namespace std;
using namespace CXXR;

#include <Defn.h>
#include <Graphics.h> /* display lists */
#include <Rdevices.h> /* GetDevice */
#include "Rdynpriv.h"

#if defined(Win32) && defined(LEA_MALLOC)
/*#include <stddef.h> */
extern void *Rm_malloc(size_t n);
extern void *Rm_calloc(size_t n_elements, size_t element_size);
extern void Rm_free(void * p);
extern void *Rm_realloc(void * p, size_t n);
#define calloc Rm_calloc
#define malloc Rm_malloc
#define realloc Rm_realloc
#define free Rm_free
#endif

/* malloc uses size_t.  We are assuming here that size_t is at least
   as large as unsigned long.  Changed from int at 1.6.0 to (i) allow
   2-4Gb objects on 32-bit system and (ii) objects limited only by
   length on a 64-bit system.
*/

#define GC_TORTURE

#ifdef GC_TORTURE
# define FORCE_GC !gc_inhibit_torture
#else
# define FORCE_GC 0
#endif

extern SEXP framenames;

#define GC_PROT(X) {int __t = gc_inhibit_torture; \
	gc_inhibit_torture = 1 ; X ; gc_inhibit_torture = __t;}

/* Miscellaneous Globals. */

static SEXP R_PreciousList = NULL;      /* List of Persistent Objects */

/* Node Classes ... don't exist any more.  The sxpinfo.gccls field is
   ignored. */

/* Debugging Routines. */

#ifdef DEBUG_ADJUST_HEAP
static void DEBUG_ADJUST_HEAP_PRINT(double node_occup, double vect_occup)
{
    R_size_t alloc;
    REprintf("Node occupancy: %.0f%%\nVector occupancy: %.0f%%\n",
	     100.0 * node_occup, 100.0 * vect_occup);
    alloc = MemoryBank::bytesAllocated();
    REprintf("Total allocation: %lu\n", alloc);
    REprintf("Ncells %lu\nVcells %lu\n", R_NSize, R_VSize);
}
#else
#define DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup)
#endif /* DEBUG_ADJUST_HEAP */

namespace {
    inline void CHECK_OLD_TO_NEW(SEXP x, SEXP y)
    {
	x->devolveAge(y);
    }
}

/* Finalization and Weak References */

/* The design of this mechanism is very close to the one described in
   "Stretching the storage manager: weak pointers and stable names in
   Haskell" by Peyton Jones, Marlow, and Elliott (at
   www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz). --LT */

static void checkKey(SEXP key)
{
    switch (TYPEOF(key)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
	break;
    default: error(_("can only weakly reference/finalize reference objects"));
    }
}

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    checkKey(key);
    switch (TYPEOF(fin)) {
    case NILSXP:
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
	break;
    default: error(_("finalizer must be a function or NULL"));
    }
    return new WeakRef(key, val, fin, onexit);
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
    checkKey(key);
    return new WeakRef(key, val, fin, onexit);
}

SEXP R_WeakRefKey(SEXP w)
{
    WeakRef* wr = dynamic_cast<WeakRef*>(w);
    if (!wr)
	error(_("not a weak reference"));
    return wr->key();
}

SEXP R_WeakRefValue(SEXP w)
{
    WeakRef* wr = dynamic_cast<WeakRef*>(w);
    if (!wr)
	error(_("not a weak reference"));
    SEXP v = wr->value();
    if (v && NAMED(v) != 2)
	SET_NAMED(v, 2);
    return v;
}

void WeakRef::finalize()
{
    R_CFinalizer_t Cfin = m_Cfinalizer;
    GCRoot<> key(m_key);
    GCRoot<> Rfin(m_Rfinalizer);
    // Do this now to ensure that finalizer is run only once, even if
    // an error occurs:
    tombstone();
    if (Cfin) Cfin(key);
    else if (Rfin) {
	GCRoot<> e(LCONS(Rfin, CONS(key, 0)));
	eval(e, R_GlobalEnv);
    }
}

bool WeakRef::runFinalizers()
{
    WeakRef::check();
    bool finalizer_run = !s_f10n_pending.empty();
    WRList::iterator lit = s_f10n_pending.begin();
    while (lit != s_f10n_pending.end()) {
	WeakRef* wr = *lit++;
	RCNTXT thiscontext;
	// A top level context is established for the finalizer to
	// insure that any errors that might occur do not spill into
	// the call that triggered the collection:
	begincontext(&thiscontext, CTXT_TOPLEVEL,
		     0, R_GlobalEnv, R_BaseEnv, 0, 0);
	RCNTXT* saveToplevelContext = R_ToplevelContext;
	GCRoot<> topExp(R_CurrentExpr);
	unsigned int savestack = GCRootBase::ppsSize();
	//	cout << __FILE__":" << __LINE__ << " Entering try/catch for "
	//	     << &thiscontext << endl;
	try {
	    R_GlobalContext = R_ToplevelContext = &thiscontext;
	    wr->finalize();
	}
	catch (JMPException& e) {
	    //	    cout << __FILE__":" << __LINE__
	    //		 << " Seeking " << e.context
	    //		 << "; in " << &thiscontext << endl;
	    if (e.context != &thiscontext)
		throw;
	}
	//	cout << __FILE__":" << __LINE__ << " Exiting try/catch for "
	//	     << &thiscontext << endl;
	endcontext(&thiscontext);
	R_ToplevelContext = saveToplevelContext;
	GCRootBase::ppsRestoreSize(savestack);
	R_CurrentExpr = topExp;
    }
    return finalizer_run;
}


/* R interface function */

SEXP do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int onexit;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != ENVSXP && TYPEOF(CAR(args)) != EXTPTRSXP)
	error(_("first argument must be environment or external pointer"));
    if (TYPEOF(CADR(args)) != CLOSXP)
	error(_("second argument must be a function"));

    onexit = asLogical(CADDR(args));
    if(onexit == NA_LOGICAL)
	error(_("third argument must be 'TRUE' or 'FALSE'"));

    R_RegisterFinalizerEx(CAR(args), CADR(args), Rboolean(onexit));
    return R_NilValue;
}


/* The Generational Collector. */

#define MARK_THRU(marker, node) if (node) (node)->conductVisitor(marker)

// The MARK_THRU invocations below could be eliminated by
// encapsulating the pointers concerned in GCRoot<> objects declared
// at file/global/static scope.

void GCNode::gc(unsigned int num_old_gens_to_collect)
{
    // cout << "GCNode::gc(" << num_old_gens_to_collect << ")\n";
    // GCNode::check();
    // cout << "Precheck completed OK\n";

    GCNode::Marker marker(num_old_gens_to_collect);
    GCRootBase::visitRoots(&marker);
    MARK_THRU(&marker, NA_STRING);	        /* Builtin constants */
    MARK_THRU(&marker, R_BlankString);
    MARK_THRU(&marker, R_UnboundValue);
    MARK_THRU(&marker, R_RestartToken);
    MARK_THRU(&marker, R_MissingArg);
    MARK_THRU(&marker, R_CommentSxp);

    MARK_THRU(&marker, R_GlobalEnv);	           /* Global environment */
    MARK_THRU(&marker, R_BaseEnv);
    MARK_THRU(&marker, R_EmptyEnv);
    MARK_THRU(&marker, R_Warnings);	           /* Warnings, if any */

    MARK_THRU(&marker, R_HandlerStack);          /* Condition handler stack */
    MARK_THRU(&marker, R_RestartStack);          /* Available restarts stack */

    for (unsigned int i = 0; i < HSIZE; i++)	           /* Symbol table */
	MARK_THRU(&marker, R_SymbolTable[i]);

    if (R_CurrentExpr != NULL)	           /* Current expression */
	MARK_THRU(&marker, R_CurrentExpr);

    for (unsigned int i = 0; i < R_MaxDevices; i++)  /* Device display lists */
	{
	    DevDesc* dd = GetDevice(i);
	    if (dd) {
		if (dd->newDevStruct) {
		    MARK_THRU(&marker,
			      (reinterpret_cast<GEDevDesc*>(dd))->dev->displayList);
		    MARK_THRU(&marker,
			      (reinterpret_cast<GEDevDesc*>(dd))->dev->savedSnapshot);
		}
		else
		    MARK_THRU(&marker, dd->displayList);
	    }
	}

    for (RCNTXT* ctxt = R_GlobalContext;
	 ctxt != NULL ; ctxt = ctxt->nextcontext) {
	MARK_THRU(&marker, ctxt->conexit);       /* on.exit expressions */
	MARK_THRU(&marker, ctxt->promargs);	   /* promises supplied to closure */
	MARK_THRU(&marker, ctxt->callfun);       /* the closure called */
        MARK_THRU(&marker, ctxt->sysparent);     /* calling environment */
	MARK_THRU(&marker, ctxt->call);          /* the call */
	MARK_THRU(&marker, ctxt->cloenv);        /* the closure environment */
	MARK_THRU(&marker, ctxt->handlerstack);  /* the condition handler stack */
	MARK_THRU(&marker, ctxt->restartstack);  /* the available restarts stack */
    }

    MARK_THRU(&marker, framenames); 		   /* used for interprocedure
					      communication in model.c */

    MARK_THRU(&marker, R_PreciousList);

#ifdef BYTECODE
    {
	SEXP *sp;
	for (sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++)
	    MARK_THRU(&marker, *sp);
    }
#endif

    WeakRef::markThru(num_old_gens_to_collect);

    // Purge R_StringHash hash table.  In future, when CHARSXP is
    // embodied as a class, the hash table will be a static member of
    // the class, and be maintained automatically by the constructors
    // and destructor, so this special treatment will become
    // unnecessary.
    //
    // Note that CXXR does *not*
    // USE_ATTRIB_FIELD_FOR_CHARSXP_CACHE_CHAINS.
    { 
	int nc = 0;
	for (int i = 0; i < LENGTH(R_StringHash); i++) {
	    SEXP s = VECTOR_ELT(R_StringHash, i);
	    SEXP t = R_NilValue;
	    while (s != R_NilValue) {
		SEXP node = CXHEAD(s);
		if (node->m_gcgen <= num_old_gens_to_collect
		    && !node->isMarked()) {
		    if (t == R_NilValue) /* head of list */
			SET_VECTOR_ELT(R_StringHash, i, CXTAIL(s));
		    else
			SETCDR(t, CXTAIL(s));
		}	
		else
		    t = s;
		s = CXTAIL(s);
	    }
	    if(VECTOR_ELT(R_StringHash, i) != R_NilValue) nc++;
	}
	SET_TRUELENGTH(R_StringHash, nc); /* SET_HASHPRI, really */
	MARK_THRU(&marker, R_StringHash);
    }

    // Sweep.  gen must be signed here or the loop won't terminate!
    for (int gen = num_old_gens_to_collect; gen >= 0; --gen) {
	if (gen == int(s_num_generations - 1)) {
	    // Delete unmarked nodes and unmark the rest:
	    const GCNode* node = s_genpeg[gen]->next();
	    while (node != s_genpeg[gen]) {
		const GCNode* next = node->next();
		if (!node->isMarked())
		    delete node;
		else node->m_marked = false;
		node = next;
	    }
	} else {
	    // Delete unmarked nodes, unmark the rest and promote them
	    // to the next generation:
	    const GCNode* node = s_genpeg[gen]->next();
	    while (node != s_genpeg[gen]) {
		const GCNode* next = node->next();
		if (!node->isMarked())
		    delete node;
		else {
		    node->m_marked = false;
		    ++node->m_gcgen;
		}
		node = next;
	    }
	    s_genpeg[gen+1]->splice(s_genpeg[gen]->next(), s_genpeg[gen]);
	    s_gencount[gen+1] += s_gencount[gen];
	    s_gencount[gen] = 0;
	}
    }

    // cout << "Finishing garbage collection\n";
    // GCNode::check();
    // cout << "Postcheck completed OK\n";
}


SEXP do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP old = ScalarLogical(!gc_inhibit_torture);

    checkArity(op, args);
    i = asLogical(CAR(args));
    if (i != NA_LOGICAL)
	gc_inhibit_torture = !i;
    return old;
}

SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    ostream* report_os = GCManager::setReporting(0);
    bool want_reporting = asLogical(CAR(args));
    if (want_reporting != NA_LOGICAL)
	GCManager::setReporting(want_reporting ? &cerr : 0);
    else
	GCManager::setReporting(report_os);
    return ScalarLogical(report_os != 0);
}

/* reports memory use to profiler in eval.c */

void attribute_hidden get_current_mem(unsigned long *smallvsize,
				      unsigned long *largevsize,
				      unsigned long *nodes)
{
    // All subject to change in CXXR:
    *smallvsize = 0;
    *largevsize = MemoryBank::bytesAllocated()/sizeof(VECREC);
    *nodes = GCNode::numNodes();
    return;
}

SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    ostream* report_os
	= GCManager::setReporting(asLogical(CAR(args)) ? &cerr : 0);
    bool reset_max = asLogical(CADR(args));
    GCManager::gc(0, true);
    GCManager::setReporting(report_os);
    /*- now return the [used , gc trigger size] for cells and heap */
    GCRoot<> value(allocVector(REALSXP, 6));
    REAL(value)[0] = GCNode::numNodes();
    REAL(value)[1] = NA_REAL;
    REAL(value)[2] = GCManager::maxNodes();
    /* next four are in 0.1MB, rounded up */
    REAL(value)[3] = 0.1*ceil(10. * MemoryBank::bytesAllocated()/Mega);
    REAL(value)[4] = 0.1*ceil(10. * GCManager::triggerLevel()/Mega);
    REAL(value)[5] = 0.1*ceil(10. * GCManager::maxBytes()/Mega);
    if (reset_max) GCManager::resetMaxTallies();
    return value;
}


#ifdef _R_HAVE_TIMING_

/* Use header files! 2007/06/11 arr
// defined in unix/sys-unix.c :
double R_getClockIncrement(void);
void R_getProcTime(double *data);
*/

static double gctimes[5], gcstarttimes[5];
static Rboolean gctime_enabled = FALSE;

SEXP do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    if (args == R_NilValue)
	gctime_enabled = TRUE;
    else
	gctime_enabled = Rboolean(asLogical(CAR(args)));
    ans = allocVector(REALSXP, 5);
    REAL(ans)[0] = gctimes[0];
    REAL(ans)[1] = gctimes[1];
    REAL(ans)[2] = gctimes[2];
    REAL(ans)[3] = gctimes[3];
    REAL(ans)[4] = gctimes[4];
    return ans;
}

static void gc_start_timing(void)
{
    if (gctime_enabled)
	R_getProcTime(gcstarttimes);
}

static void gc_end_timing(void)
{
    if (gctime_enabled) {
	double times[5], delta;
	R_getProcTime(times);
	delta = R_getClockIncrement();

	/* add delta to compensate for timer resolution */
	gctimes[0] += times[0] - gcstarttimes[0] + delta;
	gctimes[1] += times[1] - gcstarttimes[1] + delta;
	gctimes[2] += times[2] - gcstarttimes[2] + delta;
	gctimes[3] += times[3] - gcstarttimes[3];
	gctimes[4] += times[4] - gcstarttimes[4];
    }
}
#else /* not _R_HAVE_TIMING_ */
SEXP attribute_hidden do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error(_("gc.time() is not implemented on this system"));
    return R_NilValue;		/* -Wall */
}
#endif /* not _R_HAVE_TIMING_ */


/* InitMemory : Initialise the memory to be used in R. */
/* This includes: stack space, node space and vector space */

void InitMemory()
{
#ifdef _R_HAVE_TIMING_
    GCManager::setMonitors(gc_start_timing, gc_end_timing);
#endif
    GCManager::setReporting(R_Verbose ? &cerr : 0);
    GCManager::enableGC(R_VSize);

#ifdef BYTECODE
    R_BCNodeStackBase = reinterpret_cast<SEXP *>(malloc(R_BCNODESTACKSIZE * sizeof(SEXP)));
    if (R_BCNodeStackBase == NULL)
	R_Suicide("couldn't allocate node stack");
# ifdef BC_INT_STACK
    R_BCIntStackBase =
      (IStackval *) malloc(R_BCINTSTACKSIZE * sizeof(IStackval));
    if (R_BCIntStackBase == NULL)
	R_Suicide("couldn't allocate integer stack");
# endif
    R_BCNodeStackTop = R_BCNodeStackBase;
    R_BCNodeStackEnd = R_BCNodeStackBase + R_BCNODESTACKSIZE;
# ifdef BC_INT_STACK
    R_BCIntStackTop = R_BCIntStackBase;
    R_BCIntStackEnd = R_BCIntStackBase + R_BCINTSTACKSIZE;
# endif
#endif

    R_HandlerStack = R_RestartStack = R_NilValue;

    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = R_NilValue;
}


/*----------------------------------------------------------------------

  NewEnvironment

  Create an environment by extending "rho" with a frame obtained by
  pairing the variable names given by the tags on "namelist" with
  the values given by the elements of "valuelist".

  NewEnvironment is defined directly to avoid the need to protect its
  arguments unless a GC will actually occur.  This definition allows
  the namelist argument to be shorter than the valuelist; in this
  case the remaining values must be named already.  (This is useful
  in cases where the entire valuelist is already named--namelist can
  then be R_NilValue.)

  The valuelist is destructively modified and used as the
  environment's frame.
*/
SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho)
{
    SEXP v, n, newrho;

    PROTECT(namelist);
    PROTECT(valuelist);
    PROTECT(rho);
    newrho = new RObject(ENVSXP);
    UNPROTECT(3);
#if VALGRIND_LEVEL > 2
    VALGRIND_MAKE_READABLE(newrho, sizeof(*newrho));
#endif
    newrho->u.envsxp.frame = valuelist;  // FRAME
    newrho->u.envsxp.enclos = rho;  // ENCLOS

    v = valuelist;
    n = namelist;
    while (v != R_NilValue && n != R_NilValue) {
	SET_TAG(v, TAG(n));
	v = CDR(v);
	n = CDR(n);
    }
    return (newrho);
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP mkPROMISE(SEXP expr, SEXP rho)
{
    SEXP s;
    PROTECT(expr);
    PROTECT(rho);
    s = new RObject(PROMSXP);
    UNPROTECT(2);
#if VALGRIND_LEVEL > 2
    VALGRIND_MAKE_READABLE(s,sizeof(*s));
#endif
    s->u.promsxp.expr = expr;  // PRCODE
    s->u.promsxp.env = rho;  // PRENV
    s->u.promsxp.value = R_UnboundValue;  // PRVALUE
    SET_PRSEEN(s, 0);
    return s;
}

/* All vector objects  must be a multiple of sizeof(ALIGN) */
/* bytes so that alignment is preserved for all objects */

/* allocString is now a macro */

/* Allocate a vector object.  This ensures only validity of list-like
   SEXPTYPES (as the elements must be initialized).  Initializing of
   other vector types is done in do_makevector */

SEXP allocVector(SEXPTYPE type, R_len_t length)
{
    SEXP s;

    if (length < 0 )
	errorcall(R_GlobalContext->call,
		  _("negative length vectors are not allowed"));
    /* number of vector cells to allocate */
    switch (type) {
    case NILSXP:
	return R_NilValue;
    case RAWSXP:
	return new RawVector(length);
    case CHARSXP:
	return new UncachedString(length);
    case LGLSXP:
	return new LogicalVector(length);
    case INTSXP:
	return new IntVector(length);
    case REALSXP:
	return new RealVector(length);
    case CPLXSXP:
	return new ComplexVector(length);
    case STRSXP:
	return new StringVector(length);
    case EXPRSXP:
	return new ExpressionVector(length);
    case VECSXP:
	return new ListVector(length);
    case LANGSXP:
	if(length == 0) return R_NilValue;
	s = new Expression(length);
	return s;
    case LISTSXP:
	return allocList(length);
    default:
	error(_("invalid type/length (%s/%d) in vector allocation"),
	      type2char(type), length);
	return 0;  // -Wall
    }
}

SEXP allocS4Object()
{
   SEXP s;
   GC_PROT(s = new RObject(S4SXP));
   SET_S4_OBJECT(s);
   return s;
}


/* "gc" a mark-sweep or in-place generational garbage collector */

void R_gc(void)
{
    GCManager::gc(0);
}


#define R_MAX(a,b) (a) < (b) ? (b) : (a)

SEXP do_memlimits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 2));
    INTEGER(ans)[0] = NA_INTEGER;
    INTEGER(ans)[1] = NA_INTEGER;
    UNPROTECT(1);
    return ans;
}

SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nms;
    PROTECT(ans = allocVector(INTSXP, 23));
    PROTECT(nms = allocVector(STRSXP, 23));
    for (int i = 0; i < 23; i++) {
        INTEGER(ans)[i] = 0;
	SET_STRING_ELT(nms, i, type2str(SEXPTYPE(i > LGLSXP? i+2 : i)));
    }
    setAttrib(ans, R_NamesSymbol, nms);
    // Just return a vector of zeroes in CXXR.
    UNPROTECT(2);
    return ans;
}

/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if(nelem == 0)
	return(NULL);
#endif
    p = calloc(nelem, elsize);
    if(!p) error(_("Calloc could not allocate (%d of %d) memory"),
		 nelem, elsize);
    return(p);
}

void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    /* Protect against broken realloc */
    if(ptr) p = realloc(ptr, size); else p = malloc(size);
    if(!p) error(_("Realloc could not re-allocate (size %d) memory"), size);
    return(p);
}

void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if(!ptr) warning("attempt to free NULL pointer by Free"); */
    if(ptr) free(ptr); /* ANSI C says free has no effect on NULL, but
			  better to be safe here */
}

/* This code keeps a list of objects which are not assigned to variables
   but which are required to persist across garbage collections.  The
   objects are registered with R_PreserveObject and deregistered with
   R_ReleaseObject. */

void R_PreserveObject(SEXP object)
{
    R_PreciousList = CONS(object, R_PreciousList);
}

static SEXP RecursiveRelease(SEXP object, SEXP list)
{
    if (!isNull(list)) {
        if (object == CAR(list))
            return CDR(list);
        else
            SETCDR(list, RecursiveRelease(object, CDR(list)));
    }
    return list;
}

void R_ReleaseObject(SEXP object)
{
    R_PreciousList =  RecursiveRelease(object, R_PreciousList);
}


/* External Pointer Objects */

/* Work around casting issues: works where it is needed */
typedef union {void *p; DL_FUNC fn;} fn_ptr;

/* used in package methods */
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot)
{
    fn_ptr tmp;
    tmp.fn = p;
    return R_MakeExternalPtr(tmp.p, tag, prot);
}

DL_FUNC R_ExternalPtrAddrFn(SEXP s)
{
    fn_ptr tmp;
    tmp.p =  EXTPTR_PTR(s);
    return tmp.fn;
}


#define USE_TYPE_CHECKING

#if defined(USE_TYPE_CHECKING_STRICT) && !defined(USE_TYPE_CHECKING)
# define USE_TYPE_CHECKING
#endif


/* The following functions are replacements for the accessor macros.
   They are used by code that does not have direct access to the
   internal representation of objects.  The assignment functions
   implement the write barrier. */

/* General Cons Cell Attributes */

void (SET_ATTRIB)(SEXP x, SEXP v) { 
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(v) != LISTSXP && TYPEOF(v) != NILSXP) 
	error("value of 'SET_ATTRIB' must be a pairlist or NULL, not a '%s'", 
	      type2char(TYPEOF(x)));
#endif
    CHECK_OLD_TO_NEW(x, v); 
    x->m_attrib = v; 
}
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(to, duplicate(ATTRIB(from)));
    SET_OBJECT(to, OBJECT(from));
    IS_S4_OBJECT(from) ?  SET_S4_OBJECT(to) : UNSET_S4_OBJECT(to);
}


/* Closure Accessors */

void (SET_FORMALS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.closxp.formals = v; }
void (SET_BODY)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.closxp.body = v; }
void (SET_CLOENV)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.closxp.env = v; }

/* Symbol Accessors */

void (SET_PRINTNAME)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.symsxp.pname = v; }
void (SET_SYMVALUE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.symsxp.value = v; }
void (SET_INTERNAL)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.symsxp.internal = v; }

/* Environment Accessors */

void (SET_FRAME)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.envsxp.frame = v; }
void (SET_ENCLOS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.envsxp.enclos = v; }
void (SET_HASHTAB)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.envsxp.hashtab = v; }

/* Promise Accessors */

void (SET_PRENV)(SEXP x, SEXP v){ CHECK_OLD_TO_NEW(x, v); x->u.promsxp.env = v; }
void (SET_PRVALUE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.promsxp.value = v; }
void (SET_PRCODE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); x->u.promsxp.expr = v; }

/* R_FunTab accessors */
/* Not used:
void (SET_PRIMFUN)(SEXP x, CCODE f) { PRIMFUN(x) = f; }
*/

/*******************************************/
/* Non-sampling memory use profiler
   reports all large vector heap
   allocations */
/*******************************************/

#ifndef R_MEMORY_PROFILING

SEXP do_Rprofmem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error(_("memory profiling is not available on this system"));
    return R_NilValue; /* not reached */
}

#else
static FILE *R_MemReportingOutfile;

static void R_OutputStackTrace(FILE *file)
{
    RCNTXT *cptr;

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    fprintf(file, "\"%s\" ",
		    TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    fprintf(file, "\n");
}

static void R_ReportAllocation(R_size_t size)
{
    fprintf(R_MemReportingOutfile, "%ld :", static_cast<unsigned long>(size));
    R_OutputStackTrace(R_MemReportingOutfile);
}

static void R_EndMemReporting()
{
    if(R_MemReportingOutfile != NULL) {
	/* does not fclose always flush? */
	fflush(R_MemReportingOutfile);
	fclose(R_MemReportingOutfile);
	R_MemReportingOutfile=NULL;
    }
    MemoryBank::setMonitor(0);
    return;
}

static void R_InitMemReporting(const char *filename, int append,
			       R_size_t threshold)
{
    if(R_MemReportingOutfile != NULL) R_EndMemReporting();
    R_MemReportingOutfile = fopen(filename, append ? "a" : "w");
    if (R_MemReportingOutfile == NULL)
	error(_("Rprofmem: cannot open output file '%s'"), filename);
    MemoryBank::setMonitor(R_ReportAllocation, threshold);
}

SEXP attribute_hidden do_Rprofmem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const char *filename;
    R_size_t threshold;
    int append_mode;

    checkArity(op, args);
    if (!isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
	error(_("invalid '%s' argument"), "filename");
    append_mode = asLogical(CADR(args));
    filename = R_ExpandFileName(CHAR(STRING_ELT(CAR(args), 0)));
    threshold = R_size_t(REAL(CADDR(args))[0]);
    if (strlen(filename))
	R_InitMemReporting(filename, append_mode, threshold);
    else
	R_EndMemReporting();
    return R_NilValue;
}

#endif /* R_MEMORY_PROFILING */

/* RBufferUtils, moved from deparse.c */

#include "RBufferUtils.h"

void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf)
{
    size_t blen1, bsize = buf->defaultSize;

    /* for backwards compatibility, probably no longer needed */
    if(blen == size_t(-1)) {
	warning("R_AllocStringBuffer(-1) used: please report");
	R_FreeStringBufferL(buf);
	return NULL;
    }

    if(blen * sizeof(char) < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    if(buf->data == NULL) {
	buf->data = reinterpret_cast<char *>(malloc(blen));
	buf->data[0] = '\0';
    } else
	buf->data = reinterpret_cast<char *>(realloc(buf->data, blen));
    buf->bufsize = blen;
    if(!buf->data) {
	buf->bufsize = 0;
	/* don't translate internal error message */
	error("could not allocate memory (%u Mb) in C function 'R_AllocStringBuffer'",
	      static_cast<unsigned int>(blen)/1024/1024);
    }
    return buf->data;
}

void R_FreeStringBuffer(R_StringBuffer *buf)
{
    if (buf->data != NULL) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

void R_FreeStringBufferL(R_StringBuffer *buf)
{
    if (buf->bufsize > buf->defaultSize) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}
