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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
 */

/*
 *	This code implements a non-moving generational collector
 *      with two or three generations.
 *
 *	Memory allocated by R_alloc is maintained in a stack.  Code
 *	that R_allocs memory must use vmaxget and vmaxset to obtain
 *	and reset the stack pointer.
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

#if defined(HAVE_GLIBC2)
/* for isnan in Rinlinedfuns.h */
# define _SVID_SOURCE 1
#endif

#include <R_ext/RS.h> /* for S4 allocation */
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCManager.hpp"
#include "CXXR/Heap.hpp"
#include "CXXR/JMPException.hpp"
#include "CXXR/WeakRef.h"

using namespace std;
using namespace CXXR;

#if defined(Win32) && defined(LEA_MALLOC)
#include <stddef.h>
extern void *Rm_malloc(size_t n);
extern void *Rm_calloc(size_t n_elements, size_t element_size);
extern void Rm_free(void * p);
extern void *Rm_realloc(void * p, size_t n);
#define calloc Rm_calloc
#define malloc Rm_malloc
#define realloc Rm_realloc
#define free Rm_free
#endif

#include <Defn.h>
#include <Graphics.h> /* display lists */
#include <Rdevices.h> /* GetDevice */

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

void R_SetPPSize(R_size_t size)
{
    R_PPStackSize = size;
}

/* Miscellaneous Globals. */

static SEXP R_VStack = NULL;		/* R_alloc stack pointer */
static SEXP R_PreciousList = NULL;      /* List of Persistent Objects */

/* Node Classes ... don't exist any more.  The sxpinfo.gccls field is
   ignored. */

/* Processing Node Children */

// 2007/08/07 arr: memory.cpp's own non-type-checked versions!
namespace {
    inline char* memCHAR(SEXP x) {return reinterpret_cast<char*>(DATAPTR(x));}

    inline const RObject* memSTRING_ELT(const RObject* x, int i)
    {
        return reinterpret_cast<SEXP*>(x->m_data)[i];
    }
}

/* Debugging Routines. */

#ifdef DEBUG_ADJUST_HEAP
static void DEBUG_ADJUST_HEAP_PRINT(double node_occup, double vect_occup)
{
    R_size_t alloc;
    REprintf("Node occupancy: %.0f%%\nVector occupancy: %.0f%%\n",
	     100.0 * node_occup, 100.0 * vect_occup);
    alloc = Heap::bytesAllocated();
    REprintf("Total allocation: %lu\n", alloc);
    REprintf("Ncells %lu\nVcells %lu\n", R_NSize, R_VSize);
}
#else
#define DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup)
#endif /* DEBUG_ADJUST_HEAP */

namespace {
    inline void CHECK_OLD_TO_NEW(SEXP x, SEXP y)
    {
	GCEdge<> e(x, 0);
	e.redirect(x, y);
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
    SEXP key, Rfin;
    PROTECT(key = m_key);
    PROTECT(Rfin = m_Rfinalizer);
    // Do this now to ensure that finalizer is run only once, even if
    // an error occurs:
    tombstone();
    if (Cfin) Cfin(key);
    else if (Rfin) {
	SEXP e;
	PROTECT(e = LCONS(Rfin, LCONS(key, 0)));
	eval(e, R_GlobalEnv);
	UNPROTECT(1);
    }
    UNPROTECT(2);
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
	SEXP topExp;
	PROTECT(topExp = R_CurrentExpr);
	int savestack = R_PPStackTop;
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
	R_PPStackTop = savestack;
	R_CurrentExpr = topExp;
	UNPROTECT(1);
    }
    return finalizer_run;
}

void WeakRef::runExitFinalizers()
{
    WeakRef::check();
    WRList::iterator lit = s_live.begin();
    while (lit != s_live.end()) {
	WeakRef* wr = *lit++;
	if (wr->m_flags[FINALIZE_ON_EXIT]) {
	    wr->m_flags[READY_TO_FINALIZE] = true;
	    wr->transfer(&s_live, &s_f10n_pending);
	}
    }
    runFinalizers();
}

void R_RunExitFinalizers(void)
{
    WeakRef::runExitFinalizers();
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
    R_MakeWeakRef(s, R_NilValue, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
    R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
    R_MakeWeakRefC(s, R_NilValue, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
    R_RegisterCFinalizerEx(s, fun, FALSE);
}

/* R interface function */

SEXP attribute_hidden do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int onexit;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != ENVSXP && TYPEOF(CAR(args)) != EXTPTRSXP)
	errorcall(call, 
		  _("first argument must be environment or external pointer"));
    if (TYPEOF(CADR(args)) != CLOSXP)
	errorcall(call, _("second argument must be a function"));

    onexit = asLogical(CADDR(args));
    if(onexit == NA_LOGICAL)
	errorcall(call, _("third argument must be 'TRUE' or 'FALSE'"));

    R_RegisterFinalizerEx(CAR(args), CADR(args), Rboolean(onexit));
    return R_NilValue;
}


/* The Generational Collector. */

#define MARK_THRU(marker, node) if (node) (node)->conductVisitor(marker)

void GCNode::gc(unsigned int num_old_gens_to_collect)
{
    // cout << "GCNode::gc(" << num_old_gens_to_collect << ")\n";
    GCNode::check();
    // cout << "Precheck completed OK\n";

    GCNode::Marker marker(num_old_gens_to_collect);
    // GCRootBase::visitRoots(&marker);
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
		    MARK_THRU(&marker, ((GEDevDesc*) dd)->dev->displayList);
		    MARK_THRU(&marker, ((GEDevDesc*) dd)->dev->savedSnapshot);
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

    for (int i = 0; i < R_PPStackTop; i++)    /* Protected pointers */
	MARK_THRU(&marker, R_PPStack[i]);

    MARK_THRU(&marker, R_VStack);		   /* R_alloc stack */

#ifdef BYTECODE
    {
	SEXP *sp;
	for (sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++)
	    MARK_THRU(&marker, *sp);
    }
#endif

    WeakRef::markThru(num_old_gens_to_collect);

    // Sweep.  gen must be signed here or the loop won't terminate!
    for (int gen = num_old_gens_to_collect; gen >= 0; --gen) {
	if (gen == int(s_last_gen)) {
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


SEXP attribute_hidden do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP old = allocVector(LGLSXP, 1);

    checkArity(op, args);
    i = asLogical(CAR(args));
    LOGICAL(old)[0] = !gc_inhibit_torture;
    if (i != NA_LOGICAL)
	gc_inhibit_torture = !i;
    return old;
}

SEXP attribute_hidden do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP old = allocVector(LGLSXP, 1);

    checkArity(op, args);
    ostream* report_os = GCManager::setReporting(0);
    bool want_reporting = asLogical(CAR(args));
    LOGICAL(old)[0] = (report_os != 0);
    if (want_reporting != NA_LOGICAL)
	GCManager::setReporting(want_reporting ? &cerr : 0);
    else
	GCManager::setReporting(report_os);
    return old;
}

/* reports memory use to profiler in eval.c */

void attribute_hidden get_current_mem(unsigned long *smallvsize,
				      unsigned long *largevsize,
				      unsigned long *nodes)
{
    // All subject to change in CXXR:
    *smallvsize = 0;
    *largevsize = Heap::bytesAllocated()/sizeof(VECREC);
    *nodes = GCNode::numNodes();
    return;
}

SEXP attribute_hidden do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;
    checkArity(op, args);
    ostream* report_os
	= GCManager::setReporting(asLogical(CAR(args)) ? &cerr : 0);
    bool reset_max = asLogical(CADR(args));
    GCManager::gc(0, true);
    GCManager::setReporting(report_os);
    /*- now return the [used , gc trigger size] for cells and heap */
    PROTECT(value = allocVector(REALSXP, 6));
    REAL(value)[0] = GCNode::numNodes();
    REAL(value)[1] = NA_REAL;
    REAL(value)[2] = GCManager::maxNodes();
    /* next four are in 0.1MB, rounded up */
    REAL(value)[3] = 0.1*ceil(10. * Heap::bytesAllocated()/Mega);
    REAL(value)[4] = 0.1*ceil(10. * GCManager::triggerLevel()/Mega);
    REAL(value)[5] = 0.1*ceil(10. * GCManager::maxBytes()/Mega);
    if (reset_max) GCManager::resetMaxTallies();
    UNPROTECT(1);
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

SEXP attribute_hidden do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
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

#define PP_REDZONE_SIZE 1000L
static R_size_t R_StandardPPStackSize, R_RealPPStackSize;

void attribute_hidden InitMemory()
{
#ifdef _R_HAVE_TIMING_
    GCManager::initialize(R_VSize, gc_start_timing, gc_end_timing);
#else
    GCManager::initialize(R_VSize);
#endif
    GCManager::setReporting(R_Verbose ? &cerr : 0);
    R_StandardPPStackSize = R_PPStackSize;
    R_RealPPStackSize = R_PPStackSize + PP_REDZONE_SIZE;
    if (!(R_PPStack = reinterpret_cast<SEXP *>(malloc(R_RealPPStackSize * sizeof(SEXP)))))
	R_Suicide("couldn't allocate memory for pointer stack");
    R_PPStackTop = 0;
#if VALGRIND_LEVEL > 1
    VALGRIND_MAKE_NOACCESS(R_PPStackTop+R_PPStackSize,PP_REDZONE_SIZE);
#endif

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

/* Since memory allocated from the heap is non-moving, R_alloc just
   allocates off the heap as CHARSXP's and maintains the stack of
   allocations through the ATTRIB pointer.  The stack pointer R_VStack
   is traced by the collector. */
char *vmaxget(void)
{
    return (char *) R_VStack;
}

void vmaxset(char *ovmax)
{
    R_VStack = (SEXP) ovmax;
}

/* <FIXME> this really needs to be R_size_t with an appropriate test.
   That would mean exporting R_size_t.
 */
char *R_alloc(long nelem, int eltsize)
{
    R_size_t size = nelem * eltsize;
    double dsize = double(nelem) * eltsize;
    if (dsize > 0) { /* precaution against integer overflow */
	SEXP s;
#if SIZEOF_LONG > 4
	/* In this case by allocating larger units we can get up to
	   size(double) * (2^31 - 1) bytes, approx 16Gb */
	if(dsize < R_LEN_T_MAX)
	    s = allocString(size); /**** avoid extra null byte?? */
	else if(dsize < sizeof(double) * (R_LEN_T_MAX - 1))
	    s = allocVector(REALSXP, (int)(0.99+dsize/sizeof(double)));
	else {
	    s = R_NilValue; /* -Wall */
	    if(dsize > 1024.0*1024.0*1024.0)
		error(_("cannot allocate memory block of size %0.1f GB"), 
		      dsize/1024.0/1024.0/1024.0);
	    else if(dsize > 1024.0*1024.0)
		error(_("cannot allocate memory block of size %0.1f MB"), 
		      dsize/1024.0/1024.0);
	    else if(dsize > 1024.0)
		error(_("cannot allocate memory block of size %0.1f KB"), 
		      dsize/1024.0);
	    else
		error(_("cannot allocate memory block of size %.0f"),
		      dsize);
	}
#else
	if(dsize > R_LEN_T_MAX) {
	    if(dsize > 1024.0*1024.0*1024.0)
		error(_("cannot allocate memory block of size %0.1f GB"), 
		      dsize/1024.0/1024.0/1024.0);
	    else if(dsize > 1024.0*1024.0)
		error(_("cannot allocate memory block of size %0.1f MB"), 
		      dsize/1024.0/1024.0);
	    else if(dsize > 1024.0)
		error(_("cannot allocate memory block of size %0.1f KB"), 
		      dsize/1024.0);
	    else
		error(_("cannot allocate memory block of size %.0f"),
		      dsize);
	}	
	s = allocString(size); /**** avoid extra null byte?? */
#endif
	s->m_attrib = R_VStack;
	R_VStack = s;
#if VALGRIND_LEVEL > 0
	VALGRIND_MAKE_WRITABLE(memCHAR(s), (int) dsize);
#endif
	return memCHAR(s);
    }
    else return NULL;
}



/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
    R_size_t /*i,*/ size  = nelem * eltsize;
    char *p = R_alloc(nelem, eltsize);

    memset(p, 0, size);
    /* for(i = 0; i < size; i++)
       p[i] = 0; */
    return p;
}


char *S_realloc(char *p, long newct, long old, int size)
{
    int /*i,*/ nold;
    char *q;
    /* shrinking is a no-op */
    if(newct <= old) return p;
    q = R_alloc(newct, size);
    nold = old * size;
    memcpy(q, p, nold);
    memset(q + nold, 0, newct*size - nold);
    /* for(i = 0; i < nold; i++)
	q[i] = p[i];
    for(i = nold; i < newct*size; i++)
        q[i] = 0; */
    return q;
}

/* Node Allocation. */

SEXP allocSExp(SEXPTYPE t)
{
    return new RObject(t);
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP cons(SEXP car, SEXP cdr)
{
    PROTECT(car);
    PROTECT(cdr);
    SEXP s = new RObject(LISTSXP);
    UNPROTECT(2);
#if VALGRIND_LEVEL > 2
    VALGRIND_MAKE_READABLE(s, sizeof(*s));
#endif
    SETCAR(s, car);
    SETCDR(s, cdr);
    return s;
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
SEXP attribute_hidden mkPROMISE(SEXP expr, SEXP rho)
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
    SEXP s;     /* For the generational collector it would be safer to
		   work in terms of a VECSEXP here, but that would
		   require several casts below... */
    R_len_t i;
    R_size_t size = 0, actual_size = 0, alloc_size, old_R_VSize;

    if (length < 0 )
	errorcall(R_GlobalContext->call,
		  _("negative length vectors are not allowed"));
    /* number of vector cells to allocate */
    switch (type) {
    case NILSXP:
	return R_NilValue;
    case RAWSXP:
	size = BYTE2VEC(length);
	actual_size=length;
	break;
    case CHARSXP:
	size = BYTE2VEC(length + 1);
	actual_size=length+1;
	break;
    case LGLSXP:
    case INTSXP:
	if (length <= 0)
	    actual_size = size = 0;
	else {
	    if (length > int(R_SIZE_T_MAX / sizeof(int)))
		errorcall(R_GlobalContext->call,
			  _("cannot allocate vector of length %d"), length);
	    size = INT2VEC(length);
	    actual_size = length*sizeof(int);
	}
	break;
    case REALSXP:
	if (length <= 0)
	    actual_size = size = 0;
	else {
	    if (length > int(R_SIZE_T_MAX / sizeof(double)))
		errorcall(R_GlobalContext->call,
			  _("cannot allocate vector of length %d"), length);
	    size = FLOAT2VEC(length);
	    actual_size = length * sizeof(double);
	}
	break;
    case CPLXSXP:
	if (length <= 0)
	    actual_size = size = 0;
	else {
	    if (length > int(R_SIZE_T_MAX / sizeof(Rcomplex)))
		errorcall(R_GlobalContext->call,
			  _("cannot allocate vector of length %d"), length);
	    size = COMPLEX2VEC(length);
	    actual_size = length * sizeof(Rcomplex);
	}
	break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	if (length <= 0)
	    actual_size = size = 0;
	else {
	    if (length > int(R_SIZE_T_MAX / sizeof(SEXP)))
		errorcall(R_GlobalContext->call,
			  _("cannot allocate vector of length %d"), length);
	    size = PTR2VEC(length);
	    actual_size = length * sizeof(SEXP);
	}
	break;
    case LANGSXP:
	if(length == 0) return R_NilValue;
	s = allocList(length);
	SET_TYPEOF(s, LANGSXP);
	return s;
    case LISTSXP:
	return allocList(length);
    default:
	error(_("invalid type/length (%d/%d) in vector allocation"),
	      type, length);
    }

    alloc_size = size;

    /* save current R_VSize to roll back adjustment if malloc fails */
    old_R_VSize = R_VSize;

    s = new RObject(type);
    s->m_databytes = size * sizeof(VECREC);
    if (size > 0) {
	bool success = false;
	if (size < R_SIZE_T_MAX/sizeof(VECREC)) {
	    // We don't want the garbage collector trying to mark this
	    // node's children yet:
	    SETLENGTH(s, 0);
	    try {
		PROTECT(s);
		s->m_data = Heap::allocate(s->m_databytes);
		UNPROTECT(1);
		success = true;
	    }
	    catch (bad_alloc) {
		// Leave s itself to the garbage collector.
		success = false;
	    }
	}
	if (!success) {
	    double dsize = double(size) * sizeof(VECREC)/1024.0;
	    /* reset the vector heap limit */
	    R_VSize = old_R_VSize;
	    if(dsize > 1024.0*1024.0)
		errorcall(R_NilValue, 
			  _("cannot allocate vector of size %0.1f GB"),
			  dsize/1024.0/1024.0);
	    if(dsize > 1024.0)
		errorcall(R_NilValue,
			  _("cannot allocate vector of size %0.1f MB"),
			  dsize/1024.0);
	    else
		errorcall(R_NilValue, 
			  _("cannot allocate vector of size %0.f KB"),
			  dsize);
	}
    }
    SETLENGTH(s, length);

    /* The following prevents disaster in the case */
    /* that an uninitialised string vector is marked */
    /* Direct assignment is OK since the node was just allocated and */
    /* so is at least as new as R_NilValue and R_BlankString */
    if (type == EXPRSXP || type == VECSXP) {
	SEXP *data = STRING_PTR(s);
#if VALGRIND_LEVEL > 1
	VALGRIND_MAKE_READABLE(STRING_PTR(s), actual_size);
#endif
	for (i = 0; i < length; i++)
	    data[i] = R_NilValue;
    }
    else if(type == STRSXP) {
	SEXP *data = STRING_PTR(s);
#if VALGRIND_LEVEL > 1
	VALGRIND_MAKE_READABLE(STRING_PTR(s), actual_size);
#endif
	for (i = 0; i < length; i++){
	    data[i] = R_BlankString;
	}
    }
    else if (type == CHARSXP){
#if VALGRIND_LEVEL > 0
 	VALGRIND_MAKE_WRITABLE(memCHAR(s), actual_size);
#endif
	memCHAR(s)[length] = 0;
    }
    else if (type == REALSXP){
#if VALGRIND_LEVEL > 0
	VALGRIND_MAKE_WRITABLE(REAL(s), actual_size);
#endif
    }
    else if (type == INTSXP){
#if VALGRIND_LEVEL > 0
	VALGRIND_MAKE_WRITABLE(INTEGER(s), actual_size);
#endif
    }
    /* <FIXME> why not valgrindify LGLSXP, CPLXSXP and RAWSXP/ */
    return s;
}

SEXP allocList(unsigned int n)
{
    unsigned int i;
    SEXP result;
    result = R_NilValue;
    for (i = 0; i < n; i++)
	result = CONS(R_NilValue, result);
    return result;
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

SEXP attribute_hidden do_memlimits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 2));
    INTEGER(ans)[0] = NA_INTEGER;
    INTEGER(ans)[1] = NA_INTEGER;
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
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

/* "protect" push a single argument onto R_PPStack */

/* In handling a stack overflow we have to be careful not to use
   PROTECT. error("protect(): stack overflow") would call deparse1,
   which uses PROTECT and segfaults.*/

/* However, the traceback creation in the normal error handler also
   does a PROTECT, as does the jumping code, at least if there are
   cleanup expressions to handle on the way out.  So for the moment
   we'll allocate a slightly larger PP stack and only enable the added
   red zone during handling of a stack overflow error.  LT */

static void reset_pp_stack(void *data)
{
    R_size_t *poldpps = reinterpret_cast<R_size_t*>(data);
    R_PPStackSize =  *poldpps;
}

SEXP protect(SEXP s)
{
    if (R_PPStackTop >= R_PPStackSize) {
	RCNTXT cntxt;
	R_size_t oldpps = R_PPStackSize;

	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &reset_pp_stack;
	cntxt.cenddata = &oldpps;

	if (R_PPStackSize < int(R_RealPPStackSize))
	    R_PPStackSize = R_RealPPStackSize;
	errorcall(R_NilValue, _("protect(): protection stack overflow"));

	endcontext(&cntxt); /* not reached */
    }
    R_PPStack[R_PPStackTop++] = s;
    return s;
}


/* "unprotect" pop argument list from top of R_PPStack */

void unprotect(int l)
{
    if (R_PPStackTop >=  l)
	R_PPStackTop -= l;
    else
	error(_("unprotect(): only %d protected items"), R_PPStackTop);
}


/* "unprotect_ptr" remove pointer from somewhere in R_PPStack */

void unprotect_ptr(SEXP s)
{
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    /* (should be among the top few items) */
    do {
    	if (i == 0)
	    error(_("unprotect_ptr: pointer not found"));
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    /* Now drop stack above it */

    do {
    	R_PPStack[i] = R_PPStack[i + 1];
    } while ( i++ < R_PPStackTop );

    R_PPStackTop--;
}

void R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    protect(s);
    *pi = R_PPStackTop - 1;
}

void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    R_PPStack[i] = s;
}


/* "initStack" initialize environment stack */
void initStack(void)
{
    R_PPStackTop = 0;
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

// y doesn't point to a GCNode, so no old-to-new check:
#define SET_EXTPTR_PTR(x, y) (x)->u.listsxp.carval = (y)

SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    SEXP s = allocSExp(EXTPTRSXP);
    SET_EXTPTR_PTR(s, reinterpret_cast<SEXPREC*>(p));
    SET_EXTPTR_PROT(s, prot);
    SET_EXTPTR_TAG(s, tag);
    return s;
}

void *R_ExternalPtrAddr(SEXP s)
{
    return EXTPTR_PTR(s);
}

SEXP R_ExternalPtrTag(SEXP s)
{
    return EXTPTR_TAG(s);
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    return EXTPTR_PROT(s);
}

void R_ClearExternalPtr(SEXP s)
{
    SET_EXTPTR_PTR(s, NULL);
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    SET_EXTPTR_PTR(s, reinterpret_cast<SEXPREC*>(p));
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    CHECK_OLD_TO_NEW(s, tag);
    SET_EXTPTR_TAG(s, tag);
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    CHECK_OLD_TO_NEW(s, p);
    SET_EXTPTR_PROT(s, p);
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

/* Vector Accessors */

char *(R_CHAR)(SEXP x) {
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(x) != CHARSXP) 
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "CHAR", "CHARSXP", type2char(TYPEOF(x)));
#endif
    return reinterpret_cast<char*>(DATAPTR(x));
}

SEXP (STRING_ELT)(SEXP x, int i) {
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "STRING_ELT", "character vector", type2char(TYPEOF(x)));
#endif
    return reinterpret_cast<SEXP *>(DATAPTR(x))[i];
}

SEXP (VECTOR_ELT)(SEXP x, int i) {
#ifdef USE_TYPE_CHECKING_STRICT
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP && 
       TYPEOF(x) != EXPRSXP && 
       TYPEOF(x) != WEAKREFSXP)
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "VECTOR_ELT", "list", type2char(TYPEOF(x)));
#elif defined(USE_TYPE_CHECKING)
    /* also allow STRSXP */
    if(TYPEOF(x) != VECSXP && TYPEOF(x) != STRSXP &&
       TYPEOF(x) != EXPRSXP && 
       TYPEOF(x) != WEAKREFSXP)
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "VECTOR_ELT", "list", type2char(TYPEOF(x)));
#endif
    return reinterpret_cast<SEXP *>(DATAPTR(x))[i];
}

int *(LOGICAL)(SEXP x) {
#ifdef USE_TYPE_CHECKING_STRICT
    if(TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "LOGICAL",  "logical", type2char(TYPEOF(x)));
#elif defined(USE_TYPE_CHECKING)
    /* Currently harmless, and quite widely used */
    if(TYPEOF(x) != LGLSXP && TYPEOF(x) != INTSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "LOGICAL",  "logical", type2char(TYPEOF(x)));
#endif
  return reinterpret_cast<int *>(DATAPTR(x)); 
}

int *(INTEGER)(SEXP x) {
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
        error("%s() can only be applied to a '%s', not a '%s'",
	      "INTEGER", "integer", type2char(TYPEOF(x)));
#endif
    return reinterpret_cast<int *>(DATAPTR(x)); 
}

Rbyte *(RAW)(SEXP x) { 
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(x) != RAWSXP) 
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "RAW", "raw", type2char(TYPEOF(x)));
#endif
    return reinterpret_cast<Rbyte *>(DATAPTR(x)); 
}

double *(REAL)(SEXP x) { 
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(x) != REALSXP) 
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "REAL", "numeric", type2char(TYPEOF(x)));
#endif
    return reinterpret_cast<double *>(DATAPTR(x)); 
}

Rcomplex *(COMPLEX)(SEXP x) { 
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(x) != CPLXSXP)
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "COMPLEX", "complex", type2char(TYPEOF(x)));
#endif
    return reinterpret_cast<Rcomplex *>(DATAPTR(x)); 
}

SEXP *(VECTOR_PTR)(SEXP x)
{
  error(_("not safe to return vector pointer"));
  return NULL;
}

void (SET_STRING_ELT)(SEXP x, int i, SEXP v) { 
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "SET_STRING_ELT", "character vector", type2char(TYPEOF(x)));
    if(TYPEOF(v) != CHARSXP && TYPEOF(v) != NILSXP)
       error("Value of SET_STRING_ELT() must be a 'CHARSXP' not a '%s'", 
	     type2char(TYPEOF(v)));
#endif
    CHECK_OLD_TO_NEW(x, v);
    reinterpret_cast<SEXP *>(DATAPTR(x))[i] = v; 
}

SEXP (SET_VECTOR_ELT)(SEXP x, int i, SEXP v) { 
#ifdef USE_TYPE_CHECKING_STRICT
    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP) {
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "SET_VECTOR_ELT", "list", type2char(TYPEOF(x)));
    }
#elif defined(USE_TYPE_CHECKING)
    /* also allow STRSXP */
    if(TYPEOF(x) != VECSXP && TYPEOF(x) != STRSXP &&
       TYPEOF(x) != EXPRSXP && TYPEOF(x) != WEAKREFSXP) {
	error("%s() can only be applied to a '%s', not a '%s'", 
	      "SET_VECTOR_ELT", "list", type2char(TYPEOF(x)));
    }
#endif
    CHECK_OLD_TO_NEW(x, v); 
    return reinterpret_cast<SEXP *>(DATAPTR(x))[i] = v; 
}


/* List Accessors */

void (SET_TAG)(SEXP x, SEXP v)
{
    CHECK_OLD_TO_NEW(x, v);
    x->u.listsxp.tagval = v;
}

SEXP (SETCAR)(SEXP x, SEXP y)
{
    if (x == NULL || x == R_NilValue)
	error(_("bad value"));
    CHECK_OLD_TO_NEW(x, y);
    x->u.listsxp.carval = y;
    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    if (x == NULL || x == R_NilValue)
	error(_("bad value"));
    CHECK_OLD_TO_NEW(x, y);
    x->u.listsxp.cdrval = y;
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (x == NULL || x == R_NilValue ||
	CDR(x) == NULL || CDR(x) == R_NilValue)
	error(_("bad value"));
    cell = CDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    SETCAR(cell, y);
    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (x == NULL || x == R_NilValue ||
	CDR(x) == NULL || CDR(x) == R_NilValue ||
	CDDR(x) == NULL || CDDR(x) == R_NilValue)
	error(_("bad value"));
    cell = CDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    SETCAR(cell, y);
    return y;
}

namespace {
    inline SEXP CDDDR(SEXP x) {return CDR(CDR(CDR(x)));}

    inline SEXP CD4R(SEXP x) {return CDR(CDR(CDR(CDR(x))));}
}

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (x == NULL || x == R_NilValue ||
	CDR(x) == NULL || CDR(x) == R_NilValue ||
	CDDR(x) == NULL || CDDR(x) == R_NilValue ||
	CDDDR(x) == NULL || CDDDR(x) == R_NilValue)
	error(_("bad value"));
    cell = CDDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    SETCAR(cell, y);
    return y;
}

SEXP (SETCAD4R)(SEXP x, SEXP y)
{
    SEXP cell;
    if (x == NULL || x == R_NilValue ||
	CDR(x) == NULL || CDR(x) == R_NilValue ||
	CDDR(x) == NULL || CDDR(x) == R_NilValue ||
	CDDDR(x) == NULL || CDDDR(x) == R_NilValue ||
	CD4R(x) == NULL || CD4R(x) == R_NilValue)
	error(_("bad value"));
    cell = CD4R(x);
    CHECK_OLD_TO_NEW(cell, y);
    SETCAR(cell, y);
    return y;
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

/*******************************************/
/* Non-sampling memory use profiler
   reports all large vector heap
   allocations */
/*******************************************/

#ifndef R_MEMORY_PROFILING

SEXP attribute_hidden do_Rprofmem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    errorcall(call,_("memory profiling is not available on this system"));
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
		    TYPEOF(fun) == SYMSXP ? memCHAR(PRINTNAME(fun)) :
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
    Heap::setMonitor(0);
    return;
}

static void R_InitMemReporting(char *filename, int append,
			       R_size_t threshold)
{
    if(R_MemReportingOutfile != NULL) R_EndMemReporting();
    R_MemReportingOutfile = fopen(filename, append ? "a" : "w");
    if (R_MemReportingOutfile == NULL)
	error(_("Rprofmem: cannot open output file '%s'"), filename);
    Heap::setMonitor(R_ReportAllocation, threshold);
}

SEXP attribute_hidden do_Rprofmem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *filename;
    R_size_t threshold;
    int append_mode;

    checkArity(op, args);
    if (!isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
	errorcall(call, _("invalid '%s' argument"), "filename");
    append_mode = asLogical(CADR(args));
    filename = R_ExpandFileName(memCHAR(memSTRING_ELT(CAR(args), 0)));
    threshold = R_size_t(REAL(CADDR(args))[0]);
    if (strlen(filename))
	R_InitMemReporting(filename, append_mode, threshold);
    else
	R_EndMemReporting();
    return R_NilValue;
}

#endif /* R_MEMORY_PROFILING */
