/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2015   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include "basedecl.h"
#include "rho/Closure.hpp"
#include "rho/FunctionBase.hpp"
#include "rho/FunctionContext.hpp"

using namespace rho;

SEXP attribute_hidden do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;

#define find_char_fun \
    if (isValidString(CAR(args))) {				\
	SEXP s;							\
	PROTECT(s = installTrChar(STRING_ELT(CAR(args), 0)));	\
	SETCAR(args, findFun(s, rho));				\
	UNPROTECT(1);						\
    }
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP)
	errorcall(call, _("argument must be a function"));
    switch(PRIMVAL(op)) {
    case 0: // debug()
	SET_RDEBUG(CAR(args), RHO_TRUE);
	break;
    case 1: // undebug()
	if( RDEBUG(CAR(args)) != 1 )
	    warningcall(call, "argument is not being debugged");
	SET_RDEBUG(CAR(args), RHO_FALSE);
	break;
    case 2: // isdebugged()
	ans = ScalarLogical(RDEBUG(CAR(args)));
	break;
    case 3: // debugonce()
	SET_RSTEP(CAR(args), 1);
	break;
    }
    return ans;
}

/* primitives .primTrace() and .primUntrace() */
SEXP attribute_hidden do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP)
	    errorcall(call, _("argument must be a function"));

    switch(PRIMVAL(op)) {
    case 0:
	SET_RTRACE(CAR(args), 1);
	break;
    case 1:
	SET_RTRACE(CAR(args), 0);
	break;
    }
    return R_NilValue;
}


/* maintain global trace & debug state */

SEXP attribute_hidden do_traceOnOff(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* on_)
{
    SEXP onOff = on_;
    bool trace = op->variant() == 0;  // Otherwise it's debug.
    Rboolean prev = Rboolean(trace ? FunctionBase::tracingEnabled()
			     : Closure::debuggingEnabled());
    if(length(onOff) > 0) {
	Rboolean _new = RHOCONSTRUCT(Rboolean, asLogical(onOff));
	if(_new == TRUE || _new == FALSE) {
	    if (trace)
		FunctionBase::enableTracing(_new);
	    else
		Closure::enableDebugging(_new);
	}
	else
	    error(_("Value for '%s' must be TRUE or FALSE"),
		  trace ? "tracingState" : "debuggingState");
    }
    return ScalarLogical(prev);
}

// GUIs, packages, etc can query:
Rboolean attribute_hidden
R_current_trace_state() { return Rboolean(FunctionBase::tracingEnabled()); }

Rboolean attribute_hidden
R_current_debugging_state() { return Rboolean(Closure::debuggingEnabled()); }


/* memory tracing */
/* report when a traced object is duplicated */

#ifdef R_MEMORY_PROFILING
SEXP attribute_hidden do_tracemem(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::Environment* rho, rho::RObject* const* args, int num_args, const rho::PairList* tags)
{
    SEXP object;
    char buffer[21];

    object = args[0];
    if(object == R_NilValue)
	errorcall(call, _("cannot trace NULL"));

    if(TYPEOF(object) == ENVSXP || TYPEOF(object) == PROMSXP)
	warningcall(call,
		    _("'tracemem' is not useful for promise and environment objects"));
    if(TYPEOF(object) == EXTPTRSXP || TYPEOF(object) == WEAKREFSXP)
	warningcall(call,
		    _("'tracemem' is not useful for weak reference or external pointer objects"));

    object->setMemoryTracing(true);
    snprintf(buffer, 21, "<%p>", (void *) object);
    return mkString(buffer);
}


SEXP attribute_hidden do_untracemem(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::Environment* rho, rho::RObject* const* args, int num_args, const rho::PairList* tags)
{
    SEXP object;

    object=args[0];
    object->setMemoryTracing(false);
    return R_NilValue;
}

#else

SEXP attribute_hidden do_tracemem(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_)
{
    errorcall(call, _("R was not compiled with support for memory profiling"));
}

SEXP attribute_hidden do_untracemem(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_)
{
    errorcall(call, _("R was not compiled with support for memory profiling"));
}

#endif /* R_MEMORY_PROFILING */

#ifdef R_MEMORY_PROFILING
static void memtrace_stack_dump(void)
{
    Evaluator::Context *cptr;

    for (cptr = Evaluator::Context::innermost();
	 cptr; cptr = cptr->nextOut()) {
	Evaluator::Context::Type type = cptr->type();
	if (type == Evaluator::Context::FUNCTION
	    || type == Evaluator::Context::CLOSURE) {
	    FunctionContext* fctxt = static_cast<FunctionContext*>(cptr);
	    SEXP fun = fctxt->call()->car();
	    Rprintf("%s ",
		    TYPEOF(fun) == SYMSXP ? translateChar(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    Rprintf("\n");
}

void RObject::traceMemory(const RObject* src1, const RObject* src2,
			  const RObject* src3)
{
    setMemoryTracing(true);
    Rprintf("tracemem[");
    bool needs_comma = false;
    if (src1->memoryTraced()) {
	Rprintf("%p", src1);
	needs_comma = true;
    }
    if (src2 && src2->memoryTraced()) {
	if (needs_comma)
	    Rprintf(", ");
	Rprintf("%p", src2);
	needs_comma = true;
    }
    if (src3 && src3->memoryTraced()) {
	if (needs_comma)
	    Rprintf(", ");
	Rprintf("%p", src3);
    }
    Rprintf(" -> %p]: ", this);
    memtrace_stack_dump();
}

#endif /* R_MEMORY_PROFILING */

SEXP do_retracemem(SEXP call, SEXP op, SEXP arg, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object, previous, ans, argList;
    char buffer[21];
    static SEXP do_retracemem_formals = NULL;

    if (do_retracemem_formals == NULL)
	do_retracemem_formals = allocFormalsList2(install("x"),
						  R_PreviousSymbol);

    PROTECT(argList =  matchArgs(do_retracemem_formals, args, call));
    if(CAR(argList) == R_MissingArg) SETCAR(argList, R_NilValue);
    if(CADR(argList) == R_MissingArg) SETCAR(CDR(argList), R_NilValue);

    object = CAR(arglist);
    previous = CADR(arglist);
    if(!isNull(previous) && !isString(previous))
	    errorcall(call, _("invalid '%s' argument"), "previous");

    if (object->memoryTraced()){
	snprintf(buffer, 21, "<%p>", (void *) object);
	ans = mkString(buffer);
    } else {
	R_Visible = RHO_FALSE;
	ans = R_NilValue;
    }

    if (previous != R_NilValue){
	object->setMemoryTracing(true);
	if (R_current_trace_state()) {
	    /* FIXME: previous will have <0x....> whereas other values are
	       without the < > */
	    Rprintf("tracemem[%s -> %p]: ",
		    translateChar(STRING_ELT(previous, 0)), (void *) object);
	    memtrace_stack_dump();
	}
    }
    UNPROTECT(1);
    return ans;
#else
    R_Visible = RHO_FALSE; /* for consistency with other case */
    return R_NilValue;
#endif
}
