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
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2013   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include "basedecl.h"
#include "CXXR/FunctionBase.h"
#include "CXXR/FunctionContext.hpp"

using namespace CXXR;

SEXP attribute_hidden do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;

    checkArity(op,args);
#define find_char_fun \
    if (isValidString(CAR(args))) {				\
	SEXP s;							\
	PROTECT(s = installTrChar(STRING_ELT(CAR(args), 0)));	\
	SETCAR(args, findFun(s, rho));				\
	UNPROTECT(1);						\
    }
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP && TYPEOF(CAR(args)) != SPECIALSXP 
         &&  TYPEOF(CAR(args)) != BUILTINSXP )
	errorcall(call, _("argument must be a closure"));
    switch(PRIMVAL(op)) {
    case 0:
	SET_RDEBUG(CAR(args), CXXRTRUE);
	break;
    case 1:
	if( RDEBUG(CAR(args)) != 1 )
	    warningcall(call, "argument is not being debugged");
	SET_RDEBUG(CAR(args), CXXRFALSE);
	break;
    case 2:
        ans = ScalarLogical(RDEBUG(CAR(args)));
        break;
    case 3:
        SET_RSTEP(CAR(args), 1);
        break;
    }
    return ans;
}

/* primitives .primTrace and .primUntrace */
SEXP attribute_hidden do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP)
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


/* maintain global trace state */

SEXP attribute_hidden do_traceOnOff(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP onOff = CAR(args);
    Rboolean prev = Rboolean(FunctionBase::tracingEnabled());
    if(length(onOff) > 0) {
	Rboolean _new = CXXRCONSTRUCT(Rboolean, asLogical(onOff));
	if(_new == TRUE || _new == FALSE)
	    FunctionBase::enableTracing(_new);
	else
	    error("Value for tracingState must be TRUE or FALSE");
    }
    return ScalarLogical(prev);
}

Rboolean attribute_hidden
R_current_trace_state() { return Rboolean(FunctionBase::tracingEnabled()); }


/* memory tracing */
/* report when a traced object is duplicated */

SEXP attribute_hidden do_tracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object;
    char buffer[21];

    checkArity(op, args);
    check1arg(args, call, "x");

    object = CAR(args);
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
#else
    errorcall(call, _("R was not compiled with support for memory profiling"));
    return R_NilValue;
#endif
}


SEXP attribute_hidden do_untracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object;

    checkArity(op, args);
    check1arg(args, call, "x");

    object=CAR(args);
    object->setMemoryTracing(false);
#else
    errorcall(call, _("R was not compiled with support for memory profiling"));
#endif
    return R_NilValue;
}


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

SEXP attribute_hidden do_retracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object, previous, ans, ap, argList;
    char buffer[21];

    PROTECT(ap = list2(R_NilValue, R_NilValue));
    SET_TAG(ap,  install("x"));
    SET_TAG(CDR(ap), install("previous"));
    PROTECT(argList =  matchArgs(ap, args, call));
    if(CAR(argList) == R_MissingArg) SETCAR(argList, R_NilValue);
    if(CADR(argList) == R_MissingArg) SETCAR(CDR(argList), R_NilValue);

    object = CAR(ap);
    previous = CADR(ap);
    if(!isNull(previous) && !isString(previous))
	    errorcall(call, _("invalid '%s' argument"), "previous");

    if (object->memoryTraced()){
	snprintf(buffer, 21, "<%p>", (void *) object);
	ans = mkString(buffer);
    } else {
	R_Visible = CXXRFALSE;
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
    UNPROTECT(2);
    return ans;
#else
    R_Visible = CXXRFALSE; /* for consistency with other case */
    return R_NilValue;
#endif
}
