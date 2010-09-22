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
 *  Copyright (C) 2002-3	      The R Foundation
 *  Copyright (C) 1999-2007   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
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

/*  This module contains support for S-style generic */
/*  functions and "class" support.  Gag, barf ...  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <R_ext/RS.h> /* for Calloc, Realloc and for S4 object bit */
#include "basedecl.h"
#include "CXXR/ClosureContext.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/ReturnBailout.hpp"

using namespace std;
using namespace CXXR;

static SEXP GetObject(ClosureContext *cptr)
{
    SEXP s, sysp, b, formals, funcall, tag;
    sysp = ClosureContext::innermost()->callEnvironment();

    PROTECT(funcall = R_syscall(0, cptr));

    if ( TYPEOF(CAR(funcall)) == SYMSXP )
	PROTECT(b = findFun(CAR(funcall), sysp));
    else
	PROTECT(b = eval(CAR(funcall), sysp));
    /**** use R_sysfunction here instead */
    if (TYPEOF(b) != CLOSXP) error(_("generic 'function' is not a function"));
    formals = FORMALS(b);

    tag = TAG(formals);
    if (tag != R_NilValue && tag != R_DotsSymbol) {
	s = R_NilValue;
	/** exact matches **/
	for (b = CXXRCCAST(PairList*, cptr->promiseArgs()) ; b != R_NilValue ; b = CDR(b))
	    if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), CXXRTRUE)) {
		if (s != R_NilValue)
		    error(_("formal argument \"%s\" matched by multiple actual arguments"), tag);
		else
		    s = CAR(b);
	    }

	if (s == R_NilValue)
	    /** partial matches **/
	    for (b = CXXRCCAST(PairList*, cptr->promiseArgs()) ; b != R_NilValue ; b = CDR(b))
		if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), CXXRFALSE)) {
		    if ( s != R_NilValue)
			error(_("formal argument \"%s\" matched by multiple actual arguments"), tag);
		    else
			s = CAR(b);
		}
	if (s == R_NilValue)
	    /** first untagged argument **/
	    for (b = CXXRCCAST(PairList*, cptr->promiseArgs()) ; b != R_NilValue ; b = CDR(b))
		if (TAG(b) == R_NilValue )
		{
		    s = CAR(b);
		    break;
		}
	if (s == R_NilValue)
	    s = CAR(CXXRCCAST(PairList*, cptr->promiseArgs()));
/*
	    error("failed to match argument for dispatch");
*/
    }
    else
	s = CAR(CXXRCCAST(PairList*, cptr->promiseArgs()));

    UNPROTECT(2);
    if (TYPEOF(s) == PROMSXP) {
	if (PRVALUE(s) == R_UnboundValue)
	    s = eval(s, R_BaseEnv);
	else
	    s = PRVALUE(s);
    }
    return(s);
}

static SEXP applyMethod(SEXP call, SEXP op, SEXP args, SEXP rho, SEXP newrho)
{
    Expression* callx = SEXP_downcast<Expression*>(call);
    SEXP ans = 0;
    if (TYPEOF(op) == SPECIALSXP || TYPEOF(op) == BUILTINSXP) {
	BuiltInFunction* func = static_cast<BuiltInFunction*>(op);
	GCStackRoot<PairList> argslist(SEXP_downcast<PairList*>(args));
	Environment* env = SEXP_downcast<Environment*>(newrho);
	ans = func->apply(callx, argslist, env);
    } else if (TYPEOF(op) == CLOSXP) {
	Closure* func = SEXP_downcast<Closure*>(op);
	PairList* arglist = SEXP_downcast<PairList*>(args);
	Environment* callenv = SEXP_downcast<Environment*>(rho);
	Environment* supp_env = SEXP_downcast<Environment*>(newrho);
	ans = func->invoke(callx, arglist, callenv, supp_env->frame());
    }
    return ans;
}


/* "newintoold" -  a destructive matching of arguments; */
/* newargs comes first; any element of oldargs with */
/* a name that matches a named newarg is deleted; the */
/* two resulting lists are appended and returned. */
/* S claims to do this (white book) but doesn't seem to. */

static SEXP newintoold(SEXP _new, SEXP old)
{
    if (_new == R_NilValue) return R_NilValue;
    SETCDR(_new, newintoold(CDR(_new),old));
    while (old != R_NilValue) {
	if (TAG(old) != R_NilValue && TAG(old) == TAG(_new)) {
	    SETCAR(old, CAR(_new));
	    return CDR(_new);
	}
	old = CDR(old);
    }
    return _new;
}

static SEXP matchmethargs(SEXP oldargs, SEXP newargs)
{
    newargs = newintoold(newargs, oldargs);
    return listAppend(oldargs, newargs);
}

#ifdef S3_for_S4_warn /* not currently used */
static SEXP s_check_S3_for_S4 = 0;
void R_warn_S3_for_S4(SEXP method) {
  SEXP call;
  if(!s_check_S3_for_S4)
    s_check_S3_for_S4 = install(".checkS3forS4");
  PROTECT(call = lang2(s_check_S3_for_S4, method));
  eval(call, R_MethodsNamespace);
  UNPROTECT(1);
}
#endif

/*  usemethod  -  calling functions need to evaluate the object
 *  (== 2nd argument).	They also need to ensure that the
 *  argument list is set up in the correct manner.
 *
 *    1. find the context for the calling function (i.e. the generic)
 *	 this gives us the unevaluated arguments for the original call
 *
 *    2. create an environment for evaluating the method and insert
 *	 a handful of variables (.Generic, .Class and .Method) into
 *	 that environment. Also copy any variables in the env of the
 *	 generic that are not formal (or actual) arguments.
 *
 *    3. fix up the argument list; it should be the arguments to the
 *	 generic matched to the formals of the method to be invoked */

SEXP R_LookupMethod(SEXP method, SEXP rho, SEXP callrho, SEXP defrho)
{
    if (TYPEOF(callrho) == NILSXP) {
	error(_("use of NULL environment is defunct"));
	callrho = R_BaseEnv;
    } else
	if (TYPEOF(callrho) != ENVSXP)
	    error(_("bad generic call environment"));
    if (TYPEOF(defrho) == NILSXP) {
	    error(_("use of NULL environment is defunct"));
	    defrho = R_BaseEnv;
    } else
	if (TYPEOF(defrho) != ENVSXP)
	    error(_("bad generic definition environment"));
    if (defrho == R_BaseEnv)
	defrho = R_BaseNamespace;

    Symbol* sym = SEXP_downcast<Symbol*>(method);
    pair<FunctionBase*, bool>
	pr = findS3Method(sym, static_cast<Environment*>(callrho),
			  static_cast<Environment*>(defrho));
    return (pr.first ? pr.first : R_UnboundValue);
}

#ifdef UNUSED
static int match_to_obj(SEXP arg, SEXP obj) {
  return (arg == obj) ||
    (TYPEOF(arg) == PROMSXP && PRVALUE(arg) == obj);
}
#endif

/* look up the class name in the methods package table of S3 classes
   which should be explicitly converted when an S3 method is applied
   to an object from an S4 subclass.
*/
int isBasicClass(const char *ss) {
    static SEXP s_S3table = 0;
    if(!s_S3table) {
      s_S3table = findVarInFrame3(R_MethodsNamespace, install(".S3MethodsClasses"), TRUE);
      if(s_S3table == R_UnboundValue)
	error(_("No .S3MethodsClass table, can't use S4 objects with S3 methods (methods package not attached?)"));
	if (TYPEOF(s_S3table) == PROMSXP)  /* findVar... ignores lazy data */
	    s_S3table = eval(s_S3table, R_MethodsNamespace);
    }
    if(s_S3table == R_UnboundValue)
      return FALSE; /* too screwed up to do conversions */
    return findVarInFrame3(s_S3table, install(ss), FALSE) != R_UnboundValue;
}
    
    

int usemethod(const char *generic, SEXP obj, SEXP call, SEXP args,
	      SEXP rho, SEXP callrho, SEXP defrho, SEXP *ans)
{
    Environment* callenv = SEXP_downcast<Environment*>(callrho);
    Environment* defenv = SEXP_downcast<Environment*>(defrho);

    ClosureContext *cptr = 0;

    // Get the context which UseMethod was called from.
    {
	FunctionContext* fcptr = FunctionContext::innermost();
	if (fcptr && fcptr->type() == Evaluator::Context::CLOSURE) {
	    cptr = static_cast<ClosureContext*>(fcptr);
	    if (cptr->workingEnvironment() != rho)
		cptr = 0;
	}
	if (!cptr)
	    Rf_error(_("'UseMethod' used in an inappropriate fashion"));
    }

    // Determine the functor:
    FunctionBase* op;
    {
	RObject* opcar = cptr->call()->car();
	if (opcar->sexptype() == LANGSXP)
	    opcar = opcar->evaluate(cptr->callEnvironment());
	switch (opcar->sexptype()) {
	case SYMSXP: {
	    const Symbol* symbol = static_cast<Symbol*>(opcar);
	    pair<Environment*, FunctionBase*> pr
		= findFunction(symbol, cptr->callEnvironment());
	    if (!pr.first)
		Rf_error(_("could not find function '%s'"),
			 symbol->name()->c_str());
	    op = pr.second;
	    break;
	}
	case CLOSXP:
	case BUILTINSXP:
	case SPECIALSXP:
	    op = static_cast<FunctionBase*>(opcar);
	    break;
	default:
	    Rf_error(_("Invalid generic function in 'usemethod'"));
	}
    }

    // Create a new frame without any of the formals to the
    // generic in it:
    GCStackRoot<Frame> newframe(GCNode::expose(new StdFrame));
    RObject* match_obj = 0;
    if (op->sexptype() == CLOSXP) {
	Closure* clos = static_cast<Closure*>(op);
	const Environment* generic_wk_env = cptr->workingEnvironment();
	// Look for definition of first formal argument of the generic:
	{
	    const PairList* formal_list = clos->matcher()->formalArgs();
	    if (formal_list) {
		const Symbol* first_formal
		    = static_cast<const Symbol*>(formal_list->tag());
		const Frame::Binding* bdg
		    = generic_wk_env->frame()->binding(first_formal);
		if (bdg)
		    match_obj = bdg->value();
	    }
	}
	// Prepare newframe:
	{
	    newframe = generic_wk_env->frame()->clone();
	    clos->stripFormals(newframe);
	}
    }

    GCStackRoot<const PairList> matchedarg(cptr->promiseArgs());
    GCStackRoot<StringVector> dotclass;  // value for .Class
    Symbol* method_symbol = 0;
    FunctionBase* method = 0;

    // Seek non-default method:
    {
	GCStackRoot<StringVector>
	    klass(SEXP_downcast<StringVector*>(R_data_class2(obj)));
	dotclass = klass;
	size_t nclass = klass->size();
	for (unsigned int i = 0; !method && i < nclass; ++i) {
	    const RObject* se = (*klass)[i];
	    string ss(translateChar(const_cast<RObject*>(se)));
	    method_symbol = Symbol::obtain(string(generic) + "." + ss);
	    method = findS3Method(method_symbol, callenv, defenv).first;
	    if (method && i > 0) {
		dotclass = GCNode::expose(new StringVector(nclass - i));
		for (unsigned int j = 0; j < dotclass->size(); ++j)
		    (*dotclass)[j] = (*klass)[j + i];
		dotclass->setAttribute(PreviousSymbol, klass);
		// In CR, the following very obscure code disappeared
		// some time after 2.11.1, so it can be expected to
		// disappear from CXXR in due course.  At that time
		// the declaration of newframe can be moved to a later
		// point in the function.
		if (obj && obj->isS4Object() && isBasicClass(ss.c_str())) {
		    SEXP S3Part = R_getS4DataSlot(obj, S4SXP);
		    if (S3Part && TYPEOF(obj) == S4SXP) // could be type, e.g. "environment"
			S3Part = R_getS4DataSlot(obj, ANYSXP);
		    // At this point S3Part is the S3 class object or an
		    // object of an abnormal type, or NULL.
		    if (S3Part) {  // use S3Part as inherited object
			obj = S3Part;
			if (!match_obj) // use the first arg, for "[",e.g.
			    match_obj = matchedarg->car();
			if (NAMED(obj))
			    SET_NAMED(obj, 2);
			if (match_obj->sexptype() == PROMSXP)
			    static_cast<Promise*>(match_obj)->setValue(obj); // must have been eval'd
			else {
			    // not possible ?
			    Closure* clos = SEXP_downcast<Closure*>(method);
			    const Symbol* sym
				= static_cast<const Symbol*>(clos->matcher()->formalArgs()->tag());
			    newframe->bind(sym, obj);
			}
		    } // else, use the S4 object
		}
	    }
	}
    }

    if (!method) {
	// Look for default method:
	method_symbol = Symbol::obtain(string(generic) + ".default");
	method = findS3Method(method_symbol, callenv, defenv).first;
    }

    if (!method)
	return 0;

    if (op->sexptype() == CLOSXP && (RDEBUG(op) || RSTEP(op)) )
	SET_RSTEP(method, 1);
    newframe->bind(DotClassSymbol, dotclass);
    newframe->bind(DotGenericSymbol,
		   GCNode::expose(new StringVector(generic)));
    CachedString* method_name
	= const_cast<CachedString*>(method_symbol->name());
    newframe->bind(DotMethodSymbol,
		   GCNode::expose(new StringVector(method_name)));
    newframe->bind(DotGenericCallEnvSymbol, callrho);
    newframe->bind(DotGenericDefEnvSymbol, defrho);
    GCStackRoot<Expression> newcall(cptr->call()->clone());
    newcall->setCar(method_symbol);
    GCStackRoot<Environment>
	newrho(GCNode::expose(new Environment(0, newframe)));
    *ans = applyMethod(newcall, method,
		       const_cast<PairList*>(matchedarg.get()),
		       rho, newrho);
    return 1;
}

/* Note: "do_usemethod" is not the only entry point to
   "usemethod". Things like [ and [[ call usemethod directly,
   hence do_usemethod should just be an interface to usemethod.
 */

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_usemethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    // Determine argList:
    GCStackRoot<> argList;
    {
	GCStackRoot<> ap(list2(R_NilValue, R_NilValue));
	SET_TAG(ap,  install("generic"));
	SET_TAG(CDR(ap), install("object"));
	argList =  matchArgs(ap, args, call);
    }

    if (CAR(argList) == R_MissingArg)
	errorcall(call, _("there must be a 'generic' argument"));	
    GCStackRoot<> generic(eval(CAR(argList), env));
    if(!isString(generic) || length(generic) != 1)
	errorcall(call, _("'generic' argument must be a character string"));

    ClosureContext *cptr = 0;
    {
	FunctionContext* fcptr = FunctionContext::innermost();
	if (fcptr && fcptr->type() == Evaluator::Context::CLOSURE) {
	    cptr = static_cast<ClosureContext*>(fcptr);
	    if (cptr->workingEnvironment() != env)
		cptr = 0;
	}
	if (!cptr)
	    Rf_errorcall(call,
			 _("'UseMethod' used in an inappropriate fashion"));
    }

    /* get environments needed for dispatching.
       callenv = environment from which the generic was called
       defenv = environment where the generic was defined */
    SEXP callenv = cptr->callEnvironment();
    SEXP defenv;
    /* We need to find the generic to find out where it is defined.
       This is set up to avoid getting caught by things like

	mycoef <- function(x)
       {
	   mycoef <- function(x) stop("not this one")
	   UseMethod("mycoef")
       }

	The generic need not be a closure (Henrik Bengtsson writes
	UseMethod("$"), although only functions are documented.)
    */
    SEXP val
	= findVar1(install(translateChar(STRING_ELT(generic, 0))),
		   ENCLOS(env), FUNSXP, TRUE); /* That has evaluated promises */
    if(TYPEOF(val) == CLOSXP) defenv = CLOENV(val);
    else defenv = R_BaseNamespace;

    GCStackRoot<> obj;
    if (CADR(argList) != R_MissingArg)
	obj = eval(CADR(argList), env);
    else
	obj = GetObject(cptr);

    if (TYPEOF(generic) != STRSXP ||
	LENGTH(generic) < 1 ||
	CHAR(STRING_ELT(generic, 0))[0] == '\0')
	errorcall(call, _("first argument must be a generic name"));

    SEXP ans;
    if (usemethod(translateChar(STRING_ELT(generic, 0)), obj, call, CDR(args),
		  env, callenv, defenv, &ans) == 1) {
	Environment* envir = SEXP_downcast<Environment*>(env);
	GCStackRoot<> ansrt(ans);
	if (!envir->canReturn())
	    Rf_error(_("no function to return from, jumping to top level"));
	ReturnBailout* rbo = GCNode::expose(new ReturnBailout(envir, ans));
	Evaluator::Context* callctxt
	    = Evaluator::Context::innermost()->nextOut();
	if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT)
	    rbo->throwException();
	return rbo;
    }
    else {
	string cl;
	GCStackRoot<> klass(R_data_class2(obj));
	int nclass = length(klass);
	if (nclass == 1) 
	    cl = translateChar(STRING_ELT(klass, 0));
	else {
	    cl = string("c('") + translateChar(STRING_ELT(klass, 0));
	    for (int i = 1; i < nclass; ++i)
		cl += string("', '") + translateChar(STRING_ELT(klass, i));
	    cl += "')";
	}
	errorcall(call, _("no applicable method for '%s'"
			  " applied to an object of class '%s'"),
		  translateChar(STRING_ELT(generic, 0)), cl.c_str());
    }
    /* Not reached */
    return R_NilValue;
}

/*
   fixcall: fixes up the call when arguments to the function may
   have changed; for now we only worry about tagged args, appending
   them if they are not already there
*/

static SEXP fixcall(SEXP call, SEXP args)
{
    SEXP s, t;
    int found;

    for(t = args; t != R_NilValue; t = CDR(t)) {
	if(TAG(t) != R_NilValue) {
		found = 0;
		for(s = call; CDR(s) != R_NilValue; s = CDR(s))
		    if(TAG(CDR(s)) == TAG(t)) found = 1;
		if( !found ) {
			SETCDR(s, allocList(1));
			SET_TAG(CDR(s), TAG(t));
			SETCAR(CDR(s), duplicate(CAR(t)));
		}
	}
    }
    return call;
}

/* If NextMethod has any arguments the first must be the generic */
/* the second the object and any remaining are matched with the */
/* formals of the chosen method. */

/* This is a special .Internal */
SEXP attribute_hidden do_nextmethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    ClosureContext* cptr = ClosureContext::innermost();

    /* get the env NextMethod was called from */
    Environment* sysp = cptr->callEnvironment();
    while (cptr && cptr->workingEnvironment() != sysp)
	cptr = ClosureContext::innermost(cptr->nextOut());
    if (cptr == NULL)
	error(_("'NextMethod' called from outside a function"));

    GCStackRoot<> newcall(cptr->call()->clone());

    /* eg get("print.ts")(1) */
    if (TYPEOF(CAR(CXXRCCAST(Expression*, cptr->call()))) == LANGSXP)
	error(_("'NextMethod' called from an anonymous function"));

    /* Find dispatching environments. Promises shouldn't occur, but
       check to be on the safe side.  If the variables are not in the
       environment (the method was called outside a method dispatch)
       then chose reasonable defaults. */
    SEXP callenv = findVarInFrame3(sysp, install(".GenericCallEnv"), TRUE);
    if (TYPEOF(callenv) == PROMSXP)
	callenv = eval(callenv, R_BaseEnv);
    else if (callenv == R_UnboundValue)
	callenv = env;
    SEXP defenv = findVarInFrame3(sysp, install(".GenericDefEnv"), TRUE);
    if (TYPEOF(defenv) == PROMSXP) defenv = eval(defenv, R_BaseEnv);
    else if (defenv == R_UnboundValue) defenv = R_GlobalEnv;

    /* set up the arglist */
    SEXP s; // *****
    s = R_LookupMethod(CAR(CXXRCCAST(Expression*, cptr->call())), env, callenv, defenv);
    if (TYPEOF(s) == SYMSXP && s == R_UnboundValue)
	error(_("no calling generic was found: was a method called directly?"));
    if (TYPEOF(s) != CLOSXP){ /* R_LookupMethod looked for a function */
	errorcall(R_NilValue,
		  _("'function' is not a function, but of type %d"),
		  TYPEOF(s));
    }
    /* get formals and actuals; attach the names of the formals to
       the actuals, expanding any ... that occurs */
    SEXP formals = FORMALS(s);
    GCStackRoot<> actuals(matchArgs(formals, CXXRCCAST(PairList*, cptr->promiseArgs()), call));

    int i = 0;
    GCStackRoot<> t;  // *****
    for(s = formals, t = actuals; s != R_NilValue; s = CDR(s), t = CDR(t)) {
	SET_TAG(t, TAG(s));
	if(TAG(t) == R_DotsSymbol) i = length(CAR(t));
    }
    GCStackRoot<> m;  // *****
    SEXP a;  // *****
    if(i) {   /* we need to expand out the dots */
	t = allocList(i+length(actuals)-1);
	for(s = actuals, m = t; s != R_NilValue; s = CDR(s)) {
	    if(TYPEOF(CAR(s)) == DOTSXP) {
		for(i = 1, a = CAR(s); a != R_NilValue;
		    a = CDR(a), i++, m = CDR(m)) {
		    SET_TAG(m, Symbol::obtainDotDotSymbol(i));
		    SETCAR(m, CAR(a));
		}
	    } else {
		SET_TAG(m, TAG(s));
		SETCAR(m, CAR(s));
		m = CDR(m);
	    }
	}
	actuals = t;
    }


    /* we can't duplicate because it would force the promises */
    /* so we do our own duplication of the promargs */

    GCStackRoot<> matchedarg(allocList(length(CXXRCCAST(PairList*, cptr->promiseArgs()))));
    for (t = matchedarg, s = CXXRCCAST(PairList*, cptr->promiseArgs()); t != R_NilValue;
	 s = CDR(s), t = CDR(t)) {
	SETCAR(t, CAR(s));
	SET_TAG(t, TAG(s));
    }
    for (t = matchedarg; t != R_NilValue; t = CDR(t)) {
	for (m = actuals; m != R_NilValue; m = CDR(m))
	    if (CAR(m) == CAR(t))  {
		if (CAR(m) == R_MissingArg) {
		    SEXP tmp = findVarInFrame3(cptr->workingEnvironment(), TAG(m), TRUE);
		    if (tmp == R_MissingArg) break;
		}
		SETCAR(t, mkPROMISE(TAG(m), cptr->workingEnvironment()));
		break;
	    }
    }
    /*
      Now see if there were any other arguments passed in
      Currently we seem to only allow named args to change
      or to be added, this is at variance with p. 470 of the
      White Book
    */

    s = CADDR(args); /* this is ... and we need to see if it's bound */
    if (s == R_DotsSymbol) {
	t = findVarInFrame3(env, s, TRUE);
	if (t != R_NilValue && t != R_MissingArg) {
	    // Convert t to a PairList:
	    {
		GCStackRoot<ConsCell> cc(SEXP_downcast<ConsCell*>(t.get()));
		t = ConsCell::convert<PairList>(cc);
	    }
	    s = matchmethargs(matchedarg, t);
	    matchedarg = s;
	    newcall = fixcall(newcall, matchedarg);
	}
    }
    else
	error(_("wrong argument ..."));

    /*
      .Class is used to determine the next method; if it doesn't
      exist the first argument to the current method is used
      the second argument to NextMethod is another option but
      isn't currently used).
    */
    GCStackRoot<> klass(findVarInFrame3(sysp, install(".Class"), TRUE));

    if (klass == R_UnboundValue) {
	s = GetObject(cptr);
	if (!isObject(s)) error(_("object not specified"));
	klass = getAttrib(s, R_ClassSymbol);
    }

    /* the generic comes from either the sysparent or it's named */
    GCStackRoot<> generic(findVarInFrame3(sysp, install(".Generic"), TRUE));
    if (generic == R_UnboundValue)
	generic = eval(CAR(args), env);
    if( generic == R_NilValue )
	error(_("generic function not specified"));

    if (!isString(generic) || length(generic) > 1)
	error(_("invalid generic argument to NextMethod"));

    if (CHAR(STRING_ELT(generic, 0))[0] == '\0')
	error(_("generic function not specified"));

    /* determine whether we are in a Group dispatch */

    GCStackRoot<> group(findVarInFrame3(sysp, install(".Group"), TRUE));
    if (group == R_UnboundValue)
	group = mkString("");

    if (!isString(group) || length(group) > 1)
	error(_("invalid 'group' argument found in NextMethod"));

    /* determine the root: either the group or the generic will be it */

    SEXP basename;
    if (CHAR(STRING_ELT(group, 0))[0] == '\0') basename = generic;
    else basename = group;

    SEXP nextfun = R_NilValue;

    /*
      Find the method currently being invoked and jump over the current call
      If t is R_UnboundValue then we called the current method directly
    */

    GCStackRoot<> method(findVarInFrame3(sysp, install(".Method"), TRUE));
    char b[512]; // *****
    if( method != R_UnboundValue) {
	const char *ss;
	if( !isString(method) )
	    error(_("wrong value for .Method"));
	for(i = 0; i < length(method); i++) {
	    ss = translateChar(STRING_ELT(method, i));
	    if(strlen(ss) >= 512)
		error(_("method name too long in '%s'"), ss);
	    sprintf(b, "%s", ss);
	    if(strlen(b)) break;
	}
	/* for binary operators check that the second argument's method
	   is the same or absent */
	for(int j = i; j < length(method); j++){
	    char bb[512];
	    const char *ss = translateChar(STRING_ELT(method, j));
	    if(strlen(ss) >= 512)
		error(_("method name too long in '%s'"), ss);
	    sprintf(bb, "%s", ss);
	    if (strlen(bb) && strcmp(b,bb))
		warning(_("Incompatible methods ignored"));
	}
    }
    else {
	if(strlen(CHAR(PRINTNAME(CAR(CXXRCCAST(Expression*, cptr->call()))))) >= 512)
	    error(_("call name too long in '%s'"),
		  CHAR(PRINTNAME(CAR(CXXRCCAST(Expression*, cptr->call())))));
	sprintf(b, "%s", CHAR(PRINTNAME(CAR(CXXRCCAST(Expression*, cptr->call())))));
    }

    const char* sb = translateChar(STRING_ELT(basename, 0));
    char buf[512]; // *****
    const char* sk;
    int j;  // *****
    for (j = 0; j < length(klass); j++) {
	sk = translateChar(STRING_ELT(klass, j));
	if(strlen(sb) + strlen(sk) + 2 > 512)
	    error(_("class name too long in '%s'"), sb);
	sprintf(buf, "%s.%s", sb, sk);
	if (!strcmp(buf, b)) break;
    }

    if (!strcmp(buf, b)) /* we found a match and start from there */
	j++;
    else
	j = 0;  /*no match so start with the first element of .Class */

    /* we need the value of i on exit from the for loop to figure out
       how many classes to drop. */

    const char* sg = translateChar(STRING_ELT(generic, 0));
    for (i = j ; i < length(klass); i++) {
	sk = translateChar(STRING_ELT(klass, i));
	if(strlen(sg) + strlen(sk) + 2 > 512)
	    error(_("class name too long in '%s'"), sg);
	sprintf(buf, "%s.%s", sg, sk);
	nextfun = R_LookupMethod(install(buf), env, callenv, defenv);
	if (isFunction(nextfun)) break;
	if (group != R_UnboundValue) {
	    /* if not Generic.foo, look for Group.foo */
	    if(strlen(sb) + strlen(sk) + 2 > 512)
		error(_("class name too long in '%s'"), sb);
	    sprintf(buf, "%s.%s", sb, sk);
	    nextfun = R_LookupMethod(install(buf), env, callenv, defenv);
	    if(isFunction(nextfun))
		break;
	}
	if (isFunction(nextfun))
	    break;
    }
    if (!isFunction(nextfun)) {
	sprintf(buf, "%s.default", sg);
	nextfun = R_LookupMethod(install(buf), env, callenv, defenv);
	/* If there is no default method, try the generic itself,
	   provided it is primitive or a wrapper for a .Internal
	   function of the same name.
	*/
	if (!isFunction(nextfun)) {
	    t = install(sg);
	    nextfun = findVar(t, env);
	    if (TYPEOF(nextfun) == PROMSXP)
		nextfun = eval(nextfun, env);
	    if (!isFunction(nextfun))
		error(_("no method to invoke"));
	    if (TYPEOF(nextfun) == CLOSXP) {
		if (INTERNAL(t) != R_NilValue)
		    nextfun = INTERNAL(t);
		else
		    error(_("no method to invoke"));
	    }
	}
    }
    GCStackRoot<> sv(allocVector(STRSXP, length(klass) - i));
    klass = duplicate(klass);
    m = GCNode::expose(new Environment(0));
    for (j = 0; j < length(sv); j++)
	SET_STRING_ELT(sv, j, duplicate(STRING_ELT(klass, i++)));
    setAttrib(s, install("previous"), klass);
    defineVar(install(".Class"), sv, m);
    /* It is possible that if a method was called directly that
       'method' is unset */
    if (method != R_UnboundValue) {
	/* for Ops we need `method' to be a vector */
	method = duplicate(method);
	for(j = 0; j < length(method); j++) {
	    if (strlen(CHAR(STRING_ELT(method,j))))
		SET_STRING_ELT(method, j,  mkChar(buf));
	}
    } else
	method = mkString(buf);
    defineVar(install(".Method"), method, m);
    defineVar(install(".GenericCallEnv"), callenv, m);
    defineVar(install(".GenericDefEnv"), defenv, m);

    method = install(buf);

    defineVar(install(".Generic"), generic, m);

    defineVar(install(".Group"), group, m);

    SETCAR(newcall, method);
    return applyMethod(newcall, nextfun, matchedarg, env, m);
}

/* primitive */
SEXP attribute_hidden do_unclass(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    switch(TYPEOF(CAR(args))) {
    case ENVSXP:
	errorcall(call, _("cannot unclass an environment"));
	break;
    case EXTPTRSXP:
	errorcall(call, _("cannot unclass an external pointer"));
	break;
    default:
	break;
    }
    if (isObject(CAR(args))) {
	SETCAR(args, duplicate(CAR(args)));
	setAttrib(CAR(args), R_ClassSymbol, R_NilValue);
    }
    return CAR(args);
}

static SEXP s_S4inherits;
static SEXP do_S4inherits(SEXP obj, SEXP what, SEXP which) {
    SEXP e, val;
    if(!s_S4inherits)
      s_S4inherits = install(".S4inherits");
    PROTECT(e = allocVector(LANGSXP, 4));
    SETCAR(e, s_S4inherits);
    val = CDR(e);
    SETCAR(val, obj);
    val = CDR(val);
    SETCAR(val, what);
    val = CDR(val);
    SETCAR(val, which);
    val = eval(e, R_MethodsNamespace);
    UNPROTECT(1);
    return val;
}


SEXP attribute_hidden do_inherits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, what, which;
    int i, j, nwhat, isvec, nclass;

    checkArity(op, args);

    x = CAR(args);
    if(IS_S4_OBJECT(x))
      return do_S4inherits(x, CADR(args), CADDR(args));
    GCStackRoot<> klass(R_data_class(x, FALSE));
    nclass = length(klass);

    what = CADR(args);
    if(!isString(what))
	error(_("'what' must be a character vector"));
    nwhat = length(what);

    which = CADDR(args);
    if( !isLogical(which) || (length(which) != 1) )
	error(_("'which' must be a length 1 logical vector"));
    isvec = asLogical(which);

#ifdef _be_too_picky_
    if(IS_S4_OBJECT(x) && nwhat == 1 && !isvec &&
       !isNull(R_getClassDef(translateChar(STRING_ELT(what, 0)))))
	warning(_("use 'is()' instead of 'inherits()' on S4 objects"));
#endif

    GCStackRoot<> rval;
    if(isvec)
	rval = allocVector(INTSXP, nwhat);

    for(j = 0; j < nwhat; j++) {
	const char *ss = translateChar(STRING_ELT(what, j));
	for(i = 0; i < nclass; i++) {
	    if(isvec)
		INTEGER(rval)[j] = 0;
	    if(!strcmp(translateChar(STRING_ELT(klass, i)), ss)) {
		if(isvec)
		   INTEGER(rval)[j] = i+1;
		else
		    return mkTrue();
		break;
	    }
	}
    }
    if(!isvec)
	return mkFalse();
    return rval;
}


/*
   ==============================================================

     code from here on down is support for the methods package

   ==============================================================
*/

/* standardGeneric:  uses a pointer to R_standardGeneric, to be
   initialized when the methods package is attached.  When and if the
   methods code is automatically included, the pointer will not be
   needed

*/
static R_stdGen_ptr_t R_standardGeneric_ptr = 0;
static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef);
#define NOT_METHODS_DISPATCH_PTR(ptr) (ptr == 0 || ptr == dispatchNonGeneric)

R_stdGen_ptr_t R_get_standardGeneric_ptr(void)
{
    return R_standardGeneric_ptr;
}

R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t val, SEXP envir)
{
    R_stdGen_ptr_t old = R_standardGeneric_ptr;
    R_standardGeneric_ptr = val;
    if(envir && !isNull(envir))
	R_MethodsNamespace = envir;
    /* just in case ... */
    if(!R_MethodsNamespace)
	R_MethodsNamespace = R_GlobalEnv;
    return old;
}

SEXP R_isMethodsDispatchOn(SEXP onOff) {
    SEXP value = allocVector(LGLSXP, 1);
    Rboolean onOffValue;
    R_stdGen_ptr_t old = R_get_standardGeneric_ptr();
    LOGICAL(value)[0] = !NOT_METHODS_DISPATCH_PTR(old);
    if(length(onOff) > 0) {
	    onOffValue = CXXRCONSTRUCT(Rboolean, asLogical(onOff));
	    if(onOffValue == FALSE)
		    R_set_standardGeneric_ptr(0, 0);
	    else if(NOT_METHODS_DISPATCH_PTR(old)) {
		    SEXP call;
		    PROTECT(call = allocList(2));
		    SETCAR(call, install("initMethodsDispatch"));
		    eval(call, R_GlobalEnv); /* only works with
						methods	 attached */
		    UNPROTECT(1);
	    }
    }
    return value;
}

/* simpler version for internal use */

attribute_hidden
Rboolean isMethodsDispatchOn(void)
{
    return CXXRCONSTRUCT(Rboolean, !NOT_METHODS_DISPATCH_PTR(R_standardGeneric_ptr));
}


static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef)
{
    /* dispatch the non-generic definition of `name'.  Used to trap
       calls to standardGeneric during the loading of the methods package */
    SEXP e, value, rho, fun, symbol, dot_Generic;
    ClosureContext *cptr;
    /* find a non-generic function */
    symbol = install(translateChar(asChar(name)));
    dot_Generic = install(".Generic");
    for(rho = ENCLOS(env); rho != R_EmptyEnv;
	rho = ENCLOS(rho)) {
	fun = findVarInFrame3(rho, symbol, TRUE);
	if(fun == R_UnboundValue) continue;
	switch(TYPEOF(fun)) {
	case CLOSXP:
	    value = findVarInFrame3(CLOENV(fun), dot_Generic, TRUE);
	    if(value == R_UnboundValue) break;
	case BUILTINSXP:  case SPECIALSXP:
	default:
	    /* in all other cases, go on to the parent environment */
	    break;
	}
	fun = R_UnboundValue;
    }
    fun = SYMVALUE(symbol);
    if(fun == R_UnboundValue)
	error(_("unable to find a non-generic version of function \"%s\""),
	      translateChar(asChar(name)));
    cptr = ClosureContext::innermost();
    /* check this is the right context */
    while (cptr && cptr->workingEnvironment() != env)
	cptr = ClosureContext::innermost(cptr->nextOut());

    PROTECT(e = duplicate(R_syscall(0, cptr)));
    SETCAR(e, fun);
    /* evaluate a call the non-generic with the same arguments and from
       the same environment as the call to the generic version */
    value = eval(e, cptr->callEnvironment());
    UNPROTECT(1);
    return value;
}


static SEXP get_this_generic(SEXP args);

SEXP attribute_hidden do_standardGeneric(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, value, fdef; R_stdGen_ptr_t ptr = R_get_standardGeneric_ptr();

    checkArity(op, args);
    check1arg(args, call, "f");

    if(!ptr) {
	warningcall(call,
		    _("standardGeneric called without methods dispatch enabled (will be ignored)"));
	R_set_standardGeneric_ptr(dispatchNonGeneric, NULL);
	ptr = R_get_standardGeneric_ptr();
    }

    checkArity(op, args); /* set to -1 */
    arg = CAR(args);
    if(!isValidStringF(arg))
	errorcall(call,
		  _("argument to standardGeneric must be a non-empty character string"));

    PROTECT(fdef = get_this_generic(args));

    if(isNull(fdef))
	error(_("call to standardGeneric(\"%s\") apparently not from the body of that generic function"), translateChar(STRING_ELT(arg, 0)));

    value = (*ptr)(arg, env, fdef);

    UNPROTECT(1);
    return value;
}

static int maxMethodsOffset = 0, curMaxOffset;
static Rboolean allowPrimitiveMethods = TRUE;
typedef enum {NO_METHODS, NEEDS_RESET, HAS_METHODS, SUPPRESSED} prim_methods_t;

static prim_methods_t *prim_methods;
static SEXP *prim_generics;
static SEXP *prim_mlist;
#define DEFAULT_N_PRIM_METHODS 100

/* This is used in the methods package, in src/methods_list_dispatch.c */
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef,
		       SEXP mlist)
{
    const char *code_string;
    if(!isValidString(code_vec))
	error(_("argument 'code' must be a character string"));
    code_string = translateChar(asChar(code_vec));
    /* with a NULL op, turns all primitive matching off or on (used to avoid possible infinite
     recursion in methods computations*/
    if(op == R_NilValue) {
	SEXP value;
	value = allowPrimitiveMethods ? mkTrue() : mkFalse();
	switch(code_string[0]) {
	case 'c': case 'C':/* clear */
	    allowPrimitiveMethods = FALSE; break;
	case 's': case 'S': /* set */
	    allowPrimitiveMethods = TRUE; break;
	default: /* just report the current state */
	    break;
	}
	return value;
    }
    do_set_prim_method(op, code_string, fundef, mlist);
    return(fname);
}

SEXP R_primitive_methods(SEXP op)
{
    int offset = PRIMOFFSET(op);
    if(offset < 0 || offset > curMaxOffset)
	return R_NilValue;
    else {
	SEXP value = prim_mlist[offset];
	return value ? value : R_NilValue;
    }
}

SEXP R_primitive_generic(SEXP op)
{
    int offset = PRIMOFFSET(op);
    if(offset < 0 || offset > curMaxOffset)
	return R_NilValue;
    else {
	SEXP value = prim_generics[offset];
	return value ? value : R_NilValue;
    }
}

/* This is used in the methods package, in src/methods_list_dispatch.c */
SEXP do_set_prim_method(SEXP op, const char *code_string, SEXP fundef,
			SEXP mlist)
{
    int offset = 0;
    prim_methods_t code = NO_METHODS; /* -Wall */
    SEXP value;
    Rboolean errorcase = FALSE;
    switch(code_string[0]) {
    case 'c': /* clear */
	code = NO_METHODS; break;
    case 'r': /* reset */
	code = NEEDS_RESET; break;
    case 's': /* set or suppress */
	switch(code_string[1]) {
	case 'e': code = HAS_METHODS; break;
	case 'u': code = SUPPRESSED; break;
	default: errorcase = TRUE;
	}
	break;
    default:
	errorcase = TRUE;
    }
    if(errorcase) {
	error(_("invalid primitive methods code (\"%s\"): should be \"clear\", \"reset\", \"set\", or \"suppress\""), code_string);
	return R_NilValue;
    }
    switch(TYPEOF(op)) {
    case BUILTINSXP: case SPECIALSXP:
	offset = PRIMOFFSET(op);
	break;
    default:
	error(_("invalid object: must be a primitive function"));
    }
    if(offset >= maxMethodsOffset) {
	int n;
	n = offset + 1;
	if(n < DEFAULT_N_PRIM_METHODS)
	    n = DEFAULT_N_PRIM_METHODS;
	if(n < 2*maxMethodsOffset)
	    n = 2 * maxMethodsOffset;
	if(prim_methods) {
	    int i;

	    prim_methods  = Realloc(prim_methods,  n, prim_methods_t);
	    prim_generics = Realloc(prim_generics, n, SEXP);
	    prim_mlist	  = Realloc(prim_mlist,	   n, SEXP);

	    /* Realloc does not clear the added memory, hence: */
	    for (i = maxMethodsOffset ; i < n ; i++) {
		prim_methods[i]	 = NO_METHODS;
		prim_generics[i] = NULL;
		prim_mlist[i]	 = NULL;
	    }
	}
	else {
	    prim_methods  = Calloc(n, prim_methods_t);
	    prim_generics = Calloc(n, SEXP);
	    prim_mlist	  = Calloc(n, SEXP);
	}
	maxMethodsOffset = n;
    }
    if(offset > curMaxOffset)
	curMaxOffset = offset;
    prim_methods[offset] = code;
    /* store a preserved pointer to the generic function if there is not
       one there currently.  Unpreserve it if no more methods, but don't
       replace it otherwise:  the generic definition is not allowed to
       change while it's still defined! (the stored methods list can,
       however) */
    value = prim_generics[offset];
    if(code == SUPPRESSED) {} /* leave the structure alone */
    else if(code == NO_METHODS && prim_generics[offset]) {
	R_ReleaseObject(prim_generics[offset]);
	prim_generics[offset] = 0;
	prim_mlist[offset] = 0;
    }
    else if(fundef && !isNull(fundef) && !prim_generics[offset]) {
	if(TYPEOF(fundef) != CLOSXP)
	    error(_("the formal definition of a primitive generic must be a function object (got type '%s')"),
		  type2char(TYPEOF(fundef)));
	R_PreserveObject(fundef);
	prim_generics[offset] = fundef;
    }
    if(code == HAS_METHODS) {
	if(!mlist  || isNull(mlist)) {
	    /* turning methods back on after a SUPPRESSED */
	} else {
	    if(prim_mlist[offset])
		R_ReleaseObject(prim_mlist[offset]);
	    R_PreserveObject(mlist);
	    prim_mlist[offset] = mlist;
	}
    }
    return value;
}

static SEXP get_primitive_methods(SEXP op, SEXP rho)
{
    SEXP f, e, val;
    int nprotect = 0;
    f = PROTECT(allocVector(STRSXP, 1));  nprotect++;
    SET_STRING_ELT(f, 0, mkChar(PRIMNAME(op)));
    PROTECT(e = allocVector(LANGSXP, 2)); nprotect++;
    SETCAR(e, install("getGeneric"));
    val = CDR(e); SETCAR(val, f);
    val = eval(e, rho);
    /* a rough sanity check that this looks like a generic function */
    if(TYPEOF(val) != CLOSXP || !IS_S4_OBJECT(val))
	error(_("object returned as generic function \"%s\" doesn't appear to be one"), PRIMNAME(op));
    UNPROTECT(nprotect);
    return CLOENV(val);
}


/* get the generic function, defined to be the function definition for
the call to standardGeneric(), or for primitives, passed as the second
argument to standardGeneric.
*/
static SEXP get_this_generic(SEXP args)
{
    SEXP value = R_NilValue; static SEXP gen_name;
    int i, n;
    ClosureContext *cptr;
    const char *fname;

    /* a second argument to the call, if any, is taken as the function */
    if(CDR(args) != R_NilValue)
	return CAR(CDR(args));
    /* else use sys.function (this is fairly expensive-- would be good
     * to force a second argument if possible) */
    PROTECT(args);
    if(!gen_name)
	gen_name = install("generic");
    cptr = ClosureContext::innermost();
    fname = translateChar(asChar(CAR(args)));
    n = framedepth(cptr);
    /* check for a matching "generic" slot */
    for(i=0;  i<n; i++) {
	SEXP rval = R_sysfunction(i, cptr);
	if(isObject(rval)) {
	    SEXP generic = getAttrib(rval, gen_name);
	    if(TYPEOF(generic) == STRSXP &&
	       !strcmp(translateChar(asChar(generic)), fname)) {
	      value = rval;
	      break;
	    }
	}
    }
    UNPROTECT(1);
    return(value);
}

/* Could there be methods for this op?	Checks
   only whether methods are currently being dispatched and, if so,
   whether methods are currently defined for this op. */
Rboolean R_has_methods(SEXP op)
{
    R_stdGen_ptr_t ptr = R_get_standardGeneric_ptr(); int offset;
    if(NOT_METHODS_DISPATCH_PTR(ptr))
	return(FALSE);
    if(!op || TYPEOF(op) == CLOSXP) /* except for primitives, just test for the package */
	return(TRUE);
    if(!allowPrimitiveMethods) /* all primitives turned off by a call to R_set_prim */
	return FALSE;
    offset = PRIMOFFSET(op);
    if(offset > curMaxOffset || prim_methods[offset] == NO_METHODS
       || prim_methods[offset] == SUPPRESSED)
	return(FALSE);
    return(TRUE);
}

static SEXP deferred_default_object;

SEXP R_deferred_default_method()
{
    if(!deferred_default_object)
	deferred_default_object = install("__Deferred_Default_Marker__");
    return(deferred_default_object);
}


static R_stdGen_ptr_t quick_method_check_ptr = NULL;
void R_set_quick_method_check(R_stdGen_ptr_t value)
{
    quick_method_check_ptr = value;
}

/* try to dispatch the formal method for this primitive op, by calling
   the stored generic function corresponding to the op.	 Requires that
   the methods be set up to return a special object rather than trying
   to evaluate the default (which would get us into a loop). */

/* called from DispatchOrEval, DispatchGroup, do_matprod
   When called from the first the arguments have been enclosed in
   promises, but not from the other two: there all the arguments have
   already been evaluated.
 */
pair<bool, SEXP> attribute_hidden
R_possible_dispatch(SEXP call, SEXP op, SEXP args, SEXP rho,
		    Rboolean promisedArgs)
{
    Expression* callx = SEXP_downcast<Expression*>(call);
    GCStackRoot<PairList> arglist(SEXP_downcast<PairList*>(args));
    Environment* callenv = SEXP_downcast<Environment*>(rho);
    SEXP value;
    GCStackRoot<> mlist;
    int offset = PRIMOFFSET(op);
    if(offset < 0 || offset > curMaxOffset)
	error(_("invalid primitive operation given for dispatch"));
    prim_methods_t current = prim_methods[offset];
    if(current == NO_METHODS || current == SUPPRESSED)
	return pair<bool, SEXP>(false, 0);
    // check that the methods for this function have been set
    if(current == NEEDS_RESET) {
	// get the methods and store them in the in-core primitive
	// method table.	The entries will be preserved via
	// R_preserveobject, so later we can just grab mlist from
	// prim_mlist 
	do_set_prim_method(op, "suppressed", R_NilValue, mlist);
	mlist = get_primitive_methods(op, rho);
	do_set_prim_method(op, "set", R_NilValue, mlist);
	current = prim_methods[offset]; // as revised by do_set_prim_method
    }
    mlist = prim_mlist[offset];
    if(mlist && !isNull(mlist)
       && quick_method_check_ptr) {
	value = (*quick_method_check_ptr)(args, mlist, op);
	if(isPrimitive(value))
	    return pair<bool, SEXP>(false, 0);
	if(isFunction(value)) {
	    Closure* func = static_cast<Closure*>(value);
	    // found a method, call it with promised args
	    if(!promisedArgs) {
		PairList* pargs
		    = ArgMatcher::prepareArgs(callx->tail(), callenv);
		PairList *a, *b;
		for (a = arglist, b = pargs;
		     a != 0 && b != 0;
		     a = a->tail(), b = b->tail())
		    SET_PRVALUE(b->car(), a->car());
		// Check for unequal list lengths:
		if (a != 0 || b != 0)
		    error(_("dispatch error"));
		arglist = pargs;
	    }
	    value = func->invoke(callx, arglist, callenv);
	    return make_pair(true, value);
	}
	// else, need to perform full method search
    }
    RObject* fundef = prim_generics[offset];
    if(!fundef || TYPEOF(fundef) != CLOSXP)
	error(_("primitive function \"%s\" has been set for methods"
		" but no generic function supplied"),
	      PRIMNAME(op));
    Closure* func = static_cast<Closure*>(fundef);
    // To do:  arrange for the setting to be restored in case of an
    // error in method search
    if(!promisedArgs) {
	PairList* pargs = ArgMatcher::prepareArgs(callx->tail(), callenv);
	PairList *a, *b;
	for (a = arglist, b = pargs;
	     a != 0 && b != 0;
	     a = a->tail(), b = b->tail())
	    SET_PRVALUE(b->car(), a->car());
	// Check for unequal list lengths:
	if (a != 0 || b != 0)
	    error(_("dispatch error"));
	arglist = pargs;
    }
    value = func->invoke(callx, arglist, callenv);
    prim_methods[offset] = current;
    if (value == deferred_default_object)
	return pair<bool, SEXP>(false, 0);
    else
	return make_pair(true, value);
}

SEXP R_do_MAKE_CLASS(const char *what)
{
    static SEXP s_getClass = NULL;
    SEXP e, call;
    if(!what)
	error(_("C level MAKE_CLASS macro called with NULL string pointer"));
    if(!s_getClass) s_getClass = install("getClass");
    PROTECT(call = allocVector(LANGSXP, 2));
    SETCAR(call, s_getClass);
    SETCAR(CDR(call), mkString(what));
    e = eval(call, R_GlobalEnv);
    UNPROTECT(1);
    return(e);
}

/* this very similar, but gives NULL instead of an error for a non-existing class */
SEXP R_getClassDef(const char *what)
{
    static SEXP s_getClassDef = NULL;
    SEXP e, call;
    if(!what)
	error(_("R_getClassDef(.) called with NULL string pointer"));
    if(!s_getClassDef) s_getClassDef = install("getClassDef");
    PROTECT(call = allocVector(LANGSXP, 2));
    SETCAR(call, s_getClassDef);
    SETCAR(CDR(call), mkString(what));
    e = eval(call, R_GlobalEnv);
    UNPROTECT(1);
    return(e);
}

SEXP R_do_new_object(SEXP class_def)
{
    static SEXP s_virtual = NULL, s_prototype, s_className;
    SEXP e, value;
    if(!s_virtual) {
	s_virtual = install("virtual");
	s_prototype = install("prototype");
	s_className = install("className");
    }
    if(!class_def)
	error(_("C level NEW macro called with null class definition pointer"));
    e = R_do_slot(class_def, s_virtual);
    if(asLogical(e) != 0)  { /* includes NA, TRUE, or anything other than FALSE */
	e = R_do_slot(class_def, s_className);
	error(_("trying to generate an object from a virtual class (\"%s\")"),
	      translateChar(asChar(e)));
    }
    e = R_do_slot(class_def, s_className);
    value = duplicate(R_do_slot(class_def, s_prototype));
    if(TYPEOF(value) == S4SXP || getAttrib(e, R_PackageSymbol) != R_NilValue)
    { /* Anything but an object from a base "class" (numeric, matrix,..) */
	GCStackRoot<> valrt(value);
	setAttrib(value, R_ClassSymbol, e);
	SET_S4_OBJECT(value);
    }
    return value;
}

Rboolean attribute_hidden R_seemsOldStyleS4Object(SEXP object)
{
    SEXP klass;
    if(!isObject(object) || IS_S4_OBJECT(object)) return FALSE;
    /* We want to know about S4SXPs with no S4 bit */
    /* if(TYPEOF(object) == S4SXP) return FALSE; */
    klass = getAttrib(object, R_ClassSymbol);
    return (klass != R_NilValue && LENGTH(klass) == 1 &&
	    getAttrib(klass, R_PackageSymbol) != R_NilValue) ? TRUE: FALSE;
}



SEXP R_isS4Object(SEXP object)
{
    /* wanted: return isS4(object) ? mkTrue() : mkFalse(); */
    return IS_S4_OBJECT(object) ? mkTrue() : mkFalse(); ;
}

SEXP R_setS4Object(SEXP object, SEXP onOff, SEXP do_complete)
{
    Rboolean flag = CXXRCONSTRUCT(Rboolean, asLogical(onOff)), complete = CXXRCONSTRUCT(Rboolean, asInteger(do_complete));
    if(flag == CXXRCONSTRUCT(Rboolean, IS_S4_OBJECT(object)))
	return object;
    else
      return asS4(object, flag, complete);
}

SEXP R_get_primname(SEXP object)
{
    SEXP f;
    if(TYPEOF(object) != BUILTINSXP && TYPEOF(object) != SPECIALSXP)
	error(_("'R_get_primname' called on a non-primitive"));
    PROTECT(f = allocVector(STRSXP, 1));
    SET_STRING_ELT(f, 0, mkChar(PRIMNAME(object)));
    UNPROTECT(1);
    return f;
}

Rboolean isS4(SEXP s)
{
    return IS_S4_OBJECT(s);
}

SEXP asS4(SEXP s, Rboolean flag, int complete)
{
    if(flag == IS_S4_OBJECT(s))
	return s;
    PROTECT(s);
    if(NAMED(s) == 2)
	s = duplicate(s);
    UNPROTECT(1);
    if(flag) SET_S4_OBJECT(s);
    else {
	if(complete) {
	    SEXP value;
	    /* TENTATIVE:  how much does this change? */
	    if((value = R_getS4DataSlot(s, ANYSXP))
	       != R_NilValue && !IS_S4_OBJECT(value))
	      return value;
	    /* else no plausible S3 object*/
	    else if(complete == 1) /* ordinary case (2, for conditional) */
	      error(_("Object of class \"%s\" does not correspond to a valid S3 object"),
		      CHAR(STRING_ELT(R_data_class(s, FALSE), 0)));
	    else return s; /*  unchanged */
	}
	UNSET_S4_OBJECT(s);
    }
    return s;
}
