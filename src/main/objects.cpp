/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2002-3	      The R Foundation
 *  Copyright (C) 1999-2013   The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <R_ext/RS.h> /* for Calloc, Realloc and for S4 object bit */
#include "basedecl.h"
#include "CXXR/ArgList.hpp"
#include "CXXR/ClosureContext.hpp"
#include "CXXR/DottedArgs.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/ListFrame.hpp"
#include "CXXR/ReturnBailout.hpp"
#include "CXXR/S3Launcher.hpp"

using namespace CXXR;

static RObject* GetObject(ClosureContext *cptr)
{
    Environment* callenv = cptr->callEnvironment();

    // Determine the generic closure:
    const Closure* closure;
    {
	const Expression* funcall(cptr->call());
	RObject* op(funcall->car());
	RObject* func;
	if (op->sexptype() == SYMSXP)
	    func = findFunction(static_cast<Symbol*>(op), callenv);
	else
	    func = Evaluator::evaluate(op, callenv);
	if (func->sexptype() != CLOSXP)
	    Rf_error(_("generic 'function' is not a function"));
	closure = static_cast<Closure*>(func);
    }

    // Get name of first formal argument:
    const Symbol* formal1;
    {
	const PairList* formals = closure->matcher()->formalArgs();
	formal1 = static_cast<const Symbol*>(formals->tag());
    }

    if (formal1 && formal1 != DotsSymbol) {
	// Get value of first formal argument:
	Frame::Binding* bdg
	    = cptr->workingEnvironment()->frame()->binding(formal1);
	if (bdg->origin() != Frame::Binding::MISSING)
	    return bdg->forcedValue();
    }

    // If we reach this point, either there was no first formal
    // argument, or it was "..." or was unbound.  In that case we use
    // the first *actual* argument as the object.  (This behaviour
    // follows CR, but does not appear to be documented in the R
    // language definition.)
    {
	const PairList* pargs = cptr->promiseArgs();
	if (!pargs)
	    return R_NilValue;
	return forceIfPromise(pargs->car());
    }
}

static RObject* applyMethod(const Expression* call, const FunctionBase* func,
			    ArgList* arglist, Environment* env,
			    Frame* method_bindings)
{
    RObject* ans;
    if (func->sexptype() == CLOSXP) {
	const Closure* clos = static_cast<const Closure*>(func);
	ans = clos->invoke(env, arglist, call, method_bindings);
    } else {
	GCStackRoot<Environment>
	    newenv(new Environment(nullptr, method_bindings));
	ans = func->apply(arglist, newenv, call);
    }
    return ans;
}


/* R_MethodsNamespace is initialized to R_GlobalEnv when R is
   initialized.  If it set to the methods namespace when the latter is
   loaded, and back to R_GlobalEnv when it is unloaded. */

#ifdef S3_for_S4_warn /* not currently used */
static GCRoot<> s_check_S3_for_S4 = 0;
void R_warn_S3_for_S4(SEXP method) {
  SEXP call;
  if(!s_check_S3_for_S4)
    s_check_S3_for_S4 = Rf_install(".checkS3forS4");
  PROTECT(call = lang2(s_check_S3_for_S4, method));
  Rf_eval(call, R_MethodsNamespace);
  UNPROTECT(1);
}
#endif

/*  Rf_usemethod  -  calling functions need to evaluate the object
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

attribute_hidden
SEXP R_LookupMethod(SEXP method, SEXP rho, SEXP callrho, SEXP defrho)
{
    if (TYPEOF(callrho) == NILSXP) {
	Rf_error(_("use of NULL environment is defunct"));
	callrho = R_BaseEnv;
    } else
	if (TYPEOF(callrho) != ENVSXP)
	    Rf_error(_("bad generic call environment"));
    if (TYPEOF(defrho) == NILSXP) {
	    Rf_error(_("use of NULL environment is defunct"));
	    defrho = R_BaseEnv;
    } else
	if (TYPEOF(defrho) != ENVSXP)
	    Rf_error(_("bad generic definition environment"));
    if (defrho == R_BaseEnv)
	defrho = R_BaseNamespace;

    Symbol* sym = SEXP_downcast<Symbol*>(method);
    std::pair<FunctionBase*, bool>
	pr = S3Launcher::findMethod(sym, static_cast<Environment*>(callrho),
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
int Rf_isBasicClass(const char *ss) {
    static GCRoot<> s_S3table = nullptr;
    if(!s_S3table) {
      s_S3table = Rf_findVarInFrame3(R_MethodsNamespace, Rf_install(".S3MethodsClasses"), TRUE);
      if(s_S3table == R_UnboundValue)
	Rf_error(_("no .S3MethodsClass table, cannot use S4 objects with S3 methods (methods package not attached?)"));
	if (TYPEOF(s_S3table) == PROMSXP)  /* Rf_findVar... ignores lazy data */
	    s_S3table = Rf_eval(s_S3table, R_MethodsNamespace);
    }
    if(s_S3table == R_UnboundValue)
      return FALSE; /* too screwed up to do conversions */
    return Rf_findVarInFrame3(s_S3table, Rf_install(ss), FALSE) != R_UnboundValue;
}


// Note the fourth argument is not used.
int Rf_usemethod(const char *generic, SEXP obj, SEXP call, SEXP,
		 SEXP rho, SEXP callrho, SEXP defrho, SEXP *ans)
{
    Environment* env = SEXP_downcast<Environment*>(rho);
    Environment* callenv = SEXP_downcast<Environment*>(callrho);
    Environment* defenv = SEXP_downcast<Environment*>(defrho);

    // Get the ClosureContext which UseMethod was called from.
    ClosureContext* cptr = ClosureContext::innermost();
    if (!cptr || cptr->workingEnvironment() != rho)
	Rf_error(_("'UseMethod' used in an inappropriate fashion"));

    // Determine the functor:
    FunctionBase* op;
    {
	RObject* opcar = cptr->call()->car();
	if (opcar->sexptype() == LANGSXP)
	    opcar = Evaluator::evaluate(opcar, cptr->callEnvironment());
	switch (opcar->sexptype()) {
	case SYMSXP: {
	    const Symbol* symbol = static_cast<Symbol*>(opcar);
	    FunctionBase* fun = findFunction(symbol, cptr->callEnvironment());
	    if (!fun)
		Rf_error(_("could not find function '%s'"),
			 symbol->name()->c_str());
	    op = fun;
	    break;
	}
	case CLOSXP:
	case BUILTINSXP:
	case SPECIALSXP:
	    op = static_cast<FunctionBase*>(opcar);
	    break;
	default:
	    Rf_error(_("Invalid generic function in 'usemethod'"));
	    op = nullptr;  // avoid compiler warning
	}
    }

    // Create a new frame without any of the formals to the
    // generic in it:
    GCStackRoot<Frame> newframe(new ListFrame);
    if (op->sexptype() == CLOSXP) {
	Closure* clos = static_cast<Closure*>(op);
	const Environment* generic_wk_env = cptr->workingEnvironment();
	newframe = generic_wk_env->frame()->clone();
	clos->stripFormals(newframe);
    }

    GCStackRoot<const PairList> matchedarg(cptr->promiseArgs());
    GCStackRoot<S3Launcher>
	m(S3Launcher::create(obj, generic, "", callenv, defenv, true));
    if (!m)
	return 0;
    if (op->sexptype() == CLOSXP && (RDEBUG(op) || RSTEP(op)) )
	SET_RSTEP(m->function(), 1);
    m->addMethodBindings(newframe);
    GCStackRoot<Expression> newcall(cptr->call()->clone());
    newcall->setCar(m->symbol());
    ArgList arglist(matchedarg, ArgList::PROMISED);
    *ans = applyMethod(newcall, m->function(), &arglist, env, newframe);
    return 1;
}

/* While UseMethod is a primitive, it is documented as using normal argument
 * argument matching with parameter names 'generic' and 'object'.
 * This function does the matching.
 */
static void matchArgsForUseMethod(SEXP call, SEXP args, Environment* argsenv,
				  ClosureContext* cptr,
				  StringVector** generic, GCStackRoot<>* obj) {
    static Symbol* genericsym(Symbol::obtain("generic"));
    static Symbol* objectsym(Symbol::obtain("object"));
    static GCRoot<ArgMatcher>
	matcher(ArgMatcher::make(genericsym, objectsym));
    
    GCStackRoot<Frame> matchframe(new ListFrame);
    GCStackRoot<Environment>
	matchenv(new Environment(nullptr, matchframe));
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);
    matcher->match(matchenv, &arglist);

    // "generic":
    {
	RObject* genval = matchenv->frame()->binding(genericsym)->forcedValue();
	if (genval == Symbol::missingArgument())
	    Rf_errorcall(call, _("there must be a 'generic' argument"));
	if (genval->sexptype() == STRSXP)
	    *generic = static_cast<StringVector*>(genval);
	if (!(*generic) || (*generic)->size() != 1)
	    Rf_errorcall(call,
			 _("'generic' argument must be a character string"));
	if ((**generic)[0] == String::blank())
	    Rf_errorcall(call, _("first argument must be a generic name"));
    }

    // "object":
    {
	RObject* objval = matchenv->frame()->binding(objectsym)->forcedValue();
	if (objval != Symbol::missingArgument()) {
	    *obj = Evaluator::evaluate(objval, argsenv);
	}
	else *obj = GetObject(cptr);
    }
}

static std::string classTypeAsString(RObject* obj) {
    std::string cl;
    GCStackRoot<StringVector>
	klass(static_cast<StringVector*>(R_data_class2(obj)));
    int nclass = klass->size();
    if (nclass == 1)
	cl = Rf_translateChar((*klass)[0]);
    else {
	cl = std::string("c('") + Rf_translateChar((*klass)[0]);
	for (int i = 1; i < nclass; ++i)
	    cl += std::string("', '") + Rf_translateChar((*klass)[i]);
	cl += "')";
    }
    return cl;
}


/* Note: "do_usemethod" is not the only entry point to
   "Rf_usemethod". Things like [ and [[ call Rf_usemethod directly,
   hence do_usemethod should just be an interface to Rf_usemethod.
*/

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_usemethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Environment* argsenv = SEXP_downcast<Environment*>(env);

    // Find and check ClosureContext:
    ClosureContext* cptr = ClosureContext::innermost();
    if (!cptr || cptr->workingEnvironment() != argsenv)
	Rf_error(_("'UseMethod' used in an inappropriate fashion"));

    StringVector* generic = nullptr;
    GCStackRoot<> obj;
    matchArgsForUseMethod(call, args, argsenv, cptr, &generic, &obj);

    /* get environments needed for dispatching.
       callenv = environment from which the generic was called
       defenv = environment where the generic was defined */
    Environment* callenv = cptr->callEnvironment();
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
    Environment* defenv = Environment::baseNamespace();
    {
	std::string generic_name = Rf_translateChar((*generic)[0]);
	FunctionBase* func
	    = findFunction(Symbol::obtain(generic_name),
			   argsenv->enclosingEnvironment());
	if (func && func->sexptype() == CLOSXP)
	    defenv = static_cast<Closure*>(func)->environment();
    }

    // Try invoking method:
    SEXP ans;
    if (Rf_usemethod(Rf_translateChar((*generic)[0]), obj, call, nullptr,
		     env, callenv, defenv, &ans) != 1) {
	// Failed, so prepare error message:
	std::string cl;
	GCStackRoot<StringVector>
	    klass(static_cast<StringVector*>(R_data_class2(obj)));
	size_t nclass = klass->size();
	if (nclass == 1)
	    cl = Rf_translateChar((*klass)[0]);
	else {
	    cl = std::string("c('") + Rf_translateChar((*klass)[0]);
	    for (size_t i = 1; i < nclass; ++i)
		cl += std::string("', '") + Rf_translateChar((*klass)[i]);
	    cl += "')";
	}
	Rf_errorcall(call, _("no applicable method for '%s'"
			     " applied to an object of class '%s'"),
		     Rf_translateChar((*generic)[0]),
		     classTypeAsString(obj).c_str());
    }

    // Prepare return value:
    {
	GCStackRoot<> ansrt(ans);
	ReturnBailout* rbo = new ReturnBailout(argsenv, ans);
	Evaluator::Context* callctxt
	    = Evaluator::Context::innermost()->nextOut();
	if (!callctxt || callctxt->type() != Evaluator::Context::BAILOUT)
	    rbo->throwException();
	return rbo;
    }
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
			SETCDR(s, Rf_allocList(1));
			SET_TAG(CDR(s), TAG(t));
			SETCAR(CDR(s), Rf_duplicate(CAR(t)));
		}
	}
    }
    return call;
}

static Environment* lookupEnvironmentValueFromBinding(
    Environment* env,
    const Symbol* symbol,
    Environment* value_if_not_found)
{
    Frame::Binding* bdg
	= env->frame()->binding(symbol);
    if (bdg && bdg->origin() != Frame::Binding::MISSING) {
	return SEXP_downcast<Environment*>(bdg->forcedValue());
    }
    return value_if_not_found;
}

/* If NextMethod has any arguments the first must be the generic */
/* the second the object and any remaining are matched with the */
/* formals of the chosen method. */

/* This is a special .Internal */
SEXP attribute_hidden do_nextmethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const PairList* callargs = SEXP_downcast<const PairList*>(args);
    Environment* callenv = SEXP_downcast<Environment*>(env);

    // Determine the ClosureContext from which NextMethod was called,
    // and the Environment of that call.  (The ClosureContext will
    // will be two out because NextMethod is an internal function.)
    ClosureContext* cptr = ClosureContext::innermost();
    Environment* nmcallenv = cptr->callEnvironment();
    cptr = ClosureContext::findClosureWithWorkingEnvironment(nmcallenv, cptr);
    if (cptr == nullptr) {
	Rf_error(_("'NextMethod' called from outside a function"));
    }

    // Find dispatching environments. Promises shouldn't occur, but
    // check to be on the safe side.  If the variables are not in the
    // environment (the method was called outside a method dispatch)
    // then chose reasonable defaults.

    // Environment in which the generic was called:
    Environment* gencallenv = lookupEnvironmentValueFromBinding(
	nmcallenv, DotGenericCallEnvSymbol, callenv);

    // Environment in which the generic was defined:
    Environment* gendefenv = lookupEnvironmentValueFromBinding(
	nmcallenv, DotGenericDefEnvSymbol, Environment::global());

    // Find the generic closure:
    Closure* genclos = nullptr;  // -Wall
    {
	RObject* callcar = cptr->call()->car();
	if (callcar->sexptype() == LANGSXP)
	    Rf_error(_("'NextMethod' called from an anonymous function"));
	else if (callcar->sexptype() == CLOSXP)
	    // e.g., in do.call(function(x) NextMethod('foo'),list())
	    genclos = static_cast<Closure*>(callcar);
	else {
	    Symbol* gensym = SEXP_downcast<Symbol*>(callcar);
	    FunctionBase* func
		= S3Launcher::findMethod(gensym, gencallenv, gendefenv).first;
	    if (!func)
		Rf_error(_("no calling generic was found:"
			   " was a method called directly?"));
	    if (func->sexptype() != CLOSXP)
		Rf_errorcall(nullptr, _("'function' is not a function,"
				  " but of type %d"), func->sexptype());
	    genclos = static_cast<Closure*>(func);
	}
    }

    // FIXME: the process of computing matchedarg that follows is
    // thoroughly nasty - arr.

    /* get formals and actuals; attach the names of the formals to
       the actuals, expanding any ... that occurs */
    const PairList* formals = genclos->matcher()->formalArgs();
    GCStackRoot<PairList> actuals;
    {
	{
	    RObject* ac
		= Rf_matchArgs(const_cast<PairList*>(formals),
			       const_cast<PairList*>(cptr->promiseArgs()),
			       call);
	    actuals = static_cast<PairList*>(ac);
	}

	bool dots = false;
	{
	    const PairList* s;
	    PairList* t;
	    for (s = formals, t = actuals; s; s = s->tail(), t = t->tail()) {
		t->setTag(s->tag());
		if (t->tag() == DotsSymbol)
		    dots = true;
	    }
	}
	if (dots) {   /* we need to expand out the dots */
	    GCStackRoot<PairList> t(PairList::cons(nullptr));  // dummy first element
	    for (PairList *s = actuals, *m = t; s; s = s->tail()) {
		RObject* scar = s->car();
		if (scar && scar->sexptype() == DOTSXP) {
		    int i = 1;
		    for (ConsCell* a = static_cast<ConsCell*>(scar);
			 a; a = a->tail()) {
			Symbol* ddsym = Symbol::obtainDotDotSymbol(i);
			m->setTail(PairList::cons(a->car(), nullptr, ddsym));
			m = m->tail();
			++i;
		    }
		} else {
		    m->setTail(PairList::cons(s->car(), nullptr, s->tag()));
		    m = m->tail();
		}
	    }
	    actuals = t->tail();
	}
    }

    /* we can't duplicate because it would force the promises */
    /* so we do our own duplication of the promargs */

    GCStackRoot<PairList> matchedarg;
    {
	// Duplicate cptr->promiseArgs():
	{
	    matchedarg = PairList::cons(nullptr);  // Dummy first element
	    PairList* t = matchedarg;
	    for (const PairList* s = cptr->promiseArgs(); s; s = s->tail()) {
		t->setTail(PairList::cons(s->car(), nullptr, s->tag()));
		t = t->tail();
	    }
	    matchedarg = matchedarg->tail();  // Discard dummy element
	}

	for (PairList* t = matchedarg; t; t = t->tail()) {
	    for (const PairList* m = actuals; m; m = m->tail()) {
		if (m->car() == t->car()) {
		    const Symbol* sym = static_cast<const Symbol*>(m->tag());
		    if (m->car() == Symbol::missingArgument()) {
			Frame::Binding* bdg
			    = cptr->workingEnvironment()->frame()->binding(sym);
			if (bdg && bdg->origin() == Frame::Binding::MISSING)
			    break;
		    }
		    t->setCar(new Promise(const_cast<Symbol*>(sym),
					  cptr->workingEnvironment()));
		    break;
		}
	    }
	}
    }

    ArgList newarglist(matchedarg, ArgList::PROMISED);

    /*
      .Class is used to determine the next method; if it doesn't
      exist the first argument to the current method is used
      the second argument to NextMethod is another option but
      isn't currently used).
    */
    GCStackRoot<StringVector> klass;
    {
	Frame::Binding* bdg = nmcallenv->frame()->binding(DotClassSymbol);
	RObject* klassval;
	if (bdg)
	    klassval = bdg->forcedValue();
	else {
	    RObject* s = GetObject(cptr);
	    if (!s)
		Rf_error(_("object not specified"));
	    if (s->hasClass())
		klassval = s->getAttribute(ClassSymbol);
	    else
		klassval = StringVector::create(0);
	}
	klass = SEXP_downcast<StringVector*>(klassval);
    }

    /* the generic comes from either the sysparent or it's named */
    GCStackRoot<StringVector> dotgeneric;
    std::string genericname;
    {
	Frame::Binding* bdg = nmcallenv->frame()->binding(DotGenericSymbol);
	RObject* genval
	    = (bdg ? bdg->forcedValue()
	       : Evaluator::evaluate(callargs->car(), callenv));
	if (!genval)
	    Rf_error(_("generic function not specified"));
	if (genval->sexptype() == STRSXP)
	    dotgeneric = static_cast<StringVector*>(genval);
	if (!dotgeneric || dotgeneric->size() != 1)
	    Rf_error(_("invalid generic argument to NextMethod"));
	genericname = Rf_translateChar((*dotgeneric)[0]);
	if (genericname.empty())
	    Rf_error(_("generic function not specified"));
    }

    // Determine whether we are in a Group dispatch.
    GCStackRoot<StringVector> dotgroup;
    std::string groupname;
    {
	Frame::Binding* bdg = nmcallenv->frame()->binding(DotGroupSymbol);
	if (bdg) {
	    RObject* grpval = bdg->forcedValue();
	    if (grpval->sexptype() == STRSXP)
		dotgroup = static_cast<StringVector*>(grpval);
	    if (!dotgroup || dotgroup->size() != 1)
		Rf_error(_("invalid .Group found in NextMethod"));
	    groupname = Rf_translateChar((*dotgroup)[0]);
	}
    }

    // Find the method currently being invoked:
    GCStackRoot<StringVector> dotmethod;
    std::string currentmethodname;
    {
	Frame::Binding* bdg = nmcallenv->frame()->binding(DotMethodSymbol);
	if (!bdg) {
	    Symbol* opsym = SEXP_downcast<Symbol*>(cptr->call()->car());
	    currentmethodname = opsym->name()->stdstring();
	} else {
	    RObject* methval = bdg->forcedValue();
	    if (!methval || methval->sexptype() != STRSXP)
		Rf_error(_("wrong value for .Method"));
	    dotmethod = static_cast<StringVector*>(methval);
	    unsigned int i;
	    for (i = 0; currentmethodname.empty() && i < dotmethod->size(); ++i)
		currentmethodname = Rf_translateChar((*dotmethod)[i]);
	    // for binary operators check that the second argument's
	    // method is the same or absent:
	    for (unsigned int j = i; j < dotmethod->size(); ++j) {
		std::string bb = Rf_translateChar((*dotmethod)[j]);
		if (!bb.empty() && bb != currentmethodname)
		    Rf_warning(_("Incompatible methods ignored"));
	    }
	}
    }

    // Locate the class suffix of the current method within the klass vector:
    std::string suffix;
    unsigned int nextidxstart;  // Index within the klass vector at
				// which the search for the next
				// method should start.
    {
	std::string basename = (dotgroup ? groupname : genericname);
	bool found = false;
	for (nextidxstart = 0;
	     !found && nextidxstart < klass->size();
	     ++nextidxstart) {
	    suffix = Rf_translateChar((*klass)[nextidxstart]);
	    found = (basename + "." + suffix == currentmethodname);
	}
	// If a match was found, nextidxstart will now be pointing to the next
	// element (if any).  If there's no match start with the first
	// element.
	if (!found)
	    nextidxstart = 0;
    }

    FunctionBase* nextfun = nullptr;
    std::string nextmethodname;
    unsigned int nextidx;  // Index within the klass vector at which
			   // the next method was found.  Set to
			   // klass->size() if no class-specific
			   // method was found.
    for (nextidx = nextidxstart;
	 !nextfun && nextidx < klass->size();
	 ++nextidx) {
	suffix = Rf_translateChar((*klass)[nextidx]);
	nextmethodname = genericname + "." + suffix;
	Symbol* nextmethodsym(Symbol::obtain(nextmethodname));
	nextfun = S3Launcher::findMethod(nextmethodsym,
					 gencallenv, gendefenv).first;
	if (!nextfun && dotgroup) {
	    // if not Generic.foo, look for Group.foo
	    nextmethodname = groupname + "." + suffix;
	    nextmethodsym = Symbol::obtain(nextmethodname);
	    nextfun = S3Launcher::findMethod(nextmethodsym,
					     gencallenv, gendefenv).first;
	}
    }
    if (!nextfun) {
	nextmethodname = genericname + ".default";
	Symbol* nextmethodsym(Symbol::obtain(nextmethodname));
	nextfun = S3Launcher::findMethod(nextmethodsym,
					 gencallenv, gendefenv).first;
	// If there is no default method, try the generic itself,
	// provided it is primitive or a wrapper for a .Internal
	// function of the same name.
	if (!nextfun) {
	    Symbol* genericsym(Symbol::obtain(genericname));
	    Frame::Binding* bdg = callenv->findBinding(genericsym);
	    if (!bdg)
		Rf_error(_("no method to invoke"));
	    RObject* nfval = bdg->forcedValue();
	    if (!nfval)
		Rf_error(_("no method to invoke"));
	    nextfun = dynamic_cast<FunctionBase*>(nfval);
	    if (nextfun && nextfun->sexptype() == CLOSXP)
		nextfun = BuiltInFunction::obtainInternal(genericsym);
	    if (!nextfun)
		Rf_error(_("no method to invoke"));
	}
    }

    GCStackRoot<Expression> newcall(cptr->call()->clone());
    {
	Symbol* nextmethodsym(Symbol::obtain(nextmethodname));
	newcall->setCar(nextmethodsym);
    }

    /*
      Now see if there were any other arguments passed in
      Currently we seem to only allow named args to change
      or to be added, this is at variance with p. 470 of the
      White Book
    */
    {
	Frame::Binding* bdg = callenv->frame()->binding(DotsSymbol);
	if (bdg && bdg->origin() != Frame::Binding::MISSING) {
	    GCStackRoot<DottedArgs>
		dots(SEXP_downcast<DottedArgs*>(bdg->forcedValue()));
	    GCStackRoot<PairList> newargs(ConsCell::convert<PairList>(dots));
	    newarglist.merge(newargs);
	    newcall
		= static_cast<Expression*>(fixcall(newcall,
						   const_cast<PairList*>(newarglist.list())));
	}
    }

    // Set up special method bindings:
    GCStackRoot<Frame> method_bindings(new ListFrame);
    {
	if (klass) {
	    size_t sz = klass->size() - nextidx;
	    GCStackRoot<StringVector>
		newdotclass(StringVector::create(sz));
	    klass = klass->clone();
	    for (unsigned int j = 0; j < sz; ++j)
		(*newdotclass)[j] = (*klass)[nextidx++];
	    newdotclass->setAttribute(PreviousSymbol, klass);
	    method_bindings->bind(DotClassSymbol, newdotclass);
	}
	// It is possible that if a method was called directly that
	// 'method' is unset.
	if (!dotmethod)
	    dotmethod = asStringVector(nextmethodname);
	else {
	    dotmethod = dotmethod->clone();
	    // For Ops we need `method' to be a vector
	    for (unsigned int j = 0; j < dotmethod->size(); ++j) {
		if (!(*dotmethod)[j])
		    (*dotmethod)[j] = String::obtain(nextmethodname);
	    }
	}
	method_bindings->bind(DotMethodSymbol, dotmethod);
	method_bindings->bind(DotGenericCallEnvSymbol, gencallenv);
	method_bindings->bind(DotGenericDefEnvSymbol, gendefenv);
	method_bindings->bind(DotGenericSymbol, dotgeneric);
	if (dotgroup)
	    method_bindings->bind(DotGroupSymbol, dotgroup);
    }

    return applyMethod(newcall, nextfun, &newarglist, callenv, method_bindings);
}

/* primitive */
SEXP attribute_hidden do_unclass(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    op->checkNumArgs(num_args, call);
    Rf_check1arg(tags, call, "x");

    RObject* object = args[0];
    switch(TYPEOF(object)) {
    case ENVSXP:
	Rf_errorcall(call, _("cannot unclass an environment"));
	break;
    case EXTPTRSXP:
	Rf_errorcall(call, _("cannot unclass an external pointer"));
	break;
    default:
	break;
    }
    if (Rf_isObject(object)) {
	object = Rf_duplicate(object);
	Rf_setAttrib(object, R_ClassSymbol, R_NilValue);
    }
    return object;
}



/* NOTE: Fast  inherits(x, what)    in ../include/Rinlinedfuns.h
 * ----        ----------------- */
/** C API for  R  inherits(x, what, which)
 *
 * @param x any R object
 * @param what character vector
 * @param which logical: "want vector result" ?
 *
 * @return if which is false, logical TRUE or FALSE
 *	   if which is true, integer vector of length(what) ..
 */
static SEXP inherits3(SEXP x, SEXP what, SEXP which)
{
    const void *vmax = vmaxget();
    GCStackRoot<> klass;

    if(IS_S4_OBJECT(x))
	klass = R_data_class2(x);
    else
	klass = R_data_class(x, FALSE);
    int nclass = length(klass);

    if(!Rf_isString(what))
	Rf_error(_("'what' must be a character vector"));
    int j, nwhat = length(what);

    if( !Rf_isLogical(which) || (length(which) != 1) )
	Rf_error(_("'which' must be a length 1 logical vector"));
    int isvec = Rf_asLogical(which);

#ifdef _be_too_picky_
    if(IS_S4_OBJECT(x) && nwhat == 1 && !isvec &&
       !isNull(R_getClassDef(translateChar(STRING_ELT(what, 0)))))
	warning(_("use 'is()' instead of 'inherits()' on S4 objects"));
#endif

    GCStackRoot<> rval;
    if(isvec)
	rval = Rf_allocVector(INTSXP, nwhat);

    for(j = 0; j < nwhat; j++) {
	const char *ss = Rf_translateChar(STRING_ELT(what, j)); int i;
	if(isvec)
	    INTEGER(rval)[j] = 0;
	for(i = 0; i < nclass; i++) {
	    if(!strcmp(Rf_translateChar(STRING_ELT(klass, i)), ss)) {
		if(isvec)
		    INTEGER(rval)[j] = i+1;
		else {
		    return Rf_mkTrue();
		}
		break;
	    }
	}
    }
    vmaxset(vmax);
    if(!isvec) {
	return Rf_mkFalse();
    }
    return rval;
}

SEXP attribute_hidden do_inherits(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    op->checkNumArgs(num_args, call);

    return inherits3(/* x = */ args[0],
		     /* what = */ args[1],
		     /* which = */ args[2]);
}


/*
   ==============================================================

     code from here on down is support for the methods package

   ==============================================================
*/

/**
 * Return the 0-based index of an is() match in a vector of class-name
 * strings terminated by an empty string.  Returns -1 for no match.
 *
 * @param x  an R object, about which we want is(x, .) information.
 * @param valid vector of possible matches terminated by an empty string.
 * @param rho  the environment in which the class definitions exist.
 *
 * @return index of match or -1 for no match
 */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho)
{
    int ans;
    SEXP cl = Rf_getAttrib(x, R_ClassSymbol);
    const char *class_str = CHAR(Rf_asChar(cl));
    for (ans = 0; ; ans++) {
	if (!strlen(valid[ans])) // empty string
	    break;
	if (!strcmp(class_str, valid[ans])) return ans;
    }
    /* if not found directly, now search the non-virtual super classes :*/
    if(IS_S4_OBJECT(x)) {
	/* now try the superclasses, i.e.,  try   is(x, "....");  superCl :=
	   .selectSuperClasses(getClass("....")@contains, dropVirtual=TRUE)  */
	SEXP classExts, superCl, _call;
	static GCRoot<> s_contains = nullptr, s_selectSuperCl = nullptr;
	int i;
	if(!s_contains) {
	    s_contains      = Rf_install("contains");
	    s_selectSuperCl = Rf_install(".selectSuperClasses");
	}

	PROTECT(classExts = R_do_slot(R_getClassDef(class_str), s_contains));
	PROTECT(_call = Rf_lang3(s_selectSuperCl, classExts,
			      /* dropVirtual = */ Rf_ScalarLogical(1)));
	superCl = Rf_eval(_call, rho);
	UNPROTECT(2);
	PROTECT(superCl);
	for(i=0; i < length(superCl); i++) {
	    const char *s_class = CHAR(STRING_ELT(superCl, i));
	    for (ans = 0; ; ans++) {
		if (!strlen(valid[ans]))
		    break;
		if (!strcmp(s_class, valid[ans])) {
		    UNPROTECT(1);
		    return ans;
		}
	    }
	}
	UNPROTECT(1);
    }
    return -1;
}


/**
 * Return the 0-based index of an is() match in a vector of class-name
 * strings terminated by an empty string.  Returns -1 for no match.
 * Strives to find the correct environment() for is(), using .classEnv()
 * (from \pkg{methods}).
 *
 * @param x  an R object, about which we want is(x, .) information.
 * @param valid vector of possible matches terminated by an empty string.
 *
 * @return index of match or -1 for no match
 */
int R_check_class_etc(SEXP x, const char **valid)
{
    static GCRoot<> meth_classEnv = nullptr;
    SEXP cl = Rf_getAttrib(x, R_ClassSymbol), rho = R_GlobalEnv, pkg;
    if(!meth_classEnv)
	meth_classEnv = Rf_install(".classEnv");

    pkg = Rf_getAttrib(cl, R_PackageSymbol); /* ==R== packageSlot(class(x)) */
    if(!Rf_isNull(pkg)) { /* find  rho := correct class Environment */
	SEXP clEnvCall;
	// FIXME: fails if 'methods' is not loaded.
	PROTECT(clEnvCall = Rf_lang2(meth_classEnv, cl));
	rho = Rf_eval(clEnvCall, R_MethodsNamespace);
	UNPROTECT(1);
	if(!Rf_isEnvironment(rho))
	    Rf_error(_("could not find correct environment; please report!"));
    }
    return R_check_class_and_super(x, valid, rho);
}

/* standardGeneric:  uses a pointer to R_standardGeneric, to be
   initialized when the methods namespace is loaded,
   via R_initMethodDispatch.
*/
static R_stdGen_ptr_t R_standardGeneric_ptr = nullptr;
static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef);
#define NOT_METHODS_DISPATCH_PTR(ptr) (ptr == 0 || ptr == dispatchNonGeneric)

static
R_stdGen_ptr_t R_get_standardGeneric_ptr(void)
{
    return R_standardGeneric_ptr;
}

/* Also called from R_initMethodDispatch in methods C code, which is
   called when the methods namespace is loaded. */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t val, SEXP envir)
{
    R_stdGen_ptr_t old = R_standardGeneric_ptr;
    R_standardGeneric_ptr = val;
    if(envir && !Rf_isNull(envir))
	R_MethodsNamespace = envir;
    /* just in case ... */
    if(!R_MethodsNamespace)
	R_MethodsNamespace = R_GlobalEnv;
    return old;
}

static SEXP R_isMethodsDispatchOn(SEXP onOff)
{
    R_stdGen_ptr_t old = R_get_standardGeneric_ptr();
    int ival =  !NOT_METHODS_DISPATCH_PTR(old);
    if(length(onOff) > 0) {
	Rboolean onOffValue = CXXRCONSTRUCT(Rboolean, Rf_asLogical(onOff));
	if(onOffValue == NA_INTEGER)
	    Rf_error(_("'onOff' must be TRUE or FALSE"));
	else if(onOffValue == FALSE)
	    R_set_standardGeneric_ptr(nullptr, R_GlobalEnv);
	// TRUE is not currently used
	else if(NOT_METHODS_DISPATCH_PTR(old)) {
	    // so not already on
	    // This may not work correctly: the default arg is incorrect.
	    Rf_warning("R_isMethodsDispatchOn(TRUE) called -- may not work correctly");
	    // FIXME: use call = PROTECT(lang1(install("initMethodDispatch")));
	    SEXP call = PROTECT(Rf_allocList(2));
	    SETCAR(call, Rf_install("initMethodDispatch"));
	    Rf_eval(call, R_MethodsNamespace); // only works with methods loaded
	    UNPROTECT(1);
	}
    }
    return Rf_ScalarLogical(ival);
}

/* simpler version for internal use, in attrib.c and print.c */
attribute_hidden
Rboolean isMethodsDispatchOn(void)
{
    return CXXRCONSTRUCT(Rboolean, !NOT_METHODS_DISPATCH_PTR(R_standardGeneric_ptr));
}


/* primitive for .isMethodsDispatchOn
   This is generally called without an arg, but is call with
   onOff=FALSE when package methods is detached/unloaded.

   It seems it is not currently called with onOff = TRUE (and would
   not have worked prior to 3.0.2).
*/ 
SEXP attribute_hidden do_S4on(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* rho, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    if(num_args == 0) return Rf_ScalarLogical(isMethodsDispatchOn());
    return R_isMethodsDispatchOn(args[0]);
}


static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef)
{
    /* dispatch the non-generic definition of `name'.  Used to trap
       calls to standardGeneric during the loading of the methods package */
    SEXP e, value, rho, fun, symbol;
    ClosureContext *cptr;

    /* find a non-generic function */
    symbol = Rf_installTrChar(Rf_asChar(name));
    for(rho = ENCLOS(env); rho != R_EmptyEnv;
	rho = ENCLOS(rho)) {
	fun = Rf_findVarInFrame3(rho, symbol, TRUE);
	if(fun == R_UnboundValue) continue;
	switch(TYPEOF(fun)) {
	case CLOSXP:
	    value = Rf_findVarInFrame3(CLOENV(fun), R_dot_Generic, TRUE);
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
	Rf_error(_("unable to find a non-generic version of function \"%s\""),
	      Rf_translateChar(Rf_asChar(name)));
    cptr = ClosureContext::innermost();
    /* check this is the right context */
    while (cptr && cptr->workingEnvironment() != env)
	cptr = ClosureContext::innermost(cptr->nextOut());

    PROTECT(e = Rf_duplicate(R_syscall(0, cptr)));
    SETCAR(e, fun);
    /* evaluate a call the non-generic with the same arguments and from
       the same environment as the call to the generic version */
    value = Rf_eval(e, cptr->callEnvironment());
    UNPROTECT(1);
    return value;
}


static SEXP get_this_generic(RObject* const* args, int num_args);



SEXP attribute_hidden do_standardGeneric(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    SEXP arg, value, fdef; R_stdGen_ptr_t ptr = R_get_standardGeneric_ptr();

    op->checkNumArgs(num_args, call);
    Rf_check1arg(tags, call, "f");

    if(!ptr) {
	Rf_warningcall(call,
		    _("'standardGeneric' called without methods dispatch enabled (will be ignored)"));
	R_set_standardGeneric_ptr(dispatchNonGeneric, nullptr);
	ptr = R_get_standardGeneric_ptr();
    }

    op->checkNumArgs(num_args, call); /* set to -1 */
    if (num_args == 0 || !Rf_isValidStringF(args[0]))
	Rf_errorcall(call,
		  _("argument to 'standardGeneric' must be a non-empty character string"));
    arg = args[0];

    PROTECT(fdef = get_this_generic(args, num_args));

    if(Rf_isNull(fdef))
	Rf_error(_("call to standardGeneric(\"%s\") apparently not from the body of that generic function"), Rf_translateChar(STRING_ELT(arg, 0)));

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
    const void *vmax = vmaxget();
    if(!Rf_isValidString(code_vec))
	Rf_error(_("argument 'code' must be a character string"));
    code_string = Rf_translateChar(Rf_asChar(code_vec));
    /* with a NULL op, turns all primitive matching off or on (used to avoid possible infinite
     recursion in methods computations*/
    if(op == R_NilValue) {
	SEXP value;
	value = allowPrimitiveMethods ? Rf_mkTrue() : Rf_mkFalse();
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
    vmaxset(vmax);
    return fname;
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
	Rf_error(_("invalid primitive methods code (\"%s\"): should be \"clear\", \"reset\", \"set\", or \"suppress\""), code_string);
	return R_NilValue;
    }
    switch(TYPEOF(op)) {
    case BUILTINSXP: case SPECIALSXP:
	offset = PRIMOFFSET(op);
	break;
    default:
	Rf_error(_("invalid object: must be a primitive function"));
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
		prim_generics[i] = nullptr;
		prim_mlist[i]	 = nullptr;
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
	prim_generics[offset] = nullptr;
	prim_mlist[offset] = nullptr;
    }
    else if(fundef && !Rf_isNull(fundef) && !prim_generics[offset]) {
	if(TYPEOF(fundef) != CLOSXP)
	    Rf_error(_("the formal definition of a primitive generic must be a function object (got type '%s')"),
		  Rf_type2char(TYPEOF(fundef)));
	R_PreserveObject(fundef);
	prim_generics[offset] = fundef;
    }
    if(code == HAS_METHODS) {
	if(!mlist  || Rf_isNull(mlist)) {
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
    f = PROTECT(Rf_allocVector(STRSXP, 1));  nprotect++;
    SET_STRING_ELT(f, 0, Rf_mkChar(PRIMNAME(op)));
    PROTECT(e = Rf_allocVector(LANGSXP, 2)); nprotect++;
    SETCAR(e, Rf_install("getGeneric"));
    val = CDR(e); SETCAR(val, f);
    val = Rf_eval(e, rho);
    /* a rough sanity check that this looks like a generic function */
    if(TYPEOF(val) != CLOSXP || !IS_S4_OBJECT(val))
	Rf_error(_("object returned as generic function \"%s\" does not appear to be one"), PRIMNAME(op));
    UNPROTECT(nprotect);
    return CLOENV(val);
}


/* get the generic function, defined to be the function definition for
the call to standardGeneric(), or for primitives, passed as the second
argument to standardGeneric.
*/
static SEXP get_this_generic(RObject* const* args, int num_args)
{
    const void *vmax = vmaxget();
    SEXP value = R_NilValue; static GCRoot<> gen_name;
    int i, n;
    ClosureContext *cptr;
    const char *fname;

    /* a second argument to the call, if any, is taken as the function */
    if (num_args > 1) {
	return args[1];
    }
    /* else use sys.function (this is fairly expensive-- would be good
     * to force a second argument if possible) */
    if(!gen_name)
	gen_name = Rf_install("generic");
    cptr = ClosureContext::innermost();
    fname = Rf_translateChar(Rf_asChar(args[0]));
    n = Rf_framedepth(cptr);
    /* check for a matching "generic" slot */
    for(i=0;  i<n; i++) {
	SEXP rval = R_sysfunction(i, cptr);
	if(Rf_isObject(rval)) {
	    SEXP generic = Rf_getAttrib(rval, gen_name);
	    if(TYPEOF(generic) == STRSXP &&
	       !strcmp(Rf_translateChar(Rf_asChar(generic)), fname)) {
	      value = rval;
	      break;
	    }
	}
    }
    vmaxset(vmax);

    return value;
}

/* Could there be methods for this op?	Checks
   only whether methods are currently being dispatched and, if so,
   whether methods are currently defined for this op. */
attribute_hidden
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

static GCRoot<> deferred_default_object;

SEXP R_deferred_default_method()
{
    if(!deferred_default_object)
	deferred_default_object = Rf_install("__Deferred_Default_Marker__");
    return(deferred_default_object);
}


static R_stdGen_ptr_t quick_method_check_ptr = nullptr;
void R_set_quick_method_check(R_stdGen_ptr_t value)
{
    quick_method_check_ptr = value;
}

static RObject *call_closure_from_prim(Closure *func, PairList *args,
				       Expression *call_expression,
				       Environment *call_env,
				       Rboolean promisedArgs) {
    if(!promisedArgs) {
	/* Because we call this from a primitive op, args either contains
	 * promises or actual values.  In the later case, we create promises
	 * that have already been forced to the value in args.
	 *
	 * TODO(kmillar): why is this necessary?  We should be able to pass
	 * either the args or the unforced promises directly to the closure.
	 */
	ArgList al(call_expression->tail(), ArgList::RAW);
	al.wrapInPromises(call_env);

	PairList* pargs = const_cast<PairList*>(al.list());
	PairList *a, *b;
	for (a = args, b = pargs;
	     a != nullptr && b != nullptr;
	     a = a->tail(), b = b->tail())
	    SET_PRVALUE(b->car(), a->car());
	// Check for unequal list lengths:
	if (a != nullptr || b != nullptr)
	    Rf_error(_("dispatch error"));
	args = pargs;
    }
    ArgList al(args, ArgList::PROMISED);
    return func->invoke(call_env, &al, call_expression);
}

/* try to dispatch the formal method for this primitive op, by calling
   the stored generic function corresponding to the op.	 Requires that
   the methods be set up to return a special object rather than trying
   to evaluate the default (which would get us into a loop). */

/* called from DispatchOrEval, Rf_DispatchGroup, do_matprod
   When called from the first the arguments have been enclosed in
   promises, but not from the other two: there all the arguments have
   already been evaluated.
 */
std::pair<bool, SEXP> attribute_hidden
R_possible_dispatch(SEXP call, SEXP op, SEXP args, SEXP rho,
		    Rboolean promisedArgs)
{
    Expression* callx = SEXP_downcast<Expression*>(call);
    GCStackRoot<PairList> argspl(SEXP_downcast<PairList*>(args));
    Environment* callenv = SEXP_downcast<Environment*>(rho);
    SEXP value;
    GCStackRoot<> mlist;
    int offset = PRIMOFFSET(op);
    if(offset < 0 || offset > curMaxOffset)
	Rf_error(_("invalid primitive operation given for dispatch"));
    prim_methods_t current = prim_methods[offset];
    if(current == NO_METHODS || current == SUPPRESSED)
	return std::pair<bool, SEXP>(false, nullptr);
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
    if(mlist && !Rf_isNull(mlist)
       && quick_method_check_ptr) {
	value = (*quick_method_check_ptr)(args, mlist, op);
	if(Rf_isPrimitive(value))
	    return std::pair<bool, SEXP>(false, nullptr);
	if(Rf_isFunction(value)) {
	    Closure* func = static_cast<Closure*>(value);
	    // found a method, call it with promised args
	    value = call_closure_from_prim(func, argspl, callx, callenv,
					   promisedArgs);
	    return std::make_pair(true, value);
	}
	// else, need to perform full method search
    }
    RObject* fundef = prim_generics[offset];
    if(!fundef || TYPEOF(fundef) != CLOSXP)
	Rf_error(_("primitive function \"%s\" has been set for methods"
		" but no generic function supplied"),
	      PRIMNAME(op));
    Closure* func = static_cast<Closure*>(fundef);
    // To do:  arrange for the setting to be restored in case of an
    // error in method search
    value = call_closure_from_prim(func, argspl, callx, callenv, promisedArgs);
    prim_methods[offset] = current;
    if (value == deferred_default_object)
	return std::pair<bool, SEXP>(false, nullptr);
    else
	return std::make_pair(true, value);
}

SEXP R_do_MAKE_CLASS(const char *what)
{
    static GCRoot<> s_getClass = nullptr;
    SEXP e, call;
    if(!what)
	Rf_error(_("C level MAKE_CLASS macro called with NULL string pointer"));
    if(!s_getClass) s_getClass = Rf_install("getClass");
    PROTECT(call = Rf_allocVector(LANGSXP, 2));
    SETCAR(call, s_getClass);
    SETCAR(CDR(call), Rf_mkString(what));
    e = Rf_eval(call, R_MethodsNamespace);
    UNPROTECT(1);
    return(e);
}

/* this very similar, but gives NULL instead of an error for a non-existing class */
SEXP R_getClassDef(const char *what)
{
    static GCRoot<> s_getClassDef = nullptr;
    SEXP e, call;
    if(!what)
	Rf_error(_("R_getClassDef(.) called with NULL string pointer"));
    if(!s_getClassDef) s_getClassDef = Rf_install("getClassDef");
    PROTECT(call = Rf_allocVector(LANGSXP, 2));
    SETCAR(call, s_getClassDef);
    SETCAR(CDR(call), Rf_mkString(what));
    e = Rf_eval(call, R_MethodsNamespace);
    UNPROTECT(1);
    return(e);
}

/* in Rinternals.h */
SEXP R_do_new_object(SEXP class_def)
{
    static GCRoot<> s_virtual = nullptr, s_prototype, s_className;
    SEXP e, value;
    const void *vmax = vmaxget();
    if(!s_virtual) {
	s_virtual = Rf_install("virtual");
	s_prototype = Rf_install("prototype");
	s_className = Rf_install("className");
    }
    if(!class_def)
	Rf_error(_("C level NEW macro called with null class definition pointer"));
    e = R_do_slot(class_def, s_virtual);
    if(Rf_asLogical(e) != 0)  { /* includes NA, TRUE, or anything other than FALSE */
	e = R_do_slot(class_def, s_className);
	Rf_error(_("trying to generate an object from a virtual class (\"%s\")"),
	      Rf_translateChar(Rf_asChar(e)));
    }
    e = R_do_slot(class_def, s_className);
    value = Rf_duplicate(R_do_slot(class_def, s_prototype));
    if(TYPEOF(value) == S4SXP || Rf_getAttrib(e, R_PackageSymbol) != R_NilValue)
    { /* Anything but an object from a base "class" (numeric, matrix,..) */
	GCStackRoot<> valrt(value);
	Rf_setAttrib(value, R_ClassSymbol, e);
	SET_S4_OBJECT(value);
    }
    vmaxset(vmax);
    return value;
}

Rboolean attribute_hidden R_seemsOldStyleS4Object(SEXP object)
{
    SEXP klass;
    if(!Rf_isObject(object) || IS_S4_OBJECT(object)) return FALSE;
    /* We want to know about S4SXPs with no S4 bit */
    /* if(TYPEOF(object) == S4SXP) return FALSE; */
    klass = Rf_getAttrib(object, R_ClassSymbol);
    return (klass != R_NilValue && LENGTH(klass) == 1 &&
	    Rf_getAttrib(klass, R_PackageSymbol) != R_NilValue) ? TRUE: FALSE;
}

SEXP attribute_hidden do_setS4Object(/*const*/ CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::Environment* env, CXXR::RObject* const* args, int num_args, const CXXR::PairList* tags)
{
    op->checkNumArgs(num_args, call);
    SEXP object = args[0];
    int flag = Rf_asLogical(args[1]), complete = Rf_asInteger(args[2]);
    if(length(args[1]) != 1 || flag == NA_INTEGER)
	Rf_error("invalid '%s' argument", "flag");
    if(complete == NA_INTEGER)
	Rf_error("invalid '%s' argument", "complete");
    if(flag == CXXRCONSTRUCT(Rboolean, IS_S4_OBJECT(object)))
	return object;
    else
      return Rf_asS4(object, CXXRCONSTRUCT(Rboolean, flag), complete);
}

#ifdef UNUSED
SEXP R_get_primname(SEXP object)
{
    SEXP f;
    if(TYPEOF(object) != BUILTINSXP && TYPEOF(object) != SPECIALSXP)
	Rf_error("'R_get_primname' called on a non-primitive");
    PROTECT(f = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(f, 0, Rf_mkChar(PRIMNAME(object)));
    UNPROTECT(1);
    return f;
}
#endif


Rboolean Rf_isS4(SEXP s)
{
    return IS_S4_OBJECT(s);
}

SEXP Rf_asS4(SEXP s, Rboolean flag, int complete)
{
    if(flag == IS_S4_OBJECT(s))
	return s;
    PROTECT(s);
    if(NAMED(s) == 2)
	s = Rf_duplicate(s);
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
	      Rf_error(_("object of class \"%s\" does not correspond to a valid S3 object"),
		      CHAR(STRING_ELT(R_data_class(s, FALSE), 0)));
	    else return s; /*  unchanged */
	}
	UNSET_S4_OBJECT(s);
    }
    return s;
}

S3Launcher*
S3Launcher::create(RObject* object, std::string generic, std::string group,
		   Environment* call_env, Environment* table_env,
		   bool allow_default)
{
    GCStackRoot<S3Launcher>
	ans(new S3Launcher(generic, group, call_env, table_env));
    ans->m_classes = static_cast<StringVector*>(R_data_class2(object));

    // Look for pukka method.  Need to interleave looking for generic
    // and group methods, e.g. if class(x) is c("foo", "bar") then
    // x > 3 should invoke "Ops.foo" rather than ">.bar".
    {
	size_t len = ans->m_classes->size();
	for (ans->m_index = 0; ans->m_index < len; ++ans->m_index) {
	    const char *ss = Rf_translateChar((*ans->m_classes)[ans->m_index]);
	    ans->m_symbol = Symbol::obtain(generic + "." + ss);
	    ans->m_function
		= findMethod(ans->m_symbol, call_env, table_env).first;
	    if (ans->m_function) {
		// Kludge because sort.list is not a method:
		static GCRoot<const Symbol> sort_list
		    = Symbol::obtain("sort.list");
		if (ans->m_function->sexptype() == CLOSXP
		    && ans->m_symbol == sort_list) {
		    const Closure* closure
			= static_cast<Closure*>(ans->m_function.get());
		    if (closure->environment() == Environment::baseNamespace())
			continue;
		}
		break;  // Mustn't increment m_index if found
	    }
	    if (!group.empty()) {
		// Try for group method:
		ans->m_symbol = Symbol::obtain(group + "." + ss);
		ans->m_function
		    = findMethod(ans->m_symbol, call_env, table_env).first;
		if (ans->m_function) {
		    ans->m_using_group = true;
		    break;  // Mustn't increment m_index if found
		}
	    }
	}
    }
    if (!ans->m_function && allow_default) {
	// Look for default method:
	ans->m_symbol = Symbol::obtain(generic + ".default");
	ans->m_function = findMethod(ans->m_symbol, call_env, table_env).first;
    }
    if (!ans->m_function)
	return nullptr;
    return ans;
}
