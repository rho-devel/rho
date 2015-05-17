/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
 *  Copyright (C) 2003	      The R Foundation
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <ctype.h> /* for tolower */
#include <string.h>
#include <errno.h>

#include <Rmath.h>
#include <boost/preprocessor.hpp>

#include "CXXR/ClosureContext.hpp"

#ifndef max
#define max(a, b) ((a > b)?(a):(b))
#endif

using namespace CXXR;

/* Was 'name' prior to 2.13.0, then .NAME, but checked as
   'name' up to 2.15.1. */
static void check1arg2(SEXP arg, SEXP call, const char *formal)
{
    if (TAG(arg) == R_NilValue) return;
    errorcall(call, "the first argument should not be named");
 }



/* These are set during the first call to do_dotCode() below. */

static GCRoot<> NaokSymbol = nullptr;
static GCRoot<> DupSymbol = nullptr;
static GCRoot<> PkgSymbol = nullptr;
static GCRoot<> EncSymbol = nullptr;
static GCRoot<> CSingSymbol = nullptr;

#include <Rdynpriv.h>
// Odd: 'type' is really this enum
enum {NOT_DEFINED, FILENAME, DLL_HANDLE, R_OBJECT};
typedef struct {
    char DLLname[PATH_MAX];
    HINSTANCE dll;
    SEXP  obj;
    int type;
} DllReference;

/* Maximum length of entry-point name, including nul terminator */
#define MaxSymbolBytes 1024

/* Maximum number of args to .C, .Fortran and .Call */
#define MAX_ARGS 65

/* This looks up entry points in DLLs in a platform specific way. */
static DL_FUNC
R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol, SEXP env);

static SEXP naokfind(SEXP args, int * len, int *naok, int *dup,
		     DllReference *dll);
static SEXP pkgtrim(SEXP args, DllReference *dll);

/*
  Called from resolveNativeRoutine (and itself).

  Checks whether the specified object correctly identifies a native routine.
  op is the supplied value for .NAME.  This can be
   a) a string (when this does nothing).
   b) an external pointer giving the address of the routine
      (e.g. getNativeSymbolInfo("foo")$address)
   c) or a NativeSymbolInfo itself  (e.g. getNativeSymbolInfo("foo"))

   It copies the symbol name to buf.

   NB: in the last two cases it sets fun and symbol as well!
 */
static void
checkValidSymbolId(SEXP op, SEXP call, DL_FUNC *fun,
		   R_RegisteredNativeSymbol *symbol, char *buf)
{
    if (isValidString(op)) return;

    if(TYPEOF(op) == EXTPTRSXP) {
	char *p = nullptr;
	if(R_ExternalPtrTag(op) == install("native symbol"))
	   *fun = R_ExternalPtrAddrFn(op);
	else if(R_ExternalPtrTag(op) == install("registered native symbol")) {
	   R_RegisteredNativeSymbol *tmp;
	   tmp = static_cast<R_RegisteredNativeSymbol *>( R_ExternalPtrAddr(op));
	   if(tmp) {
	      if(symbol->type != R_ANY_SYM && symbol->type != tmp->type)
		 errorcall(call, _("NULL value passed as symbol address"));
		/* Check the type of the symbol. */
	      switch(symbol->type) {
	      case R_C_SYM:
		  *fun = tmp->symbol.c->fun;
		  p = tmp->symbol.c->name;
		  break;
	      case R_CALL_SYM:
		  *fun = tmp->symbol.call->fun;
		  p = tmp->symbol.call->name;
		  break;
	      case R_FORTRAN_SYM:
		  *fun = tmp->symbol.fortran->fun;
		  p = tmp->symbol.fortran->name;
		  break;
	      case R_EXTERNAL_SYM:
		  *fun = tmp->symbol.external->fun;
		  p = tmp->symbol.external->name;
		  break;
	      default:
		 /* Something unintended has happened if we get here. */
		  errorcall(call, _("Unimplemented type %d in createRSymbolObject"),
			    symbol->type);
		  break;
	      }
	      *symbol = *tmp;
	   }
	}
	/* This is illegal C */
	if(*fun == nullptr)
	    errorcall(call, _("NULL value passed as symbol address"));

	/* copy the symbol name. */
	if (p) {
	    if (strlen(p) >= MaxSymbolBytes)
		error(_("symbol '%s' is too long"), p);
	    memcpy(buf, p, strlen(p)+1);
	}

	return;
    }
    else if(inherits(op, "NativeSymbolInfo")) {
	checkValidSymbolId(VECTOR_ELT(op, 1), call, fun, symbol, buf);
	return;
    }

    errorcall(call,
      _("first argument must be a string (of length 1) or native symbol reference"));
    return; /* not reached */
}


/*
  This is the routine that is called by do_dotCode, do_dotcall and
  do_External to find the DL_FUNC to invoke. It handles processing the
  arguments for the PACKAGE argument, if present, and also takes care
  of the cases where we are given a NativeSymbolInfo object, an
  address directly, and if the DLL is specified. If no PACKAGE is
  provided, we check whether the calling function is in a namespace
  and look there.
*/

static SEXP
resolveNativeRoutine(SEXP args, DL_FUNC *fun,
		     R_RegisteredNativeSymbol *symbol, char *buf,
		     int *nargs, int *naok, int *dup, SEXP call, SEXP env)
{
    SEXP op;
    const char *p; char *q;
    DllReference dll;
    /* This is used as shorthand for 'all' in R_FindSymbol, but
       should never be supplied */
    strcpy(dll.DLLname, ""); 
    dll.dll = nullptr; dll.obj = nullptr; dll.type = NOT_DEFINED;
    
    op = CAR(args);  // value of .NAME =
    /* NB, this sets fun, symbol and buf and is not just a check! */
    checkValidSymbolId(op, call, fun, symbol, buf);

    /* The following code modifies the argument list */
    /* We know this is ok because do_dotCode is entered */
    /* with its arguments evaluated. */

    if(symbol->type == R_C_SYM || symbol->type == R_FORTRAN_SYM) {
	/* And that also looks for PACKAGE = */
	args = naokfind(CDR(args), nargs, naok, dup, &dll);
	if(*naok == NA_LOGICAL)
	    errorcall(call, _("invalid '%s' value"), "naok");
	if(*nargs > MAX_ARGS)
	    errorcall(call, _("too many arguments in foreign function call"));
    } else {
	/* This has the side effect of setting dll.type if a PACKAGE=
	   argument if found, but it will only be used if a string was
	   passed in  */
	args = pkgtrim(args, &dll);
    }

    /* We were given a symbol (or an address), so we are done. */
    if (*fun) return args;

    if (dll.type == FILENAME && !strlen(dll.DLLname))
	errorcall(call, _("PACKAGE = \"\" is invalid"));

    // find if we were called from a namespace
    SEXP env2 = ENCLOS(env);
    const char *ns = "";
    if(R_IsNamespaceEnv(env2))
	ns = CHAR(STRING_ELT(R_NamespaceEnvSpec(env2), 0));
    else env2 = R_NilValue;

#ifdef CHECK_CROSS_USAGE
    if (dll.type == FILENAME && strcmp(dll.DLLname, "base")) {
	if(strlen(ns) && strcmp(dll.DLLname, ns) &&
	   !(streql(dll.DLLname, "BioC_graph") && streql(ns, "graph")))
	    warningcall(call, 
			"using PACKAGE = \"%s\" from namespace '%s'",
			dll.DLLname, ns);
    }
#endif

    /* Make up the load symbol */
    if(TYPEOF(op) == STRSXP) {
	const void *vmax = vmaxget();
	p = translateChar(STRING_ELT(op, 0));
	if(strlen(p) >= MaxSymbolBytes)
	    error(_("symbol '%s' is too long"), p);
	q = buf;
	while ((*q = *p) != '\0') {
	    if(symbol->type == R_FORTRAN_SYM) *q = char( tolower(*q));
	    p++;
	    q++;
	}
	vmaxset(vmax);
    }

    if(dll.type != FILENAME && strlen(ns)) {
	/* no PACKAGE= arg, so see if we can identify a DLL
	   from the namespace defining the function */
	*fun = R_FindNativeSymbolFromDLL(buf, &dll, symbol, env2);
	if (*fun) return args;
	errorcall(call, "\"%s\" not resolved from current namespace (%s)", 
		  buf, ns);
    }

    /* NB: the actual conversion to the symbol is done in
       R_dlsym in Rdynload.c.  That prepends an underscore (usually),
       and may append one or more underscores.
    */

    *fun = R_FindSymbol(buf, dll.DLLname, symbol);
    if (*fun) return args;

    /* so we've failed and bail out */
    if(strlen(dll.DLLname)) {
	switch(symbol->type) {
	case R_C_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".C", dll.DLLname);
	    break;
	case R_FORTRAN_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".Fortran", dll.DLLname);
	    break;
	case R_CALL_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".Call", dll.DLLname);
	    break;
	case R_EXTERNAL_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".External", dll.DLLname);
	    break;
	case R_ANY_SYM:
	    errorcall(call,
		      _("%s symbol name \"%s\" not in DLL for package \"%s\""),
		      "C/Fortran", buf, dll.DLLname);
	    break;
	}
    } else
	errorcall(call, _("%s symbol name \"%s\" not in load table"),
		  symbol->type == R_FORTRAN_SYM ? "Fortran" : "C", buf);

    return args; /* -Wall */
}


static Rboolean
checkNativeType(int targetType, int actualType)
{
    if(targetType > 0) {
	if(targetType == INTSXP || targetType == LGLSXP) {
	    return(CXXRCONSTRUCT(Rboolean, actualType == INTSXP || actualType == LGLSXP));
	}
	return(CXXRCONSTRUCT(Rboolean, targetType == actualType));
    }

    return(TRUE);
}

static Rboolean
comparePrimitiveTypes(R_NativePrimitiveArgType type, SEXP s, Rboolean dup)
{
    SEXPTYPE stype = SEXPTYPE(type);
    if(stype == ANYSXP || TYPEOF(s) == stype)
	return(TRUE);

    if(dup && stype == SINGLESXP)
	return Rboolean(asLogical(getAttrib(s, install("Csingle"))) == TRUE);

    return(FALSE);
}


/* Foreign Function Interface.  This code allows a user to call C */
/* or Fortran code which is either statically or dynamically linked. */

/* NB: this leaves NAOK and DUP arguments on the list */

/* find NAOK and DUP, find and remove PACKAGE */
static SEXP naokfind(SEXP args, int * len, int *naok, int *dup,
		     DllReference *dll)
{
    SEXP s, prev;
    int nargs=0, naokused=0, dupused=0, pkgused=0;
    const char *p;

    *naok = 0;
    *dup = 1;
    *len = 0;
    for(s = args, prev=args; s != R_NilValue;) {
	if(TAG(s) == NaokSymbol) {
	    *naok = asLogical(CAR(s));
	    /* SETCDR(prev, s = CDR(s)); */
	    if(naokused++ == 1) warning(_("'%s' used more than once"), "NAOK");
	} else if(TAG(s) == DupSymbol) {
	    *dup = asLogical(CAR(s));
	    /* SETCDR(prev, s = CDR(s)); */
	    if(dupused++ == 1) warning(_("'%s' used more than once"), "DUP");
	} else if(TAG(s) == PkgSymbol) {
	    dll->obj = CAR(s);  // really? 
	    if(TYPEOF(CAR(s)) == STRSXP) {
		p = translateChar(STRING_ELT(CAR(s), 0));
		if(strlen(p) > PATH_MAX - 1)
		    error(_("DLL name is too long"));
		dll->type = FILENAME;
		strcpy(dll->DLLname, p);
		if(pkgused++ > 1) 
		    warning(_("'%s' used more than once"), "PACKAGE");
		/* More generally, this should allow us to process
		   any additional arguments and not insist that PACKAGE
		   be the last argument.
		*/
	    } else {
		/* Have a DLL object, which is not something documented .... */
		if(TYPEOF(CAR(s)) == EXTPTRSXP) {
		    dll->dll = static_cast<HINSTANCE>( R_ExternalPtrAddr(CAR(s)));
		    dll->type = DLL_HANDLE;
		} else if(TYPEOF(CAR(s)) == VECSXP) {
		    dll->type = R_OBJECT;
		    dll->obj = s;
		    strcpy(dll->DLLname,
			   translateChar(STRING_ELT(VECTOR_ELT(CAR(s), 1), 0)));
		    dll->dll = static_cast<HINSTANCE>( R_ExternalPtrAddr(VECTOR_ELT(s, 4)));
		} else 
		    error("incorrect type (%s) of PACKAGE argument\n",
			  type2char(TYPEOF(CAR(s))));

	    }
	} else {
	    nargs++;
	    prev = s;
	    s = CDR(s);
	    continue;
	}
	if(s == args)
	    args = s = CDR(s);
	else
	    SETCDR(prev, s = CDR(s));
    }
    *len = nargs;
    return args;
}

static void setDLLname(SEXP s, char *DLLname)
{
    SEXP ss = CAR(s);
    const char *name;

    if(TYPEOF(ss) != STRSXP || length(ss) != 1)
	error(_("PACKAGE argument must be a single character string"));
    name = translateChar(STRING_ELT(ss, 0));
    /* allow the package: form of the name, as returned by find */
    if(strncmp(name, "package:", 8) == 0)
	name += 8;
    if(strlen(name) > PATH_MAX - 1)
	error(_("PACKAGE argument is too long"));
    strcpy(DLLname, name);
}

static SEXP pkgtrim(SEXP args, DllReference *dll)
{
    SEXP s, ss;
    int pkgused = 0;

    if (PkgSymbol == nullptr) PkgSymbol = install("PACKAGE");

    for(s = args ; s != R_NilValue;) {
	ss = CDR(s);
	/* Look for PACKAGE=. We look at the next arg, unless
	   this is the last one (which will only happen for one arg),
	   and remove it */
	if(ss == R_NilValue && TAG(s) == PkgSymbol) {
	    if(pkgused++ == 1)
		warning(_("'%s' used more than once"), "PACKAGE");
	    setDLLname(s, dll->DLLname);
	    dll->type = FILENAME;
	    return R_NilValue;
	}
	if(TAG(ss) == PkgSymbol) {
	    if(pkgused++ == 1)
		warning(_("'%s' used more than once"), "PACKAGE");
	    setDLLname(ss, dll->DLLname);
	    dll->type = FILENAME;
	    SETCDR(s, CDR(ss));
	}
	s = CDR(s);
    }
    return args;
}

static SEXP enctrim(SEXP args)
{
    SEXP s, ss;

    for(s = args ; s != R_NilValue;) {
	ss = CDR(s);
	/* Look for ENCODING=. We look at the next arg, unless
	   this is the last one (which will only happen for one arg),
	   and remove it */
	if(ss == R_NilValue && TAG(s) == EncSymbol) {
	    warning("ENCODING is defunct and will be ignored");
	    return R_NilValue;
	}
	if(TAG(ss) == EncSymbol) {
	    warning("ENCODING is defunct and will be ignored");
	    SETCDR(s, CDR(ss));
	}
	s = CDR(s);
    }
    return args;
}



SEXP attribute_hidden do_isloaded(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const char *sym, *type="", *pkg = "";
    int val = 1, nargs = length(args);
    R_RegisteredNativeSymbol symbol = {R_ANY_SYM, {nullptr}, nullptr};

    if (nargs < 1) error(_("no arguments supplied"));
    if (nargs > 3) error(_("too many arguments"));

    if(!isValidString(CAR(args)))
	error(_("invalid '%s' argument"), "symbol");
    sym = translateChar(STRING_ELT(CAR(args), 0));
    if(nargs >= 2) {
	if(!isValidString(CADR(args)))
	    error(_("invalid '%s' argument"), "PACKAGE");
	pkg = translateChar(STRING_ELT(CADR(args), 0));
    }
    if(nargs >= 3) {
	if(!isValidString(CADDR(args)))
	    error(_("invalid '%s' argument"), "type");
	type = CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
	if(strcmp(type, "C") == 0) symbol.type = R_C_SYM;
	else if(strcmp(type, "Fortran") == 0) symbol.type = R_FORTRAN_SYM;
	else if(strcmp(type, "Call") == 0) symbol.type = R_CALL_SYM;
	else if(strcmp(type, "External") == 0) symbol.type = R_EXTERNAL_SYM;
    }
    if(!(R_FindSymbol(sym, pkg, &symbol))) val = 0;
    return ScalarLogical(val);
}

/*   Call dynamically loaded "internal" functions.
     Original code by Jean Meloche <jean@stat.ubc.ca> */

typedef SEXP (*R_ExternalRoutine)(SEXP);
typedef SEXP (*R_ExternalRoutine2)(SEXP, SEXP, SEXP, SEXP);

SEXP attribute_hidden do_External(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = nullptr;
    SEXP retval;
    R_RegisteredNativeSymbol symbol = {R_EXTERNAL_SYM, {nullptr}, nullptr};
    const void *vmax = vmaxget();
    char buf[MaxSymbolBytes];

    if (length(args) < 1) errorcall(call, _("'.NAME' is missing"));
    check1arg2(args, call, ".NAME");
    args = resolveNativeRoutine(args, &ofun, &symbol, buf, nullptr, nullptr,
				nullptr, call, env);

    if(symbol.symbol.external && symbol.symbol.external->numArgs > -1) {
	int nargs = length(args) - 1;
	if(symbol.symbol.external->numArgs != nargs)
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      nargs, symbol.symbol.external->numArgs, buf);
    }

    if (PRIMVAL(op) == 1) {
	R_ExternalRoutine2 fun = R_ExternalRoutine2( ofun);
	retval = fun(call, op, args, env);
    } else {
	R_ExternalRoutine fun = R_ExternalRoutine( ofun);
	retval = fun(args);
    }
    vmaxset(vmax);
    return retval;
}

typedef SEXP (*VarFun)(...);

/* .Call(name, <args>) */
SEXP attribute_hidden do_dotcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = nullptr;
    VarFun fun = nullptr;
    SEXP retval, cargs[MAX_ARGS], pargs;
    R_RegisteredNativeSymbol symbol = {R_CALL_SYM, {nullptr}, nullptr};

    int nargs;
    const void *vmax = vmaxget();
    char buf[MaxSymbolBytes];

    if (length(args) < 1) errorcall(call, _("'.NAME' is missing"));
    check1arg2(args, call, ".NAME");
    args = resolveNativeRoutine(args, &ofun, &symbol, buf, nullptr, nullptr,
				nullptr, call, env);
    args = CDR(args);

    for(nargs = 0, pargs = args ; pargs != R_NilValue; pargs = CDR(pargs)) {
	if (nargs == MAX_ARGS)
	    errorcall(call, _("too many arguments in foreign function call"));
	cargs[nargs] = CAR(pargs);
	nargs++;
    }
    if(symbol.symbol.call && symbol.symbol.call->numArgs > -1) {
	if(symbol.symbol.call->numArgs != nargs)
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      nargs, symbol.symbol.call->numArgs, buf);
    }

    retval = R_NilValue;	/* -Wall */
    fun = reinterpret_cast<VarFun>( ofun);
    switch (nargs) {
    case 0:
	retval = static_cast<SEXP>(ofun());
	break;

/*  This macro expands out to:
    case 1:
	retval = CXXRNOCAST(SEXP)fun(cargs[0]);
	break;
    case 2:
	retval = CXXRNOCAST(SEXP)fun(cargs[0], cargs[1]);
	break;
    ... on to case 65
*/

#define ARGUMENT_LIST(Z, N, IGNORED) BOOST_PP_COMMA_IF(N) cargs[N]
#define CASE_STATEMENT(Z, N, IGNORED)                                       \
    case N:                                                                 \
	retval = CXXRNOCAST(SEXP)fun(BOOST_PP_REPEAT(N, ARGUMENT_LIST, 0)); \
	break;

	BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MAX_ARGS), CASE_STATEMENT, 0);

#undef ARGUMENT_LIST
#undef CASE_STATEMENT

    default:
	errorcall(call, _("too many arguments, sorry"));
    }
    vmaxset(vmax);
    return retval;
}

/*  Call dynamically loaded "internal" graphics functions
    .External.graphics (used in graphics) and  .Call.graphics (used in grid).

    If there is an error or user-interrupt in the above
    evaluation, dd->recordGraphics is set to TRUE
    on all graphics devices (see GEonExit(); called in errors.c)

    NOTE: if someone uses try() around this call and there
    is an error, then dd->recordGraphics stays FALSE, so
    subsequent pages of graphics output are NOT saved on
    the display list.  A workaround is to deliberately
    force an error in a graphics call (e.g., a grid popViewport()
    while in the ROOT viewport) which will reset dd->recordGraphics
    to TRUE as per the comment above.
*/

#include <R_ext/GraphicsEngine.h>

SEXP attribute_hidden do_Externalgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    pGEDevDesc dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_External(call, op, args, env));
    dd->recordGraphics = record;
    if (GErecording(call, dd)) { // which is record && call != R_NilValue
	if (!GEcheckState(dd))
	    errorcall(call, _("invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

SEXP attribute_hidden do_dotcallgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    pGEDevDesc dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_dotcall(call, op, args, env));
    dd->recordGraphics = record;
    if (GErecording(call, dd)) {
	if (!GEcheckState(dd))
	    errorcall(call, _("invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

static SEXP
Rf_getCallingDLL(void)
{
    SEXP e, ans;
    SEXP rho = R_NilValue;
    Rboolean found = FALSE;

    /* First find the environment of the caller.
       Testing shows this is the right caller, despite the .C/.Call ...
     */
    {
	ClosureContext* cptr = ClosureContext::innermost();
	if (cptr)
	    rho = cptr->workingEnvironment();
    }

    /* Then search up until we hit a namespace or globalenv.
       The idea is that we will not find a namespace unless the caller
       was defined in one. */
    while(rho != R_NilValue) {
	if (rho == R_GlobalEnv) break;
	else if (R_IsNamespaceEnv(rho)) {
	    found = TRUE;
	    break;
	}
	rho = ENCLOS(rho);
    }
    if(!found) return R_NilValue;

    PROTECT(e = lang2(install("getCallingDLLe"), rho));
    ans = eval(e,  R_GlobalEnv);
    UNPROTECT(1);
    return(ans);
}


/*
  We are given the PACKAGE argument in dll.obj
  and we can try to figure out how to resolve this.
  0) dll.obj is NULL.  Then find the environment of the
   calling function and if it is a namespace, get the first registered DLL.

  1) dll.obj is a DLLInfo object
*/
static DL_FUNC
R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol,
			  SEXP env)
{
    int numProtects = 0;
    DllInfo *info;
    DL_FUNC fun = nullptr;

    if(dll->obj == nullptr) {
	/* Rprintf("\nsearching for %s\n", name); */
	dll->obj = Rf_getCallingDLL();
	if (env != R_NilValue) {
	    SEXP e;
	    PROTECT(e = lang2(install("getCallingDLLe"), env));
	    dll->obj = eval(e, R_GlobalEnv);
	    UNPROTECT(1);
	} else dll->obj = Rf_getCallingDLL();
	PROTECT(dll->obj); numProtects++;
    }

    if(inherits(dll->obj, "DLLInfo")) {
	SEXP tmp;
	tmp = VECTOR_ELT(dll->obj, 4);
	info = static_cast<DllInfo *>( R_ExternalPtrAddr(tmp));
	if(!info)
	    error(_("NULL value for DLLInfoReference when looking for DLL"));
	fun = R_dlsym(info, name, symbol);
    }

    if(numProtects) UNPROTECT(numProtects);

    return fun;
}



/* .C() {op=0}  or  .Fortran() {op=1} */
/* Use of this except for atomic vectors is not allowed for .Fortran,
   and is only kept for legacy code for .C.

   CRAN packages R2Cuba, RCALI, ars, coxme, fCopulae, locfit, nlme,
   splinesurv and survival pass functions, the case of RCALI as a list
   of two functions.

   RecordLinkage and locfit pass lists.
*/

/* pattern and number of guard bytes */
#define FILL 0xee
#define NG 64

SEXP attribute_hidden do_dotCode(SEXP call, SEXP op, SEXP args, SEXP env)
{
    void **cargs, **cargs0 = nullptr /* -Wall */;
    int dup, naok, na, nargs, Fort;
    Rboolean havenames, copy = R_CBoundsCheck; /* options(CboundsCheck) */
    DL_FUNC ofun = nullptr;
    VarFun fun = nullptr;
    SEXP ans, pa, s;
    R_RegisteredNativeSymbol symbol = {R_C_SYM, {nullptr}, nullptr};
    R_NativePrimitiveArgType *checkTypes = nullptr;
    R_NativeArgStyle *argStyles = nullptr;
    const void *vmax;
    char symName[MaxSymbolBytes];

    if (length(args) < 1) errorcall(call, _("'.NAME' is missing"));
    check1arg2(args, call, ".NAME");
    if (NaokSymbol == nullptr || DupSymbol == nullptr || PkgSymbol == nullptr) {
	NaokSymbol = install("NAOK");
	DupSymbol = install("DUP");
	PkgSymbol = install("PACKAGE");
    }
    if (EncSymbol == nullptr) EncSymbol = install("ENCODING");
    if (CSingSymbol == nullptr) CSingSymbol = install("Csingle");
    vmax = vmaxget();
    Fort = PRIMVAL(op);
    if(Fort) symbol.type = R_FORTRAN_SYM;

    args = enctrim(args);
    args = resolveNativeRoutine(args, &ofun, &symbol, symName, &nargs,
				&naok, &dup, call, env);
    fun = reinterpret_cast<VarFun>( ofun);

    if(symbol.symbol.c && symbol.symbol.c->numArgs > -1) {
	if(symbol.symbol.c->numArgs != nargs)
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      nargs, symbol.symbol.c->numArgs, symName);

	checkTypes = symbol.symbol.c->types;
	argStyles = symbol.symbol.c->styles;
    }

    /* Construct the return value */
    nargs = 0;
    havenames = FALSE;
    for(pa = args ; pa != R_NilValue; pa = CDR(pa)) {
	if (TAG(pa) != R_NilValue) havenames = TRUE;
	nargs++;
    }

    PROTECT(ans = allocVector(VECSXP, nargs));
    if (havenames) {
	SEXP names;
	PROTECT(names = allocVector(STRSXP, nargs));
	for (na = 0, pa = args ; pa != R_NilValue ; pa = CDR(pa), na++) {
	    if (TAG(pa) == R_NilValue)
		SET_STRING_ELT(names, na, R_BlankString);
	    else
		SET_STRING_ELT(names, na, PRINTNAME(TAG(pa)));
	}
	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }

    /* Convert the arguments for use in foreign function calls. */
    cargs = static_cast<void**>( CXXR_alloc(nargs, sizeof(void*)));
    if (copy) cargs0 = static_cast<void**>( CXXR_alloc(nargs, sizeof(void*)));
    for(na = 0, pa = args ; pa != R_NilValue; pa = CDR(pa), na++) {
	if(checkTypes &&
	   !comparePrimitiveTypes(checkTypes[na], CAR(pa), CXXRCONSTRUCT(Rboolean, dup))) {
	    /* We can loop over all the arguments and report all the
	       erroneous ones, but then we would also want to avoid
	       the conversions.  Also, in the future, we may just
	       attempt to coerce the value to the appropriate
	       type. */
	    errorcall(call, _("wrong type for argument %d in call to %s"),
		      na+1, symName);
	}
	int nprotect = 0, targetType =  checkTypes ? checkTypes[na] : 0;
	R_xlen_t n;
	s = CAR(pa);
	/* start with return value a copy of the inputs, as that is
	   what is needed for DUP = FALSE and for non-atomic-vector inputs */
	SET_VECTOR_ELT(ans, na, s);

	if(checkNativeType(targetType, TYPEOF(s)) == FALSE) {
	    if(!dup) {
		error(_("explicit request not to duplicate arguments in call to '%s', but argument %d is of the wrong type (%d != %d)"),
		      symName, na + 1, targetType, TYPEOF(s));
	    }

	    if(targetType != SINGLESXP) {
		/* Cannot be called if DUP = FALSE, so only needs to live
		   until copied in the switch.
		   But R_alloc allocates, so missed protection < R 2.15.0.
		*/
		PROTECT(s = coerceVector(s, CXXRCONSTRUCT(SEXPTYPE, targetType)));
		nprotect++;
	    }
	}

	/* We create any copies needed for the return value here,
	   except for character vectors.  The compiled code works on
	   the data pointer of the return value for the other atomic
	   vectors, and anything else is supposed to be read-only.

	   We do not need to copy if the inputs have NAMED = 0 */

#ifdef LONG_VECTOR_SUPPORT
	if (isVector(s) && IS_LONG_VEC(s))
	    error(_("long vectors (argument %d) are not supported in %s"), 
		  na + 1, Fort ? ".C" : ".Fortran");
#endif
	SEXPTYPE t = TYPEOF(s);
	switch(t) {
	case RAWSXP:
	    if (copy && dup) {
		n = XLENGTH(s);
		char *ptr = R_alloc(n * sizeof(Rbyte) + 2 * NG, 1);
		memset(ptr, FILL, n * sizeof(Rbyte) + 2 * NG);
		ptr += NG;
		memcpy(ptr, RAW(s), n);
		cargs[na] = CXXRNOCAST(void *) ptr;
	    } else if (dup && NAMED(s)) {
		n = XLENGTH(s);
		SEXP ss = allocVector(t, n);
		memcpy(RAW(ss), RAW(s), n * sizeof(Rbyte));
		SET_VECTOR_ELT(ans, na, ss);
		cargs[na] = CXXRNOCAST(void*) RAW(ss);
#ifdef R_MEMORY_PROFILING
		if (RTRACE(s)) memtrace_report(s, ss);
#endif
	    } else cargs[na] = CXXRNOCAST(void *) RAW(s);
	    break;
	case LGLSXP:
	case INTSXP:
	    n = XLENGTH(s);
	    {
		int *iptr = INTEGER(s);
		if (!naok)
		    for (R_xlen_t i = 0 ; i < n ; i++)
			if(iptr[i] == NA_INTEGER)
			    error(_("NAs in foreign function call (arg %d)"), na + 1);
		if (copy && dup) {
		    char *ptr = R_alloc(n * sizeof(int) + 2 * NG, 1);
		    memset(ptr, FILL, n * sizeof(int) + 2 * NG);
		    ptr += NG;
		    memcpy(ptr, INTEGER(s), n * sizeof(int));
		    cargs[na] = CXXRNOCAST(void*) ptr;
		} else if (dup && NAMED(s)) {
		    SEXP ss = allocVector(t, n);
		    memcpy(INTEGER(ss), INTEGER(s), n * sizeof(int));
		    SET_VECTOR_ELT(ans, na, ss);
		    cargs[na] = CXXRNOCAST(void*) INTEGER(ss);
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, ss);
#endif
		} else cargs[na] = CXXRNOCAST(void*) iptr;
	    }
	    break;
	case REALSXP:
	    {
		n = XLENGTH(s);
		double *rptr = REAL(s);
		if (!naok)
		    for (R_xlen_t i = 0 ; i < n ; i++)
			if(!R_FINITE(rptr[i]))
			    error(_("NA/NaN/Inf in foreign function call (arg %d)"), na + 1);
		if (asLogical(getAttrib(s, CSingSymbol)) == 1) {
		    float *sptr = static_cast<float*>( CXXR_alloc(n, sizeof(float)));
		    for (R_xlen_t i = 0 ; i < n ; i++) sptr[i] = float( REAL(s)[i]);
		    cargs[na] = CXXRNOCAST(void*) sptr;
		    if (!dup) warning(_("single values not returned if not duplicated"));
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, sptr);
#endif
		} else if (copy && dup) {
		    char *ptr = R_alloc(n * sizeof(double) + 2 * NG, 1);
		    memset(ptr, FILL, n * sizeof(double) + 2 * NG);
		    ptr += NG;
		    memcpy(ptr, REAL(s), n * sizeof(double));
		    cargs[na] = CXXRNOCAST(void*) ptr;
		} else if (dup && NAMED(s)) {
		    SEXP ss  = allocVector(t, n);
		    memcpy(REAL(ss), REAL(s), n * sizeof(double));
		    SET_VECTOR_ELT(ans, na, ss);
		    cargs[na] = CXXRNOCAST(void*) REAL(ss);
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, ss);
#endif
		} else cargs[na] = CXXRNOCAST(void*) rptr;
	    }
	    break;
	case CPLXSXP:
	    {
		n = XLENGTH(s);
		Rcomplex *zptr = COMPLEX(s);
		if (!naok)
		    for (R_xlen_t i = 0 ; i < n ; i++)
			if(!R_FINITE(zptr[i].r) || !R_FINITE(zptr[i].i))
			    error(_("complex NA/NaN/Inf in foreign function call (arg %d)"), na + 1);
		if (copy && dup) {
		    char *ptr = R_alloc(n * sizeof(Rcomplex) + 2 * NG, 1);
		    memset(ptr, FILL, n * sizeof(Rcomplex) + 2 * NG);
		    ptr += NG;
		    memcpy(ptr, COMPLEX(s), n * sizeof(Rcomplex));
		    cargs[na] = CXXRNOCAST(void*) ptr;
		} else if (dup && NAMED(s)) {
		    SEXP ss = allocVector(t, n);
		    memcpy(COMPLEX(ss), COMPLEX(s), n * sizeof(Rcomplex));
		    SET_VECTOR_ELT(ans, na, ss);
		    cargs[na] = CXXRNOCAST(void*) COMPLEX(ss);
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, ss);
#endif
		} else cargs[na] = CXXRNOCAST(void *) zptr;
	    }
	    break;
	case STRSXP:
	    if (!dup)
		error(_("character variables must be duplicated in .C/.Fortran"));
	    n = XLENGTH(s);
	    if (Fort) {
		const char *ss = translateChar(STRING_ELT(s, 0));
		if (n > 1)
		    warning(_("only first string in char vector used in .Fortran"));
		char *fptr = static_cast<char*>( CXXR_alloc(max(255, strlen(ss)) + 1, sizeof(char)));
		strcpy(fptr, ss);
		cargs[na] =  CXXRNOCAST(void*) fptr;
	    } else if (copy) {
		char **cptr = static_cast<char**>( CXXR_alloc(n, sizeof(char*))),
		    **cptr0 = static_cast<char**>( CXXR_alloc(n, sizeof(char*)));
		for (R_xlen_t i = 0 ; i < n ; i++) {
		    const char *ss = translateChar(STRING_ELT(s, i));
		    size_t nn = strlen(ss) + 1 + 2 * NG;
		    char *ptr = static_cast<char*>( CXXR_alloc(nn, sizeof(char)));
		    memset(ptr, FILL, nn);
		    cptr[i] = cptr0[i] = ptr + NG;
		    strcpy(cptr[i], ss);
		}
		cargs[na] = CXXRNOCAST(void*) cptr;
		cargs0[na] = CXXRNOCAST(void*) cptr0;
#ifdef R_MEMORY_PROFILING
		if (RTRACE(s)) memtrace_report(s, cargs[na]);
#endif
	    } else {
		char **cptr = static_cast<char**>( CXXR_alloc(n, sizeof(char*)));
		for (R_xlen_t i = 0 ; i < n ; i++) {
		    const char *ss = translateChar(STRING_ELT(s, i));
		    size_t nn = strlen(ss) + 1;
		    if(nn > 1) {
			cptr[i] = static_cast<char*>( CXXR_alloc(nn, sizeof(char)));
			strcpy(cptr[i], ss);
		    } else {
			/* Protect ourselves against those who like to
			   extend "", maybe using strncpy */
			nn = 128;
			cptr[i] = static_cast<char*>( CXXR_alloc(nn, sizeof(char)));
			memset(cptr[i], 0, nn);
		    }
		    cargs[na] = CXXRNOCAST(void*) cptr;
#ifdef R_MEMORY_PROFILING
		    if (RTRACE(s)) memtrace_report(s, cargs[na]);
#endif
		}
	    }
	    break;
	case VECSXP:
	    {
		if (Fort) error(_("invalid mode (%s) to pass to Fortran (arg %d)"),
				type2char(t), na + 1);
		/* Used read-only, so this is safe */
		n = XLENGTH(s);
		SEXP *lptr = static_cast<SEXP *>( CXXR_alloc(n, sizeof(SEXP)));
		for (R_xlen_t i = 0 ; i < n ; i++) lptr[i] = VECTOR_ELT(s, i);
		cargs[na] = CXXRNOCAST(void*) lptr;
	    }
	    break;
	case CLOSXP:
	case BUILTINSXP:
	case SPECIALSXP:
	case ENVSXP:
	    if (Fort) error(_("invalid mode (%s) to pass to Fortran (arg %d)"), 
			    type2char(t), na + 1);
	    cargs[na] =  CXXRNOCAST(void*) s;
	    break;
	case NILSXP:
	    error(_("invalid mode (%s) to pass to C or Fortran (arg %d)"), 
		  type2char(t), na + 1);
	    cargs[na] =  CXXRNOCAST(void*) s;
	    break;
	default:
	    /* Includes pairlists from R 2.15.0 */
	    if (Fort) error(_("invalid mode (%s) to pass to Fortran (arg %d)"), 
			    type2char(t), na + 1);
	    warning("passing an object of type '%s' to .C (arg %d) is deprecated", 
		    type2char(t), na + 1);
	    if (t == LISTSXP)
		warning(_("pairlists are passed as SEXP as from R 2.15.0"));
	    cargs[na] =  CXXRNOCAST(void*) s;
	    continue;
	}
	if (nprotect) UNPROTECT(nprotect);
    }

    switch (nargs) {
    case 0:
	/* Silicon graphics C chokes here */
	/* if there is no argument to fun. */
	fun(0);
	break;
/*  This macro expands out to:
    case 1:
	fun(cargs[0]);
	break;
    case 2:
	fun(cargs[0], cargs[1]);
	break;
    ... on to case 65
*/

#define ARGUMENT_LIST(Z, N, IGNORED) BOOST_PP_COMMA_IF(N) cargs[N]
#define CASE_STATEMENT(Z, N, IGNORED)              \
    case N:                                        \
	fun(BOOST_PP_REPEAT(N, ARGUMENT_LIST, 0)); \
	break;

	BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MAX_ARGS), CASE_STATEMENT, 0);

#undef ARGUMENT_LIST
#undef CASE_STATEMENT

    default:
	errorcall(call, _("too many arguments, sorry"));
    }

    if (dup) {
	for (na = 0, pa = args ; pa != R_NilValue ; pa = CDR(pa), na++) {
	    if(argStyles && argStyles[na] == R_ARG_IN) {
		SET_VECTOR_ELT(ans, na, R_NilValue);
		continue;
	    } else {
		void *p = cargs[na];
		SEXP arg = CAR(pa);
		s = VECTOR_ELT(ans, na);
		R_NativePrimitiveArgType type =
		    checkTypes ? checkTypes[na] : TYPEOF(arg);
	        R_xlen_t n = xlength(arg);

		switch(type) {
		case RAWSXP:
		    if (copy) {
			s = allocVector(CXXRCONSTRUCT(SEXPTYPE, type), n);
			unsigned char *ptr = static_cast<unsigned char *>( p);
			memcpy(RAW(s), ptr, n * sizeof(Rbyte));
			ptr += n * sizeof(Rbyte);
			for (int i = 0; i < NG; i++)
			    if(*ptr++ != FILL)
				error("array over-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
			ptr = static_cast<unsigned char *>( p);
			for (int i = 0; i < NG; i++)
			    if(*--ptr != FILL)
				error("array under-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
		    }
		    break;
		case INTSXP:
		    if (copy) {
			s = allocVector(CXXRCONSTRUCT(SEXPTYPE, type), n);
			unsigned char *ptr = static_cast<unsigned char *>( p);
			memcpy(INTEGER(s), ptr, n * sizeof(int));
			ptr += n * sizeof(int);
			for (int i = 0; i < NG; i++)
			    if(*ptr++ != FILL)
				error("array over-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
			ptr = static_cast<unsigned char *>( p);
			for (int i = 0; i < NG; i++)
			    if(*--ptr != FILL)
				error("array under-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
		    }
		    break;
		case LGLSXP:
		    if (copy) {
			s = allocVector(CXXRCONSTRUCT(SEXPTYPE, type), n);
			unsigned char *ptr = static_cast<unsigned char *>( p);
			int *iptr = reinterpret_cast<int*>( ptr), tmp;
			for (R_xlen_t i = 0 ; i < n ; i++) {
			    tmp =  iptr[i];
			    LOGICAL(s)[i] = (tmp == NA_INTEGER || tmp == 0) ? tmp : 1;
			}
			ptr += n * sizeof(int);
			for (int i = 0; i < NG;  i++)
			    if(*ptr++ != FILL)
				error("array over-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
			ptr = static_cast<unsigned char *>( p);
			for (int i = 0; i < NG; i++)
			    if(*--ptr != FILL)
				error("array under-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
		    } else {
			int *iptr = INTEGER(arg), tmp;
			for (R_xlen_t i = 0 ; i < n ; i++) {
			    tmp =  iptr[i];
			    iptr[i] = (tmp == NA_INTEGER || tmp == 0) ? tmp : 1;
			}
		    }
		    break;
		case REALSXP:
		case SINGLESXP:
		    if (copy) {
			s = allocVector(REALSXP, n);
			if (type == SINGLESXP || asLogical(getAttrib(arg, CSingSymbol)) == 1) {
			    float *sptr = static_cast<float*>( p);
			    for(R_xlen_t i = 0 ; i < n ; i++) 
				REAL(s)[i] = double( sptr[i]);
			} else {
			    unsigned char *ptr = static_cast<unsigned char *>( p);
			    memcpy(REAL(s), ptr, n * sizeof(double));
			    ptr += n * sizeof(double);
			    for (int i = 0; i < NG; i++)
				if(*ptr++ != FILL)
				    error("array over-run in %s(\"%s\") in %s argument %d\n", 
					  Fort ? ".Fortran" : ".C",
					  symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
			    ptr = static_cast<unsigned char *>( p);
			    for (int i = 0; i < NG; i++)
				if(*--ptr != FILL)
				    error("array under-run in %s(\"%s\") in %s argument %d\n", 
					  Fort ? ".Fortran" : ".C",
					  symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
			}
		    } else {
			if (type == SINGLESXP || asLogical(getAttrib(arg, CSingSymbol)) == 1) {
			    s = allocVector(REALSXP, n);
			    float *sptr = static_cast<float*>( p);
			    for(int i = 0 ; i < n ; i++) 
				REAL(s)[i] = double( sptr[i]);
			}
		    }
		    break;
		case CPLXSXP:
		    if (copy) {
			s = allocVector(CXXRCONSTRUCT(SEXPTYPE, type), n);
			unsigned char *ptr = static_cast<unsigned char *>( p);
			memcpy(COMPLEX(s), p, n * sizeof(Rcomplex));
			ptr += n * sizeof(Rcomplex);
			for (int i = 0; i < NG;  i++) 
			    if(*ptr++ != FILL)
				error("array over-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
			ptr = static_cast<unsigned char *>( p);
			for (int i = 0; i < NG; i++)
			    if(*--ptr != FILL)
				error("array under-run in %s(\"%s\") in %s argument %d\n", 
				      Fort ? ".Fortran" : ".C",
				      symName, type2char(CXXRCONSTRUCT(SEXPTYPE, type)), na+1);
		    }
		    break;
		case STRSXP:
		    if(Fort) {
			char buf[256];
			/* only return one string: warned on the R -> Fortran step */
			strncpy(buf, static_cast<char*>(p), 255);
			buf[255] = '\0';
			PROTECT(s = allocVector(CXXRCONSTRUCT(SEXPTYPE, type), 1));
			SET_STRING_ELT(s, 0, mkChar(buf));
			UNPROTECT(1);
		    } else if (copy) {
			SEXP ss = arg;
			PROTECT(s = allocVector(CXXRCONSTRUCT(SEXPTYPE, type), n));
			char **cptr = static_cast<char**>( p), **cptr0 = static_cast<char**>( cargs0[na]);
			for (R_xlen_t i = 0 ; i < n ; i++) {
			    unsigned char *ptr = reinterpret_cast<unsigned char *>( cptr[i]);
			    SET_STRING_ELT(s, i, mkChar(cptr[i]));
			    if (cptr[i] == cptr0[i]) {
				    const char *z = translateChar(STRING_ELT(ss, i));
				for (int j = 0; j < NG; j++)
				    if(*--ptr != FILL)
					error("array under-run in .C(\"%s\") in character argument %d, element %d", 
					      symName, na+1, int(i+1));
				ptr = reinterpret_cast<unsigned char *>( cptr[i]);
				ptr += strlen(z) + 1;
				for (int j = 0; j < NG;  j++) 
				    if(*ptr++ != FILL) {
					 // force termination
					unsigned char *p = ptr;
					for (int k = 1; k < NG - j; k++, p++)
					    if (*p == FILL) *p = '\0';
					error("array over-run in .C(\"%s\") in character argument %d, element %d\n'%s'->'%s'\n", 
					      symName, na+1, int(i+1), 
					      z, cptr[i]);
				    }
			    }
			}
			UNPROTECT(1);
		    } else {
			PROTECT(s = allocVector(CXXRCONSTRUCT(SEXPTYPE, type), n));
			char **cptr = static_cast<char**>( p);
			for (R_xlen_t i = 0 ; i < n ; i++)
			    SET_STRING_ELT(s, i, mkChar(cptr[i]));
			UNPROTECT(1);
		    }
		    break;
		default:
		    break;
		}
		if (s != arg) {
		    PROTECT(s);
		    DUPLICATE_ATTRIB(s, arg);
		    SET_VECTOR_ELT(ans, na, s);
		    UNPROTECT(1);
		}
	    }
	}
    }
    UNPROTECT(1);
    vmaxset(vmax);
    return ans;
}

#ifndef NO_CALL_R
static const struct {
    const char *name;
    const SEXPTYPE type;
}

typeinfo[] = {
    {"logical",	  LGLSXP },
    {"integer",	  INTSXP },
    {"double",	  REALSXP},
    {"complex",	  CPLXSXP},
    {"character", STRSXP },
    {"list",	  VECSXP },
    {nullptr,	  NILSXP }
};

static SEXPTYPE string2type(char *s)
{
    int i;
    for (i = 0 ; typeinfo[i].name ; i++) {
	if(!strcmp(typeinfo[i].name, s)) {
	    return typeinfo[i].type;
	}
    }
    error(_("type \"%s\" not supported in interlanguage calls"), s);
    return NILSXP; /* for -Wall */
}

/* This is entirely legacy, with no known users (Mar 2012).
   So we freeze the code involved. 
 */

static void *RObjToCPtr2(SEXP s)
{
    int n;

    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	{
	    n = LENGTH(s);
	    int *iptr = INTEGER(s);
	    iptr = static_cast<int*>( CXXR_alloc(n, sizeof(int)));
	    for (int i = 0 ; i < n ; i++) iptr[i] = INTEGER(s)[i];
	    return CXXRNOCAST(void*) iptr;
	}
	break;
    case REALSXP:
	{
	    n = LENGTH(s);
	    double *rptr = REAL(s);
	    rptr = static_cast<double*>( CXXR_alloc(n, sizeof(double)));
	    for (int i = 0 ; i < n ; i++) rptr[i] = REAL(s)[i];
	    return CXXRNOCAST(void*) rptr;
	}
	break;
    case CPLXSXP:
	{
	    n = LENGTH(s);
	    Rcomplex *zptr = COMPLEX(s);
	    zptr = static_cast<Rcomplex*>( CXXR_alloc(n, sizeof(Rcomplex)));
	    for (int i = 0 ; i < n ; i++) zptr[i] = COMPLEX(s)[i];
	    return CXXRNOCAST(void*) zptr;
	}
	break;
    case STRSXP:
	{
	    n = LENGTH(s);
	    char **cptr = static_cast<char**>( CXXR_alloc(n, sizeof(char*)));
	    for (int i = 0 ; i < n ; i++) {
		const char *ss = translateChar(STRING_ELT(s, i));
		cptr[i] = CXXRNOCAST(char*) R_alloc(strlen(ss) + 1, sizeof(char));
		strcpy(cptr[i], ss);
	    }
	    return CXXRNOCAST(void*) cptr;
	}
	break;
	/* From here down, probably not right */
    case VECSXP:
	{
	    n = length(s);
	    SEXP *lptr = static_cast<SEXP *>( CXXR_alloc(n, sizeof(SEXP)));
	    for (int i = 0 ; i < n ; i++) lptr[i] = VECTOR_ELT(s, i);
	    return CXXRNOCAST(void*) lptr;
	}
	break;
    default:
	return CXXRNOCAST(void*) s;
    }
    return nullptr;  // -Wall
}



void call_R(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    SEXP call, pcall, s;
    SEXPTYPE type;
    int i, j, n;

    if (!isFunction(reinterpret_cast<SEXP>(func)))
	error("invalid function in call_R");
    if (nargs < 0)
	error("invalid argument count in call_R");
    if (nres < 0)
	error("invalid return value count in call_R");
    GCStackRoot<PairList> tl(PairList::make(nargs));
    PROTECT(pcall = call = new Expression(nullptr, tl));
    SETCAR(pcall, reinterpret_cast<SEXP>(func));
    s = R_NilValue;		/* -Wall */
    for (i = 0 ; i < nargs ; i++) {
	pcall = CDR(pcall);
	type = string2type(modes[i]);
	switch(type) {
	case LGLSXP:
	case INTSXP:
	    n = int( lengths[i]);
	    SETCAR(pcall, allocVector(type, n));
	    memcpy(INTEGER(CAR(pcall)), arguments[i], n * sizeof(int));
	    break;
	case REALSXP:
	    n = int( lengths[i]);
	    SETCAR(pcall, allocVector(REALSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(double));
	    break;
	case CPLXSXP:
	    n = int( lengths[i]);
	    SETCAR(pcall, allocVector(CPLXSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(Rcomplex));
	    break;
	case STRSXP:
	    n = int( lengths[i]);
	    SETCAR(pcall, allocVector(STRSXP, n));
	    for (j = 0 ; j < n ; j++) {
		char *str = static_cast<char*>((arguments[i]));
		SET_STRING_ELT(CAR(pcall), i, mkChar(str));
	    }
	    break;
	default:
	    error(_("mode '%s' is not supported in call_R"), modes[i]);
	}
	if(names && names[i])
	    SET_TAG(pcall, install(names[i]));
	SET_NAMED(CAR(pcall), 2);
    }
    PROTECT(s = eval(call, R_GlobalEnv));
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	if(nres > 0)
	    results[0] = static_cast<char *>( RObjToCPtr2(s));
	break;
    case VECSXP:
	n = length(s);
	if (nres < n) n = int( nres);
	for (i = 0 ; i < n ; i++)
	    results[i] = static_cast<char *>( RObjToCPtr2(VECTOR_ELT(s, i)));
	break;
    case LISTSXP:
	n = length(s);
	if(nres < n) n = int( nres);
	for(i = 0 ; i < n ; i++) {
	    results[i] = static_cast<char *>( RObjToCPtr2(s));
	    s = CDR(s);
	}
	break;
    default:  // -Wswitch
	break;
    }
    UNPROTECT(2);
    return;
}

void call_S(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    call_R(func, nargs, arguments, modes, lengths, names, nres, results);
}
#endif
