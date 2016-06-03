/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2016  The R Core Team
 *  Copyright (C) 2003--2016  The R Foundation
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

#include <iostream>

#include "rho/Evaluator_Context.hpp"
#include "rho/GCManager.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/LogicalVector.hpp"

#include <Defn.h>
#include <Internal.h>

#include <Print.h>
#include "arithmetic.h" /* for do_math[1234], do_cmathfuns */

#include <Rinterface.h>

using namespace rho;

/* Table of  .Internal(.) and .Primitive(.)  R functions
 * =====     =========	      ==========
 *
 * Each entry is a line with
 *
 *  printname	c-entry	 offset	 eval	arity	  pp-kind   precedence	    rightassoc
 *  ---------	-------	 ------	 ----	-----	  -------   ----------	    ----------
 *2 name	cfun	 code	 eval	arity	  gram.kind gram.precedence gram.rightassoc
 *3 PRIMNAME	PRIMFUN	 PRIMVAL [*]    PRIMARITY PPINFO    PPINFO	    PPINFO
 *
 * where "2" are the component names of the FUNTAB struct (Defn.h)
 * and	 "3" are the accessor macros. [*]: PRIMPRINT(.) uses the eval component
 *
 * printname:	The function name in R
 *
 * c-entry:	The name of the corresponding C function,
 *		actually declared in ../include/Internal.h .
 *		Convention:
 *		 - all start with "do_",
 *		 - all return SEXP.
 *		 - all have argument list
 *			 (SEXP call, SEXP op, SEXP args, SEXP env)
 *
 * quick_entry:  Alternative C function, where the args are passed in an array
 *               for efficiency.
 *               Signature is RObject*(*)(const Expression* call,
 *                                        const BuiltInFunction* op,
 *                                        Environment* env,
 *                                        int num_args,
 *                                        RObject** args,
 *                                        const PairList* tags)
 *
 * offset:	the 'op' (offset pointer) above; used for C functions
 *		which deal with more than one R function...
 *
 * eval:	= XYZ (three digits) --- where e.g. '1' means '001'
 *		X=1 says that we should force R_Visible off
 *		X=0 says that we should force R_Visible on
 *		X=2 says that we should switch R_Visible on but let the C
 *                  code update this.
 *		Y=1 says that this is an internal function which must
 *		    be accessed with a	.Internal(.) call, any other value is
 *		    accessible directly and printed in R as ".Primitive(..)".
 *		Z=1 says evaluate arguments before calling (BUILTINSXP) and
 *		Z=0 says don't evaluate (SPECIALSXP).
 *
 * arity:	How many arguments are required/allowed;  "-1"	meaning ``any''
 *
 * pp-kind:	Deparsing Info (-> PPkind in ../include/Defn.h )
 *
 * precedence: Operator precedence (-> PPprec in ../include/Defn.h )
 *
 * rightassoc: Right (1) or left (0) associative operator
 *
 */
const std::vector<BuiltInFunction*>&
BuiltInFunction::getFunctionTable() {
    using Dispatch = BuiltInFunction::DispatchType;

    static std::vector<BuiltInFunction*> s_function_table = {

/* printname	c-entry		quick-entry    offset	eval	arity	pp-kind	     precedence	rightassoc
 * ---------	-------		------	----	-----	-------      ----------	----------*/

/* Language Related Constructs */

/* Primitives */
new BuiltInFunction("if",		do_if,		0,	200,	-1,	{PP_IF,	     PREC_FN,	  1}),
new BuiltInFunction("while",	do_while,	0,	100,	2,	{PP_WHILE,   PREC_FN,	  0}),
new BuiltInFunction("for",		do_for,		0,	100,	3,	{PP_FOR,     PREC_FN,	  0}),
new BuiltInFunction("repeat",	do_repeat,	0,	100,	1,	{PP_REPEAT,  PREC_FN,	  0}),
new BuiltInFunction("break",	do_break,       0,	0,	0,	{PP_BREAK,   PREC_FN,	  0}),
new BuiltInFunction("next",	do_break,       1,	0,	0,	{PP_NEXT,    PREC_FN,	  0}),
new BuiltInFunction("return",	do_return,	0,	0,	-1,	{PP_RETURN,  PREC_FN,	  0}),
new BuiltInFunction("function",	do_function,	0,	0,	-1,	{PP_FUNCTION,PREC_FN,	  0}),
new BuiltInFunction("<-",		do_set,		1,	100,	-1,	{PP_ASSIGN,  PREC_LEFT,	  1}),
new BuiltInFunction("=",		do_set,		3,	100,	-1,	{PP_ASSIGN,  PREC_EQ,	  1}),
new BuiltInFunction("<<-",		do_set,		2,	100,	-1,	{PP_ASSIGN2, PREC_LEFT,	  1}),
new BuiltInFunction("{",		do_begin,	0,	200,	-1,	{PP_CURLY,   PREC_FN,	  0}),
new BuiltInFunction("(",		do_paren,	0,	1,	1,	{PP_PAREN,   PREC_FN,	  0}),
new BuiltInFunction(".subset",	do_subset_dflt,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".subset2",	do_subset2_dflt,2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("[",		do_subset,	1,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}),
new BuiltInFunction("[[",		do_subset2,	2,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}),
new BuiltInFunction("$",		do_subset3,	3,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}),
new BuiltInFunction("@",		do_AT,		0,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}),
new BuiltInFunction("[<-",		do_subassign,	0,	0,	-1,	{PP_SUBASS,  PREC_LEFT,	  1}),
new BuiltInFunction("[[<-",	do_subassign2,	1,	0,	-1,	{PP_SUBASS,  PREC_LEFT,	  1}),
new BuiltInFunction("$<-",		do_subassign3,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}),
new BuiltInFunction("switch",	do_switch,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	  0}, "EXPR"),
new BuiltInFunction("browser",	do_browser,	0,	101,	-1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".primTrace",	do_trace,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".primUntrace",do_trace,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".Internal",	do_internal,	0,	200,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".Primitive",	do_primitive,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("call",	do_call,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, "name"),
new BuiltInFunction("quote",	do_quote,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}, "expr"),
new BuiltInFunction("substitute",	do_substitute,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("missing",	do_missing,	1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("nargs",	do_nargs,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("on.exit",	do_onexit,	0,	100,	-1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("forceAndCall",do_forceAndCall,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	  0}),

/* .Internals */

new BuiltInFunction("stop",	do_stop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("warning",	do_warning,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("gettext",	do_gettext,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("ngettext",	do_ngettext,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("bindtextdomain",do_bindtextdomain,0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".addCondHands",do_addCondHands,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".signalCondition",do_signalCondition,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".dfltStop",do_dfltStop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".dfltWarn",do_dfltWarn,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".addRestart",do_addRestart,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".getRestart",do_getRestart,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction(".invokeRestart",do_invokeRestart,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("geterrmessage",do_geterrmessage, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("seterrmessage",do_seterrmessage, 0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("printDeferredWarnings",do_printDeferredWarnings, 0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("as.function.default",do_asfunction,0,	11,	2,	{PP_FUNCTION,PREC_FN,	  0}),
new BuiltInFunction("debug",	do_debug,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("undebug",	do_debug,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("isdebugged",	do_debug,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("debugonce",	do_debug,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("Recall",	do_recall,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("delayedAssign",do_delayed,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("makeLazy",	do_makelazy,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}),
new BuiltInFunction("identical",	do_identical,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	  0}),


/* Binary Operators, all primitives */
/* these are group generic and so need to eval args */
new BuiltInFunction("+",		do_arith,	PLUSOP,	1,	-1,	{PP_BINARY,  PREC_SUM,	  0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("-",		do_arith,	MINUSOP,1,	-1,	{PP_BINARY,  PREC_SUM,	  0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("*",		do_arith,	TIMESOP,1,	2,	{PP_BINARY,  PREC_PROD,	  0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("/",		do_arith,	DIVOP,	1,	2,	{PP_BINARY2, PREC_PROD,	  0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("^",		do_arith,	POWOP,	1,	2,	{PP_BINARY2, PREC_POWER,  1}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("%%",		do_arith,	MODOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("%/%",		do_arith,	IDIVOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("%*%",		do_matprod,	0,	1,	2,	{PP_BINARY,  PREC_PERCENT,0}),

new BuiltInFunction("==",		do_relop, EQOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("!=",		do_relop, NEOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("<",		do_relop, LTOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("<=",		do_relop, LEOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction(">=",		do_relop, GEOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction(">",		do_relop, GTOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("&",		do_logic,	1,	1,	-1,	{PP_BINARY,  PREC_AND,	  0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("|",		do_logic,	2,	1,	-1,	{PP_BINARY,  PREC_OR,	  0}, nullptr, Dispatch::GROUP_OPS),
new BuiltInFunction("!",		do_logic,	3,	1,	-1,	{PP_UNARY,   PREC_NOT,	  0}, nullptr, Dispatch::GROUP_OPS),

/* specials as conditionally evaluate second arg */
new BuiltInFunction("&&",		do_logic2,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}),
new BuiltInFunction("||",		do_logic2,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}),
new BuiltInFunction(":",		do_colon,	0,	1,	2,	{PP_BINARY2, PREC_COLON,  0}),
/* does not evaluate */
new BuiltInFunction("~",		do_tilde,	0,	0,	-1,	{PP_BINARY,  PREC_TILDE,  0}),


/* Logic Related Functions */
/* these are group generic and so need to eval args */
new BuiltInFunction("all",		do_logic3,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}, nullptr, Dispatch::GROUP_SUMMARY),
new BuiltInFunction("any",		do_logic3,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}, nullptr, Dispatch::GROUP_SUMMARY),


/* Vectors, Matrices and Arrays */

/* Primitives */

new BuiltInFunction("length",	do_length,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL),
new BuiltInFunction("length<-",	do_lengthgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL),
new BuiltInFunction("c",/* bind.c:*/do_c,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("oldClass",	do_class,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("oldClass<-",	do_classgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT, 1}, "x"),
new BuiltInFunction("class",	R_do_data_class,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction(".cache_class",R_do_data_class,1,	1,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("class<-",	R_do_set_class,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("unclass",	do_unclass,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("names",	do_names,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("names<-",	do_namesgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL),
new BuiltInFunction("dimnames",	do_dimnames,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("dimnames<-",	do_dimnamesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL),
new BuiltInFunction("dim",		do_dim,		0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("dim<-",	do_dimgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, nullptr, Dispatch::INTERNAL),
new BuiltInFunction("attributes",	do_attributes,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("attributes<-",do_attributesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x"),
new BuiltInFunction("attr",	do_attr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("attr<-",	do_attrgets,	0,	1,	3,	{PP_FUNCALL, PREC_LEFT,	1}),
new BuiltInFunction("@<-",		do_slotgets,	0,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}),
new BuiltInFunction("levels<-",	do_levelsgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL),

/* .Internals */

new BuiltInFunction("vector",	do_makevector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("complex",	do_complex,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("matrix",	do_matrix,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("array",	do_array,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("diag",	do_diag,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("backsolve",	do_backsolve,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("max.col",	do_maxcol,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("row",		do_rowscols,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("col",		do_rowscols,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("unlist",	do_unlist,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL),
new BuiltInFunction("cbind",	do_bind,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rbind",	do_bind,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("drop",	do_drop,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("all.names",	do_allnames,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("comment",	do_comment,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("comment<-",	do_commentgets,	0,	11,	2,	{PP_FUNCALL, PREC_LEFT,	1}),
new BuiltInFunction("get",		do_get,		1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("get0",	do_get,		2,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("mget",	do_mget,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("exists",	do_get,		0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("assign",	do_assign,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("list2env",	do_list2env,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("remove",	do_remove,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("duplicated",	do_duplicated,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("unique",	do_duplicated,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("anyDuplicated",do_duplicated,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("anyNA",	do_anyNA,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("which",	do_which,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("which.min",	do_first_min,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pmin",	do_pmin,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pmax",	do_pmin,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("which.max",	do_first_min,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("match",	do_match,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pmatch",	do_pmatch,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("charmatch",	do_charmatch,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("match.call",	do_matchcall,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("crossprod",	do_crossprod,	1,	11,	2,	{PP_FUNCALL, PREC_FN,   0}),
new BuiltInFunction("tcrossprod",	do_crossprod,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("lengths",	do_lengths,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL),

new BuiltInFunction("attach",	do_attach,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("detach",	do_detach,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("search",	do_search,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("setFileTime",	do_setFileTime,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}),


/* Mathematical Functions */
/* primitives: these are group generic and so need to eval args (possibly internally) */
new BuiltInFunction("round",	do_Math2,	10001,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),  // Special
new BuiltInFunction("signif",	do_Math2,	10004,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),  // Special
new BuiltInFunction("log",		do_log,		10003,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),  // Special
new BuiltInFunction("log10",	do_log1arg,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("log2",	do_log1arg,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("abs",		do_abs,		6,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("floor",	do_math1,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("ceiling",	do_math1,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("sqrt",	do_math1,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("sign",	do_math1,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("trunc",	do_trunc,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),

new BuiltInFunction("exp",		do_math1,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("expm1",	do_math1,	11,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("log1p",	do_math1,	12,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),

new BuiltInFunction("cos",		do_math1,	20,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("sin",		do_math1,	21,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("tan",		do_math1,	22,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("acos",	do_math1,	23,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("asin",	do_math1,	24,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("atan",	do_math1,	25,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),

new BuiltInFunction("cosh",	do_math1,	30,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("sinh",	do_math1,	31,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("tanh",	do_math1,	32,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("acosh",	do_math1,	33,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("asinh",	do_math1,	34,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("atanh",	do_math1,	35,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),

new BuiltInFunction("lgamma",	do_math1,	40,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("gamma",	do_math1,	41,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),

new BuiltInFunction("digamma",	do_math1,	42,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("trigamma",	do_math1,	43,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
/* see "psigamma" below !*/

new BuiltInFunction("cospi",	do_math1,	47,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("sinpi",	do_math1,	48,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),
new BuiltInFunction("tanpi",	do_math1,	49,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH),

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

new BuiltInFunction("atan2",	do_math2,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("lbeta",	do_math2,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("beta",	do_math2,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("lchoose",	do_math2,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("choose",	do_math2,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),

/*
  This is most of the do_math[23]
*/
new BuiltInFunction("besselJ",	do_math2,	24,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("besselY",	do_math2,	25,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("psigamma",	do_math2,	26,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),


/* Mathematical Functions of a Complex Argument:
 * These are group generic and so need to eval args --> ./complex.c */

new BuiltInFunction("Re",		do_cmathfuns,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"),
new BuiltInFunction("Im",		do_cmathfuns,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"),
new BuiltInFunction("Mod",		do_cmathfuns,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"),
new BuiltInFunction("Arg",		do_cmathfuns,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"),
new BuiltInFunction("Conj",	do_cmathfuns,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"),


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

new BuiltInFunction("besselI",	do_math3,	43,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("besselK",	do_math3,	44,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),

/* Random Numbers */

new BuiltInFunction("sample",	do_sample,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sample2",	do_sample2,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("RNGkind",	do_RNGkind,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("set.seed",	do_setseed,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),

/* Data Summaries */
/* these four are group generic and so need to eval args */
new BuiltInFunction("sum",		do_summary,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY),
new BuiltInFunction("min",		do_summary,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY),
new BuiltInFunction("max",		do_summary,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY),
new BuiltInFunction("prod",	do_summary,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY),

new BuiltInFunction("mean",	do_summary,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY),
new BuiltInFunction("range",	do_range,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY),

/* Note that the number of arguments in this group only applies
   to the default method */
new BuiltInFunction("cumsum",	do_cum,		1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),
new BuiltInFunction("cumprod",	do_cum,		2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),
new BuiltInFunction("cummax",	do_cum,		3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),
new BuiltInFunction("cummin",	do_cum,		4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH),

/* Type coercion */

new BuiltInFunction("as.character",do_asatomic,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("as.integer",	do_asatomic,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("as.double",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("as.numeric",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("as.complex",	do_asatomic,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("as.logical",	do_asatomic,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("as.raw",	do_asatomic,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("as.call",	do_ascall,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("as.environment",do_as_environment,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "object", Dispatch::INTERNAL),
new BuiltInFunction("storage.mode<-",do_storage_mode,0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("asCharacterFactor",	do_asCharacterFactor,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("as.vector",	do_asvector,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL),
new BuiltInFunction("paste",	do_paste,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("paste0",	do_paste,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.path",	do_filepath,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("format",	do_format,	0,	11,	9,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("format.info",	do_formatinfo,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("cat",		do_cat,		0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("do.call",	do_docall,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}),

/* String Manipulation */

new BuiltInFunction("nchar",	do_nchar,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("nzchar",	do_nzchar,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("substr",	do_substr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("startsWith",	do_startsWith,  0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("endsWith",	do_startsWith,  1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("substr<-",	do_substrgets,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("strsplit",	do_strsplit,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("abbreviate",	do_abbrev,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("make.names",	do_makenames,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pcre_config", do_pcre_config,	1,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("grep",	do_grep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("grepl",	do_grep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("grepRaw",	do_grepraw,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sub",		do_gsub,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gsub",	do_gsub,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("regexpr",	do_regexpr,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gregexpr",	do_regexpr,	1,	11,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("regexec",	do_regexec,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("agrep",	do_agrep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("agrepl",	do_agrep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("adist",	do_adist,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("aregexec",	do_aregexec,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("tolower",	do_tolower,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("toupper",	do_tolower,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("chartr",	do_chartr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sprintf",	do_sprintf,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("make.unique",	do_makeunique,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("charToRaw",	do_charToRaw,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rawToChar",	do_rawToChar,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rawShift",	do_rawShift,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("intToBits",	do_intToBits,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rawToBits",	do_rawToBits,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("packBits",	do_packBits,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("utf8ToInt",	do_utf8ToInt,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("intToUtf8",	do_intToUtf8,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("validUTF8",	do_validUTF8,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("validEnc",	do_validEnc,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("encodeString",do_encodeString,1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("iconv",	do_iconv,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("strtrim",	do_strtrim,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("strtoi",	do_strtoi,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("strrep",	do_strrep,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),

/* Type Checking (typically implemented in ./coerce.c ) */

new BuiltInFunction("is.null",	do_is,		NILSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.logical",	do_is,		LGLSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.integer",	do_is,		INTSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.double",	do_is,		REALSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.complex",	do_is,		CPLXSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.character",do_is,		STRSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.symbol",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.name",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.environment",do_is,	ENVSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.list",	do_is,		VECSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.pairlist",	do_is,		LISTSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.expression",do_is,		EXPRSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.raw",	do_is,		RAWSXP, 1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("is.object",	do_is,		50,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("isS4",	do_is,		51,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("is.numeric",	do_is,		100,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("is.matrix",	do_is,		101,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("is.array",	do_is,		102,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),

new BuiltInFunction("is.atomic",	do_is,		200,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.recursive",do_is,		201,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("is.call",	do_is,		300,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.language",	do_is,		301,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("is.function",	do_is,		302,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("is.single",	do_is,		999,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("is.na",	do_isna,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("is.nan",	do_isnan,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("is.finite",	do_isfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),
new BuiltInFunction("is.infinite",	do_isinfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL),

new BuiltInFunction("is.vector",	do_isvector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),

/* Miscellaneous */

/* Primitive */
new BuiltInFunction("proc.time",	do_proctime,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gc.time",	do_gctime,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("withVisible", do_withVisible,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("expression",	do_expression,	1,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("interactive",	do_interactive,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("invisible",	do_invisible,	0,	101,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rep",		do_rep,		0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rep.int",	do_rep_int,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rep_len",	do_rep_len,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("seq.int",	do_seq,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("seq_len",	do_seq_len,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "length.out"),
new BuiltInFunction("seq_along",	do_seq_along,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "along.with"),
new BuiltInFunction("list",	do_makelist,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("xtfrm",	do_xtfrm,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("enc2native",	do_enc2,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("enc2utf8",	do_enc2,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("emptyenv",	do_emptyenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("baseenv",	do_baseenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("globalenv",	do_globalenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("environment<-",do_envirgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x"),
new BuiltInFunction("pos.to.env",	do_pos2env,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"),

new BuiltInFunction("eapply",	do_eapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("lapply",	do_lapply,	0,	10,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("vapply",	do_vapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("mapply",	do_mapply,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction(".C",		do_dotCode,	0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction(".Fortran",	do_dotCode,	1,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction(".External",   do_External,    0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction(".External2",   do_External,   1,    201,      -1,     {PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction(".Call",       do_dotcall,     0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction(".External.graphics", do_Externalgr, 0, 1,	-1,	{PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction(".Call.graphics", do_dotcallgr, 0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}),

/* .Internal */
new BuiltInFunction("Version",	do_version,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("commandArgs", do_commandArgs, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
#ifdef Win32
new BuiltInFunction("system",	do_system,	0,	211,	5,	{PP_FUNCALL, PREC_FN,	0}),
#else
new BuiltInFunction("system",	do_system,	0,	211,	2,	{PP_FUNCALL, PREC_FN,	0}),
#endif

#ifdef Win32
new BuiltInFunction("shell.exec",	do_shellexec,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.which",	do_syswhich,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("mkjunction", do_mkjunction,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("tzone_name", do_tzone_name,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
#endif
new BuiltInFunction("parse",	do_parse,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}),
//new BuiltInFunction("parse_Rd",	do_parseRd,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}),
//new BuiltInFunction("deparseRd",	do_deparseRd,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
//new BuiltInFunction("parseLatex",  do_parseLatex,  0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("save",	do_save,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("saveToConn",	do_saveToConn,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("load",	do_load,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("loadFromConn2",do_loadFromConn2,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("serializeToConn",	do_serializeToConn,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("unserializeFromConn",	do_unserializeFromConn,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("deparse",	do_deparse,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("dput",	do_dput,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("dump",	do_dump,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("quit",	do_quit,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("readline",	do_readln,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("print.default",do_printdefault,0,	111,	9,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("print.function",do_printfunction,0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("prmatrix",	do_prmatrix,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gc",		do_gc,		0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gcinfo",	do_gcinfo,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gctorture",	do_gctorture,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gctorture2",	do_gctorture2,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("memory.profile",do_memoryprofile, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("split",	do_split,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("is.loaded",	do_isloaded,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction("recordGraphics", do_recordGraphics, 0, 211,     3,      {PP_FOREIGN, PREC_FN,	0}),
new BuiltInFunction("dyn.load",	do_dynload,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("dyn.unload",	do_dynunload,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("ls",		do_ls,		1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("typeof",	do_typeof,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("eval",	do_eval,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}),
//new BuiltInFunction("returnValue",   do_returnValue,0,     11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.parent",	do_sys,		1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.call",	do_sys,		2,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.frame",	do_sys,		3,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.nframe",	do_sys,		4,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.calls",	do_sys,		5,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.frames",	do_sys,		6,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.on.exit",	do_sys,		7,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.parents",	do_sys,		8,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sys.function",do_sys,		9,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("traceback",	do_traceback,	0,      11,     1,      {PP_FUNCALL, PREC_FN,   0}),
new BuiltInFunction("browserText", do_sysbrowser,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("browserCondition", do_sysbrowser,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("browserSetDebug", do_sysbrowser,	3,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("parent.frame",do_parentframe,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sort",	do_sort,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("is.unsorted",	do_isunsorted,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL),
new BuiltInFunction("psort",	do_psort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("qsort",	do_qsort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("radixsort",	do_radixsort,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("order",	do_order,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rank",	do_rank,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("scan",	do_scan,	0,	11,	19,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("t.default",	do_transpose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("aperm",	do_aperm,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("builtins",	do_builtins,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("args",	do_args,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("formals",	do_formals,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("body",	do_body,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("environment",	do_envir,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("environmentName",do_envirName,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("env2list",	do_env2list,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("reg.finalizer",do_regFinaliz,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("options",	do_options,	0,	211,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getOption",	do_getOption,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sink",	do_sink,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sink.number",	do_sinknumber,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rapply",	do_rapply,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("islistfactor",do_islistfactor,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("colSums",	do_colsum,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("colMeans",	do_colsum,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rowSums",	do_colsum,	2,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rowMeans",	do_colsum,	3,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("tracemem",    do_tracemem,    0,      1,	1,      {PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("retracemem",  do_retracemem,  0,      201,     -1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("untracemem",  do_untracemem,  0,      101,	1,      {PP_FUNCALL, PREC_FN,	0}, "x"),
new BuiltInFunction("inspect",	do_inspect,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("address",     do_address,     0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}),
new BuiltInFunction("refcnt",      do_refcnt,      0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}),
new BuiltInFunction("merge",	do_merge,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("capabilities",do_capabilities,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("capabilitiesX11",do_capabilitiesX11,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("new.env",	do_newenv,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("parent.env",  do_parentenv,   0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("parent.env<-",do_parentenvgets, 0,	11,     2,      {PP_FUNCALL, PREC_LEFT,	1}),
new BuiltInFunction("topenv",	do_topenv,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("l10n_info",	do_l10n_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Cstack_info", do_Cstack_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),

/* Functions To Interact with the Operating System */

new BuiltInFunction("file.show",	do_fileshow,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.create",	do_filecreate,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.remove",	do_fileremove,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.rename",	do_filerename,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.append",	do_fileappend,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.symlink",do_filesymlink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.link",	do_filelink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.copy",	do_filecopy,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("list.files",	do_listfiles,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("list.dirs",	do_listdirs,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.exists", do_fileexists,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.choose", do_filechoose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.info",	do_fileinfo,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file.access",	do_fileaccess,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("dir.exists",	do_direxists,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("dir.create",	do_dircreate,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("tempfile",	do_tempfile,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("tempdir",	do_tempdir,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("R.home",	do_Rhome,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("date",	do_date,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.getenv",	do_getenv,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.setenv",	do_setenv,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.unsetenv",do_unsetenv,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getwd",	do_getwd,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("setwd",	do_setwd,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("basename",	do_basename,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("dirname",	do_dirname,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.chmod",	do_syschmod,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.umask",	do_sysumask,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.readlink", do_readlink,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.info",	do_sysinfo,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.sleep",	do_syssleep,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.getlocale",do_getlocale,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.setlocale",do_setlocale,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.localeconv",do_localeconv,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("path.expand",	do_pathexpand,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.getpid",	do_sysgetpid,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("normalizePath",do_normalizepath,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Sys.glob",	do_glob,	0,      11,	2,      {PP_FUNCALL, PREC_FN,   0}),
new BuiltInFunction("unlink",	do_unlink,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),

/* Complex Valued Functions */
new BuiltInFunction("polyroot",	do_polyroot,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),

/* Objects */
new BuiltInFunction("inherits",	do_inherits,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("UseMethod",	do_usemethod,	0,     200,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("NextMethod",	do_nextmethod,	0,     210,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("standardGeneric",do_standardGeneric,0, 201,	-1,	{PP_FUNCALL, PREC_FN,	0}),

/* date-time manipulations */
new BuiltInFunction("Sys.time",	do_systime,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("as.POSIXct",	do_asPOSIXct,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("as.POSIXlt",	do_asPOSIXlt,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("format.POSIXlt",do_formatPOSIXlt,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("strptime",	do_strptime,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("Date2POSIXlt",do_D2POSIXlt,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("POSIXlt2Date",do_POSIXlt2D,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),

// new BuiltInFunction("mkCode",     do_mkcode,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("bcClose",    do_bcclose,      0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("is.builtin.internal",    do_is_builtin_internal,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("disassemble", do_disassemble, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("bcVersion", do_bcversion,     0,      11,     0,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("load.from.file", do_loadfile, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("save.to.file", do_savefile,   0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("growconst", do_growconst,     0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("putconst", do_putconst,       0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("getconst", do_getconst,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("enableJIT",    do_enablejit,  0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),
// new BuiltInFunction("compilePKGS", do_compilepkgs, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),

new BuiltInFunction("setNumMathThreads", do_setnumthreads,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),
new BuiltInFunction("setMaxNumMathThreads", do_setmaxnumthreads,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}),

/* Connections */
new BuiltInFunction("stdin",	do_stdin,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("stdout",	do_stdout,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("stderr",	do_stderr,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("isatty",	do_isatty,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("readLines",	do_readLines,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("writeLines",	do_writelines,	0,      111,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("readBin",	do_readbin,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("writeBin",	do_writebin,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("readChar",	do_readchar,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("writeChar",	do_writechar,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("open",	do_open,	0,      111,     3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("isOpen",	do_isopen,	0,      11,     2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("isIncomplete",do_isincomplete,0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("isSeekable",	do_isseekable,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("close",	do_close,	0,      111,     2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("flush",	do_flush,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("file",	do_url,		1,      11,     6,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("url",		do_url,		0,      11,     5,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pipe",	do_pipe,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("fifo",	do_fifo,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gzfile",	do_gzfile,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bzfile",	do_gzfile,	1,      11,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("xzfile",	do_gzfile,	2,      11,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("unz",         do_unz,		0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("seek",	do_seek,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("truncate",	do_truncate,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pushBack",	do_pushback,	0,     111,     4,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("clearPushBack",do_clearpushback,0,   111,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pushBackLength",do_pushbacklength,0,  11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rawConnection",do_rawconnection,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rawConnectionValue",do_rawconvalue,0, 11,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("textConnection",do_textconnection,0,	11,     5,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("textConnectionValue",do_textconvalue,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("socketConnection",do_sockconn,0,	11,     7,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("sockSelect",do_sockselect,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getConnection",do_getconnection,0,	11,	1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getAllConnections",do_getallconnections,0,11, 0,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("summary.connection",do_sumconnection,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("gzcon",	do_gzcon,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("memCompress",do_memCompress,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("memDecompress",do_memDecompress,0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}),

/* Provenance Functions */
new BuiltInFunction("provenance", do_provenance,   0,       0,     1,      {PP_FUNCALL, PREC_FN, 0}),
new BuiltInFunction("provenance.graph", do_provenance_graph,0,11,  1,      {PP_FUNCALL, PREC_FN, 0}),

new BuiltInFunction("readDCF",	do_readDCF,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}),


new BuiltInFunction("lockEnvironment", do_lockEnv,		0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("environmentIsLocked",	do_envIsLocked,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("lockBinding", do_lockBnd,		0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("unlockBinding", do_lockBnd,		1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bindingIsLocked", do_bndIsLocked,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("makeActiveBinding", do_mkActiveBnd,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bindingIsActive", do_bndIsActive,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("isNamespaceEnv",do_isNSEnv,		0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("registerNamespace",do_regNS,		0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("unregisterNamespace",do_unregNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getRegisteredNamespace",do_getRegNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("isRegisteredNamespace", do_getRegNS,	1, 11,  1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getNamespaceRegistry",do_getNSRegistry, 0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("importIntoEnv",do_importIntoEnv, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("env.profile",  do_envprofile,    0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("Encoding",	do_encoding,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("setEncoding",	do_setencoding,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("setTimeLimit",do_setTimeLimit,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("setSessionTimeLimit",do_setSessionTimeLimit,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("icuSetCollate",do_ICUset,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("icuGetCollate",do_ICUget,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("readRenviron",do_readEnviron,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("shortRowNames",do_shortRowNames,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("copyDFattr",do_copyDFattr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getRegisteredRoutines",do_getRegisteredRoutines,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getLoadedDLLs",do_getDllTable,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getSymbolInfo",do_getSymbolInfo,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction(".isMethodsDispatchOn",do_S4on,0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("lazyLoadDBfetch",do_lazyLoadDBfetch,0,1,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("lazyLoadDBflush",do_lazyLoadDBflush,0,11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("getVarsFromFrame",do_getVarsFromFrame, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("lazyLoadDBinsertValue",do_lazyLoadDBinsertValue, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bincode",	do_bincode,	 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("tabulate",	do_tabulate,	 0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("findInterval",do_findinterval, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("pretty",	do_pretty,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("formatC",	do_formatC,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bitwiseAnd",	do_bitwise,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bitwiseNot",	do_bitwise,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bitwiseOr",	do_bitwise,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bitwiseXor",	do_bitwise,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bitwiseShiftL", do_bitwise,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("bitwiseShiftR",  do_bitwise,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("serialize",	do_serialize,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("serializeb",	do_serialize,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("unserialize",	do_serialize,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rowsum_matrix",do_rowsum,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("rowsum_df",	do_rowsum,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("setS4Object",	do_setS4Object, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("traceOnOff",	do_traceOnOff,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("debugOnOff",	do_traceOnOff,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("La_qr_cmplx",	do_lapack,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_rs",	do_lapack,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_rs_cmplx",do_lapack,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_rg",	do_lapack,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_rg_cmplx",do_lapack,	41,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
// new BuiltInFunction("La_rs",	do_lapack,     	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
// new BuiltInFunction("La_rs_cmplx",	do_lapack,     	51,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_dlange",	do_lapack,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_dgecon",	do_lapack,	7,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_dtrcon",	do_lapack,	8,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_zgecon",	do_lapack,	9,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_ztrcon",	do_lapack,	10,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_solve_cmplx",do_lapack,    11,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_solve",	do_lapack,	100,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_qr",	do_lapack,	101,	11,	1,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_chol",	do_lapack,	200,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_chol2inv",	do_lapack,	201,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("qr_coef_real",do_lapack,	300,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("qr_qy_real",	do_lapack,	301,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("det_ge_real",	do_lapack,	302,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("qr_coef_cmplx",do_lapack,	303,	11,	2,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("qr_qy_cmplx",	do_lapack,	304,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("La_svd",	do_lapack,	400,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_svd_cmplx",do_lapack,	401,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("La_version",	do_lapack,	1000,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),

// new BuiltInFunction("bcprofcounts",do_bcprofcounts,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
// new BuiltInFunction("bcprofstart",	do_bcprofstart,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
// new BuiltInFunction("bcprofstop",	do_bcprofstop,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),

new BuiltInFunction("eSoftVersion",do_eSoftVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("curlVersion", do_curlVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("curlGetHeaders",do_curlGetHeaders,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}),
new BuiltInFunction("curlDownload",do_curlDownload, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}),
};
    return s_function_table;
}

/* Table of special names.  These are marked as special with
   SET_SPECIAL_SYMBOL.  Environments on the function call stack that
   have never contained such a symbol are marked as such, so they can
   be quickly skipped when searching for a function named by such a
   special symbol.

   Any symbols can be put here, but ones that contain special
   characters, or are reserved words, are the ones unlikely to be
   defined in any environment other than base, and hence the ones
   where this is most likely to help. */

const char *Symbol::s_special_symbol_names[] = {
    "if", "while", "repeat", "for", "break", "next", "return", "function",
    "(", "{",
    "+", "-", "*", "/", "^", "%%", "%/%", "%*%", ":",
    "==", "!=", "<", ">", "<=", ">=",
    "&", "|", "&&", "||", "!",
    "<-", "<<-", "=",
    "$", "[", "[[",
    "$<-", "[<-", "[[<-",
    0
};

std::pair<BuiltInFunction::map*, BuiltInFunction::map*>
BuiltInFunction::createLookupTables()
{
    GCManager::GCInhibitor no_gc;
    map* primitive_function_cache = new map();
    map* internal_function_cache = new map();

    for (BuiltInFunction* function : getFunctionTable()) {
	Symbol* sym = Symbol::obtain(function->name());
	if (function->viaDotInternal()) {
	    (*internal_function_cache)[sym] = function;
	}
	else {
	    (*primitive_function_cache)[sym] = function;
	}
    }
    return std::make_pair(primitive_function_cache, internal_function_cache);
}

SEXP attribute_hidden R_Primitive(const char *primname)
{
    return BuiltInFunction::obtainPrimitive(primname);
}

SEXP attribute_hidden do_primitive(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* name_)
{
    SEXP name, prim;
    name = name_;
    if (!isString(name) || Rf_length(name) != 1 ||
	STRING_ELT(name, 0) == R_NilValue)
	errorcall(call, _("string argument required"));
    prim = R_Primitive(CHAR(STRING_ELT(name, 0)));
    if (prim == R_NilValue)
	errorcall(call, _("no such primitive function"));
    return prim;
}

/* initialize the symbol table */
void attribute_hidden Rf_InitNames()
{
    // Logical constants.
    Logical::initialize();

    /* String constants (CHARSXP values) */
    String::initialize();
    Symbol::initialize();
    R_print.na_string = NA_STRING;
}

	/* Internal code for the ~ operator */

SEXP attribute_hidden do_tilde(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (isObject(call))
	return duplicate(call);
    else {
	SEXP klass;
	PROTECT(call = duplicate(call));
	PROTECT(klass = mkString("formula"));
	setAttrib(call, R_ClassSymbol, klass);
	setAttrib(call, R_DotEnvSymbol, rho);
	UNPROTECT(2);
	return call;
    }
}

/* For use in packages */
extern "C"
const char *getPRIMNAME(SEXP object)
{
    return PRIMNAME(object);
}

// TODO: move to Symbol.h
extern "C"
SEXP Rf_installChar(SEXP charSXP)
{
    String* name = SEXP_downcast<String*>(charSXP);
    return Symbol::obtain(name);
}

extern "C"
attribute_hidden
SEXP Rf_installS3Signature(const char *methodName, const char *className) {
    return Symbol::obtainS3Signature(methodName, className);
}
