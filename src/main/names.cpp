/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2013  The R Core Team
 *  Copyright (C) 2003, 2004  The R Foundation
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
#include <config.h>
#endif

#include <iostream>

#include "CXXR/Evaluator_Context.hpp"
#include "CXXR/GCStackRoot.hpp"

#include <Defn.h>
#include <Internal.h>

#include <Print.h>
#include "arithmetic.h" /* for do_math[1234], do_cmathfuns */

#include <Rinterface.h>

using namespace CXXR;

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
BuiltInFunction::TableEntry BuiltInFunction::s_function_table[] = {

/* printname	c-entry		quick-entry    offset	eval	arity	pp-kind	     precedence	rightassoc
 * ---------	-------		------	----	-----	-------      ----------	----------*/

/* Language Related Constructs */

/* Primitives */
{"if",		do_if,		nullptr,	0,	200,	-1,	{PP_IF,	     PREC_FN,	  1}},
{"while",	do_while,	nullptr,	0,	100,	-1,	{PP_WHILE,   PREC_FN,	  0}},
{"for",		do_for,		nullptr,	0,	100,	-1,	{PP_FOR,     PREC_FN,	  0}},
{"repeat",	do_repeat,	nullptr,	0,	100,	-1,	{PP_REPEAT,  PREC_FN,	  0}},
{"break",	do_break,       nullptr,	2,	0,	-1,	{PP_BREAK,   PREC_FN,	  0}},
{"next",	do_break,       nullptr,	1,	0,	-1,	{PP_NEXT,    PREC_FN,	  0}},
{"return",	do_return,	nullptr,	0,	0,	-1,	{PP_RETURN,  PREC_FN,	  0}},
{"function",	do_function,	nullptr,	0,	0,	-1,	{PP_FUNCTION,PREC_FN,	  0}},
{"<-",		do_set,		nullptr,	1,	100,	-1,	{PP_ASSIGN,  PREC_LEFT,	  1}},
{"=",		do_set,		nullptr,	3,	100,	-1,	{PP_ASSIGN,  PREC_EQ,	  1}},
{"<<-",		do_set,		nullptr,	2,	100,	-1,	{PP_ASSIGN2, PREC_LEFT,	  1}},
{"{",		do_begin,	nullptr,	0,	200,	-1,	{PP_CURLY,   PREC_FN,	  0}},
{"(",		do_paren,	nullptr,	0,	1,	1,	{PP_PAREN,   PREC_FN,	  0}},
{".subset",	do_subset_dflt,	nullptr,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt,nullptr,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"[",		do_subset,	nullptr,	1,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"[[",		do_subset2,	nullptr,	2,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"$",		do_subset3,	nullptr,	3,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"@",		do_AT,		nullptr,	0,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"[<-",		do_subassign,	nullptr,	0,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	nullptr,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	nullptr,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"switch",	do_switch,	nullptr,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"browser",	do_browser,	nullptr,	0,	101,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{".primTrace",	do_trace,	nullptr,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primUntrace",do_trace,	nullptr,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Internal",	do_internal,	nullptr,	0,	200,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Primitive",	do_primitive,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"call",	do_call,	nullptr,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"quote",	do_quote,	nullptr,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"substitute",	do_substitute,	nullptr,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"missing",	do_missing,	nullptr,	1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"nargs",	do_nargs,	nullptr,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"on.exit",	do_onexit,	nullptr,	0,	100,	1,	{PP_FUNCALL, PREC_FN,	  0}},

/* .Internals */

{"stop",	do_stop,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"warning",	do_warning,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"gettext",	do_gettext,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"ngettext",	do_ngettext,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"bindtextdomain",do_bindtextdomain,nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addCondHands",do_addCondHands,	nullptr,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{".resetCondHands",do_resetCondHands,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".signalCondition",do_signalCondition,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltStop",do_dfltStop,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltWarn",do_dfltWarn,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addRestart",do_addRestart,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".getRestart",do_getRestart,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".invokeRestart",do_invokeRestart,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
//{".addTryHandlers",do_addTryHandlers,	nullptr,	0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"geterrmessage",do_geterrmessage, nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"seterrmessage",do_seterrmessage, nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"printDeferredWarnings",do_printDeferredWarnings, nullptr,	0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"interruptsSuspended",do_interruptsSuspended, nullptr,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
//{"restart",	do_restart,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"as.function.default",do_asfunction,nullptr,	0,	11,	2,	{PP_FUNCTION,PREC_FN,	  0}},
{"debug",	do_debug,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"undebug",	do_debug,	nullptr,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"isdebugged",	do_debug,	nullptr,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"debugonce",	do_debug,	nullptr,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"Recall",	do_recall,	nullptr,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"delayedAssign",do_delayed,	nullptr,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"makeLazy",	do_makelazy,	nullptr,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{"identical",	do_identical,	nullptr,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	  0}},


/* Binary Operators, all primitives */
/* these are group generic and so need to eval args */
{"+",		do_arith,	nullptr, PLUSOP,	1,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"-",		do_arith,	nullptr, MINUSOP,1,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"*",		do_arith,	nullptr, TIMESOP,1,	2,	{PP_BINARY,  PREC_PROD,	  0}},
{"/",		do_arith,	nullptr, DIVOP,	1,	2,	{PP_BINARY2, PREC_PROD,	  0}},
{"^",		do_arith,	nullptr, POWOP,	1,	2,	{PP_BINARY2, PREC_POWER,  1}},
{"%%",		do_arith,	nullptr, MODOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%/%",		do_arith,	nullptr, IDIVOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%*%",		do_matprod,	nullptr,	0,	1,	2,	{PP_BINARY,  PREC_PERCENT,0}},

{"==",		do_relop,	nullptr, EQOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"!=",		do_relop,	nullptr, NEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<",		do_relop,	nullptr, LTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<=",		do_relop,	nullptr, LEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">=",		do_relop,	nullptr, GEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">",		do_relop,	nullptr, GTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"&",		do_logic,	nullptr,	1,	1,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"|",		do_logic,	nullptr,	2,	1,	2,	{PP_BINARY,  PREC_OR,	  0}},
{"!",		do_logic,	nullptr,	3,	1,	1,	{PP_UNARY,   PREC_NOT,	  0}},

/* specials as conditionally evaluate second arg */
{"&&",		do_logic2,	nullptr,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_logic2,	nullptr,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}},
{":",		do_colon,	nullptr,	0,	1,	2,	{PP_BINARY2, PREC_COLON,  0}},
/* does not evaluate */
{"~",		do_tilde,	nullptr,	0,	0,	2,	{PP_BINARY,  PREC_TILDE,  0}},


/* Logic Related Functions */
/* these are group generic and so need to eval args */
{"all",		do_logic3,	nullptr,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"any",		do_logic3,	nullptr,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},


/* Vectors, Matrices and Arrays */

/* Primitives */

{"length",	do_length,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"length<-",	do_lengthgets,	nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"c",/* bind.c:*/do_c,		nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass",	do_class,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass<-",	do_classgets,	nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT, 1}},
{"class",	R_do_data_class,nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{".cache_class",	R_do_data_class,	nullptr,	1,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"class<-",	R_do_set_class,	nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"unclass",	do_unclass,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"names",	do_names,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"names<-",	do_namesgets,	nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"dimnames",	do_dimnames,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dimnames<-",	do_dimnamesgets,nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"dim",		do_dim,		nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dim<-",	do_dimgets,	nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"attributes",	do_attributes,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"attributes<-",do_attributesgets,nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"attr",	do_attr,	nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"attr<-",	do_attrgets,	nullptr,	0,	1,	3,	{PP_FUNCALL, PREC_LEFT,	1}},
{"@<-",		do_attrgets,	nullptr,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"levels<-",	do_levelsgets,	nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},

/* .Internals */

{"vector",	do_makevector,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"complex",	do_complex,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"matrix",	do_matrix,	nullptr,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"array",	do_array,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"diag",	do_diag,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"backsolve",	do_backsolve,	nullptr,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"max.col",	do_maxcol,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"row",		do_rowscols,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"col",		do_rowscols,	nullptr,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"unlist",	do_unlist,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cbind",	do_bind,	nullptr,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rbind",	do_bind,	nullptr,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"drop",	do_drop,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"all.names",	do_allnames,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"comment",	do_comment,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"comment<-",	do_commentgets,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"get",		do_get,		nullptr,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mget",	do_mget,	nullptr,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"exists",	do_get,		nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"assign",	do_assign,	nullptr,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"list2env",	do_list2env,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"remove",	do_remove,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"duplicated",	do_duplicated,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"unique",	do_duplicated,	nullptr,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"anyDuplicated",do_duplicated,	nullptr,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"which",	do_which,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.min",	do_first_min,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmin",	do_pmin,	nullptr,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmax",	do_pmin,	nullptr,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.max",	do_first_min,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"match",	do_match,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pmatch",	do_pmatch,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"charmatch",	do_charmatch,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"match.call",	do_matchcall,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"crossprod",	do_matprod,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"tcrossprod",	do_matprod,	nullptr,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},

{"attach",	do_attach,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"detach",	do_detach,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"search",	do_search,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setFileTime",	do_setFileTime,	nullptr,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions */
/* primitives: these are group generic and so need to eval args (possibly internally) */
{"round",	do_Math2,	nullptr,	10001,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"signif",	do_Math2,	nullptr,	10004,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log",		do_log,		nullptr,	10003,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log10",	do_log1arg,	nullptr,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log2",	do_log1arg,	nullptr,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"abs",		do_abs,		nullptr,	6,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"floor",	do_math1,	nullptr,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ceiling",	do_math1,	nullptr,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sqrt",	do_math1,	nullptr,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sign",	do_math1,	nullptr,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trunc",	do_trunc,	nullptr,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"exp",		do_math1,	nullptr,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expm1",	do_math1,	nullptr,	11,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log1p",	do_math1,	nullptr,	12,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cos",		do_math1,	nullptr,	20,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sin",		do_math1,	nullptr,	21,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tan",		do_math1,	nullptr,	22,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acos",	do_math1,	nullptr,	23,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asin",	do_math1,	nullptr,	24,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atan",	do_math1,	nullptr,	25,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cosh",	do_math1,	nullptr,	30,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sinh",	do_math1,	nullptr,	31,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tanh",	do_math1,	nullptr,	32,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acosh",	do_math1,	nullptr,	33,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asinh",	do_math1,	nullptr,	34,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atanh",	do_math1,	nullptr,	35,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"lgamma",	do_math1,	nullptr,	40,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gamma",	do_math1,	nullptr,	41,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"digamma",	do_math1,	nullptr,	42,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trigamma",	do_math1,	nullptr,	43,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
/* see "psigamma" below !*/

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

{"atan2",	do_math2,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"lbeta",	do_math2,	nullptr,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"beta",	do_math2,	nullptr,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lchoose",	do_math2,	nullptr,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"choose",	do_math2,	nullptr,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/*
  Can remove all the [dpqr]xxx once the compiler knows how to optimize
  to .External.

  This is most of the do_math[23], and all of the do_math4, do_random[123]
*/
{"dchisq",	do_math2,	nullptr,	6,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pchisq",	do_math2,	nullptr,	7,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qchisq",	do_math2,	nullptr,	8,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dexp",	do_math2,	nullptr,	9,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pexp",	do_math2,	nullptr,	10,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qexp",	do_math2,	nullptr,	11,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgeom",	do_math2,	nullptr,	12,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgeom",	do_math2,	nullptr,	13,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgeom",	do_math2,	nullptr,	14,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dpois",	do_math2,	nullptr,	15,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ppois",	do_math2,	nullptr,	16,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qpois",	do_math2,	nullptr,	17,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dt",		do_math2,	nullptr,	18,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pt",		do_math2,	nullptr,	19,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qt",		do_math2,	nullptr,	20,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dsignrank",	do_math2,	nullptr,	21,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"psignrank",	do_math2,	nullptr,	22,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsignrank",	do_math2,	nullptr,	23,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselJ",	do_math2,	nullptr,	24,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"besselY",	do_math2,	nullptr,	25,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"psigamma",	do_math2,	nullptr,	26,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of a Complex Argument */
/* these are group generic and so need to eval args */

{"Re",		do_cmathfuns,	nullptr,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Im",		do_cmathfuns,	nullptr,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Mod",		do_cmathfuns,	nullptr,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Arg",		do_cmathfuns,	nullptr,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Conj",	do_cmathfuns,	nullptr,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	do_math3,	nullptr,	1,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbeta",	do_math3,	nullptr,	2,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbeta",	do_math3,	nullptr,	3,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dbinom",	do_math3,	nullptr,	4,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbinom",	do_math3,	nullptr,	5,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbinom",	do_math3,	nullptr,	6,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dcauchy",	do_math3,	nullptr,	7,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pcauchy",	do_math3,	nullptr,	8,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qcauchy",	do_math3,	nullptr,	9,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"df",		do_math3,	nullptr,	10,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pf",		do_math3,	nullptr,	11,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qf",		do_math3,	nullptr,	12,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgamma",	do_math3,	nullptr,	13,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgamma",	do_math3,	nullptr,	14,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgamma",	do_math3,	nullptr,	15,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlnorm",	do_math3,	nullptr,	16,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plnorm",	do_math3,	nullptr,	17,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlnorm",	do_math3,	nullptr,	18,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlogis",	do_math3,	nullptr,	19,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plogis",	do_math3,	nullptr,	20,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlogis",	do_math3,	nullptr,	21,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom",	do_math3,	nullptr,	22,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom",	do_math3,	nullptr,	23,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom",	do_math3,	nullptr,	24,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnorm",	do_math3,	nullptr,	25,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnorm",	do_math3,	nullptr,	26,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnorm",	do_math3,	nullptr,	27,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dunif",	do_math3,	nullptr,	28,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"punif",	do_math3,	nullptr,	29,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qunif",	do_math3,	nullptr,	30,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dweibull",	do_math3,	nullptr,	31,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pweibull",	do_math3,	nullptr,	32,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qweibull",	do_math3,	nullptr,	33,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnchisq",	do_math3,	nullptr,	34,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnchisq",	do_math3,	nullptr,	35,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnchisq",	do_math3,	nullptr,	36,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnt",		do_math3,	nullptr,	37,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnt",		do_math3,	nullptr,	38,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnt",		do_math3,	nullptr,	39,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dwilcox",	do_math3,	nullptr,	40,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pwilcox",	do_math3,	nullptr,	41,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qwilcox",	do_math3,	nullptr,	42,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselI",	do_math3,	nullptr,	43,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"besselK",	do_math3,	nullptr,	44,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom_mu",	do_math3,	nullptr,	45,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom_mu",	do_math3,	nullptr,	46,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom_mu",	do_math3,	nullptr,	47,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Four Numeric (+ 1-2 int) Variables */

{"dhyper",	do_math4,	nullptr,	1,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"phyper",	do_math4,	nullptr,	2,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qhyper",	do_math4,	nullptr,	3,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbeta",	do_math4,	nullptr,	4,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbeta",	do_math4,	nullptr,	5,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbeta",	do_math4,	nullptr,	6,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnf",		do_math4,	nullptr,	7,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnf",		do_math4,	nullptr,	8,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnf",		do_math4,	nullptr,	9,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dtukey",	do_math4,	nullptr,	10,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ptukey",	do_math4,	nullptr,	11,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qtukey",	do_math4,	nullptr,	12,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

/* Random Numbers */

{"rchisq",	do_random1,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rexp",	do_random1,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rgeom",	do_random1,	nullptr,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rpois",	do_random1,	nullptr,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rt",		do_random1,	nullptr,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rsignrank",	do_random1,	nullptr,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"rbeta",	do_random2,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rbinom",	do_random2,	nullptr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rcauchy",	do_random2,	nullptr,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rf",		do_random2,	nullptr,	3,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rgamma",	do_random2,	nullptr,	4,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlnorm",	do_random2,	nullptr,	5,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlogis",	do_random2,	nullptr,	6,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom",	do_random2,	nullptr,	7,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom_mu",	do_random2,	nullptr,	13,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnchisq",	do_random2,	nullptr,	12,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnorm",	do_random2,	nullptr,	8,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"runif",	do_random2,	nullptr,	9,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rweibull",	do_random2,	nullptr,	10,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rwilcox",	do_random2,	nullptr,	11,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"rhyper",	do_random3,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},

{"sample",	do_sample,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sample2",	do_sample2,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"RNGkind",	do_RNGkind,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"set.seed",	do_setseed,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* Data Summaries */
/* these four are group generic and so need to eval args */
{"sum",		do_summary,	nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"min",		do_summary,	nullptr,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"max",		do_summary,	nullptr,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"prod",	do_summary,	nullptr,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"mean",	do_summary,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"range",	do_range,	nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* Note that the number of arguments in this group only applies
   to the default method */
{"cumsum",	do_cum,		nullptr,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cumprod",	do_cum,		nullptr,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cummax",	do_cum,		nullptr,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cummin",	do_cum,		nullptr,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Type coercion */

{"as.character",do_ascharacter,	nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.integer",	do_ascharacter,	nullptr,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.double",	do_ascharacter,	nullptr,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.complex",	do_ascharacter,	nullptr,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.logical",	do_ascharacter,	nullptr,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.raw",	do_ascharacter,	nullptr,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.call",	do_ascall,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.environment",do_as_environment,nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"storage.mode<-",do_storage_mode,nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"as.vector",	do_asvector,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"paste",	do_paste,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"paste0",	do_paste,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.path",	do_filepath,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format",	do_format,	nullptr,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"format.info",	do_formatinfo,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cat",		do_cat,		nullptr,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"do.call",	do_docall,	nullptr,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* String Manipulation */

{"nchar",	do_nchar,	nullptr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"nzchar",	do_nzchar,	nullptr,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"substr",	do_substr,	nullptr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"substr<-",	do_substrgets,	nullptr,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"strsplit",	do_strsplit,	nullptr,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"abbreviate",	do_abbrev,	nullptr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"make.names",	do_makenames,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"grep",	do_grep,	nullptr,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepl",	do_grep,	nullptr,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepRaw",	do_grepraw,	nullptr,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"sub",		do_gsub,	nullptr,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"gsub",	do_gsub,	nullptr,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"regexpr",	do_regexpr,	nullptr,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gregexpr",	do_regexpr,	nullptr,	1,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"regexec",	do_regexec,	nullptr,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"agrep",	do_agrep,	nullptr,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"adist",	do_adist,	nullptr,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"aregexec",	do_aregexec,	nullptr,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"tolower",	do_tolower,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"toupper",	do_tolower,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"chartr",	do_chartr,	nullptr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"sprintf",	do_sprintf,	nullptr,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"make.unique",	do_makeunique,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"charToRaw",	do_charToRaw,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToChar",	do_rawToChar,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rawShift",	do_rawShift,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"intToBits",	do_intToBits,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToBits",	do_rawToBits,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"packBits",	do_packBits,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"utf8ToInt",	do_utf8ToInt,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"intToUtf8",	do_intToUtf8,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"encodeString",do_encodeString,nullptr,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"iconv",	do_iconv,	nullptr,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"strtrim",	do_strtrim,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strtoi",	do_strtoi,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Type Checking (typically implemented in ./coerce.c ) */

{"is.null",	do_is,		nullptr, NILSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.logical",	do_is,		nullptr, LGLSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.integer",	do_is,		nullptr, INTSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.double",	do_is,		nullptr, REALSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.complex",	do_is,		nullptr, CPLXSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.character",do_is,		nullptr, STRSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.symbol",	do_is,		nullptr, SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.environment",do_is,	nullptr, ENVSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.list",	do_is,		nullptr, VECSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.pairlist",	do_is,		nullptr, LISTSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.expression",do_is,		nullptr, EXPRSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.raw",	do_is,		nullptr, RAWSXP, 1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.object",	do_is,		nullptr,	50,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"isS4",	do_is,		nullptr,	51,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.numeric",	do_is,		nullptr,	100,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.matrix",	do_is,		nullptr,	101,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.array",	do_is,		nullptr,	102,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.atomic",	do_is,		nullptr,	200,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.recursive",do_is,		nullptr,	201,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.call",	do_is,		nullptr,	300,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.language",	do_is,		nullptr,	301,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.function",	do_is,		nullptr,	302,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.single",	do_is,		nullptr,	999,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.na",	do_isna,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.nan",	do_isnan,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.finite",	do_isfinite,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.infinite",	do_isinfinite,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.vector",	do_isvector,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Miscellaneous */

/* Primitive */
{"proc.time",	do_proctime,	nullptr,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"gc.time",	do_gctime,	nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
#if 0
{"visibleflag", do_visibleflag,	nullptr,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif
{"withVisible", do_withVisible,	nullptr,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expression",	do_expression,	nullptr,	1,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"interactive",	do_interactive,	nullptr,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"invisible",	do_invisible,	nullptr,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep",		do_rep,		nullptr,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep.int",	do_rep_int,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rep_len",	do_rep_len,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"seq.int",	do_seq,		nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_len",	do_seq_len,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_along",	do_seq_along,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"list",	do_makelist,	nullptr,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"xtfrm",	do_xtfrm,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2native",	do_enc2,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2utf8",	do_enc2,	nullptr,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"emptyenv",	do_emptyenv,	nullptr,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"baseenv",	do_baseenv,	nullptr,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"globalenv",	do_globalenv,	nullptr,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"environment<-",do_envirgets,	nullptr,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"pos.to.env",	do_pos2env,	nullptr,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"eapply",	do_eapply,	nullptr,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lapply",	do_lapply,	nullptr,	0,	10,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"vapply",	do_vapply,	nullptr,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mapply",	do_mapply,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{".C",		do_dotCode,	nullptr,	0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Fortran",	do_dotCode,	nullptr,	1,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".External",   do_External,    nullptr,	0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External2",   do_External,   nullptr,	1,    201,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call",       do_dotcall,     nullptr,	0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External.graphics", do_Externalgr, nullptr,	0, 1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Call.graphics", do_dotcallgr, nullptr,	0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},

/* .Internal */
{"Version",	do_version,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"machine",	do_machine,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"commandArgs", do_commandArgs, nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
#ifdef Win32
{"system",	do_system,	nullptr,	0,	211,	5,	{PP_FUNCALL, PREC_FN,	0}},
#else
{"system",	do_system,	nullptr,	0,	211,	2,	{PP_FUNCALL, PREC_FN,	0}},
#endif

#ifdef Win32
{"shell.exec",	do_shellexec,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.which",	do_syswhich,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"useInternet2",do_setInternet2,nullptr,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mkjunction", do_mkjunction,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
#endif
{"parse",	do_parse,	nullptr,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
//{"parse_Rd", 	do_parseRd,	nullptr,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
//{"deparseRd", 	do_deparseRd, 	nullptr,	0, 	11, 	2,	{PP_FUNCALL, PREC_FN, 	0}},
//{"parseLatex",  do_parseLatex,  nullptr,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"save",	do_save,	nullptr,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"saveToConn",	do_saveToConn,	nullptr,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"load",	do_load,	nullptr,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"loadFromConn2",do_loadFromConn2,nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeToConn",	do_serializeToConn,	nullptr,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserializeFromConn",	do_unserializeFromConn,	nullptr,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"deparse",	do_deparse,	nullptr,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"dput",	do_dput,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"dump",	do_dump,	nullptr,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"quit",	do_quit,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"readline",	do_readln,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"print.default",do_printdefault,nullptr,	0,	111,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"print.function",do_printfunction,nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"prmatrix",	do_prmatrix,	nullptr,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gc",		do_gc,		nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"gcinfo",	do_gcinfo,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture",	do_gctorture,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture2",	do_gctorture2,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"memory.profile",do_memoryprofile, nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"split",	do_split,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.loaded",	do_isloaded,	nullptr,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{"recordGraphics", do_recordGraphics, nullptr,	0, 211,     3,      {PP_FOREIGN, PREC_FN,	0}},
{"dyn.load",	do_dynload,	nullptr,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"dyn.unload",	do_dynunload,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ls",		do_ls,		nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"typeof",	do_typeof,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"eval",	do_eval,	nullptr,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.parent",	do_sys,		nullptr,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.call",	do_sys,		nullptr,	2,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frame",	do_sys,		nullptr,	3,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.nframe",	do_sys,		nullptr,	4,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.calls",	do_sys,		nullptr,	5,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frames",	do_sys,		nullptr,	6,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.on.exit",	do_sys,		nullptr,	7,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.parents",	do_sys,		nullptr,	8,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.function",do_sys,		nullptr,	9,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"traceback",	do_traceback,  	nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN,   0}},
{"browserText", do_sysbrowser,	nullptr,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserCondition", do_sysbrowser,	nullptr,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserSetDebug", do_sysbrowser,	nullptr,	3,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"parent.frame",do_parentframe,	nullptr,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sort",	do_sort,	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.unsorted",	do_isunsorted,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"psort",	do_psort,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsort",	do_qsort,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"radixsort",	do_radixsort,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"order",	do_order,	nullptr,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rank",	do_rank,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"scan",	do_scan,	nullptr,	0,	11,	18,	{PP_FUNCALL, PREC_FN,	0}},
{"t.default",	do_transpose,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"aperm",	do_aperm,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"builtins",	do_builtins,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"args",	do_args,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"formals",	do_formals,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"body",	do_body,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bodyCode",	do_bodyCode,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environment",	do_envir,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environmentName",do_envirName,nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"env2list",	do_env2list,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"reg.finalizer",do_regFinaliz,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"options",	do_options,	nullptr,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sink",	do_sink,	nullptr,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sink.number",	do_sinknumber,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rapply",	do_rapply,	nullptr,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"islistfactor",do_islistfactor,nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"colSums",	do_colsum,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"colMeans",	do_colsum,	nullptr,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowSums",	do_colsum,	nullptr,	2,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowMeans",	do_colsum,	nullptr,	3,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tracemem",    do_tracemem,    nullptr,	0,      1,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"retracemem",  do_retracemem,  nullptr,	0,      201,     -1,      {PP_FUNCALL, PREC_FN,	0}},
{"untracemem",  do_untracemem,  nullptr,	0,      101,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"inspect",	do_inspect,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mem.limits",	do_memlimits,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"merge",	do_merge,	nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilities",do_capabilities,nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilitiesX11",do_capabilitiesX11,nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"new.env",	do_newenv,	nullptr,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env",  do_parentenv,   nullptr,	0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env<-",do_parentenvgets, nullptr,	0,	11,     2,      {PP_FUNCALL, PREC_LEFT,	1}},
{"l10n_info",	do_l10n_info,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Cstack_info", do_Cstack_info,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

/* Functions To Interact with the Operating System */

{"file.show",	do_fileshow,	nullptr,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"file.create",	do_filecreate,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.remove",	do_fileremove,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.rename",	do_filerename,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.append",	do_fileappend,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.symlink",do_filesymlink,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.link",	do_filelink,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.copy",	do_filecopy,	nullptr,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"list.files",	do_listfiles,	nullptr,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"list.dirs",	do_listdirs,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"file.exists", do_fileexists,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.choose", do_filechoose,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.info",	do_fileinfo,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.access",	do_fileaccess,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"dir.create",	do_dircreate,	nullptr,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tempfile",	do_tempfile,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"tempdir",	do_tempdir,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"R.home",	do_Rhome,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"date",	do_date,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getenv",	do_getenv,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setenv",	do_setenv,	nullptr,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.unsetenv",do_unsetenv,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getwd",	do_getwd,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setwd",	do_setwd,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"basename",	do_basename,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dirname",	do_dirname,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.chmod",	do_syschmod,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.umask",	do_sysumask,	nullptr,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.readlink", do_readlink,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.info",	do_sysinfo,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.sleep",	do_syssleep,	nullptr,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getlocale",do_getlocale,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setlocale",do_setlocale,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.localeconv",do_localeconv,nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"path.expand",	do_pathexpand,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getpid",	do_sysgetpid,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"normalizePath",do_normalizepath,nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.glob",	do_glob,	nullptr,	0,      11,	2,      {PP_FUNCALL, PREC_FN,   0}},
{"unlink",	do_unlink,	nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* Complex Valued Functions */
{"polyroot",	do_polyroot,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Objects */
{"inherits",	do_inherits,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"UseMethod",	do_usemethod,	nullptr,	0,     200,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"NextMethod",	do_nextmethod,	nullptr,	0,     210,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"standardGeneric",do_standardGeneric,nullptr,	0, 201,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* date-time manipulations */
{"Sys.time",	do_systime,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXct",	do_asPOSIXct,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXlt",	do_asPOSIXlt,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format.POSIXlt",do_formatPOSIXlt,nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"strptime",	do_strptime,	nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Date2POSIXlt",do_D2POSIXlt,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"POSIXlt2Date",do_POSIXlt2D,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"mkCode",     do_mkcode,       nullptr,	0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}},
{"bcClose",    do_bcclose,      nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"is.builtin.internal",    do_is_builtin_internal,      nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"disassemble", do_disassemble, nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"bcVersion", do_bcversion,     nullptr,	0,      11,     0,      {PP_FUNCALL, PREC_FN, 0}},
{"load.from.file", do_loadfile, nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"save.to.file", do_savefile,   nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"growconst", do_growconst,     nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"putconst", do_putconst,       nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"getconst", do_getconst,       nullptr,	0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}},
{"enableJIT",    do_enablejit,  nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"compilePKGS", do_compilepkgs, nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},

{"setNumMathThreads", do_setnumthreads,      nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"setMaxNumMathThreads", do_setmaxnumthreads,      nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},

/* Connections */
{"stdin",	do_stdin,	nullptr,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stdout",	do_stdout,	nullptr,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stderr",	do_stderr,	nullptr,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"isatty",	do_isatty,	nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"readLines",	do_readLines,	nullptr,	0,      11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"writeLines",	do_writelines,	nullptr,	0,      111,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"readBin",	do_readbin,	nullptr,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeBin",	do_writebin,	nullptr,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"readChar",	do_readchar,	nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"writeChar",	do_writechar,	nullptr,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"open",	do_open,	nullptr,	0,      111,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"isOpen",	do_isopen,	nullptr,	0,      11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"isIncomplete",do_isincomplete,nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"isSeekable",	do_isseekable,	nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"close",	do_close,	nullptr,	0,      111,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"flush",	do_flush,	nullptr,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"file",	do_url,		nullptr,	1,      11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"url",	        do_url,		nullptr,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"pipe",	do_pipe,	nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"fifo",	do_fifo,	nullptr,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"gzfile",	do_gzfile,	nullptr,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"bzfile",	do_gzfile,	nullptr,	1,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"xzfile",	do_gzfile,	nullptr,	2,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"unz",         do_unz,		nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"seek",	do_seek,	nullptr,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"truncate",	do_truncate,	nullptr,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBack",	do_pushback,	nullptr,	0,     111,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"clearPushBack",do_clearpushback,nullptr,	0,   111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBackLength",do_pushbacklength,nullptr,	0,  11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"rawConnection",do_rawconnection,nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rawConnectionValue",do_rawconvalue,nullptr,	0, 11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnection",do_textconnection,nullptr,	0,	11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnectionValue",do_textconvalue,nullptr,	0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"socketConnection",do_sockconn,nullptr,	0,	11,     7,      {PP_FUNCALL, PREC_FN,	0}},
{"sockSelect",do_sockselect,	nullptr,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"getConnection",do_getconnection,nullptr,	0,	11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"getAllConnections",do_getallconnections,nullptr,	0,11, 0,      {PP_FUNCALL, PREC_FN,	0}},
{"summary.connection",do_sumconnection,nullptr,	0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"gzcon",	do_gzcon,	nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"memCompress",do_memCompress,	nullptr,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"memDecompress",do_memDecompress,nullptr,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},

/* Provenance Functions */
{"provenance", do_provenance,   nullptr,	0,       0,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"provenance.graph", do_provenance_graph,nullptr,	0,11,  1,      {PP_FUNCALL, PREC_FN, 0}},

/* and my serialization function */
{"bserialize", do_bserialize,   nullptr,	0,     100,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"bdeserialize", do_bdeserialize, nullptr,	0,   100,     1,      {PP_FUNCALL, PREC_FN, 0}},

{"readDCF",	do_readDCF,	nullptr,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},


{"lockEnvironment", do_lockEnv,		nullptr,	0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}},
{"environmentIsLocked",	do_envIsLocked,	nullptr,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"lockBinding", do_lockBnd,		nullptr,	0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unlockBinding", do_lockBnd,		nullptr,	1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsLocked", do_bndIsLocked,	nullptr,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"makeActiveBinding", do_mkActiveBnd,	nullptr,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsActive", do_bndIsActive,	nullptr,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
/* looks like mkUnbound is unused in base R */
{"mkUnbound",	do_mkUnbound,		nullptr,	0, 111,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"isNamespaceEnv",do_isNSEnv,		nullptr,	0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"registerNamespace",do_regNS,		nullptr,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unregisterNamespace",do_unregNS,	nullptr,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredNamespace",do_getRegNS,	nullptr,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getNamespaceRegistry",do_getNSRegistry, nullptr,	0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}},
{"importIntoEnv",do_importIntoEnv, nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"env.profile",  do_envprofile,    nullptr,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"Encoding",	do_encoding,	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"setEncoding",	do_setencoding,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"setTimeLimit",do_setTimeLimit,nullptr,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"setSessionTimeLimit",do_setSessionTimeLimit,nullptr,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"icuSetCollate",do_ICUset,	nullptr,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"readRenviron",do_readEnviron,	nullptr,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"shortRowNames",do_shortRowNames,nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"copyDFattr",do_copyDFattr,	nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredRoutines",do_getRegisteredRoutines,nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getLoadedDLLs",do_getDllTable,	nullptr,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"getSymbolInfo",do_getSymbolInfo,nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{".isMethodsDispatchOn",do_S4on,nullptr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBfetch",do_lazyLoadDBfetch,nullptr,	0,1,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBflush",do_lazyLoadDBflush,nullptr,	0,11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getVarsFromFrame",do_getVarsFromFrame, nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBinsertValue",do_lazyLoadDBinsertValue, nullptr,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"bincode",	do_bincode,	 nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tabulate",	do_tabulate,	 nullptr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"findInterval",do_findinterval, nullptr,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pretty",	do_pretty, 	nullptr,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"formatC",	do_formatC, 	nullptr,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"crc64",	do_crc64, 	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseAnd",	do_bitwise, 	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseNot",	do_bitwise, 	nullptr,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseOr",	do_bitwise, 	nullptr,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseXor",	do_bitwise, 	nullptr,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftL", do_bitwise, 	nullptr,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftR",  do_bitwise, 	nullptr,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"serialize",	do_serialize, 	nullptr,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeb",	do_serialize, 	nullptr,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserialize",	do_serialize, 	nullptr,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_matrix",do_rowsum, 	nullptr,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_df",	do_rowsum, 	nullptr,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"setS4Object",	do_setS4Object, nullptr,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"traceOnOff",	do_traceOnOff, 	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"La_qr_cmplx",	do_lapack,     	nullptr,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack,     	nullptr,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",do_lapack,     	nullptr,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg",	do_lapack,     	nullptr,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg_cmplx",do_lapack,     	nullptr,	41,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
// {"La_rs",	do_lapack,     	nullptr,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
// {"La_rs_cmplx",	do_lapack,     	nullptr,	51,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dlange",	do_lapack,     	nullptr,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dgecon",	do_lapack,     	nullptr,	7,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dtrcon",	do_lapack,     	nullptr,	8,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgecon",	do_lapack,     	nullptr,	9,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_ztrcon",	do_lapack,     	nullptr,	10,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve_cmplx",do_lapack,    nullptr,	11,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve",	do_lapack,     	nullptr,	100,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_qr",	do_lapack,     	nullptr,	101,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol",	do_lapack,     	nullptr,	200,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol2inv",	do_lapack,     	nullptr,	201,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"qr_coef_real",do_lapack,     	nullptr,	300,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_real",	do_lapack,     	nullptr,	301,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"det_ge_real",	do_lapack,     	nullptr,	302,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_coef_cmplx",do_lapack,    	nullptr,	303,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_cmpl",	do_lapack,     	nullptr,	304,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"La_svd",	do_lapack,     	nullptr,	400,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_svd_cmplx",do_lapack,     	nullptr,	401,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},

{nullptr,		nullptr,		nullptr,	0,	0,	0,	{PP_INVALID, PREC_FN,	0}},
};

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
    map* primitive_function_cache = new map();
    map* internal_function_cache = new map();

    for (int i = 0; s_function_table[i].name; ++i) {
	const char* symname = s_function_table[i].name;
	Symbol* sym = Symbol::obtain(symname);
	GCStackRoot<BuiltInFunction> bif(new BuiltInFunction(i));
	if (bif->viaDotInternal()) {
	    (*internal_function_cache)[sym] = bif;
	}
	else {
	    (*primitive_function_cache)[sym] = bif;
	}
    }
    return std::make_pair(primitive_function_cache, internal_function_cache);
}

SEXP attribute_hidden R_Primitive(const char *primname)
{
    return BuiltInFunction::obtainPrimitive(primname);
}
    
SEXP attribute_hidden do_primitive(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, prim;
    checkArity(op, args);
    name = CAR(args);
    if (!isString(name) || length(name) != 1 ||
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
    /* String constants (CHARSXP values) */
    String::initialize();
    Symbol::initialize();
    R_print.na_string = NA_STRING;
    R_initialize_bcode();
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

