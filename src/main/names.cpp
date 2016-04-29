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
const std::vector<BuiltInFunction::TableEntry>&
BuiltInFunction::getFunctionTable() {
    using Dispatch = BuiltInFunction::DispatchType;

    static std::vector<BuiltInFunction::TableEntry> s_function_table = {

/* printname	c-entry		quick-entry    offset	eval	arity	pp-kind	     precedence	rightassoc
 * ---------	-------		------	----	-----	-------      ----------	----------*/

/* Language Related Constructs */

/* Primitives */
{"if",		do_if,		0,	200,	-1,	{PP_IF,	     PREC_FN,	  1}},
{"while",	do_while,	0,	100,	2,	{PP_WHILE,   PREC_FN,	  0}},
{"for",		do_for,		0,	100,	3,	{PP_FOR,     PREC_FN,	  0}},
{"repeat",	do_repeat,	0,	100,	1,	{PP_REPEAT,  PREC_FN,	  0}},
{"break",	do_break,       0,	0,	0,	{PP_BREAK,   PREC_FN,	  0}},
{"next",	do_break,       1,	0,	0,	{PP_NEXT,    PREC_FN,	  0}},
{"return",	do_return,	0,	0,	-1,	{PP_RETURN,  PREC_FN,	  0}},
{"function",	do_function,	0,	0,	-1,	{PP_FUNCTION,PREC_FN,	  0}},
{"<-",		do_set,		1,	100,	-1,	{PP_ASSIGN,  PREC_LEFT,	  1}},
{"=",		do_set,		3,	100,	-1,	{PP_ASSIGN,  PREC_EQ,	  1}},
{"<<-",		do_set,		2,	100,	-1,	{PP_ASSIGN2, PREC_LEFT,	  1}},
{"{",		do_begin,	0,	200,	-1,	{PP_CURLY,   PREC_FN,	  0}},
{"(",		do_paren,	0,	1,	1,	{PP_PAREN,   PREC_FN,	  0}},
{".subset",	do_subset_dflt,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt,2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"[",		do_subset,	1,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"[[",		do_subset2,	2,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"$",		do_subset3,	3,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"@",		do_AT,		0,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"[<-",		do_subassign,	0,	0,	-1,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	1,	0,	-1,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"switch",	do_switch,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	  0}, "EXPR"},
{"browser",	do_browser,	0,	101,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primTrace",	do_trace,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primUntrace",do_trace,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Internal",	do_internal,	0,	200,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Primitive",	do_primitive,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"call",	do_call,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, "name"},
{"quote",	do_quote,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}, "expr"},
{"substitute",	do_substitute,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"missing",	do_missing,	1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"nargs",	do_nargs,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"on.exit",	do_onexit,	0,	100,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"forceAndCall",do_forceAndCall,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

/* .Internals */

{"stop",	do_stop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"warning",	do_warning,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"gettext",	do_gettext,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"ngettext",	do_ngettext,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"bindtextdomain",do_bindtextdomain,0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addCondHands",do_addCondHands,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{".signalCondition",do_signalCondition,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltStop",do_dfltStop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltWarn",do_dfltWarn,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addRestart",do_addRestart,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".getRestart",do_getRestart,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".invokeRestart",do_invokeRestart,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"geterrmessage",do_geterrmessage, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"seterrmessage",do_seterrmessage, 0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"printDeferredWarnings",do_printDeferredWarnings, 0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"as.function.default",do_asfunction,0,	11,	2,	{PP_FUNCTION,PREC_FN,	  0}},
{"debug",	do_debug,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"undebug",	do_debug,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"isdebugged",	do_debug,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"debugonce",	do_debug,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"Recall",	do_recall,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"delayedAssign",do_delayed,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"makeLazy",	do_makelazy,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{"identical",	do_identical,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	  0}},


/* Binary Operators, all primitives */
/* these are group generic and so need to eval args */
{"+",		do_arith,	PLUSOP,	1,	-1,	{PP_BINARY,  PREC_SUM,	  0}, nullptr, Dispatch::GROUP_OPS},
{"-",		do_arith,	MINUSOP,1,	-1,	{PP_BINARY,  PREC_SUM,	  0}, nullptr, Dispatch::GROUP_OPS},
{"*",		do_arith,	TIMESOP,1,	2,	{PP_BINARY,  PREC_PROD,	  0}, nullptr, Dispatch::GROUP_OPS},
{"/",		do_arith,	DIVOP,	1,	2,	{PP_BINARY2, PREC_PROD,	  0}, nullptr, Dispatch::GROUP_OPS},
{"^",		do_arith,	POWOP,	1,	2,	{PP_BINARY2, PREC_POWER,  1}, nullptr, Dispatch::GROUP_OPS},
{"%%",		do_arith,	MODOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}, nullptr, Dispatch::GROUP_OPS},
{"%/%",		do_arith,	IDIVOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}, nullptr, Dispatch::GROUP_OPS},
{"%*%",		do_matprod,	0,	1,	2,	{PP_BINARY,  PREC_PERCENT,0}},

{"==",		do_relop, EQOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS},
{"!=",		do_relop, NEOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS},
{"<",		do_relop, LTOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS},
{"<=",		do_relop, LEOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS},
{">=",		do_relop, GEOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS},
{">",		do_relop, GTOP,	1,	-1,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, Dispatch::GROUP_OPS},
{"&",		do_logic,	1,	1,	-1,	{PP_BINARY,  PREC_AND,	  0}, nullptr, Dispatch::GROUP_OPS},
{"|",		do_logic,	2,	1,	-1,	{PP_BINARY,  PREC_OR,	  0}, nullptr, Dispatch::GROUP_OPS},
{"!",		do_logic,	3,	1,	-1,	{PP_UNARY,   PREC_NOT,	  0}, nullptr, Dispatch::GROUP_OPS},

/* specials as conditionally evaluate second arg */
{"&&",		do_logic2,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_logic2,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}},
{":",		do_colon,	0,	1,	2,	{PP_BINARY2, PREC_COLON,  0}},
/* does not evaluate */
{"~",		do_tilde,	0,	0,	-1,	{PP_BINARY,  PREC_TILDE,  0}},


/* Logic Related Functions */
/* these are group generic and so need to eval args */
{"all",		do_logic3,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}, nullptr, Dispatch::GROUP_SUMMARY},
{"any",		do_logic3,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}, nullptr, Dispatch::GROUP_SUMMARY},


/* Vectors, Matrices and Arrays */

/* Primitives */

{"length",	do_length,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"length<-",	do_lengthgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL},
{"c",/* bind.c:*/do_c,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass",	do_class,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"oldClass<-",	do_classgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT, 1}, "x"},
{"class",	R_do_data_class,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{".cache_class",R_do_data_class,1,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"class<-",	R_do_set_class,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"unclass",	do_unclass,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"names",	do_names,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"names<-",	do_namesgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL},
{"dimnames",	do_dimnames,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"dimnames<-",	do_dimnamesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL},
{"dim",		do_dim,		0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"dim<-",	do_dimgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, nullptr, Dispatch::INTERNAL},
{"attributes",	do_attributes,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"attributes<-",do_attributesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x"},
{"attr",	do_attr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"attr<-",	do_attrgets,	0,	1,	3,	{PP_FUNCALL, PREC_LEFT,	1}},
{"@<-",		do_slotgets,	0,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"levels<-",	do_levelsgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", Dispatch::INTERNAL},

/* .Internals */

{"vector",	do_makevector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"complex",	do_complex,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"matrix",	do_matrix,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"array",	do_array,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"diag",	do_diag,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"backsolve",	do_backsolve,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"max.col",	do_maxcol,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"row",		do_rowscols,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"col",		do_rowscols,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"unlist",	do_unlist,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL},
{"cbind",	do_bind,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rbind",	do_bind,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"drop",	do_drop,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"all.names",	do_allnames,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"comment",	do_comment,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"comment<-",	do_commentgets,	0,	11,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"get",		do_get,		1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"get0",	do_get,		2,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"mget",	do_mget,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"exists",	do_get,		0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"assign",	do_assign,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"list2env",	do_list2env,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"remove",	do_remove,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"duplicated",	do_duplicated,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"unique",	do_duplicated,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"anyDuplicated",do_duplicated,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"anyNA",	do_anyNA,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which",	do_which,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.min",	do_first_min,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmin",	do_pmin,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmax",	do_pmin,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.max",	do_first_min,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"match",	do_match,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pmatch",	do_pmatch,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"charmatch",	do_charmatch,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"match.call",	do_matchcall,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"crossprod",	do_crossprod,	1,	11,	2,	{PP_FUNCALL, PREC_FN,   0}},
{"tcrossprod",	do_crossprod,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lengths",	do_lengths,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL},

{"attach",	do_attach,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"detach",	do_detach,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"search",	do_search,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setFileTime",	do_setFileTime,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions */
/* primitives: these are group generic and so need to eval args (possibly internally) */
{"round",	do_Math2,	10001,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},  // Special
{"signif",	do_Math2,	10004,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},  // Special
{"log",		do_log,		10003,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},  // Special
{"log10",	do_log1arg,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"log2",	do_log1arg,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"abs",		do_abs,		6,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"floor",	do_math1,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"ceiling",	do_math1,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"sqrt",	do_math1,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"sign",	do_math1,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"trunc",	do_trunc,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},

{"exp",		do_math1,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"expm1",	do_math1,	11,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"log1p",	do_math1,	12,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},

{"cos",		do_math1,	20,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"sin",		do_math1,	21,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"tan",		do_math1,	22,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"acos",	do_math1,	23,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"asin",	do_math1,	24,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"atan",	do_math1,	25,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},

{"cosh",	do_math1,	30,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"sinh",	do_math1,	31,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"tanh",	do_math1,	32,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"acosh",	do_math1,	33,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"asinh",	do_math1,	34,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"atanh",	do_math1,	35,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},

{"lgamma",	do_math1,	40,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"gamma",	do_math1,	41,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},

{"digamma",	do_math1,	42,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"trigamma",	do_math1,	43,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
/* see "psigamma" below !*/

{"cospi",	do_math1,	47,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"sinpi",	do_math1,	48,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},
{"tanpi",	do_math1,	49,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::GROUP_MATH},

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

{"atan2",	do_math2,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"lbeta",	do_math2,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"beta",	do_math2,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lchoose",	do_math2,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"choose",	do_math2,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/*
  This is most of the do_math[23]
*/
{"besselJ",	do_math2,	24,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"besselY",	do_math2,	25,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"psigamma",	do_math2,	26,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of a Complex Argument:
 * These are group generic and so need to eval args --> ./complex.c */

{"Re",		do_cmathfuns,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"},
{"Im",		do_cmathfuns,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"},
{"Mod",		do_cmathfuns,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"},
{"Arg",		do_cmathfuns,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"},
{"Conj",	do_cmathfuns,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z"},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"besselI",	do_math3,	43,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"besselK",	do_math3,	44,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* Random Numbers */

{"sample",	do_sample,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sample2",	do_sample2,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"RNGkind",	do_RNGkind,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"set.seed",	do_setseed,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* Data Summaries */
/* these four are group generic and so need to eval args */
{"sum",		do_summary,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY},
{"min",		do_summary,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY},
{"max",		do_summary,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY},
{"prod",	do_summary,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY},

{"mean",	do_summary,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY},
{"range",	do_range,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_SUMMARY},

/* Note that the number of arguments in this group only applies
   to the default method */
{"cumsum",	do_cum,		1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},
{"cumprod",	do_cum,		2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},
{"cummax",	do_cum,		3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},
{"cummin",	do_cum,		4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::GROUP_MATH},

/* Type coercion */

{"as.character",do_asatomic,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"as.integer",	do_asatomic,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"as.double",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"as.numeric",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"as.complex",	do_asatomic,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"as.logical",	do_asatomic,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"as.raw",	do_asatomic,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"as.call",	do_ascall,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"as.environment",do_as_environment,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "object", Dispatch::INTERNAL},
{"storage.mode<-",do_storage_mode,0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"asCharacterFactor",	do_asCharacterFactor,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"as.vector",	do_asvector,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL},
{"paste",	do_paste,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"paste0",	do_paste,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.path",	do_filepath,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format",	do_format,	0,	11,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"format.info",	do_formatinfo,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cat",		do_cat,		0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"do.call",	do_docall,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* String Manipulation */

{"nchar",	do_nchar,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"nzchar",	do_nzchar,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"substr",	do_substr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"startsWith",	do_startsWith,  0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"endsWith",	do_startsWith,  1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"substr<-",	do_substrgets,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"strsplit",	do_strsplit,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"abbreviate",	do_abbrev,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"make.names",	do_makenames,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"pcre_config", do_pcre_config,	1,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"grep",	do_grep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepl",	do_grep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepRaw",	do_grepraw,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"sub",		do_gsub,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"gsub",	do_gsub,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"regexpr",	do_regexpr,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gregexpr",	do_regexpr,	1,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"regexec",	do_regexec,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"agrep",	do_agrep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"agrepl",	do_agrep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"adist",	do_adist,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"aregexec",	do_aregexec,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"tolower",	do_tolower,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"toupper",	do_tolower,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"chartr",	do_chartr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"sprintf",	do_sprintf,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"make.unique",	do_makeunique,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"charToRaw",	do_charToRaw,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToChar",	do_rawToChar,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rawShift",	do_rawShift,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"intToBits",	do_intToBits,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToBits",	do_rawToBits,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"packBits",	do_packBits,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"utf8ToInt",	do_utf8ToInt,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"intToUtf8",	do_intToUtf8,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"validUTF8",	do_validUTF8,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"validEnc",	do_validEnc,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"encodeString",do_encodeString,1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"iconv",	do_iconv,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"strtrim",	do_strtrim,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strtoi",	do_strtoi,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strrep",	do_strrep,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Type Checking (typically implemented in ./coerce.c ) */

{"is.null",	do_is,		NILSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.logical",	do_is,		LGLSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.integer",	do_is,		INTSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.double",	do_is,		REALSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.complex",	do_is,		CPLXSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.character",do_is,		STRSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.symbol",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.name",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.environment",do_is,	ENVSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.list",	do_is,		VECSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.pairlist",	do_is,		LISTSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.expression",do_is,		EXPRSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.raw",	do_is,		RAWSXP, 1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.object",	do_is,		50,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"isS4",	do_is,		51,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.numeric",	do_is,		100,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"is.matrix",	do_is,		101,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"is.array",	do_is,		102,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},

{"is.atomic",	do_is,		200,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.recursive",do_is,		201,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.call",	do_is,		300,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.language",	do_is,		301,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.function",	do_is,		302,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.single",	do_is,		999,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.na",	do_isna,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"is.nan",	do_isnan,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"is.finite",	do_isfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},
{"is.infinite",	do_isinfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", Dispatch::INTERNAL},

{"is.vector",	do_isvector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Miscellaneous */

/* Primitive */
{"proc.time",	do_proctime,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"gc.time",	do_gctime,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"withVisible", do_withVisible,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expression",	do_expression,	1,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"interactive",	do_interactive,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"invisible",	do_invisible,	0,	101,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep",		do_rep,		0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep.int",	do_rep_int,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rep_len",	do_rep_len,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"seq.int",	do_seq,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_len",	do_seq_len,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "length.out"},
{"seq_along",	do_seq_along,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "along.with"},
{"list",	do_makelist,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"xtfrm",	do_xtfrm,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"enc2native",	do_enc2,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"enc2utf8",	do_enc2,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"emptyenv",	do_emptyenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"baseenv",	do_baseenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"globalenv",	do_globalenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"environment<-",do_envirgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x"},
{"pos.to.env",	do_pos2env,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"eapply",	do_eapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lapply",	do_lapply,	0,	10,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"vapply",	do_vapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mapply",	do_mapply,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{".C",		do_dotCode,	0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Fortran",	do_dotCode,	1,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".External",   do_External,    0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External2",   do_External,   1,    201,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call",       do_dotcall,     0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External.graphics", do_Externalgr, 0, 1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Call.graphics", do_dotcallgr, 0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},

/* .Internal */
{"Version",	do_version,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"commandArgs", do_commandArgs, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
#ifdef Win32
{"system",	do_system,	0,	211,	5,	{PP_FUNCALL, PREC_FN,	0}},
#else
{"system",	do_system,	0,	211,	2,	{PP_FUNCALL, PREC_FN,	0}},
#endif

#ifdef Win32
{"shell.exec",	do_shellexec,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.which",	do_syswhich,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mkjunction", do_mkjunction,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"tzone_name", do_tzone_name,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif
{"parse",	do_parse,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
//{"parse_Rd",	do_parseRd,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
//{"deparseRd",	do_deparseRd,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
//{"parseLatex",  do_parseLatex,  0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"save",	do_save,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"saveToConn",	do_saveToConn,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"load",	do_load,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"loadFromConn2",do_loadFromConn2,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeToConn",	do_serializeToConn,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserializeFromConn",	do_unserializeFromConn,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"deparse",	do_deparse,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"dput",	do_dput,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"dump",	do_dump,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"quit",	do_quit,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"readline",	do_readln,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"print.default",do_printdefault,0,	111,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"print.function",do_printfunction,0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"prmatrix",	do_prmatrix,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gc",		do_gc,		0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"gcinfo",	do_gcinfo,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture",	do_gctorture,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture2",	do_gctorture2,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"memory.profile",do_memoryprofile, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"split",	do_split,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.loaded",	do_isloaded,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{"recordGraphics", do_recordGraphics, 0, 211,     3,      {PP_FOREIGN, PREC_FN,	0}},
{"dyn.load",	do_dynload,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"dyn.unload",	do_dynunload,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ls",		do_ls,		1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"typeof",	do_typeof,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"eval",	do_eval,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},
//{"returnValue",   do_returnValue,0,     11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"sys.parent",	do_sys,		1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.call",	do_sys,		2,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frame",	do_sys,		3,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.nframe",	do_sys,		4,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.calls",	do_sys,		5,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frames",	do_sys,		6,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.on.exit",	do_sys,		7,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.parents",	do_sys,		8,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.function",do_sys,		9,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"traceback",	do_traceback,	0,      11,     1,      {PP_FUNCALL, PREC_FN,   0}},
{"browserText", do_sysbrowser,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserCondition", do_sysbrowser,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserSetDebug", do_sysbrowser,	3,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"parent.frame",do_parentframe,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sort",	do_sort,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.unsorted",	do_isunsorted,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, nullptr, Dispatch::INTERNAL},
{"psort",	do_psort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsort",	do_qsort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"radixsort",	do_radixsort,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"order",	do_order,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rank",	do_rank,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"scan",	do_scan,	0,	11,	19,	{PP_FUNCALL, PREC_FN,	0}},
{"t.default",	do_transpose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"aperm",	do_aperm,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"builtins",	do_builtins,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"args",	do_args,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"formals",	do_formals,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"body",	do_body,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environment",	do_envir,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environmentName",do_envirName,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"env2list",	do_env2list,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"reg.finalizer",do_regFinaliz,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"options",	do_options,	0,	211,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"getOption",	do_getOption,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sink",	do_sink,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sink.number",	do_sinknumber,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rapply",	do_rapply,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"islistfactor",do_islistfactor,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"colSums",	do_colsum,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"colMeans",	do_colsum,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowSums",	do_colsum,	2,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowMeans",	do_colsum,	3,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tracemem",    do_tracemem,    0,      1,	1,      {PP_FUNCALL, PREC_FN,	0}, "x"},
{"retracemem",  do_retracemem,  0,      201,     -1,      {PP_FUNCALL, PREC_FN,	0}},
{"untracemem",  do_untracemem,  0,      101,	1,      {PP_FUNCALL, PREC_FN,	0}, "x"},
{"inspect",	do_inspect,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"address",     do_address,     0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}},
{"refcnt",      do_refcnt,      0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}},
{"merge",	do_merge,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilities",do_capabilities,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilitiesX11",do_capabilitiesX11,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"new.env",	do_newenv,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env",  do_parentenv,   0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env<-",do_parentenvgets, 0,	11,     2,      {PP_FUNCALL, PREC_LEFT,	1}},
{"topenv",	do_topenv,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"l10n_info",	do_l10n_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Cstack_info", do_Cstack_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

/* Functions To Interact with the Operating System */

{"file.show",	do_fileshow,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"file.create",	do_filecreate,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.remove",	do_fileremove,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.rename",	do_filerename,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.append",	do_fileappend,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.symlink",do_filesymlink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.link",	do_filelink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.copy",	do_filecopy,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"list.files",	do_listfiles,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"list.dirs",	do_listdirs,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"file.exists", do_fileexists,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.choose", do_filechoose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.info",	do_fileinfo,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.access",	do_fileaccess,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"dir.exists",	do_direxists,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dir.create",	do_dircreate,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tempfile",	do_tempfile,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"tempdir",	do_tempdir,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"R.home",	do_Rhome,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"date",	do_date,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getenv",	do_getenv,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setenv",	do_setenv,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.unsetenv",do_unsetenv,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getwd",	do_getwd,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setwd",	do_setwd,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"basename",	do_basename,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dirname",	do_dirname,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.chmod",	do_syschmod,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.umask",	do_sysumask,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.readlink", do_readlink,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.info",	do_sysinfo,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.sleep",	do_syssleep,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getlocale",do_getlocale,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setlocale",do_setlocale,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.localeconv",do_localeconv,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"path.expand",	do_pathexpand,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getpid",	do_sysgetpid,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"normalizePath",do_normalizepath,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.glob",	do_glob,	0,      11,	2,      {PP_FUNCALL, PREC_FN,   0}},
{"unlink",	do_unlink,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* Complex Valued Functions */
{"polyroot",	do_polyroot,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Objects */
{"inherits",	do_inherits,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"UseMethod",	do_usemethod,	0,     200,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"NextMethod",	do_nextmethod,	0,     210,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"standardGeneric",do_standardGeneric,0, 201,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* date-time manipulations */
{"Sys.time",	do_systime,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXct",	do_asPOSIXct,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXlt",	do_asPOSIXlt,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format.POSIXlt",do_formatPOSIXlt,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"strptime",	do_strptime,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Date2POSIXlt",do_D2POSIXlt,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"POSIXlt2Date",do_POSIXlt2D,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

// {"mkCode",     do_mkcode,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}},
// {"bcClose",    do_bcclose,      0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
// {"is.builtin.internal",    do_is_builtin_internal,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
// {"disassemble", do_disassemble, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
// {"bcVersion", do_bcversion,     0,      11,     0,      {PP_FUNCALL, PREC_FN, 0}},
// {"load.from.file", do_loadfile, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
// {"save.to.file", do_savefile,   0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
// {"growconst", do_growconst,     0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
// {"putconst", do_putconst,       0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
// {"getconst", do_getconst,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}},
// {"enableJIT",    do_enablejit,  0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
// {"compilePKGS", do_compilepkgs, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},

{"setNumMathThreads", do_setnumthreads,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"setMaxNumMathThreads", do_setmaxnumthreads,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},

/* Connections */
{"stdin",	do_stdin,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stdout",	do_stdout,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stderr",	do_stderr,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"isatty",	do_isatty,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"readLines",	do_readLines,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeLines",	do_writelines,	0,      111,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"readBin",	do_readbin,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeBin",	do_writebin,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"readChar",	do_readchar,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"writeChar",	do_writechar,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"open",	do_open,	0,      111,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"isOpen",	do_isopen,	0,      11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"isIncomplete",do_isincomplete,0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"isSeekable",	do_isseekable,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"close",	do_close,	0,      111,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"flush",	do_flush,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"file",	do_url,		1,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"url",		do_url,		0,      11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"pipe",	do_pipe,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"fifo",	do_fifo,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"gzfile",	do_gzfile,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"bzfile",	do_gzfile,	1,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"xzfile",	do_gzfile,	2,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"unz",         do_unz,		0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"seek",	do_seek,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"truncate",	do_truncate,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBack",	do_pushback,	0,     111,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"clearPushBack",do_clearpushback,0,   111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBackLength",do_pushbacklength,0,  11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"rawConnection",do_rawconnection,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rawConnectionValue",do_rawconvalue,0, 11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnection",do_textconnection,0,	11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnectionValue",do_textconvalue,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"socketConnection",do_sockconn,0,	11,     7,      {PP_FUNCALL, PREC_FN,	0}},
{"sockSelect",do_sockselect,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"getConnection",do_getconnection,0,	11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"getAllConnections",do_getallconnections,0,11, 0,      {PP_FUNCALL, PREC_FN,	0}},
{"summary.connection",do_sumconnection,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"gzcon",	do_gzcon,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"memCompress",do_memCompress,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"memDecompress",do_memDecompress,0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},

/* Provenance Functions */
{"provenance", do_provenance,   0,       0,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"provenance.graph", do_provenance_graph,0,11,  1,      {PP_FUNCALL, PREC_FN, 0}},

{"readDCF",	do_readDCF,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},


{"lockEnvironment", do_lockEnv,		0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}},
{"environmentIsLocked",	do_envIsLocked,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"lockBinding", do_lockBnd,		0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unlockBinding", do_lockBnd,		1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsLocked", do_bndIsLocked,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"makeActiveBinding", do_mkActiveBnd,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsActive", do_bndIsActive,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"isNamespaceEnv",do_isNSEnv,		0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"registerNamespace",do_regNS,		0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unregisterNamespace",do_unregNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredNamespace",do_getRegNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"isRegisteredNamespace", do_getRegNS,	1, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getNamespaceRegistry",do_getNSRegistry, 0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}},
{"importIntoEnv",do_importIntoEnv, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"env.profile",  do_envprofile,    0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"Encoding",	do_encoding,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"setEncoding",	do_setencoding,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"setTimeLimit",do_setTimeLimit,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"setSessionTimeLimit",do_setSessionTimeLimit,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"icuSetCollate",do_ICUset,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"icuGetCollate",do_ICUget,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"readRenviron",do_readEnviron,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"shortRowNames",do_shortRowNames,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"copyDFattr",do_copyDFattr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredRoutines",do_getRegisteredRoutines,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getLoadedDLLs",do_getDllTable,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"getSymbolInfo",do_getSymbolInfo,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{".isMethodsDispatchOn",do_S4on,0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBfetch",do_lazyLoadDBfetch,0,1,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBflush",do_lazyLoadDBflush,0,11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getVarsFromFrame",do_getVarsFromFrame, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBinsertValue",do_lazyLoadDBinsertValue, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"bincode",	do_bincode,	 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tabulate",	do_tabulate,	 0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"findInterval",do_findinterval, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"pretty",	do_pretty,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"formatC",	do_formatC,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseAnd",	do_bitwise,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseNot",	do_bitwise,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseOr",	do_bitwise,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseXor",	do_bitwise,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftL", do_bitwise,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftR",  do_bitwise,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"serialize",	do_serialize,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeb",	do_serialize,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserialize",	do_serialize,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_matrix",do_rowsum,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_df",	do_rowsum,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"setS4Object",	do_setS4Object, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"traceOnOff",	do_traceOnOff,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"debugOnOff",	do_traceOnOff,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"La_qr_cmplx",	do_lapack,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",do_lapack,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg",	do_lapack,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg_cmplx",do_lapack,	41,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
// {"La_rs",	do_lapack,     	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
// {"La_rs_cmplx",	do_lapack,     	51,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dlange",	do_lapack,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dgecon",	do_lapack,	7,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dtrcon",	do_lapack,	8,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgecon",	do_lapack,	9,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_ztrcon",	do_lapack,	10,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve_cmplx",do_lapack,    11,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve",	do_lapack,	100,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_qr",	do_lapack,	101,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol",	do_lapack,	200,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol2inv",	do_lapack,	201,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"qr_coef_real",do_lapack,	300,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_real",	do_lapack,	301,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"det_ge_real",	do_lapack,	302,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_coef_cmplx",do_lapack,	303,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_cmplx",	do_lapack,	304,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"La_svd",	do_lapack,	400,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_svd_cmplx",do_lapack,	401,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_version",	do_lapack,	1000,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

// {"bcprofcounts",do_bcprofcounts,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
// {"bcprofstart",	do_bcprofstart,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
// {"bcprofstop",	do_bcprofstop,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

{"eSoftVersion",do_eSoftVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"curlVersion", do_curlVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"curlGetHeaders",do_curlGetHeaders,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"curlDownload",do_curlDownload, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
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

    for (BuiltInFunction::TableEntry entry : getFunctionTable()) {
	BuiltInFunction* function = entry.function;
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
