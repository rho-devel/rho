/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2016  The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

/* Names of  .Internal(.) and .Primitive(.)  R functions
 *
 * Must all return SEXP because of CCODE in Defn.h.
 * do_math*() and do_cmathfuns are in ../main/arithmetic.h
 */

#ifndef R_INTERNAL_H
#define R_INTERNAL_H

#include "R_ext/Error.h"
#include "CXXR/BuiltInFunction.h"
#include "CXXR/RObject.h"
#include "CXXR/Expression.h"
#include "CXXR/Environment.h"
#include "CXXR/PairList.h"

#ifdef __cplusplus

namespace CXXR {
  typedef RObject*(quick_builtin)(Expression*,
                                  const BuiltInFunction*,
                                  Environment* env,
                                  RObject* const* args,
                                  int num_args,
                                  const PairList* tags);
}  // namespace CXXR

/* Function Names */

SEXP do_abbrev(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* these_, CXXR::RObject* minlength_, CXXR::RObject* use_classes_);
SEXP do_abs(SEXP, SEXP, SEXP, SEXP); // calls do_cmathfuns
CXXR::quick_builtin do_addCondHands;
SEXP do_address(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x);
SEXP do_addRestart(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* restart_);
SEXP do_adist(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* y_, CXXR::RObject* costs_, CXXR::RObject* counts_, CXXR::RObject* fixed_, CXXR::RObject* partial_, CXXR::RObject* ignore_case_, CXXR::RObject* useBytes_);
CXXR::quick_builtin do_agrep;
SEXP do_allnames(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* expr_, CXXR::RObject* functions_, CXXR::RObject* max_names_, CXXR::RObject* unique_);
SEXP do_anyNA(SEXP, SEXP, SEXP, SEXP);
SEXP do_aperm(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* a_, CXXR::RObject* perm_, CXXR::RObject* resize_);
CXXR::quick_builtin do_arith;
CXXR::quick_builtin do_aregexec;
SEXP do_args(SEXP, SEXP, SEXP, SEXP); // non-trivial
SEXP do_array(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* data_, CXXR::RObject* dim_, CXXR::RObject* dimnames_);
SEXP do_asPOSIXct(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* tz_);
SEXP do_asPOSIXlt(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* tz_);
SEXP do_ascall(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* args);
CXXR::quick_builtin do_as_environment;
CXXR::quick_builtin do_asatomic;
SEXP do_asfunction(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* envir_);
SEXP do_assign(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* value_, CXXR::RObject* envir_, CXXR::RObject* inherits_);
CXXR::quick_builtin do_asvector;
SEXP do_asCharacterFactor(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x);
SEXP do_AT(SEXP call, SEXP op, SEXP args, SEXP env);  // Special
SEXP do_attach(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* what_, CXXR::RObject* pos_, CXXR::RObject* name_);
SEXP do_attr(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
SEXP do_attrgets(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
SEXP do_attributes(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x);
SEXP do_attributesgets(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object, CXXR::RObject* attrs);
SEXP do_backsolve(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* r_, CXXR::RObject* x_, CXXR::RObject* k_, CXXR::RObject* upper_tri_, CXXR::RObject* transpose_);
SEXP do_baseenv(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_basename(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_);
SEXP do_bcprofcounts(SEXP, SEXP, SEXP, SEXP);
SEXP do_bcprofstart(SEXP, SEXP, SEXP, SEXP);
SEXP do_bcprofstop(SEXP, SEXP, SEXP, SEXP);
SEXP do_begin(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_bincode(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* breaks_, CXXR::RObject* right_, CXXR::RObject* include_lowest_);
SEXP do_bind(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_bindtextdomain(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* domain_, CXXR::RObject* dirname_);
CXXR::quick_builtin do_bitwise;
SEXP do_body(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* fun_);
SEXP do_break(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_browser(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
SEXP do_builtins(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* internal_);
SEXP do_c(SEXP, SEXP, SEXP, SEXP);  // Calls do_c_dflt
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP); // Tricky
SEXP do_call(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_capabilities(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_capabilitiesX11(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_cat(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* dots_, CXXR::RObject* file_, CXXR::RObject* sep_, CXXR::RObject* fill_, CXXR::RObject* labels_, CXXR::RObject* append_);
SEXP do_charmatch(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* table_, CXXR::RObject* nomatch_);
SEXP do_charToRaw(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_chartr(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* old_, CXXR::RObject* new_, CXXR::RObject* x_);
SEXP do_class(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x);
SEXP do_classgets(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object, CXXR::RObject* new_class);
CXXR::quick_builtin do_colon;
SEXP do_colsum(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* X_, CXXR::RObject* m_, CXXR::RObject* n_, CXXR::RObject* na_rm_);
SEXP do_commandArgs(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_comment(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_commentgets(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* value_);
SEXP do_copyDFattr(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* xx_, CXXR::RObject* x_);
SEXP do_crossprod(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x, CXXR::RObject* y);
SEXP do_Cstack_info(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
CXXR::quick_builtin do_cum;
SEXP do_curlDownload(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlGetHeaders(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlVersion(SEXP, SEXP, SEXP, SEXP);
SEXP do_D2POSIXlt(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_date(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_debug(SEXP, SEXP, SEXP, SEXP);  // Tricky.
SEXP do_delayed(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* value_, CXXR::RObject* eval_env_, CXXR::RObject* assign_env_);
SEXP do_deparse(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* expr_, CXXR::RObject* width_cutoff_, CXXR::RObject* backtick_, CXXR::RObject* control_, CXXR::RObject* nlines_);
SEXP do_detach(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pos_);
SEXP NORET do_dfltStop(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* message_, CXXR::RObject* call_);
SEXP do_dfltWarn(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* message_, CXXR::RObject* call_);
SEXP do_diag(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* nrow_, CXXR::RObject* ncol_);
CXXR::quick_builtin do_dim;
CXXR::quick_builtin do_dimgets;
CXXR::quick_builtin do_dimnames;
CXXR::quick_builtin do_dimnamesgets;
SEXP do_dircreate(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_, CXXR::RObject* showWarnings_, CXXR::RObject* recursive_, CXXR::RObject* mode_);
SEXP do_direxists(SEXP, SEXP, SEXP, SEXP);
SEXP do_dirname(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_);
SEXP do_docall(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* what_, CXXR::RObject* args_, CXXR::RObject* envir_);
SEXP do_dotcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcallgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotCode(SEXP, SEXP, SEXP, SEXP);
SEXP do_dput(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* file_, CXXR::RObject* control_);
SEXP do_drop(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_dump(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* list_, CXXR::RObject* file_, CXXR::RObject* envir_, CXXR::RObject* opts_, CXXR::RObject* evaluate_);
CXXR::quick_builtin do_duplicated;
SEXP do_dynload(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* local_, CXXR::RObject* now_, CXXR::RObject* dots_);
SEXP do_dynunload(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_eapply(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_emptyenv(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_encoding(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_encodeString(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* width_, CXXR::RObject* quote_, CXXR::RObject* na_encode_, CXXR::RObject* justify_);
SEXP do_enc2(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject*);
SEXP do_envir(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* fun_);
SEXP do_envirgets(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* s, CXXR::RObject* env);
SEXP do_envirName(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* env_);
SEXP do_env2list(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* all_names_, CXXR::RObject* sorted_);
SEXP do_eSoftVersion(SEXP, SEXP, SEXP, SEXP);
SEXP do_External(SEXP, SEXP, SEXP, SEXP);
SEXP do_Externalgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_eval(SEXP, SEXP, SEXP, SEXP);
SEXP do_expression(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_fileaccess(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* names_, CXXR::RObject* mode_);
SEXP do_fileappend(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* file1_, CXXR::RObject* file2_);
SEXP do_filecopy(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* from_, CXXR::RObject* to_, CXXR::RObject* overwrite_, CXXR::RObject* recursive_, CXXR::RObject* copy_mode_, CXXR::RObject* copy_date_);
SEXP do_filecreate(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* dots_, CXXR::RObject* showWarnings_);
SEXP do_fileexists(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* dots_);
SEXP do_fileinfo(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* dots_, CXXR::RObject* extra_cols_);
SEXP do_filelink(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* from_, CXXR::RObject* to_);
CXXR::quick_builtin do_filepath;
SEXP do_fileremove(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* dots_);
SEXP do_filerename(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* from_, CXXR::RObject* to_);
SEXP do_fileshow(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* files_, CXXR::RObject* header_, CXXR::RObject* title_, CXXR::RObject* delete_file_, CXXR::RObject* pager_);
SEXP do_filesymlink(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* from_, CXXR::RObject* to_);
SEXP do_findinterval(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* vec_, CXXR::RObject* x_, CXXR::RObject* rightmost_closed_, CXXR::RObject* all_inside_, CXXR::RObject* left_op_);
SEXP do_first_min(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_flush(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_);
SEXP do_for(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_forceAndCall(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_format;
CXXR::quick_builtin do_formatC;
SEXP do_formatinfo(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* digits_, CXXR::RObject* nsmall_);
SEXP do_formatPOSIXlt(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* format_, CXXR::RObject* usetz_);
SEXP do_formals(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* fun_);
SEXP do_function(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_gc(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* verbose_, CXXR::RObject* reset_);
SEXP do_gcinfo(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* verbose_);
CXXR::quick_builtin do_gctime;
SEXP do_gctorture(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* on_);
SEXP do_gctorture2(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* step_, CXXR::RObject* wait_, CXXR::RObject* inhibit_release_);
CXXR::quick_builtin do_get;
SEXP do_getDllTable(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
CXXR::quick_builtin do_getVarsFromFrame;
CXXR::quick_builtin do_getenv;
SEXP do_geterrmessage(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_getlocale(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* category_);
SEXP do_getOption(SEXP, SEXP, SEXP, SEXP);
SEXP do_getRegisteredRoutines(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* info_);
CXXR::quick_builtin do_getSymbolInfo;
SEXP do_getRestart(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* i_);
SEXP do_gettext(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* dots_, CXXR::RObject* domain_);
SEXP do_getwd(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_glob(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* paths_, CXXR::RObject* dirmark_);
SEXP do_globalenv(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_grep(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pattern_, CXXR::RObject* x_, CXXR::RObject* ignore_case_, CXXR::RObject* value_, CXXR::RObject* perl_, CXXR::RObject* fixed_, CXXR::RObject* useBytes_, CXXR::RObject* invert_);
SEXP do_grepraw(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pattern_, CXXR::RObject* x_, CXXR::RObject* offset_, CXXR::RObject* ignore_case_, CXXR::RObject* fixed_, CXXR::RObject* value_, CXXR::RObject* all_, CXXR::RObject* invert_);
SEXP do_gsub(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pattern_, CXXR::RObject* replacement_, CXXR::RObject* x_, CXXR::RObject* ignore_case_, CXXR::RObject* perl_, CXXR::RObject* fixed_, CXXR::RObject* useBytes_);
SEXP do_iconv(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* from_, CXXR::RObject* to_, CXXR::RObject* sub_, CXXR::RObject* mark_, CXXR::RObject* toRaw_);
SEXP do_ICUget(SEXP, SEXP, SEXP, SEXP);
SEXP do_ICUset(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_identical;
SEXP do_if(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_inherits(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* what_, CXXR::RObject* which_);
CXXR::quick_builtin do_inspect;
SEXP do_intToUtf8(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* multiple_);
SEXP do_interactive(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_internal(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_intToBits(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
CXXR::quick_builtin do_invisible;
SEXP NORET do_invokeRestart(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* r_, CXXR::RObject* args_);
CXXR::quick_builtin do_is;
SEXP do_isatty(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_);
CXXR::quick_builtin do_isfinite;
CXXR::quick_builtin do_isinfinite;
SEXP do_islistfactor(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* recursive_);
SEXP do_isloaded(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_isna;
CXXR::quick_builtin do_isnan;
CXXR::quick_builtin do_isunsorted;
SEXP do_isvector(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* mode_);
SEXP do_lapack(SEXP, SEXP, SEXP, SEXP);
SEXP do_lapply(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_lazyLoadDBfetch(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* key_, CXXR::RObject* file_, CXXR::RObject* compressed_, CXXR::RObject* hook_);
CXXR::quick_builtin do_lazyLoadDBflush;
CXXR::quick_builtin do_lazyLoadDBinsertValue;
CXXR::quick_builtin do_length;
CXXR::quick_builtin do_lengthgets;
CXXR::quick_builtin do_lengths;
CXXR::quick_builtin do_levelsgets;
SEXP do_listdirs(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_, CXXR::RObject* full_names_, CXXR::RObject* recursive_);
SEXP do_listfiles(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_, CXXR::RObject* pattern_, CXXR::RObject* all_files_, CXXR::RObject* full_names_, CXXR::RObject* recursive_, CXXR::RObject* ignore_case_, CXXR::RObject* include_dirs_, CXXR::RObject* no_dots_);
SEXP do_list2env(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* envir_);
SEXP do_load(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* file_, CXXR::RObject* envir_);
SEXP do_loadFromConn2(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* envir_, CXXR::RObject* verbose_);
SEXP do_localeconv(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_log(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_log1arg;
CXXR::quick_builtin do_logic;
SEXP do_logic2(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_logic3(SEXP, SEXP, SEXP, SEXP);
SEXP do_ls(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* envir_, CXXR::RObject* all_names_, CXXR::RObject* sorted_);
SEXP do_l10n_info(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_makelazy(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* vars_, CXXR::RObject* vals_, CXXR::RObject* expr_, CXXR::RObject* db_, CXXR::RObject* envir_);
SEXP do_makelist(SEXP, SEXP, SEXP, SEXP);
SEXP do_makenames(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* names_, CXXR::RObject* allow__);
SEXP do_makeunique(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* names_, CXXR::RObject* sep_);
SEXP do_makevector(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* mode_, CXXR::RObject* length_);
CXXR::quick_builtin do_mapply;
CXXR::quick_builtin do_match;
SEXP do_matchcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_matprod(SEXP, SEXP, SEXP, SEXP);
SEXP do_Math2(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_matrix;
SEXP do_maxcol(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* m_, CXXR::RObject* ties_method_);
SEXP do_memoryprofile(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_merge(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* xinds_, CXXR::RObject* yinds_, CXXR::RObject* all_x_, CXXR::RObject* all_y_);
CXXR::quick_builtin do_mget;
SEXP do_missing(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_names;
CXXR::quick_builtin do_namesgets;
CXXR::quick_builtin do_nargs;
CXXR::quick_builtin do_nchar;
SEXP do_newenv(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* hash_, CXXR::RObject* parent_, CXXR::RObject* size_);
SEXP do_nextmethod(SEXP,SEXP,SEXP,SEXP);  // Special
SEXP do_ngettext(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* n_, CXXR::RObject* msg1_, CXXR::RObject* msg2_, CXXR::RObject* domain_);
CXXR::quick_builtin do_nzchar;
SEXP do_onexit(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_options(SEXP, SEXP, SEXP, SEXP);
SEXP do_order(SEXP, SEXP, SEXP, SEXP);
SEXP do_packBits(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* type_);
SEXP do_parentenv(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* env_);
SEXP do_parentenvgets(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* env_, CXXR::RObject* value_);
SEXP do_paren(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
CXXR::quick_builtin do_parentframe;
SEXP do_parse(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* file_, CXXR::RObject* n_, CXXR::RObject* text_, CXXR::RObject* prompt_, CXXR::RObject* srcfile_, CXXR::RObject* encoding_);
CXXR::quick_builtin do_paste;
SEXP do_pathexpand(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_);
SEXP do_pcre_config(SEXP, SEXP, SEXP, SEXP);
SEXP do_pmatch(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* table_, CXXR::RObject* nomatch_, CXXR::RObject* duplicates_ok_);
CXXR::quick_builtin do_pmin;
SEXP do_pos2env(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pos);
SEXP do_POSIXlt2D(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
CXXR::quick_builtin do_pretty;
SEXP do_primitive(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* name_);
CXXR::quick_builtin do_printdefault;
SEXP do_printDeferredWarnings(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
CXXR::quick_builtin do_printfunction;
SEXP do_prmatrix(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* rowlab_, CXXR::RObject* collab_, CXXR::RObject* quote_, CXXR::RObject* right_, CXXR::RObject* na_print_);
SEXP do_proctime(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_psort(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* partial_);
SEXP do_qsort(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* index_return_);
SEXP do_quit(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* save_, CXXR::RObject* status_, CXXR::RObject* runLast_);
SEXP do_quote(SEXP, SEXP, SEXP, SEXP);  // Special
extern "C" SEXP do_radixsort(SEXP, SEXP, SEXP, SEXP);
SEXP do_range(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_rank;
CXXR::quick_builtin do_rapply;
SEXP do_rawShift(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* n_);
SEXP do_rawToBits(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_rawToChar(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* multiple_);
SEXP do_readDCF(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* file_, CXXR::RObject* fields_, CXXR::RObject* keep_white_);
SEXP do_readEnviron(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_);
SEXP do_readlink(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* paths_);
SEXP do_readLines(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* n_, CXXR::RObject* ok_, CXXR::RObject* warn_, CXXR::RObject* encoding_, CXXR::RObject* skipNul_);
SEXP do_readln(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* prompt_);
SEXP do_recall(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_recordGraphics(SEXP, SEXP, SEXP, SEXP);
SEXP do_refcnt(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x);
SEXP do_regexec(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pattern_, CXXR::RObject* text_, CXXR::RObject* ignore_case_, CXXR::RObject* fixed_, CXXR::RObject* useBytes_);
SEXP do_regexpr(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pattern_, CXXR::RObject* text_, CXXR::RObject* ignore_case_, CXXR::RObject* perl_, CXXR::RObject* fixed_, CXXR::RObject* useBytes_);
SEXP do_regFinaliz(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* e_, CXXR::RObject* f_, CXXR::RObject* onexit_);
CXXR::quick_builtin do_relop;
SEXP do_remove(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* list_, CXXR::RObject* envir_, CXXR::RObject* inherits_);
SEXP do_rep(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_rep_int(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* times_);
SEXP do_rep_len(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* length_out_);
SEXP do_repeat(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_return(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_returnValue(SEXP, SEXP, SEXP, SEXP);
SEXP do_Rhome(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_RNGkind(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* kind_, CXXR::RObject* normal_kind_);
CXXR::quick_builtin do_rowsum;
CXXR::quick_builtin do_rowscols;
CXXR::quick_builtin do_S4on;
SEXP do_sample(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* size_, CXXR::RObject* replace_, CXXR::RObject* prob_);
SEXP do_sample2(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* n_, CXXR::RObject* size_);
SEXP do_save(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* list_, CXXR::RObject* file_, CXXR::RObject* ascii_, CXXR::RObject* version_, CXXR::RObject* envir_, CXXR::RObject* eval_promises_);
SEXP do_saveToConn(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* list_, CXXR::RObject* con_, CXXR::RObject* ascii_, CXXR::RObject* version_, CXXR::RObject* envir_, CXXR::RObject* eval_promises_);
SEXP do_scan(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* file_, CXXR::RObject* what_, CXXR::RObject* nmax_, CXXR::RObject* sep_, CXXR::RObject* dec_, CXXR::RObject* quote_, CXXR::RObject* skip_, CXXR::RObject* nlines_, CXXR::RObject* na_strings_, CXXR::RObject* flush_, CXXR::RObject* fill_, CXXR::RObject* strip_white_, CXXR::RObject* quiet_, CXXR::RObject* blank_lines_skip_, CXXR::RObject* multi_line_, CXXR::RObject* comment_char_, CXXR::RObject* allowEscapes_, CXXR::RObject* encoding_, CXXR::RObject* skipNul_);
SEXP do_search(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_seq(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_seq_along;
SEXP do_seq_len(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* length);
CXXR::quick_builtin do_serialize;
SEXP do_serializeToConn(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object_, CXXR::RObject* con_, CXXR::RObject* ascii_, CXXR::RObject* version_, CXXR::RObject* refhook_);
SEXP do_set(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_setS4Object(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object_, CXXR::RObject* flag_, CXXR::RObject* complete_);
SEXP do_setFileTime(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* path_, CXXR::RObject* time_);
SEXP do_setencoding(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* value_);
CXXR::quick_builtin do_setenv;
CXXR::quick_builtin do_seterrmessage;
CXXR::quick_builtin do_setmaxnumthreads;
CXXR::quick_builtin do_setnumthreads;
SEXP do_setlocale(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* category_, CXXR::RObject* locale_);
SEXP do_setseed (CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* seed_, CXXR::RObject* kind_, CXXR::RObject* normal_kind_);
SEXP do_setSessionTimeLimit(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* cpu_, CXXR::RObject* elapsed_);
SEXP do_setTimeLimit(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* cpu_, CXXR::RObject* elapsed_, CXXR::RObject* transient_);
CXXR::quick_builtin do_setwd;
SEXP do_shortRowNames(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* type_);
SEXP do_signalCondition(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* cond_, CXXR::RObject* message_, CXXR::RObject* call_);
SEXP do_sink(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* file_, CXXR::RObject* append_, CXXR::RObject* type_, CXXR::RObject* split_);
SEXP do_sinknumber(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* type_);
SEXP do_slotgets(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_sort(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* decreasing_);
SEXP do_split(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* f_);
CXXR::quick_builtin do_sprintf;
CXXR::quick_builtin do_standardGeneric;
SEXP do_startsWith(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x, CXXR::RObject* prefix);
SEXP do_stop(SEXP, SEXP, SEXP, SEXP) NORET;
SEXP do_storage_mode(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* obj, CXXR::RObject* value);
SEXP do_strrep(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x, CXXR::RObject* n);
SEXP do_strsplit(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* split_, CXXR::RObject* fixed_, CXXR::RObject* perl_, CXXR::RObject* useBytes_);
SEXP do_strptime(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* format_, CXXR::RObject* tz_);
SEXP do_strtrim(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* width_);
SEXP do_strtoi(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* base_);
SEXP do_syschmod(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* paths_, CXXR::RObject* mode_, CXXR::RObject* use_umask_);
SEXP do_sysumask(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* mode_);
SEXP do_subassign(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_subassign_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_subassign2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign3(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_subset(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_subset_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_subset2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset3(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_substitute(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_substr(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* start_, CXXR::RObject* stop_);
SEXP do_substrgets(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* start_, CXXR::RObject* stop_, CXXR::RObject* value_);
SEXP do_summary(SEXP, SEXP, SEXP, SEXP);
SEXP do_switch(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_sys;
SEXP do_sysbrowser(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* n_);
SEXP do_sysgetpid(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_systime(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_tabulate(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* bin_, CXXR::RObject* nbins_);
SEXP do_tempdir(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_tempfile(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* pattern_, CXXR::RObject* tmpdir_, CXXR::RObject* fileext_);
SEXP do_tilde(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_tolower(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_topenv(SEXP, SEXP, SEXP, SEXP);
SEXP do_trace(SEXP, SEXP, SEXP, SEXP);
SEXP do_traceOnOff(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* on_);
SEXP do_traceback(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_transpose(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
CXXR::quick_builtin do_trunc;
SEXP do_typeof(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_unclass(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object);
SEXP do_unlink(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_, CXXR::RObject* recursive_, CXXR::RObject* force_);
CXXR::quick_builtin do_unlist;
SEXP do_unserializeFromConn(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* refhook_);
SEXP do_unsetenv(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_usemethod(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_utf8ToInt(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_validEnc(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x);
SEXP do_validUTF8(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x);
SEXP do_vapply(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_version(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_warning(SEXP, SEXP, SEXP, SEXP);
SEXP do_while(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_which(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_withVisible(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_xtfrm(SEXP, SEXP, SEXP, SEXP);

CXXR::quick_builtin R_do_data_class;
SEXP R_do_set_class(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object, CXXR::RObject* klass);
SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type);

/* Connections */
SEXP do_stdin(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_stdout(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_stderr(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_writelines(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* text_, CXXR::RObject* con_, CXXR::RObject* sep_, CXXR::RObject* useBytes_);
SEXP do_readbin(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* what_, CXXR::RObject* n_, CXXR::RObject* size_, CXXR::RObject* signed_, CXXR::RObject* endian_);
SEXP do_writebin(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object_, CXXR::RObject* con_, CXXR::RObject* size_, CXXR::RObject* endian_, CXXR::RObject* useBytes_);
SEXP do_readchar(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* nchars_, CXXR::RObject* useBytes_);
SEXP do_writechar(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object_, CXXR::RObject* con_, CXXR::RObject* nchars_, CXXR::RObject* eos_, CXXR::RObject* useBytes_);
SEXP do_open(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* open_, CXXR::RObject* blocking_);
SEXP do_isopen(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* rw_);
SEXP do_isincomplete(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_);
SEXP do_isseekable(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_);
SEXP do_close(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* dots_);
SEXP do_fifo(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* description_, CXXR::RObject* open_, CXXR::RObject* blocking_, CXXR::RObject* encoding_);
SEXP do_pipe(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* description_, CXXR::RObject* open_, CXXR::RObject* encoding_);
CXXR::quick_builtin do_url;
SEXP do_gzfile(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* description_, CXXR::RObject* open_, CXXR::RObject* encoding_, CXXR::RObject* compression_);
CXXR::quick_builtin do_unz;
CXXR::quick_builtin do_seek;
SEXP do_truncate(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_);
SEXP do_pushback(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* data_, CXXR::RObject* connection_, CXXR::RObject* newLine_, CXXR::RObject* encoding_);
SEXP do_pushbacklength(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* connection_);
SEXP do_clearpushback(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* connection_);
CXXR::quick_builtin do_rawconnection;
SEXP do_rawconvalue(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_);
SEXP do_textconnection(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* nm_, CXXR::RObject* object_, CXXR::RObject* open_, CXXR::RObject* env_, CXXR::RObject* type_);
SEXP do_textconvalue(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_);
SEXP do_getconnection(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* what_);
SEXP do_getallconnections(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_sumconnection(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* object_);
SEXP do_sockconn(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* host_, CXXR::RObject* port_, CXXR::RObject* server_, CXXR::RObject* blocking_, CXXR::RObject* open_, CXXR::RObject* encoding_, CXXR::RObject* timeout_);
SEXP do_sockselect(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* socklist_, CXXR::RObject* write_, CXXR::RObject* timeout_);
SEXP do_gzcon(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* con_, CXXR::RObject* level_, CXXR::RObject* allowNonCompressed_);
SEXP do_memCompress(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* from_, CXXR::RObject* type_);
SEXP do_memDecompress(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* from_, CXXR::RObject* type_);

SEXP do_castestfun(SEXP, SEXP, SEXP, SEXP);
SEXP do_hasProvenance(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_provCommand;
SEXP do_provenance(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_provenance_graph;
SEXP do_bserialize(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_bdeserialize(SEXP, SEXP, SEXP, SEXP);  // Special

SEXP do_lockEnv(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* env_, CXXR::RObject* bindings_);
SEXP do_envIsLocked(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* env_);
SEXP do_lockBnd(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* sym_, CXXR::RObject* env_);
SEXP do_bndIsLocked(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* sym_, CXXR::RObject* env_);
SEXP do_mkActiveBnd(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* sym_, CXXR::RObject* fun_, CXXR::RObject* env_);
SEXP do_bndIsActive(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* sym_, CXXR::RObject* env_);
SEXP do_isNSEnv(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* ns_);
SEXP do_regNS(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* name_, CXXR::RObject* env_);
SEXP do_unregNS(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* nsname_);
SEXP do_getRegNS(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* name_);
SEXP do_getNSRegistry(CXXR::Expression* call, const CXXR::BuiltInFunction* op);
SEXP do_importIntoEnv(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* impenv_, CXXR::RObject* impnames_, CXXR::RObject* expenv_, CXXR::RObject* expnames_);
SEXP do_envprofile(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* env_);

SEXP do_tracemem(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);
SEXP do_retracemem(SEXP, SEXP, SEXP, SEXP);
SEXP do_untracemem(CXXR::Expression* call, const CXXR::BuiltInFunction* op, CXXR::RObject* x_);

extern "C" {
#endif  // __cplusplus


#if Win32
SEXP do_mkjunction(SEXP, SEXP, SEXP, SEXP);
SEXP do_shellexec(SEXP, SEXP, SEXP, SEXP);
SEXP do_syswhich(SEXP, SEXP, SEXP, SEXP);
SEXP do_tzone_name(SEXP, SEXP, SEXP, SEXP);
#else
SEXP do_X11(SEXP, SEXP, SEXP, SEXP);
#endif

// Functions that are defined in or called from C.
SEXP do_complex(SEXP, SEXP, SEXP, SEXP);
SEXP do_contourLines(SEXP, SEXP, SEXP, SEXP);
SEXP do_edit(SEXP, SEXP, SEXP, SEXP);
SEXP do_filechoose(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEvent(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_normalizepath(SEXP, SEXP, SEXP, SEXP);
SEXP do_polyroot(SEXP, SEXP, SEXP, SEXP);
SEXP do_saveplot(SEXP, SEXP, SEXP, SEXP);
SEXP do_setGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_sysinfo(SEXP,SEXP,SEXP,SEXP);
SEXP do_syssleep(SEXP,SEXP,SEXP,SEXP);
SEXP do_system(SEXP, SEXP, SEXP, SEXP);
SEXP do_getSnapshot(SEXP, SEXP, SEXP, SEXP);
SEXP do_playSnapshot(SEXP, SEXP, SEXP, SEXP);

#ifdef __cplusplus
}  // extern "C"
#endif  // __cplusplus

#endif /* not R_INTERNAL_H */
