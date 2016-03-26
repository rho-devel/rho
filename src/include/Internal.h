/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2016  The R Core Team
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

/* Names of  .Internal(.) and .Primitive(.)  R functions
 *
 * Must all return SEXP because of CCODE in Defn.h.
 * do_math*() and do_cmathfuns are in ../main/arithmetic.h
 */

#ifndef R_INTERNAL_H
#define R_INTERNAL_H

#include "R_ext/Error.h"
#include "rho/BuiltInFunction.hpp"
#include "rho/RObject.hpp"
#include "rho/Expression.hpp"
#include "rho/Environment.hpp"
#include "rho/PairList.hpp"

#ifdef __cplusplus

namespace rho {
  typedef RObject*(quick_builtin)(Expression*,
                                  const BuiltInFunction*,
                                  Environment* env,
                                  RObject* const* args,
                                  int num_args,
                                  const PairList* tags);
}  // namespace rho

/* Function Names */

SEXP do_abbrev(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* these_, rho::RObject* minlength_, rho::RObject* use_classes_);
SEXP do_abs(SEXP, SEXP, SEXP, SEXP); // calls do_cmathfuns
rho::quick_builtin do_addCondHands;
SEXP do_address(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x);
SEXP do_addRestart(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* restart_);
SEXP do_adist(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* y_, rho::RObject* costs_, rho::RObject* counts_, rho::RObject* fixed_, rho::RObject* partial_, rho::RObject* ignore_case_, rho::RObject* useBytes_);
rho::quick_builtin do_agrep;
SEXP do_allnames(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* expr_, rho::RObject* functions_, rho::RObject* max_names_, rho::RObject* unique_);
SEXP do_anyNA(SEXP, SEXP, SEXP, SEXP);
SEXP do_aperm(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* a_, rho::RObject* perm_, rho::RObject* resize_);
rho::quick_builtin do_arith;
rho::quick_builtin do_aregexec;
SEXP do_args(SEXP, SEXP, SEXP, SEXP); // non-trivial
SEXP do_array(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* data_, rho::RObject* dim_, rho::RObject* dimnames_);
SEXP do_asPOSIXct(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* tz_);
SEXP do_asPOSIXlt(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* tz_);
SEXP do_ascall(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* args);
rho::quick_builtin do_as_environment;
rho::quick_builtin do_asatomic;
SEXP do_asfunction(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* envir_);
SEXP do_assign(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* value_, rho::RObject* envir_, rho::RObject* inherits_);
rho::quick_builtin do_asvector;
SEXP do_asCharacterFactor(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x);
SEXP do_AT(SEXP call, SEXP op, SEXP args, SEXP env);  // Special
SEXP do_attach(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* what_, rho::RObject* pos_, rho::RObject* name_);
SEXP do_attr(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
SEXP do_attrgets(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
SEXP do_attributes(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x);
SEXP do_attributesgets(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object, rho::RObject* attrs);
SEXP do_backsolve(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* r_, rho::RObject* x_, rho::RObject* k_, rho::RObject* upper_tri_, rho::RObject* transpose_);
SEXP do_baseenv(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_basename(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_);
SEXP do_bcprofcounts(SEXP, SEXP, SEXP, SEXP);
SEXP do_bcprofstart(SEXP, SEXP, SEXP, SEXP);
SEXP do_bcprofstop(SEXP, SEXP, SEXP, SEXP);
SEXP do_begin(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_bincode(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* breaks_, rho::RObject* right_, rho::RObject* include_lowest_);
SEXP do_bind(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_bindtextdomain(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* domain_, rho::RObject* dirname_);
rho::quick_builtin do_bitwise;
SEXP do_body(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* fun_);
SEXP do_break(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_browser(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
SEXP do_builtins(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* internal_);
SEXP do_c(SEXP, SEXP, SEXP, SEXP);  // Calls do_c_dflt
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP); // Tricky
SEXP do_call(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_capabilities(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_capabilitiesX11(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_cat(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* dots_, rho::RObject* file_, rho::RObject* sep_, rho::RObject* fill_, rho::RObject* labels_, rho::RObject* append_);
SEXP do_charmatch(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* table_, rho::RObject* nomatch_);
SEXP do_charToRaw(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_chartr(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* old_, rho::RObject* new_, rho::RObject* x_);
SEXP do_class(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x);
SEXP do_classgets(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object, rho::RObject* new_class);
rho::quick_builtin do_colon;
SEXP do_colsum(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* X_, rho::RObject* m_, rho::RObject* n_, rho::RObject* na_rm_);
SEXP do_commandArgs(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_comment(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_commentgets(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* value_);
SEXP do_copyDFattr(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* xx_, rho::RObject* x_);
SEXP do_crossprod(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x, rho::RObject* y);
SEXP do_Cstack_info(rho::Expression* call, const rho::BuiltInFunction* op);
rho::quick_builtin do_cum;
SEXP do_curlDownload(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlGetHeaders(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlVersion(SEXP, SEXP, SEXP, SEXP);
SEXP do_D2POSIXlt(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_date(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_debug(SEXP, SEXP, SEXP, SEXP);  // Tricky.
SEXP do_delayed(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* value_, rho::RObject* eval_env_, rho::RObject* assign_env_);
SEXP do_deparse(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* expr_, rho::RObject* width_cutoff_, rho::RObject* backtick_, rho::RObject* control_, rho::RObject* nlines_);
SEXP do_detach(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pos_);
SEXP NORET do_dfltStop(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* message_, rho::RObject* call_);
SEXP do_dfltWarn(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* message_, rho::RObject* call_);
SEXP do_diag(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* nrow_, rho::RObject* ncol_);
rho::quick_builtin do_dim;
rho::quick_builtin do_dimgets;
rho::quick_builtin do_dimnames;
rho::quick_builtin do_dimnamesgets;
SEXP do_dircreate(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_, rho::RObject* showWarnings_, rho::RObject* recursive_, rho::RObject* mode_);
SEXP do_direxists(SEXP, SEXP, SEXP, SEXP);
SEXP do_dirname(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_);
SEXP do_docall(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* what_, rho::RObject* args_, rho::RObject* envir_);
SEXP do_dotcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcallgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotCode(SEXP, SEXP, SEXP, SEXP);
SEXP do_dput(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* file_, rho::RObject* control_);
SEXP do_drop(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_dump(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* list_, rho::RObject* file_, rho::RObject* envir_, rho::RObject* opts_, rho::RObject* evaluate_);
rho::quick_builtin do_duplicated;
SEXP do_dynload(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* local_, rho::RObject* now_, rho::RObject* dots_);
SEXP do_dynunload(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_eapply(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_emptyenv(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_encoding(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_encodeString(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* width_, rho::RObject* quote_, rho::RObject* na_encode_, rho::RObject* justify_);
SEXP do_enc2(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject*);
SEXP do_envir(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* fun_);
SEXP do_envirgets(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* s, rho::RObject* env);
SEXP do_envirName(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* env_);
SEXP do_env2list(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* all_names_, rho::RObject* sorted_);
SEXP do_eSoftVersion(SEXP, SEXP, SEXP, SEXP);
SEXP do_External(SEXP, SEXP, SEXP, SEXP);
SEXP do_Externalgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_eval(SEXP, SEXP, SEXP, SEXP);
SEXP do_expression(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_fileaccess(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* names_, rho::RObject* mode_);
SEXP do_fileappend(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* file1_, rho::RObject* file2_);
SEXP do_filecopy(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* from_, rho::RObject* to_, rho::RObject* overwrite_, rho::RObject* recursive_, rho::RObject* copy_mode_, rho::RObject* copy_date_);
SEXP do_filecreate(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* dots_, rho::RObject* showWarnings_);
SEXP do_fileexists(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* dots_);
SEXP do_fileinfo(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* dots_, rho::RObject* extra_cols_);
SEXP do_filelink(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* from_, rho::RObject* to_);
rho::quick_builtin do_filepath;
SEXP do_fileremove(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* dots_);
SEXP do_filerename(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* from_, rho::RObject* to_);
SEXP do_fileshow(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* files_, rho::RObject* header_, rho::RObject* title_, rho::RObject* delete_file_, rho::RObject* pager_);
SEXP do_filesymlink(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* from_, rho::RObject* to_);
SEXP do_findinterval(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* vec_, rho::RObject* x_, rho::RObject* rightmost_closed_, rho::RObject* all_inside_, rho::RObject* left_op_);
SEXP do_first_min(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_flush(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_);
SEXP do_for(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_forceAndCall(SEXP, SEXP, SEXP, SEXP);
rho::quick_builtin do_format;
rho::quick_builtin do_formatC;
SEXP do_formatinfo(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* digits_, rho::RObject* nsmall_);
SEXP do_formatPOSIXlt(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* format_, rho::RObject* usetz_);
SEXP do_formals(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* fun_);
SEXP do_function(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_gc(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* verbose_, rho::RObject* reset_);
SEXP do_gcinfo(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* verbose_);
rho::quick_builtin do_gctime;
SEXP do_gctorture(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* on_);
SEXP do_gctorture2(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* step_, rho::RObject* wait_, rho::RObject* inhibit_release_);
rho::quick_builtin do_get;
SEXP do_getDllTable(rho::Expression* call, const rho::BuiltInFunction* op);
rho::quick_builtin do_getVarsFromFrame;
rho::quick_builtin do_getenv;
SEXP do_geterrmessage(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_getlocale(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* category_);
SEXP do_getOption(SEXP, SEXP, SEXP, SEXP);
SEXP do_getRegisteredRoutines(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* info_);
rho::quick_builtin do_getSymbolInfo;
SEXP do_getRestart(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* i_);
SEXP do_gettext(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* dots_, rho::RObject* domain_);
SEXP do_getwd(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_glob(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* paths_, rho::RObject* dirmark_);
SEXP do_globalenv(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_grep(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pattern_, rho::RObject* x_, rho::RObject* ignore_case_, rho::RObject* value_, rho::RObject* perl_, rho::RObject* fixed_, rho::RObject* useBytes_, rho::RObject* invert_);
SEXP do_grepraw(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pattern_, rho::RObject* x_, rho::RObject* offset_, rho::RObject* ignore_case_, rho::RObject* fixed_, rho::RObject* value_, rho::RObject* all_, rho::RObject* invert_);
SEXP do_gsub(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pattern_, rho::RObject* replacement_, rho::RObject* x_, rho::RObject* ignore_case_, rho::RObject* perl_, rho::RObject* fixed_, rho::RObject* useBytes_);
SEXP do_iconv(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* from_, rho::RObject* to_, rho::RObject* sub_, rho::RObject* mark_, rho::RObject* toRaw_);
SEXP do_ICUget(SEXP, SEXP, SEXP, SEXP);
SEXP do_ICUset(SEXP, SEXP, SEXP, SEXP);
rho::quick_builtin do_identical;
SEXP do_if(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_inherits(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* what_, rho::RObject* which_);
rho::quick_builtin do_inspect;
SEXP do_intToUtf8(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* multiple_);
SEXP do_interactive(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_internal(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_intToBits(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
rho::quick_builtin do_invisible;
SEXP NORET do_invokeRestart(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* r_, rho::RObject* args_);
rho::quick_builtin do_is;
SEXP do_isatty(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_);
rho::quick_builtin do_isfinite;
rho::quick_builtin do_isinfinite;
SEXP do_islistfactor(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* recursive_);
SEXP do_isloaded(SEXP, SEXP, SEXP, SEXP);
rho::quick_builtin do_isna;
rho::quick_builtin do_isnan;
rho::quick_builtin do_isunsorted;
SEXP do_isvector(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* mode_);
SEXP do_lapack(SEXP, SEXP, SEXP, SEXP);
SEXP do_lapply(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_lazyLoadDBfetch(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* key_, rho::RObject* file_, rho::RObject* compressed_, rho::RObject* hook_);
rho::quick_builtin do_lazyLoadDBflush;
rho::quick_builtin do_lazyLoadDBinsertValue;
rho::quick_builtin do_length;
rho::quick_builtin do_lengthgets;
rho::quick_builtin do_lengths;
rho::quick_builtin do_levelsgets;
SEXP do_listdirs(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_, rho::RObject* full_names_, rho::RObject* recursive_);
SEXP do_listfiles(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_, rho::RObject* pattern_, rho::RObject* all_files_, rho::RObject* full_names_, rho::RObject* recursive_, rho::RObject* ignore_case_, rho::RObject* include_dirs_, rho::RObject* no_dots_);
SEXP do_list2env(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* envir_);
SEXP do_load(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* file_, rho::RObject* envir_);
SEXP do_loadFromConn2(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* envir_, rho::RObject* verbose_);
SEXP do_localeconv(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_log(SEXP, SEXP, SEXP, SEXP);  // Special
rho::quick_builtin do_log1arg;
rho::quick_builtin do_logic;
SEXP do_logic2(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_logic3(SEXP, SEXP, SEXP, SEXP);
SEXP do_ls(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* envir_, rho::RObject* all_names_, rho::RObject* sorted_);
SEXP do_l10n_info(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_makelazy(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* vars_, rho::RObject* vals_, rho::RObject* expr_, rho::RObject* db_, rho::RObject* envir_);
SEXP do_makelist(SEXP, SEXP, SEXP, SEXP);
SEXP do_makenames(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* names_, rho::RObject* allow__);
SEXP do_makeunique(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* names_, rho::RObject* sep_);
SEXP do_makevector(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* mode_, rho::RObject* length_);
rho::quick_builtin do_mapply;
rho::quick_builtin do_match;
SEXP do_matchcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_matprod(SEXP, SEXP, SEXP, SEXP);
SEXP do_Math2(SEXP, SEXP, SEXP, SEXP);  // Special
rho::quick_builtin do_matrix;
SEXP do_maxcol(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* m_, rho::RObject* ties_method_);
SEXP do_memoryprofile(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_merge(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* xinds_, rho::RObject* yinds_, rho::RObject* all_x_, rho::RObject* all_y_);
rho::quick_builtin do_mget;
SEXP do_missing(SEXP, SEXP, SEXP, SEXP);  // Special
rho::quick_builtin do_names;
rho::quick_builtin do_namesgets;
rho::quick_builtin do_nargs;
rho::quick_builtin do_nchar;
SEXP do_newenv(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* hash_, rho::RObject* parent_, rho::RObject* size_);
SEXP do_nextmethod(SEXP,SEXP,SEXP,SEXP);  // Special
SEXP do_ngettext(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* n_, rho::RObject* msg1_, rho::RObject* msg2_, rho::RObject* domain_);
rho::quick_builtin do_nzchar;
SEXP do_onexit(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_options(SEXP, SEXP, SEXP, SEXP);
SEXP do_order(SEXP, SEXP, SEXP, SEXP);
SEXP do_packBits(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* type_);
SEXP do_parentenv(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* env_);
SEXP do_parentenvgets(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* env_, rho::RObject* value_);
SEXP do_paren(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
rho::quick_builtin do_parentframe;
SEXP do_parse(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* file_, rho::RObject* n_, rho::RObject* text_, rho::RObject* prompt_, rho::RObject* srcfile_, rho::RObject* encoding_);
rho::quick_builtin do_paste;
SEXP do_pathexpand(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_);
SEXP do_pcre_config(SEXP, SEXP, SEXP, SEXP);
SEXP do_pmatch(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* table_, rho::RObject* nomatch_, rho::RObject* duplicates_ok_);
rho::quick_builtin do_pmin;
SEXP do_pos2env(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pos);
SEXP do_POSIXlt2D(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
rho::quick_builtin do_pretty;
SEXP do_primitive(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* name_);
rho::quick_builtin do_printdefault;
SEXP do_printDeferredWarnings(rho::Expression* call, const rho::BuiltInFunction* op);
rho::quick_builtin do_printfunction;
SEXP do_prmatrix(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* rowlab_, rho::RObject* collab_, rho::RObject* quote_, rho::RObject* right_, rho::RObject* na_print_);
SEXP do_proctime(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_psort(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* partial_);
SEXP do_qsort(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* index_return_);
SEXP do_quit(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* save_, rho::RObject* status_, rho::RObject* runLast_);
SEXP do_quote(SEXP, SEXP, SEXP, SEXP);  // Special
extern "C" SEXP do_radixsort(SEXP, SEXP, SEXP, SEXP);
SEXP do_range(SEXP, SEXP, SEXP, SEXP);
rho::quick_builtin do_rank;
rho::quick_builtin do_rapply;
SEXP do_rawShift(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* n_);
SEXP do_rawToBits(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_rawToChar(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* multiple_);
SEXP do_readDCF(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* file_, rho::RObject* fields_, rho::RObject* keep_white_);
SEXP do_readEnviron(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_);
SEXP do_readlink(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* paths_);
SEXP do_readLines(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* n_, rho::RObject* ok_, rho::RObject* warn_, rho::RObject* encoding_, rho::RObject* skipNul_);
SEXP do_readln(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* prompt_);
SEXP do_recall(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_recordGraphics(SEXP, SEXP, SEXP, SEXP);
SEXP do_refcnt(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x);
SEXP do_regexec(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pattern_, rho::RObject* text_, rho::RObject* ignore_case_, rho::RObject* fixed_, rho::RObject* useBytes_);
SEXP do_regexpr(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pattern_, rho::RObject* text_, rho::RObject* ignore_case_, rho::RObject* perl_, rho::RObject* fixed_, rho::RObject* useBytes_);
SEXP do_regFinaliz(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* e_, rho::RObject* f_, rho::RObject* onexit_);
rho::quick_builtin do_relop;
SEXP do_remove(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* list_, rho::RObject* envir_, rho::RObject* inherits_);
SEXP do_rep(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_rep_int(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* times_);
SEXP do_rep_len(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* length_out_);
SEXP do_repeat(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_return(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_returnValue(SEXP, SEXP, SEXP, SEXP);
SEXP do_Rhome(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_RNGkind(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* kind_, rho::RObject* normal_kind_);
rho::quick_builtin do_rowsum;
rho::quick_builtin do_rowscols;
rho::quick_builtin do_S4on;
SEXP do_sample(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* size_, rho::RObject* replace_, rho::RObject* prob_);
SEXP do_sample2(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* n_, rho::RObject* size_);
SEXP do_save(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* list_, rho::RObject* file_, rho::RObject* ascii_, rho::RObject* version_, rho::RObject* envir_, rho::RObject* eval_promises_);
SEXP do_saveToConn(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* list_, rho::RObject* con_, rho::RObject* ascii_, rho::RObject* version_, rho::RObject* envir_, rho::RObject* eval_promises_);
SEXP do_scan(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* file_, rho::RObject* what_, rho::RObject* nmax_, rho::RObject* sep_, rho::RObject* dec_, rho::RObject* quote_, rho::RObject* skip_, rho::RObject* nlines_, rho::RObject* na_strings_, rho::RObject* flush_, rho::RObject* fill_, rho::RObject* strip_white_, rho::RObject* quiet_, rho::RObject* blank_lines_skip_, rho::RObject* multi_line_, rho::RObject* comment_char_, rho::RObject* allowEscapes_, rho::RObject* encoding_, rho::RObject* skipNul_);
SEXP do_search(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_seq(SEXP, SEXP, SEXP, SEXP);
rho::quick_builtin do_seq_along;
SEXP do_seq_len(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* length);
rho::quick_builtin do_serialize;
SEXP do_serializeToConn(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object_, rho::RObject* con_, rho::RObject* ascii_, rho::RObject* version_, rho::RObject* refhook_);
SEXP do_set(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_setS4Object(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object_, rho::RObject* flag_, rho::RObject* complete_);
SEXP do_setFileTime(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_, rho::RObject* time_);
SEXP do_setencoding(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* value_);
rho::quick_builtin do_setenv;
rho::quick_builtin do_seterrmessage;
rho::quick_builtin do_setmaxnumthreads;
rho::quick_builtin do_setnumthreads;
SEXP do_setlocale(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* category_, rho::RObject* locale_);
SEXP do_setseed (rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* seed_, rho::RObject* kind_, rho::RObject* normal_kind_);
SEXP do_setSessionTimeLimit(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* cpu_, rho::RObject* elapsed_);
SEXP do_setTimeLimit(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* cpu_, rho::RObject* elapsed_, rho::RObject* transient_);
rho::quick_builtin do_setwd;
SEXP do_shortRowNames(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* type_);
SEXP do_signalCondition(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* cond_, rho::RObject* message_, rho::RObject* call_);
SEXP do_sink(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* file_, rho::RObject* append_, rho::RObject* type_, rho::RObject* split_);
SEXP do_sinknumber(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* type_);
SEXP do_slotgets(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_sort(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* decreasing_);
SEXP do_split(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* f_);
rho::quick_builtin do_sprintf;
rho::quick_builtin do_standardGeneric;
SEXP do_startsWith(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x, rho::RObject* prefix);
SEXP do_stop(SEXP, SEXP, SEXP, SEXP) NORET;
SEXP do_storage_mode(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* obj, rho::RObject* value);
SEXP do_strrep(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x, rho::RObject* n);
SEXP do_strsplit(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* split_, rho::RObject* fixed_, rho::RObject* perl_, rho::RObject* useBytes_);
SEXP do_strptime(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* format_, rho::RObject* tz_);
SEXP do_strtrim(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* width_);
SEXP do_strtoi(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* base_);
SEXP do_syschmod(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* paths_, rho::RObject* mode_, rho::RObject* use_umask_);
SEXP do_sysumask(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* mode_);
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
SEXP do_substr(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* start_, rho::RObject* stop_);
SEXP do_substrgets(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* start_, rho::RObject* stop_, rho::RObject* value_);
SEXP do_summary(SEXP, SEXP, SEXP, SEXP);
SEXP do_switch(SEXP, SEXP, SEXP, SEXP);  // Special
rho::quick_builtin do_sys;
SEXP do_sysbrowser(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* n_);
SEXP do_sysgetpid(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_systime(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_tabulate(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* bin_, rho::RObject* nbins_);
SEXP do_tempdir(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_tempfile(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pattern_, rho::RObject* tmpdir_, rho::RObject* fileext_);
SEXP do_tilde(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_tolower(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_topenv(SEXP, SEXP, SEXP, SEXP);
SEXP do_trace(SEXP, SEXP, SEXP, SEXP);
SEXP do_traceOnOff(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* on_);
SEXP do_traceback(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_transpose(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
rho::quick_builtin do_trunc;
SEXP do_typeof(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_unclass(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object);
SEXP do_unlink(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* recursive_, rho::RObject* force_);
rho::quick_builtin do_unlist;
SEXP do_unserializeFromConn(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* refhook_);
SEXP do_unsetenv(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_usemethod(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_utf8ToInt(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_validEnc(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x);
SEXP do_validUTF8(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x);
SEXP do_vapply(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_version(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_warning(SEXP, SEXP, SEXP, SEXP);
SEXP do_while(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_which(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_withVisible(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_xtfrm(SEXP, SEXP, SEXP, SEXP);

rho::quick_builtin R_do_data_class;
SEXP R_do_set_class(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object, rho::RObject* klass);
SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type);

/* Connections */
SEXP do_stdin(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_stdout(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_stderr(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_writelines(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* text_, rho::RObject* con_, rho::RObject* sep_, rho::RObject* useBytes_);
SEXP do_readbin(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* what_, rho::RObject* n_, rho::RObject* size_, rho::RObject* signed_, rho::RObject* endian_);
SEXP do_writebin(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object_, rho::RObject* con_, rho::RObject* size_, rho::RObject* endian_, rho::RObject* useBytes_);
SEXP do_readchar(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* nchars_, rho::RObject* useBytes_);
SEXP do_writechar(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object_, rho::RObject* con_, rho::RObject* nchars_, rho::RObject* eos_, rho::RObject* useBytes_);
SEXP do_open(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* open_, rho::RObject* blocking_);
SEXP do_isopen(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* rw_);
SEXP do_isincomplete(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_);
SEXP do_isseekable(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_);
SEXP do_close(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* dots_);
SEXP do_fifo(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* description_, rho::RObject* open_, rho::RObject* blocking_, rho::RObject* encoding_);
SEXP do_pipe(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* description_, rho::RObject* open_, rho::RObject* encoding_);
rho::quick_builtin do_url;
SEXP do_gzfile(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* description_, rho::RObject* open_, rho::RObject* encoding_, rho::RObject* compression_);
rho::quick_builtin do_unz;
rho::quick_builtin do_seek;
SEXP do_truncate(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_);
SEXP do_pushback(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* data_, rho::RObject* connection_, rho::RObject* newLine_, rho::RObject* encoding_);
SEXP do_pushbacklength(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* connection_);
SEXP do_clearpushback(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* connection_);
rho::quick_builtin do_rawconnection;
SEXP do_rawconvalue(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_);
SEXP do_textconnection(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* nm_, rho::RObject* object_, rho::RObject* open_, rho::RObject* env_, rho::RObject* type_);
SEXP do_textconvalue(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_);
SEXP do_getconnection(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* what_);
SEXP do_getallconnections(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_sumconnection(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* object_);
SEXP do_sockconn(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* host_, rho::RObject* port_, rho::RObject* server_, rho::RObject* blocking_, rho::RObject* open_, rho::RObject* encoding_, rho::RObject* timeout_);
SEXP do_sockselect(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* socklist_, rho::RObject* write_, rho::RObject* timeout_);
SEXP do_gzcon(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* con_, rho::RObject* level_, rho::RObject* allowNonCompressed_);
SEXP do_memCompress(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* from_, rho::RObject* type_);
SEXP do_memDecompress(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* from_, rho::RObject* type_);

SEXP do_castestfun(SEXP, SEXP, SEXP, SEXP);
SEXP do_hasProvenance(SEXP, SEXP, SEXP, SEXP);
rho::quick_builtin do_provCommand;
SEXP do_provenance(SEXP, SEXP, SEXP, SEXP);  // Special
rho::quick_builtin do_provenance_graph;
SEXP do_bserialize(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_bdeserialize(SEXP, SEXP, SEXP, SEXP);  // Special

SEXP do_lockEnv(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* env_, rho::RObject* bindings_);
SEXP do_envIsLocked(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* env_);
SEXP do_lockBnd(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* sym_, rho::RObject* env_);
SEXP do_bndIsLocked(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* sym_, rho::RObject* env_);
SEXP do_mkActiveBnd(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* sym_, rho::RObject* fun_, rho::RObject* env_);
SEXP do_bndIsActive(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* sym_, rho::RObject* env_);
SEXP do_isNSEnv(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* ns_);
SEXP do_regNS(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* name_, rho::RObject* env_);
SEXP do_unregNS(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* nsname_);
SEXP do_getRegNS(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* name_);
SEXP do_getNSRegistry(rho::Expression* call, const rho::BuiltInFunction* op);
SEXP do_importIntoEnv(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* impenv_, rho::RObject* impnames_, rho::RObject* expenv_, rho::RObject* expnames_);
SEXP do_envprofile(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* env_);

SEXP do_tracemem(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);
SEXP do_retracemem(SEXP, SEXP, SEXP, SEXP);
SEXP do_untracemem(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_);

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
