/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2015  The R Core Team
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

/* Names of  .Internal(.) and .Primitive(.)  R functions
 *
 * Must all return SEXP because of CCODE in Defn.h.
 * do_math*() and do_cmathfuns are in ../main/arithmetic.h
 */

#ifndef R_INTERNAL_H
#define R_INTERNAL_H

#include "R_ext/Error.h"

#ifdef __cplusplus

namespace CXXR {
  class RObject;
  class Expression;
  class Environment;
  class PairList;

  typedef RObject*(quick_builtin)(/*const*/ Expression*,
                                  const BuiltInFunction*,
                                  Environment* env,
                                  RObject* const* args,
                                  int num_args,
                                  const PairList* tags);
#if Win32
SEXP do_mkjunction(SEXP, SEXP, SEXP, SEXP);
SEXP do_shellexec(SEXP, SEXP, SEXP, SEXP);
SEXP do_setInternet2(SEXP, SEXP, SEXP, SEXP);
SEXP do_syswhich(SEXP, SEXP, SEXP, SEXP);
SEXP do_tzone_name(SEXP, SEXP, SEXP, SEXP);
#else
SEXP do_X11(SEXP, SEXP, SEXP, SEXP);
#endif

}  // namespace CXXR

/* Function Names */

CXXR::quick_builtin do_abbrev;
SEXP do_abs(SEXP, SEXP, SEXP, SEXP); // calls do_cmathfuns
CXXR::quick_builtin do_addCondHands;
SEXP do_address(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_addRestart;
CXXR::quick_builtin do_adist;
CXXR::quick_builtin do_agrep;
CXXR::quick_builtin do_allnames;
SEXP do_anyNA(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_aperm;
CXXR::quick_builtin do_arith;
CXXR::quick_builtin do_aregexec;
SEXP do_args(SEXP, SEXP, SEXP, SEXP); // non-trivial
CXXR::quick_builtin do_array;
CXXR::quick_builtin do_asPOSIXct;
CXXR::quick_builtin do_asPOSIXlt;
CXXR::quick_builtin do_ascall;
CXXR::quick_builtin do_as_environment;
CXXR::quick_builtin do_asatomic;
CXXR::quick_builtin do_asfunction;
CXXR::quick_builtin do_assign;
CXXR::quick_builtin do_asvector;
SEXP do_AT(SEXP call, SEXP op, SEXP args, SEXP env);  // Special
CXXR::quick_builtin do_attach;
SEXP do_attr(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
SEXP do_attrgets(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_attributes;
CXXR::quick_builtin do_attributesgets;
CXXR::quick_builtin do_backsolve;
CXXR::quick_builtin do_baseenv;
CXXR::quick_builtin do_basename;
SEXP do_bcprofcounts(SEXP, SEXP, SEXP, SEXP);
SEXP do_bcprofstart(SEXP, SEXP, SEXP, SEXP);
SEXP do_bcprofstop(SEXP, SEXP, SEXP, SEXP);
SEXP do_begin(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_bincode;
SEXP do_bind(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_bindtextdomain;
CXXR::quick_builtin do_bitwise;
CXXR::quick_builtin do_body;
CXXR::quick_builtin do_bodyCode;
SEXP do_break(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_browser(SEXP, SEXP, SEXP, SEXP);  // Calls matchArgs
CXXR::quick_builtin do_builtins;
SEXP do_c(SEXP, SEXP, SEXP, SEXP);  // Calls do_c_dflt
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP); // Tricky
SEXP do_call(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_capabilities;
CXXR::quick_builtin do_capabilitiesX11;
CXXR::quick_builtin do_cat;
CXXR::quick_builtin do_charmatch;
CXXR::quick_builtin do_charToRaw;
CXXR::quick_builtin do_chartr;
CXXR::quick_builtin do_class;
CXXR::quick_builtin do_classgets;
CXXR::quick_builtin do_colon;
CXXR::quick_builtin do_colsum;
CXXR::quick_builtin do_commandArgs;
CXXR::quick_builtin do_comment;
CXXR::quick_builtin do_commentgets;
CXXR::quick_builtin do_copyDFattr;
CXXR::quick_builtin do_crc64;
CXXR::quick_builtin do_Cstack_info;
CXXR::quick_builtin do_cum;
SEXP do_curlDownload(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlGetHeaders(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlVersion(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_D2POSIXlt;
CXXR::quick_builtin do_date;
SEXP do_debug(SEXP, SEXP, SEXP, SEXP);  // Tricky.
CXXR::quick_builtin do_delayed;
CXXR::quick_builtin do_deparse;
CXXR::quick_builtin do_detach;
CXXR::quick_builtin do_dfltStop;
CXXR::quick_builtin do_dfltWarn;
CXXR::quick_builtin do_diag;
CXXR::quick_builtin do_dim;
CXXR::quick_builtin do_dimgets;
CXXR::quick_builtin do_dimnames;
CXXR::quick_builtin do_dimnamesgets;
CXXR::quick_builtin do_dircreate;
SEXP do_direxists(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_dirname;
CXXR::quick_builtin do_docall;
SEXP do_dotcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcallgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotCode(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_dput;
CXXR::quick_builtin do_drop;
CXXR::quick_builtin do_dump;
CXXR::quick_builtin do_duplicated;
CXXR::quick_builtin do_dynload;
CXXR::quick_builtin do_dynunload;
SEXP do_eapply(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_emptyenv;
CXXR::quick_builtin do_encoding;
CXXR::quick_builtin do_encodeString;
CXXR::quick_builtin do_enc2;
CXXR::quick_builtin do_envir;
CXXR::quick_builtin do_envirgets;
CXXR::quick_builtin do_envirName;
CXXR::quick_builtin do_env2list;
SEXP do_eSoftVersion(SEXP, SEXP, SEXP, SEXP);
SEXP do_External(SEXP, SEXP, SEXP, SEXP);
SEXP do_Externalgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_eval(SEXP, SEXP, SEXP, SEXP);
SEXP do_expression(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_fileaccess;
CXXR::quick_builtin do_fileappend;
CXXR::quick_builtin do_filecopy;
CXXR::quick_builtin do_filecreate;
CXXR::quick_builtin do_fileexists;
CXXR::quick_builtin do_fileinfo;
CXXR::quick_builtin do_filelink;
CXXR::quick_builtin do_filepath;
CXXR::quick_builtin do_fileremove;
CXXR::quick_builtin do_filerename;
CXXR::quick_builtin do_fileshow;
CXXR::quick_builtin do_filesymlink;
CXXR::quick_builtin do_findinterval;
CXXR::quick_builtin do_first_min;
CXXR::quick_builtin do_flush;
SEXP do_for(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_format;
CXXR::quick_builtin do_formatC;
CXXR::quick_builtin do_formatinfo;
CXXR::quick_builtin do_formatPOSIXlt;
CXXR::quick_builtin do_formals;
SEXP do_function(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_gc;
CXXR::quick_builtin do_gcinfo;
CXXR::quick_builtin do_gctime;
CXXR::quick_builtin do_gctorture;
CXXR::quick_builtin do_gctorture2;
CXXR::quick_builtin do_get;
CXXR::quick_builtin do_getDllTable;
CXXR::quick_builtin do_getVarsFromFrame;
CXXR::quick_builtin do_getenv;
CXXR::quick_builtin do_geterrmessage;
CXXR::quick_builtin do_getlocale;
SEXP do_getOption(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_getRegisteredRoutines;
CXXR::quick_builtin do_getSymbolInfo;
CXXR::quick_builtin do_getRestart;
CXXR::quick_builtin do_gettext;
CXXR::quick_builtin do_getwd;
CXXR::quick_builtin do_glob;
CXXR::quick_builtin do_globalenv;
CXXR::quick_builtin do_grep;
CXXR::quick_builtin do_grepraw;
CXXR::quick_builtin do_gsub;
CXXR::quick_builtin do_iconv;
SEXP do_ICUget(SEXP, SEXP, SEXP, SEXP);
SEXP do_ICUset(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_identical;
SEXP do_if(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_inherits;
CXXR::quick_builtin do_inspect;
CXXR::quick_builtin do_intToUtf8;
CXXR::quick_builtin do_interactive;
SEXP do_internal(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_interruptsSuspended;
CXXR::quick_builtin do_intToBits;
CXXR::quick_builtin do_invisible;
CXXR::quick_builtin do_invokeRestart NORET;
CXXR::quick_builtin do_is;
CXXR::quick_builtin do_isatty;
CXXR::quick_builtin do_isfinite;
CXXR::quick_builtin do_isinfinite;
CXXR::quick_builtin do_islistfactor;
SEXP do_isloaded(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_isna;
CXXR::quick_builtin do_isnan;
CXXR::quick_builtin do_isunsorted;
CXXR::quick_builtin do_isvector;
SEXP do_lapack(SEXP, SEXP, SEXP, SEXP);
SEXP do_lapply(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_lazyLoadDBfetch;
CXXR::quick_builtin do_lazyLoadDBflush;
CXXR::quick_builtin do_lazyLoadDBinsertValue;
CXXR::quick_builtin do_length;
CXXR::quick_builtin do_lengthgets;
SEXP do_lengths(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_levelsgets;
CXXR::quick_builtin do_listdirs;
CXXR::quick_builtin do_listfiles;
CXXR::quick_builtin do_list2env;
CXXR::quick_builtin do_load;
CXXR::quick_builtin do_loadFromConn2;
CXXR::quick_builtin do_localeconv;
SEXP do_log(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_log1arg;
CXXR::quick_builtin do_logic;
SEXP do_logic_slow(SEXP, SEXP, SEXP, SEXP); // slow version of the above, used by the bytecode interpreter.
SEXP do_logic2(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_logic3(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_ls;
CXXR::quick_builtin do_l10n_info;
CXXR::quick_builtin do_makelazy;
SEXP do_makelist(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_makenames;
CXXR::quick_builtin do_makeunique;
CXXR::quick_builtin do_makevector;
CXXR::quick_builtin do_mapply;
CXXR::quick_builtin do_match;
SEXP do_matchcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_matprod(SEXP, SEXP, SEXP, SEXP);
SEXP do_Math2(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_matrix;
CXXR::quick_builtin do_maxcol;
CXXR::quick_builtin do_memlimits;
CXXR::quick_builtin do_memoryprofile;
CXXR::quick_builtin do_merge;
CXXR::quick_builtin do_mget;
SEXP do_missing(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_names;
CXXR::quick_builtin do_namesgets;
CXXR::quick_builtin do_nargs;
CXXR::quick_builtin do_nchar;
CXXR::quick_builtin do_newenv;
SEXP do_nextmethod(SEXP,SEXP,SEXP,SEXP);  // Special
CXXR::quick_builtin do_ngettext;
CXXR::quick_builtin do_nzchar;
SEXP do_onexit(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_options(SEXP, SEXP, SEXP, SEXP);
SEXP do_order(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_packBits;
CXXR::quick_builtin do_parentenv;
CXXR::quick_builtin do_parentenvgets;
CXXR::quick_builtin do_paren;
CXXR::quick_builtin do_parentframe;
CXXR::quick_builtin do_parse;
CXXR::quick_builtin do_paste;
CXXR::quick_builtin do_pathexpand;
SEXP do_pcre_config(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_pmatch;
CXXR::quick_builtin do_pmin;
CXXR::quick_builtin do_pos2env;
CXXR::quick_builtin do_POSIXlt2D;
CXXR::quick_builtin do_pretty;
CXXR::quick_builtin do_primitive;
CXXR::quick_builtin do_printdefault;
CXXR::quick_builtin do_printDeferredWarnings;
CXXR::quick_builtin do_printfunction;
CXXR::quick_builtin do_prmatrix;
CXXR::quick_builtin do_proctime;
CXXR::quick_builtin do_psort;
CXXR::quick_builtin do_qsort;
CXXR::quick_builtin do_quit;
SEXP do_quote(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_radixsort;
CXXR::quick_builtin do_random1;
CXXR::quick_builtin do_random2;
CXXR::quick_builtin do_random3;
SEXP do_range(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_rank;
CXXR::quick_builtin do_rapply;
CXXR::quick_builtin do_rawShift;
CXXR::quick_builtin do_rawToBits;
CXXR::quick_builtin do_rawToChar;
CXXR::quick_builtin do_readDCF;
CXXR::quick_builtin do_readEnviron;
CXXR::quick_builtin do_readlink;
CXXR::quick_builtin do_readLines;
CXXR::quick_builtin do_readln;
SEXP do_recall(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_recordGraphics(SEXP, SEXP, SEXP, SEXP);
SEXP do_refcnt(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_regexec;
CXXR::quick_builtin do_regexpr;
CXXR::quick_builtin do_regFinaliz;
CXXR::quick_builtin do_relop;
CXXR::quick_builtin do_remove;
SEXP do_rep(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_rep_int;
CXXR::quick_builtin do_rep_len;
SEXP do_repeat(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_resetCondHands;
SEXP do_return(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_returnValue(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_Rhome;
CXXR::quick_builtin do_RNGkind;
CXXR::quick_builtin do_rowsum;
CXXR::quick_builtin do_rowscols;
CXXR::quick_builtin do_S4on;
CXXR::quick_builtin do_sample;
CXXR::quick_builtin do_sample2;
CXXR::quick_builtin do_save;
CXXR::quick_builtin do_saveToConn;
CXXR::quick_builtin do_scan;
CXXR::quick_builtin do_search;
SEXP do_seq(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_seq_along;
CXXR::quick_builtin do_seq_len;
CXXR::quick_builtin do_serialize;
CXXR::quick_builtin do_serializeToConn;
SEXP do_set(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_setS4Object;
CXXR::quick_builtin do_setFileTime;
CXXR::quick_builtin do_setencoding;
CXXR::quick_builtin do_setenv;
CXXR::quick_builtin do_seterrmessage;
CXXR::quick_builtin do_setmaxnumthreads;
CXXR::quick_builtin do_setnumthreads;
CXXR::quick_builtin do_setlocale;
CXXR::quick_builtin do_setseed;
CXXR::quick_builtin do_setSessionTimeLimit;
CXXR::quick_builtin do_setTimeLimit;
CXXR::quick_builtin do_setwd;
CXXR::quick_builtin do_shortRowNames;
CXXR::quick_builtin do_signalCondition;
CXXR::quick_builtin do_sink;
CXXR::quick_builtin do_sinknumber;
CXXR::quick_builtin do_sort;
CXXR::quick_builtin do_split;
CXXR::quick_builtin do_sprintf;
CXXR::quick_builtin do_standardGeneric;
SEXP do_stop(SEXP, SEXP, SEXP, SEXP) NORET;
CXXR::quick_builtin do_storage_mode;
CXXR::quick_builtin do_strsplit;
CXXR::quick_builtin do_strptime;
CXXR::quick_builtin do_strtrim;
CXXR::quick_builtin do_strtoi;
CXXR::quick_builtin do_syschmod;
CXXR::quick_builtin do_sysumask;
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
CXXR::quick_builtin do_substr;
CXXR::quick_builtin do_substrgets;
SEXP do_summary(SEXP, SEXP, SEXP, SEXP);
SEXP do_switch(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_sys;
CXXR::quick_builtin do_sysbrowser;
CXXR::quick_builtin do_sysgetpid;
CXXR::quick_builtin do_systime;
CXXR::quick_builtin do_tabulate;
CXXR::quick_builtin do_tempdir;
CXXR::quick_builtin do_tempfile;
SEXP do_tilde(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_tolower;
SEXP do_topenv(SEXP, SEXP, SEXP, SEXP);
SEXP do_trace(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_traceOnOff;
CXXR::quick_builtin do_traceback;
CXXR::quick_builtin do_transpose;
CXXR::quick_builtin do_trunc;
CXXR::quick_builtin do_typeof;
CXXR::quick_builtin do_unclass;
CXXR::quick_builtin do_unlink;
CXXR::quick_builtin do_unlist;
CXXR::quick_builtin do_unserializeFromConn;
CXXR::quick_builtin do_unsetenv;
SEXP do_usemethod(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_utf8ToInt;
SEXP do_vapply(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_version;
SEXP do_warning(SEXP, SEXP, SEXP, SEXP);
SEXP do_while(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_which;
SEXP do_withVisible(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_xtfrm(SEXP, SEXP, SEXP, SEXP);

CXXR::quick_builtin R_do_data_class;
CXXR::quick_builtin R_do_set_class;
SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type);

/* bytecode */
CXXR::quick_builtin do_mkcode;
CXXR::quick_builtin do_bcclose;
CXXR::quick_builtin do_is_builtin_internal;
CXXR::quick_builtin do_disassemble;
CXXR::quick_builtin do_bcversion;
CXXR::quick_builtin do_loadfile;
CXXR::quick_builtin do_savefile;
CXXR::quick_builtin do_growconst;
CXXR::quick_builtin do_putconst;
CXXR::quick_builtin do_getconst;
CXXR::quick_builtin do_enablejit;
CXXR::quick_builtin do_compilepkgs;

/* Connections */
CXXR::quick_builtin do_stdin;
CXXR::quick_builtin do_stdout;
CXXR::quick_builtin do_stderr;
CXXR::quick_builtin do_writelines;
CXXR::quick_builtin do_readbin;
CXXR::quick_builtin do_writebin;
CXXR::quick_builtin do_readchar;
CXXR::quick_builtin do_writechar;
CXXR::quick_builtin do_open;
CXXR::quick_builtin do_isopen;
CXXR::quick_builtin do_isincomplete;
CXXR::quick_builtin do_isseekable;
CXXR::quick_builtin do_close;
CXXR::quick_builtin do_fifo;
CXXR::quick_builtin do_pipe;
CXXR::quick_builtin do_url;
CXXR::quick_builtin do_gzfile;
CXXR::quick_builtin do_unz;
CXXR::quick_builtin do_seek;
CXXR::quick_builtin do_truncate;
CXXR::quick_builtin do_pushback;
CXXR::quick_builtin do_pushbacklength;
CXXR::quick_builtin do_clearpushback;
CXXR::quick_builtin do_rawconnection;
CXXR::quick_builtin do_rawconvalue;
CXXR::quick_builtin do_textconnection;
CXXR::quick_builtin do_textconvalue;
CXXR::quick_builtin do_getconnection;
CXXR::quick_builtin do_getallconnections;
CXXR::quick_builtin do_sumconnection;
CXXR::quick_builtin do_sockconn;
CXXR::quick_builtin do_sockselect;
CXXR::quick_builtin do_gzcon;
CXXR::quick_builtin do_memCompress;
CXXR::quick_builtin do_memDecompress;

SEXP do_castestfun(SEXP, SEXP, SEXP, SEXP);
SEXP do_hasProvenance(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_provCommand;
SEXP do_provenance(SEXP, SEXP, SEXP, SEXP);  // Special
CXXR::quick_builtin do_provenance_graph;
SEXP do_bserialize(SEXP, SEXP, SEXP, SEXP);  // Special
SEXP do_bdeserialize(SEXP, SEXP, SEXP, SEXP);  // Special

CXXR::quick_builtin do_lockEnv;
CXXR::quick_builtin do_envIsLocked;
CXXR::quick_builtin do_lockBnd;
CXXR::quick_builtin do_bndIsLocked;
CXXR::quick_builtin do_mkActiveBnd;
CXXR::quick_builtin do_bndIsActive;
CXXR::quick_builtin do_mkUnbound;
CXXR::quick_builtin do_isNSEnv;
CXXR::quick_builtin do_regNS;
CXXR::quick_builtin do_unregNS;
CXXR::quick_builtin do_getRegNS;
CXXR::quick_builtin do_getNSRegistry;
CXXR::quick_builtin do_importIntoEnv;
CXXR::quick_builtin do_envprofile;

CXXR::quick_builtin do_tracemem;
SEXP do_retracemem(SEXP, SEXP, SEXP, SEXP);
CXXR::quick_builtin do_untracemem;

extern "C" {
#endif  // __cplusplus

// Functions that are defined in or called from C.

#if Win32
SEXP do_mkjun3ction(SEXP, SEXP, SEXP, SEXP);
SEXP do_shellexec(SEXP, SEXP, SEXP, SEXP);
SEXP do_setInternet2(SEXP, SEXP, SEXP, SEXP);
SEXP do_syswhich(SEXP, SEXP, SEXP, SEXP);
#else
SEXP do_X11(SEXP, SEXP, SEXP, SEXP);
#endif

SEXP do_complex(SEXP, SEXP, SEXP, SEXP);
SEXP do_contourLines(SEXP, SEXP, SEXP, SEXP);
SEXP do_edit(SEXP, SEXP, SEXP, SEXP);
SEXP do_filechoose(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEvent(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_machine(SEXP, SEXP, SEXP, SEXP);
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
