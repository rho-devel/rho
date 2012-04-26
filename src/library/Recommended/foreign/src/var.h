/* PSPP - computes sample statistics.
   Copyright (C) 1997-9, 2000 Free Software Foundation, Inc.
   Written by Ben Pfaff <blp@gnu.org>.
   Modified 2000 Saikat DebRoy <saikat@stat.wisc.edu>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, a copy is available at
   http://www.r-project.org/Licenses/
*/

#if !var_h
#define var_h 1

#include "format.h"

/* Values. */

/* Definition of the max length of a short string value, generally
   eight characters.  */
#define MAX_SHORT_STRING 8 // ((SIZEOF_DOUBLE)>=8 ? ((SIZEOF_DOUBLE)+1)/2*2 : 8)
#define MIN_LONG_STRING (MAX_SHORT_STRING+1)

/* FYI: It is a bad situation if sizeof(R_flt64) < MAX_SHORT_STRING:
   then short string missing values can be truncated in system files
   because there's only room for as many characters as can fit in a
   R_flt64. */
#if MAX_SHORT_STRING > 8
#error MAX_SHORT_STRING must be less than 8.
#endif

/* VAR_NAME_LEN: the length of a variable.
 * SPSS supports names of 64 long
 */
#define VAR_NAME_LEN 64
/* Special values. */
#define SYSMIS (-DBL_MAX)
#define LOWEST second_lowest_double_val()
#define HIGHEST DBL_MAX

/* Describes one value, which is either a floating-point number or a
   short string. */
union value
  {
    /* A numeric value. */
    double f;

    /* A short-string value. */
    unsigned char s[MAX_SHORT_STRING];

    /* This member is used by data-in.c to return a string result,
       since it may need to return a long string.  As currently
       implemented, it's a pointer to a static internal buffer in
       data-in.c.

       Also used by evaluate_expression() to return a string result.
       As currently implemented, it's a pointer to a dynamic buffer in
       the appropriate expression.

       Also used by the AGGREGATE procedure in handling string
       values. */
    unsigned char *c;

    /* Sometimes we insert value's in a hash table. */
    unsigned long hash[sizeof(double) / sizeof(long)];
  };

/* Describes one value label. */
struct value_label
  {
    union value v;		/* The value being labeled. */
    char *s;			/* Pointer to malloc()'d label. */
    int ref_count;		/* Reference count. */
  };

/* Frequency tables. */

/* Frequency table entry. */
struct freq
  {
    union value v;		/* The value. */
    double c;			/* The number of occurrences of the value. */
  };

/* Types of frequency tables. */
enum
  {
    FRQM_GENERAL,
    FRQM_INTEGER
  };

/* Entire frequency table. */
struct freq_tab
  {
    int mode;			/* FRQM_GENERAL or FRQM_INTEGER. */

    /* General mode. */
    struct avl_tree *tree;	/* Undifferentiated data. */

    /* Integer mode. */
    double *vector;		/* Frequencies proper. */
    int min, max;		/* The boundaries of the table. */
    double out_of_range;	/* Sum of weights of out-of-range values. */
    double sysmis;		/* Sum of weights of SYSMIS values. */

    /* All modes. */
    struct freq *valid;		/* Valid freqs. */
    int n_valid;		/* Number of total freqs. */

    struct freq *missing;	/* Missing freqs. */
    int n_missing;		/* Number of missing freqs. */

    /* Statistics. */
    double total_cases;		/* Sum of weights of all cases. */
    double valid_cases;		/* Sum of weights of valid cases. */
  };

/* A complete set of 3 frequency tables. */
struct freq_tab_set
  {
    struct freq_tab miss;	/* Includes user-missing values. */
    struct freq_tab no_miss;	/* Excludes user-missing values. */
    struct freq_tab sel;	/* Identical to either miss or no_miss. */
  };

/* Procedures' private per-variable data. */

/* Structure name suffixes for private data:
   _proc: for a procedure (i.e., LIST -> list_proc).
   _trns: for a transformation (i.e., COMPUTE -> compute_trns.
   _pgm: for an input program (i.e., DATA LIST -> data_list_pgm). */

/* CROSSTABS private data. */
struct crosstab_proc
  {
    /* Integer mode only. */
    int min;			/* Minimum value. */
    int max;			/* Maximum value + 1. */
    int count;			/* max - min. */
  };

/* FREQUENCIES private data. */
enum
  {
    frq_mean = 0, frq_semean, frq_median, frq_mode, frq_stddev, frq_variance,
    frq_kurt, frq_sekurt, frq_skew, frq_seskew, frq_range, frq_min, frq_max,
    frq_sum, frq_n_stats
  };

struct frequencies_proc
  {
    /* General mode. */
    struct freq_tab tab;	/* Frequencies table to use. */

    /* Percentiles. */
    int n_groups;		/* Number of groups. */
    double *groups;		/* Groups. */

    /* Statistics. */
    double stat[frq_n_stats];
  };

/* LIST private data. */
struct list_proc
  {
    int newline;		/* Whether a new line begins here. */
    int width;			/* Field width. */
    int vert;			/* Whether to print the varname vertically. */
  };

/* DESCRIPTIVES private data.  Note that the DESCRIPTIVES procedure also
   has a transformation, descriptives_trns. */
enum
  {
    /* As these are used as bit indexes, there must be 32 or fewer.
       Be very careful in adjusting these, see the structure below
       and the table in descriptives.q. */
    dsc_mean = 0, dsc_semean, dsc_stddev, dsc_variance, dsc_kurt,
    dsc_sekurt, dsc_skew, dsc_seskew, dsc_range, dsc_min,
    dsc_max, dsc_sum, dsc_n_stats
  };

struct descriptives_proc
  {
    /* Miscellaneous. */
    int dup;			/* Finds duplicates in list of
				   variables. */
    char zname[10];		/* Name for z-score variable. */

    /* Counts. */
    double valid, miss;		/* Valid, missing--general. */

    /* Mean, moments about the mean. */
    double X_bar, M2, M3, M4;
    double min, max;

    /* Statistics. */
    double stats[dsc_n_stats];	/* Everything glommed together. */
  };

/* GET private data. */
struct get_proc
  {
    int fv, nv;			/* First, last, # of values. */
  };

/* Sort order. */
enum
  {
    SRT_ASCEND,			/* A, B, C, ..., X, Y, Z. */
    SRT_DESCEND			/* Z, Y, X, ..., C, B, A. */
  };

/* SORT CASES private data. */
struct sort_cases_proc
  {
    int order;			/* SRT_ASCEND or SRT_DESCEND. */
  };

/* MODIFY VARS private data. */
struct modify_vars_proc
  {
    char new_name[VAR_NAME_LEN +1];		/* Variable's new name. */
    int drop_this_var;		/* 0=keep this var, 1=drop this var. */
    struct variable *next;	/* Next in linked list. */
  };

/* MEANS private data. */
struct means_proc
  {
    double min, max;		/* Range for integer mode. */
  };

/* Different types of variables for MATRIX DATA procedure.  Order is
   important: these are used for sort keys. */
enum
  {
    MXD_SPLIT,			/* SPLIT FILE variables. */
    MXD_ROWTYPE,		/* ROWTYPE_. */
    MXD_FACTOR,			/* Factor variables. */
    MXD_VARNAME,		/* VARNAME_. */
    MXD_CONTINUOUS,		/* Continuous variables. */

    MXD_COUNT
  };

/* MATRIX DATA private data. */
struct matrix_data_proc
  {
    int vartype;		/* Variable type. */
    int subtype;		/* Subtype. */
  };

/* MATCH FILES private data. */
struct match_files_proc
  {
    struct variable *master;	/* Corresponding master file variable. */
  };


/* Script variables. */

/* Variable type. */
enum
  {
    NUMERIC,			/* A numeric variable. */
    ALPHA			/* A string variable.  (STRING is pre-empted by lexer.h) */
  };

/* Types of missing values.  Order is significant, see
   mis-val.c:parse_numeric(), sfm-read.c:sfm_read_dictionary()
   sfm-write.c:sfm_write_dictionary(),
   sysfile-info.c:cmd_sysfile_info(), mis-val.c:copy_missing_values(),
   pfm-read.c:read_variables(), pfm-write.c:write_variables(),
   apply-dict.c:cmd_apply_dictionary(), and more (?). */
enum
  {
    MISSING_NONE,		/* No user-missing values. */
    MISSING_1,			/* One user-missing value. */
    MISSING_2,			/* Two user-missing values. */
    MISSING_3,			/* Three user-missing values. */
    MISSING_RANGE,		/* [a,b]. */
    MISSING_LOW,		/* (-inf,a]. */
    MISSING_HIGH,		/* (a,+inf]. */
    MISSING_RANGE_1,		/* [a,b], c. */
    MISSING_LOW_1,		/* (-inf,a], b. */
    MISSING_HIGH_1,		/* (a,+inf), b. */
    MISSING_COUNT
  };

/* A variable's dictionary entry.  Note: don't reorder name[] from the
   first element; a pointer to `variable' should be a pointer to
   member `name'.*/
struct variable
  {
    /* Required by parse_variables() to be in this order.  */
    char name[VAR_NAME_LEN +1];	/* As a string. */
    int index;			/* Index into its dictionary's var[]. */
    int type;			/* NUMERIC or ALPHA. */
    int foo;			/* Used for temporary storage. */

    /* Also important but parse_variables() doesn't need it.  Still,
       check before reordering. */
    int width;			/* Size of string variables in chars. */
    int fv, nv;			/* Index into `value's, number of values. */
    int left;			/* 0=do not LEAVE, 1=LEAVE. */

    /* Missing values. */
    int miss_type;		/* One of the MISSING_* constants. */
    union value missing[3];	/* User-missing value. */

    /* Display formats. */
    struct fmt_spec print;	/* Default format for PRINT. */
    struct fmt_spec write;	/* Default format for WRITE. */

    /* Labels. */
    struct avl_tree *val_lab;	/* Avltree of value_label structures. */
    char *label;		/* Variable label. */

    /* Per-procedure info. */
    struct get_proc get;
    union
      {
	struct crosstab_proc crs;
	struct descriptives_proc dsc;
	struct frequencies_proc frq;
	struct list_proc lst;
	struct means_proc mns;
	struct sort_cases_proc srt;
	struct modify_vars_proc mfv;
	struct matrix_data_proc mxd;
	struct match_files_proc mtf;
      }
    p;
  };

/* Cases. */

/* A single case.  (This doesn't need to be a struct anymore, but it
   remains so for hysterical raisins.) */
struct ccase
  {
    union value data[1];
  };

/* Dictionary. */

/* Complete dictionary state. */
struct dictionary
  {
    struct variable **var;	/* Variable descriptions. */
    struct avl_tree *var_by_name;	/* Variables arranged by name. */
    int nvar;			/* Number of variables. */

    int N;			/* Current case limit (N command). */
    int nval;			/* Number of value structures per case. */

    int n_splits;		/* Number of SPLIT FILE variables. */
    struct variable **splits;	/* List of SPLIT FILE vars. */

    char *label;		/* File label. */

    int n_documents;		/* Number of lines of documents. */
    char *documents;		/* Documents; 80*n_documents bytes in size. */

    int weight_index;		/* `value' index of $WEIGHT, or -1 if none.
				   Call update_weighting() before using! */
    char weight_var[VAR_NAME_LEN];/* Name of WEIGHT variable. */

    char filter_var[VAR_NAME_LEN];/* Name of FILTER variable. */
    /* Do not make another field the last field! or see
       temporary.c:restore_dictionary() before doing so! */
  };

/* This is the active file dictionary. */
extern struct dictionary default_dict;

/* Transformation state. */

/* Default file handle for DATA LIST, REREAD, REPEATING DATA
   commands. */
extern struct file_handle *default_handle;

/* PROCESS IF expression. */
extern struct expression *process_if_expr;

/* TEMPORARY support. */

/* 1=TEMPORARY has been executed at some point. */
extern int temporary;

/* If temporary!=0, the saved dictionary. */
extern struct dictionary *temp_dict;

/* If temporary!=0, index into t_trns[] (declared far below) that
   gives the point at which data should be written out.  -1 means that
   the data shouldn't be changed since all transformations are
   temporary. */
extern int temp_trns;

/* If FILTER is active, whether it was executed before or after
   TEMPORARY. */
extern int FILTER_before_TEMPORARY;

void cancel_temporary (void);

/* Functions. */

int is_varname (const char *);
int is_dict_varname (const struct dictionary *, const char *);

/* Flags for passing to fill_all_vars(). */
enum
  {
    FV_NONE = 0,		/* No flags. */
    FV_NO_SYSTEM = 001,		/* Don't include system variables. */
    FV_NO_SCRATCH = 002		/* Don't include scratch variables. */
  };

void fill_all_vars (struct variable ***, int *, int flags);

int val_lab_cmp (const void *, const void *, void *);
char *get_val_lab (const struct variable *, union value, int);
void free_val_lab (void *, void *);
void free_value_label (struct value_label *);
struct avl_tree *copy_value_labels (struct avl_tree *);

void dump_split_vars (const struct ccase *);

int is_num_user_missing (double, const struct variable *);
int is_str_user_missing (const unsigned char[], const struct variable *);
int is_missing (const union value *, const struct variable *);
int is_system_missing (const union value *, const struct variable *);
int is_user_missing (const union value *, const struct variable *);
void copy_missing_values (struct variable *dest, const struct variable *src);

int cmp_variable (const void *, const void *, void *);

#if GLOBAL_DEBUGGING
struct variable *force_create_variable (struct dictionary *, const char *name,
					int type, int width);
struct variable *force_dup_variable (struct dictionary *,
				     const struct variable *src,
				     const char *name);
#else
#define force_create_variable(A, B, C, D)	\
	create_variable (A, B, C, D)
#define force_dup_variable(A, B, C)		\
	dup_variable (A, B, C)
#endif

struct variable *create_variable (struct dictionary *, const char *name,
				  int type, int width);
void delete_variable (struct dictionary *, struct variable *v);
struct variable *find_variable (const char *name);
struct variable *find_dict_variable (const struct dictionary *,
				     const char *name);
void init_variable (struct dictionary *, struct variable *, const char *name,
		    int type, int width);
void replace_variable (struct variable *, const char *name,
		       int type, int width);
void clear_variable (struct dictionary *, struct variable *);
void rename_variable (struct dictionary *, struct variable *v,
		      const char *new_name);
void discard_variables (void);
void clear_default_dict (void);
void copy_variable (struct variable *dest, const struct variable *src);
struct variable *dup_variable (struct dictionary *dict,
			       const struct variable *src, const char *name);

struct variable *update_weighting (struct dictionary *);
void stop_weighting (struct dictionary *);

struct dictionary *save_dictionary (void);
void restore_dictionary (struct dictionary *);
void free_dictionary (struct dictionary *);
struct dictionary *new_dictionary (int copy);

/* Transformations. */

/* Header for all transformations. */
struct trns_header
  {
    /* Index into t_trns[]. */
    int index;

    /* Transformation proc. */
    int (*proc) (struct trns_header *, struct ccase *);

    /* Garbage collector proc. */
    void (*free) (struct trns_header *);
  };

/* Array of transformations */
extern struct trns_header **t_trns;

/* Number of transformations, maximum number in array currently. */
extern int n_trns, m_trns;

/* Index of first transformation that is really a transformation.  Any
   transformations before this belong to INPUT PROGRAM. */
extern int f_trns;

void add_transformation (struct trns_header *trns);
void cancel_transformations (void);

/* Variable parsers. */

/* Only parse_variables() supports options other than PV_APPEND,
   PV_SINGLE. */
enum
  {
    PV_NONE = 0,		/* No options. */
    PV_SINGLE = 0001,		/* Restrict to a single varname or TO use. */
    PV_DUPLICATE = 0002,	/* Don't merge duplicates. */
    PV_APPEND = 0004,		/* Append to existing list. */
    PV_NO_DUPLICATE = 0010,	/* Error on duplicates. */
    PV_NUMERIC = 0020,		/* Vars must be numeric. */
    PV_STRING = 0040,		/* Vars must be string. */
    PV_SAME_TYPE = 00100,	/* All vars must be the same type. */
    PV_NO_SCRATCH = 00200	/* Disallow scratch variables. */
  };

struct variable *parse_variable (void);
struct variable *parse_dict_variable (struct dictionary *);
int parse_variables (struct dictionary *dict, struct variable ***v,
		     int *nv, int pv_opts);
int parse_DATA_LIST_vars (char ***names, int *nnames, int pv_opts);
int parse_mixed_vars (char ***names, int *nnames, int pv_opts);

#endif /* !var_h */
