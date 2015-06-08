/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2015  The R Core Team.
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

/** @file Defn.h
 *
 * @brief A ragbag.
 *
 * As CXXR development proceeds, the type definitions, many function
 * prototypes etc. defined in this header file will disappear, because
 * the relevant functionality will have been absorbed into the CXXR
 * core, and declared within the appropriate header file in the
 * <tt>src/include/CXXR</tt> directory.
 *
 * In a few cases, a declaration within this file is repeated in a
 * header file under <tt>src/include/CXXR</tt>; this is because source
 * files within the CXXR core never <tt>\#include</tt>s
 * <tt>Defn.h</tt> itself (nor <tt>Rinternals.h</tt>.  In such a case
 * the relevant CXXR header file is <tt>\#include</tt>d back into
 * <tt>Defn.h</tt>, so that the compiler can detect any inconsistency
 * between the two declarations.
 */

#ifndef DEFN_H_
#define DEFN_H_

#include "CXXR/BuiltInFunction.h"
#include "CXXR/Evaluator.h"
#include "CXXR/errors.h"

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_visible __attribute__ ((visibility ("default")))
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_visible
# define attribute_hidden
#endif

/* In CR, extern0 is defined as attribute_hidden if this file is
 * #included from main.c, and as extern otherwise.  In CXXR it always
 * maps to extern attribute_hidden.
 */
# define extern0 extern attribute_hidden

#define MAXELTSIZE 8192 /* Used as a default for string buffer sizes,
			   and occasionally as a limit. */

#include <R_ext/Complex.h>

#ifdef __cplusplus
extern "C" {
#endif

void Rf_CoercionWarning(int);/* warning code */
int Rf_LogicalFromInteger(int, int*);
int Rf_LogicalFromReal(double, int*);
int Rf_LogicalFromComplex(Rcomplex, int*);
int Rf_IntegerFromLogical(int, int*);
int Rf_IntegerFromReal(double, int*);
int Rf_IntegerFromComplex(Rcomplex, int*);
double Rf_RealFromLogical(int, int*);
double Rf_RealFromInteger(int, int*);
double Rf_RealFromComplex(Rcomplex, int*);
Rcomplex Rf_ComplexFromLogical(int, int*);
Rcomplex Rf_ComplexFromInteger(int, int*);
Rcomplex Rf_ComplexFromReal(double, int*);

#ifdef __cplusplus
}  /* extern "C" */
#endif

#define CALLED_FROM_DEFN_H 1
#include <Rinternals.h>		/*-> Arith.h, Boolean.h, Complex.h, Error.h,
				  Memory.h, PrtUtil.h, Utils.h */
#undef CALLED_FROM_DEFN_H
extern0 SEXP	R_CommentSymbol;    /* "comment" */
extern0 SEXP	R_DotEnvSymbol;     /* ".Environment" */
extern0 SEXP	R_ExactSymbol;	    /* "exact" */
extern0 SEXP	R_RecursiveSymbol;  /* "recursive" */
extern0 SEXP	R_WholeSrcrefSymbol;   /* "wholeSrcref" */
extern0 SEXP	R_TmpvalSymbol;     /* "*tmp*" */
extern0 SEXP	R_UseNamesSymbol;   /* "use.names" */
extern0 SEXP	R_ColonSymbol;         /* ":" */
//extern0 SEXP	R_DoubleColonSymbol;   /* "::" */
//extern0 SEXP	R_TripleColonSymbol;   /* ":::" */
extern0 SEXP    R_ConnIdSymbol;  /* "conn_id" */
extern0 SEXP    R_DevicesSymbol;  /* ".Devices" */

extern0 SEXP    R_dot_Generic;  /* ".Generic" */
extern0 SEXP    R_dot_Methods;  /* ".Methods" */
extern0 SEXP    R_dot_Group;  /* ".Group" */
extern0 SEXP    R_dot_Class;  /* ".Class" */
extern0 SEXP    R_dot_GenericCallEnv;  /* ".GenericCallEnv" */
extern0 SEXP    R_dot_GenericDefEnv;  /* ".GenericDefEnv" */


int IS_BYTES(SEXP x);
void SET_BYTES(SEXP x);
int IS_ASCII(SEXP x);

#include "Errormsg.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void R_ProcessEvents(void);
#ifdef Win32
extern void R_WaitEvent(void);
#endif

#ifdef __cplusplus
}  /* extern "C" */
#endif

#ifdef R_USE_SIGNALS
#ifdef Win32
# include <psignal.h>
#else
# include <signal.h>
# include <setjmp.h>
#endif
#endif

#ifdef Unix
# define OSTYPE      "unix"
# define FILESEP     "/"
#endif /* Unix */

#ifdef Win32
# define OSTYPE      "windows"
# define FILESEP     "/"
#endif /* Win32 */

#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif

/*  Heap and Pointer Protection Stack Sizes.  */

/* These are all required by C99 */
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
/* According to POSIX inttypes.h should include stdint.h,
   but let's be sure. */
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#if defined HAVE_DECL_SIZE_MAX && HAVE_DECL_SIZE_MAX
  typedef size_t R_size_t;
#ifdef __cplusplus
# define R_SIZE_T_MAX std::numeric_limits<size_t>::max() 
#else
# define R_SIZE_T_MAX SIZE_MAX
#endif /* __cplusplus */
#else
# error SIZE_MAX is required for C99
#endif


#define Mega 1048576. /* 1 Mega Byte := 2^20 (= 1048576) Bytes */
#define Giga 1073741824. /* 1 Giga Byte := 2^30 Bytes */

/*  R_VSIZE	   The initial heap size in bytes */
/*  This is a default value and can be overridden in config.h
    The maxima and minima are in startup.c */
#ifndef R_VSIZE
#define	R_VSIZE		16000000L
#endif

/* some commonly needed headers */
#include <math.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* declare substitutions */
#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif
#if !defined(strncascmp) && defined(HAVE_DECL_STRNCASECMP) && !HAVE_DECL_STRNCASECMP
extern int strncasecmp(const char *s1, const char *s2, size_t n);
#endif

/* Glibc manages to not define this in -pedantic -ansi */
#if defined(HAVE_PUTENV) && !defined(putenv) && defined(HAVE_DECL_PUTENV) && !HAVE_DECL_PUTENV
extern int putenv(char *string);
#endif


/* Maximal length in bytes of an entire path name.
   POSIX has required this to be at least 255/256, and X/Open at least 1024.
   Solaris has 1024, Linux glibc has 4192.
   File names are limited to FILENAME_MAX bytes (usually the same as PATH_MAX)
   or NAME_MAX (often 255/256).
 */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
/* Try BSD name */
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
/* seems this is now defined by MinGW to be 259, whereas FILENAME_MAX
   and MAX_PATH are 260.  It is not clear that this really is in bytes,
   but might be chars for the Unicode interfaces.

   260 is d:\ plus 256 chars plus nul.  Some but not all API calls
   allow filepaths of the form \\?\D:\very_long_path .
*/
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
#endif

#ifdef HAVE_POSIX_SETJMP
# define SIGJMP_BUF sigjmp_buf
# define SIGSETJMP(x,s) sigsetjmp(x,s)
# define SIGLONGJMP(x,i) siglongjmp(x,i)
# define JMP_BUF sigjmp_buf
# define SETJMP(x) sigsetjmp(x,0)
# define LONGJMP(x,i) siglongjmp(x,i)
#else
# define SIGJMP_BUF jmp_buf
# define SIGSETJMP(x,s) setjmp(x)
# define SIGLONGJMP(x,i) longjmp(x,i)
# define JMP_BUF jmp_buf
# define SETJMP(x) setjmp(x)
# define LONGJMP(x,i) longjmp(x,i)
#endif

#ifdef __cplusplus

/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hasking groups.
 */

/* Vector Heap Structure */
/* sizeof(VECREC) is used for some backwards-compatibility purposes in
   CXXR, and that's all.
*/
typedef struct {
	union {
		SEXP		backpointer;
		double		align;
	} u;
} VECREC, *VECP;

/* Vector Heap Macros */

inline size_t BYTE2VEC(int n)
{
    return (n > 0) ? (std::size_t(n) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t INT2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(int) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t FLOAT2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(double) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t COMPLEX2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(Rcomplex) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t PTR2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(SEXP) - 1)/sizeof(VECREC) + 1 : 0;
}

#else /* if not __cplusplus */

typedef SEXP R_bcstack_t;

#endif // __cplusplus

/* Miscellaneous Definitions */
#define streql(s, t)	(!strcmp((s), (t)))

/* Arithmetic and Relation Operators */
typedef enum {
    PLUSOP = 1,
    MINUSOP,
    TIMESOP,
    DIVOP,
    POWOP,
    MODOP,
    IDIVOP
} ARITHOP_TYPE;

typedef enum {
    EQOP = 1,
    NEOP,
    LTOP,
    LEOP,
    GEOP,
    GTOP
} RELOP_TYPE;

/* File Handling */
/*
#define R_EOF	65535
*/
#define R_EOF	-1


/*--- Global Variables ---------------------------------------------------- */

#include <R_ext/libextern.h>

/* In CR, if this file is included from main.c, 'INI_as(v)' expands to
 * '= v', and 'extern0' expands to 'attribute_hidden'; otherwise
 * 'INI_as(v)' expands to nothing and 'extern0' expands to 'extern'.
 */
# define INI_as(v)
#define extern0 extern attribute_hidden

LibExtern SEXP  R_SrcfileSymbol;    /* "srcfile" */
LibExtern SEXP  R_SrcrefSymbol;     /* "srcref" */


LibExtern Rboolean R_interrupts_suspended INI_as(FALSE);
LibExtern int R_interrupts_pending INI_as(0);

/* R Home Directory */
LibExtern char *R_Home;		    /* Root of the R tree */

/* Memory Management */
extern0 R_size_t R_VSize  INI_as(R_VSIZE);/* Initial size of the heap */
extern0 int	R_Is_Running;	    /* for Windows memory manager */

/* Evaluation Environment */
extern Rboolean R_Visible;	    /* Value visibility flag */
extern0 int	R_EvalDepth	INI_as(0);	/* Evaluation recursion depth */
extern0 int	R_BrowseLines	INI_as(0);	/* lines/per call in browser */

extern0 Rboolean R_KeepSource	INI_as(FALSE);	/* options(keep.source) */
extern0 Rboolean R_CBoundsCheck	INI_as(FALSE);	/* options(CBoundsCheck) */
extern0 int	R_WarnLength	INI_as(1000);	/* Error/warning max length */
extern0 int	R_nwarnings	INI_as(50);
extern uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
extern int	R_CStackDir	INI_as(1);	/* C stack direction */

/* File Input/Output */
LibExtern Rboolean R_Interactive INI_as(TRUE);	/* TRUE during interactive use*/
extern0 Rboolean R_Quiet	INI_as(FALSE);	/* Be as quiet as possible */
extern Rboolean  R_Slave	INI_as(FALSE);	/* Run as a slave process */
extern0 Rboolean R_Verbose	INI_as(FALSE);	/* Be verbose */
/* extern int	R_Console; */	    /* Console active flag */
/* IoBuffer R_ConsoleIob; : --> ./IOStuff.h */
/* R_Consolefile is used in the internet module */
extern FILE*	R_Consolefile	INI_as(NULL);	/* Console output file */
extern FILE*	R_Outputfile	INI_as(NULL);	/* Output file */
extern0 int	R_ErrorCon	INI_as(2);	/* Error connection */
LibExtern char *R_TempDir	INI_as(NULL);	/* Name of per-session dir */
extern0 char   *Sys_TempDir	INI_as(NULL);	/* Name of per-session dir
						   if set by R itself */
extern0 char	R_StdinEnc[31]  INI_as("");	/* Encoding assumed for stdin */

/* Objects Used In Parsing  */
LibExtern int	R_ParseError	INI_as(0); /* Line where parse error occurred */
extern0 int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
extern0 SEXP	R_ParseErrorFile;   /* Source file where parse error was seen.  Either a
				       STRSXP or (when keeping srcrefs) a SrcFile ENVSXP */
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
LibExtern char	R_ParseErrorMsg[PARSE_ERROR_SIZE] INI_as("");
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */
LibExtern char	R_ParseContext[PARSE_CONTEXT_SIZE] INI_as("");
LibExtern int	R_ParseContextLast INI_as(0); /* last character in context buffer */
LibExtern int	R_ParseContextLine; /* Line in file of the above */

/* Image Dump/Restore */
extern int	R_DirtyImage	INI_as(1);	/* Current image dirty */

/* History */
LibExtern char *R_HistoryFile;	/* Name of the history file */
LibExtern int	R_HistorySize;	/* Size of the history file */
LibExtern int	R_RestoreHistory;	/* restore the history file? */
extern void 	R_setupHistory(void);

/* Warnings/Errors */
extern0 int	R_CollectWarnings INI_as(0);	/* the number of warnings */
#ifdef __cplusplus
extern CXXR::GCRoot<> R_Warnings;	    /* the warnings and their calls */
#endif
extern0 int	R_ShowErrorMessages INI_as(1);	/* show error messages? */
#ifdef __cplusplus
extern CXXR::GCRoot<> R_HandlerStack;	/* Condition handler stack */
extern CXXR::GCRoot<> R_RestartStack;	/* Stack of available restarts */
#endif
extern0 Rboolean R_warn_partial_match_dollar INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_attr INI_as(FALSE);
extern0 Rboolean R_ShowWarnCalls INI_as(FALSE);
extern0 Rboolean R_ShowErrorCalls INI_as(FALSE);
extern0 int	R_NShowCalls INI_as(50);

LibExtern Rboolean utf8locale  INI_as(FALSE);  /* is this a UTF-8 locale? */
LibExtern Rboolean mbcslocale  INI_as(FALSE);  /* is this a MBCS locale? */
extern0   Rboolean latin1locale INI_as(FALSE); /* is this a Latin-1 locale? */
#ifdef Win32
LibExtern unsigned int localeCP  INI_as(1252); /* the locale's codepage */
extern0   Rboolean WinUTF8out  INI_as(FALSE);  /* Use UTF-8 for output */
extern0   void WinCheckUTF8(void);
#endif

extern char* OutDec	INI_as(".");  /* decimal point used for output */
extern0 Rboolean R_DisableNLinBrowser	INI_as(FALSE);
extern0 char R_BrowserLastCommand	INI_as('n');

/* Initialization of the R environment when it is embedded */
extern int Rf_initEmbeddedR(int argc, char **argv);

/* GUI type */

extern const char	*R_GUIType	INI_as("unknown");
extern Rboolean R_isForkedChild		INI_as(FALSE); /* was this forked? */

extern0 double cpuLimit			INI_as(-1.0);
extern0 double cpuLimit2	       	INI_as(-1.0);
extern0 double cpuLimitValue		INI_as(-1.0);
extern0 double elapsedLimit		INI_as(-1.0);
extern0 double elapsedLimit2		INI_as(-1.0);
extern0 double elapsedLimitValue       	INI_as(-1.0);

void resetTimeLimits(void);

extern0 int R_jit_enabled INI_as(0);
extern0 int R_compile_pkgs INI_as(0);
extern SEXP R_cmpfun(SEXP);
extern void R_init_jit_enabled(void);
extern void R_initAsignSymbols(void);

LibExtern int R_num_math_threads INI_as(1);
LibExtern int R_max_num_math_threads INI_as(1);

/* Pointer  type and utilities for dispatch in the methods package */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP, SEXP); /* typedef */
//R_stdGen_ptr_t R_get_standardGeneric_ptr(void); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t, SEXP); /* set method */
LibExtern SEXP R_MethodsNamespace;
SEXP R_deferred_default_method(void);
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef,
		       SEXP mlist);
SEXP do_set_prim_method(SEXP op, const char *code_string, SEXP fundef,
			SEXP mlist);
void R_set_quick_method_check(R_stdGen_ptr_t);
SEXP R_primitive_methods(SEXP op);
SEXP R_primitive_generic(SEXP op);

/* smallest decimal exponent, needed in format.c, set in Init_R_Machine */
extern0 int R_dec_min_exponent		INI_as(-308);

/* structure for caching machine accuracy values */
typedef struct {
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
} AccuracyInfo;

LibExtern AccuracyInfo R_AccuracyInfo;

extern unsigned int max_contour_segments INI_as(25000);

/* used in package utils */
extern Rboolean known_to_be_latin1 INI_as(FALSE);
extern0 Rboolean known_to_be_utf8 INI_as(FALSE);

/* pre-allocated boolean values */
LibExtern SEXP R_TrueValue INI_as(NULL);
LibExtern SEXP R_FalseValue INI_as(NULL);
LibExtern SEXP R_LogicalNAValue INI_as(NULL);

#ifdef Win32
LibExtern Rboolean UseInternet2;
#endif

#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as

#define checkArity(a,b) Rf_checkArityCall(a,b,call)

/*--- FUNCTIONS ------------------------------------------------------ */

/* These Rf_ macros are retained for backwards compatibility, but
 * their use is deprecated within CXXR.  In particular CXXR's own
 * header files should always use the Rf_ prefix explicitly, and not
 * rely on these macros to paste it in.
 */

#ifndef R_NO_REMAP
# define allocCharsxp		Rf_allocCharsxp
# define asVecSize		Rf_asVecSize
# define begincontext		Rf_begincontext
# define BindDomain		Rf_BindDomain
# define check1arg		Rf_check1arg
# define CleanEd		Rf_CleanEd
# define CoercionWarning       	Rf_CoercionWarning
# define ComplexFromInteger	Rf_ComplexFromInteger
# define ComplexFromLogical	Rf_ComplexFromLogical
# define ComplexFromReal	Rf_ComplexFromReal
# define ComplexFromString	Rf_ComplexFromString
# define copyMostAttribNoTs	Rf_copyMostAttribNoTs
# define createS3Vars		Rf_createS3Vars
# define currentTime		Rf_currentTime
# define CustomPrintValue	Rf_CustomPrintValue
# define DataFrameClass		Rf_DataFrameClass
# define ddfindVar		Rf_ddfindVar
# define deparse1		Rf_deparse1
# define deparse1w		Rf_deparse1w
# define deparse1line		Rf_deparse1line
# define deparse1s		Rf_deparse1s
# define DispatchGroup		Rf_DispatchGroup
# define DispatchOrEval		Rf_DispatchOrEval
# define dynamicfindVar		Rf_dynamicfindVar
# define EncodeChar             Rf_EncodeChar
# define EncodeRaw              Rf_EncodeRaw
# define EncodeReal2            Rf_EncodeReal2
# define EncodeString           Rf_EncodeString
# define EnsureString 		Rf_EnsureString
# define envlength		Rf_envlength
# define ErrorMessage		Rf_ErrorMessage
# define evalListKeepMissing	Rf_evalListKeepMissing
# define factorsConform		Rf_factorsConform
# define findcontext		Rf_findcontext
# define findVar1		Rf_findVar1
# define FrameClassFix		Rf_FrameClassFix
# define framedepth		Rf_framedepth
# define frameSubscript		Rf_frameSubscript
# define get1index		Rf_get1index
# define GetOptionCutoff       	Rf_GetOptionCutoff
# define getVar			Rf_getVar
# define getVarInFrame		Rf_getVarInFrame
# define InitArithmetic		Rf_InitArithmetic
# define InitConnections	Rf_InitConnections
# define InitEd			Rf_InitEd
# define InitFunctionHashing	Rf_InitFunctionHashing
# define InitGlobalEnv		Rf_InitGlobalEnv
# define InitGraphics		Rf_InitGraphics
# define InitMemory		Rf_InitMemory
# define InitNames		Rf_InitNames
# define InitOptions		Rf_InitOptions
# define InitStringHash		Rf_InitStringHash
# define InitS3DefaultTypes	Rf_InitS3DefaultTypes
# define InitTempDir		Rf_InitTempDir
# define InitTypeTables		Rf_InitTypeTables
# define initStack		Rf_initStack
# define IntegerFromComplex	Rf_IntegerFromComplex
# define IntegerFromLogical	Rf_IntegerFromLogical
# define IntegerFromReal	Rf_IntegerFromReal
# define IntegerFromString	Rf_IntegerFromString
# define internalTypeCheck	Rf_internalTypeCheck
# define isValidName		Rf_isValidName
# define installTrChar		Rf_installTrChar
# define ItemName		Rf_ItemName
# define jump_to_toplevel	Rf_jump_to_toplevel
# define KillAllDevices		Rf_KillAllDevices
# define levelsgets		Rf_levelsgets
# define LogicalFromComplex	Rf_LogicalFromComplex
# define LogicalFromInteger	Rf_LogicalFromInteger
# define LogicalFromReal	Rf_LogicalFromReal
# define LogicalFromString	Rf_LogicalFromString
# define mainloop		Rf_mainloop
# define makeSubscript		Rf_makeSubscript
# define markKnown		Rf_markKnown
# define mat2indsub		Rf_mat2indsub
# define matchArg		Rf_matchArg
# define matchArgExact		Rf_matchArgExact
# define matchArgs		Rf_matchArgs
# define matchPar		Rf_matchPar
# define Mbrtowc		Rf_mbrtowc
# define mbtoucs		Rf_mbtoucs
# define mkCLOSXP		Rf_mkCLOSXP
# define mkFalse		Rf_mkFalse
# define mkPROMISE		Rf_mkPROMISE
# define mkQUOTE		Rf_mkQUOTE
# define mkTrue			Rf_mkTrue
# define NewEnvironment		Rf_NewEnvironment
# define OneIndex		Rf_OneIndex
# define onintr			Rf_onintr
# define onsigusr1              Rf_onsigusr1
# define onsigusr2              Rf_onsigusr2
# define parse			Rf_parse
# define patchArgsByActuals	Rf_patchArgsByActuals
# define PrintDefaults		Rf_PrintDefaults
# define PrintGreeting		Rf_PrintGreeting
# define PrintValueEnv		Rf_PrintValueEnv
# define PrintValueRec		Rf_PrintValueRec
# define PrintVersion		Rf_PrintVersion
# define PrintVersion_part_1	Rf_PrintVersion_part_1
# define PrintVersionString    	Rf_PrintVersionString
# define PrintWarnings		Rf_PrintWarnings
# define promiseArgs		Rf_promiseArgs
# define RealFromComplex	Rf_RealFromComplex
# define RealFromInteger	Rf_RealFromInteger
# define RealFromLogical	Rf_RealFromLogical
# define RealFromString		Rf_RealFromString
# define Seql			Rf_Seql
# define sexptype2char		Rf_sexptype2char
# define Scollate		Rf_Scollate
# define sortVector		Rf_sortVector
# define SrcrefPrompt		Rf_SrcrefPrompt
# define ssort			Rf_ssort
# define StringFromComplex	Rf_StringFromComplex
# define StringFromInteger	Rf_StringFromInteger
# define StringFromLogical	Rf_StringFromLogical
# define StringFromReal		Rf_StringFromReal
# define strIsASCII		Rf_strIsASCII
# define strmat2intmat		Rf_strmat2intmat
# define substituteList		Rf_substituteList
# define TimeToSeed		Rf_TimeToSeed
# define tsConform		Rf_tsConform
# define tspgets		Rf_tspgets
# define type2symbol		Rf_type2symbol
# define unbindVar		Rf_unbindVar
# define usemethod		Rf_usemethod
# define ucstomb		Rf_ucstomb
# define ucstoutf8		Rf_ucstoutf8
# define utf8toucs		Rf_utf8toucs
# define utf8towcs		Rf_utf8towcs
# define vectorIndex		Rf_vectorIndex
# define warningcall		Rf_warningcall
# define WarningMessage		Rf_WarningMessage
# define wcstoutf8		Rf_wcstoutf8
# define wtransChar		Rf_wtransChar
#endif /* R_NO_REMAP */

/* Platform Dependent Gui Hooks */

#define	R_CONSOLE	1
#define	R_FILE		2
#define R_TEXT		3

/* The maximum length of input line which will be asked for,
   in bytes, including the terminator */
#define CONSOLE_BUFFER_SIZE 4096
int	R_ReadConsole(const char *, unsigned char *, int, int);
void	R_WriteConsole(const char *, int); /* equivalent to R_WriteConsoleEx(a, b, 0) */
void	R_WriteConsoleEx(const char *, int, int);
void	R_ResetConsole(void);
void	R_FlushConsole(void);
void	R_ClearerrConsole(void);
void	R_Busy(int);
int	R_ShowFiles(int, const char **, const char **, const char *,
		    Rboolean, const char *);
int     R_EditFiles(int, const char **, const char **, const char *);
int	R_ChooseFile(int, char *, int);
char	*R_HomeDir(void);
Rboolean R_FileExists(const char *);
Rboolean R_HiddenFile(const char *);
double	R_FileMtime(const char *);

/* environment cell access */
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
SEXP R_GetVarLocValue(R_varloc_t);
SEXP R_GetVarLocSymbol(R_varloc_t);
Rboolean R_GetVarLocMISSING(R_varloc_t);
void R_SetVarLocValue(R_varloc_t, SEXP);

/* deparse option bits: change do_dump if more are added */

#define KEEPINTEGER 		1
#define QUOTEEXPRESSIONS 	2
#define SHOWATTRIBUTES 		4
#define USESOURCE 		8
#define WARNINCOMPLETE 		16
#define DELAYPROMISES 		32
#define KEEPNA			64
#define S_COMPAT       		128
#define HEXNUMERIC             	256
#define DIGITS16             	512
/* common combinations of the above */
#define SIMPLEDEPARSE		0
#define DEFAULTDEPARSE		65 /* KEEPINTEGER | KEEPNA, used for calls */
#define FORSOURCING		95 /* not DELAYPROMISES, used in edit.c */

/* Coercion functions */
int Rf_LogicalFromString(SEXP, int*);
int Rf_IntegerFromString(SEXP, int*);
double Rf_RealFromString(SEXP, int*);
Rcomplex Rf_ComplexFromString(SEXP, int*);
SEXP Rf_StringFromLogical(int, int*);
SEXP Rf_StringFromInteger(int, int*);
SEXP Rf_StringFromReal(double, int*);
SEXP Rf_StringFromComplex(Rcomplex, int*);
SEXP Rf_EnsureString(SEXP);

/* Other Internally Used Functions */

SEXP Rf_allocCharsxp(R_len_t);
SEXP Rf_append(SEXP, SEXP); /* apparently unused now */
R_xlen_t asVecSize(SEXP x);
void Rf_checkArityCall(SEXP, SEXP, SEXP);
void R_check_locale(void);
void Rf_CleanEd(void);
void Rf_copyListMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttribNoTs(SEXP, SEXP);
double Rf_currentTime(void);
void Rf_CustomPrintValue(SEXP, SEXP);
void Rf_DataFrameClass(SEXP);
SEXP Rf_ddfindVar(SEXP, SEXP);
SEXP Rf_deparse1(SEXP,Rboolean,int);
SEXP Rf_deparse1w(SEXP,Rboolean,int);
SEXP Rf_deparse1line(SEXP,Rboolean);
SEXP Rf_deparse1s(SEXP call);
int Rf_DispatchOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, int, int);
int Rf_DispatchGroup(const char *, SEXP,SEXP,SEXP,SEXP,SEXP*);
SEXP duplicated(SEXP, Rboolean);
R_xlen_t any_duplicated(SEXP, Rboolean);
R_xlen_t any_duplicated3(SEXP, SEXP, Rboolean);
int Rf_envlength(SEXP);
SEXP Rf_evalListKeepMissing(SEXP, SEXP);
int Rf_factorsConform(SEXP, SEXP);
SEXP Rf_findVar1(SEXP, SEXP, SEXPTYPE, int);
void Rf_FrameClassFix(SEXP);
SEXP Rf_frameSubscript(int, SEXP, SEXP);
R_xlen_t Rf_get1index(SEXP, SEXP, R_xlen_t, int, int, SEXP);
int Rf_GetOptionCutoff(void);
SEXP Rf_getVar(SEXP, SEXP);
SEXP Rf_getVarInFrame(SEXP, SEXP);
void Rf_InitArithmetic(void);
void Rf_InitConnections(void);
void Rf_InitEd(void);
void Rf_InitFunctionHashing(void);
void Rf_InitGlobalEnv(void);
Rboolean R_current_trace_state(void);
Rboolean R_current_debug_state(void);
Rboolean R_has_methods(SEXP);
void R_InitialData(void);

#ifdef __cplusplus
}  // extern "C"
std::pair<bool, SEXP> R_possible_dispatch(SEXP, SEXP, SEXP, SEXP, Rboolean);
// TODO: need a C version for complex.c
void Rf_check1arg(const CXXR::RObject* args, const CXXR::RObject* call, const char*);
extern "C" {
#endif

void Rf_InitGraphics(void);
void Rf_InitMemory(void);
void Rf_InitNames(void);
void Rf_InitOptions(void);
void Rf_InitStringHash(void);
void Init_R_Variables(SEXP);
void Rf_InitTempDir(void);
void Rf_initStack(void);
void Rf_internalTypeCheck(SEXP, SEXP, SEXPTYPE);
void Rf_InitTypeTables(void);
void Rf_InitS3DefaultTypes(void);
Rboolean isMethodsDispatchOn(void);
int Rf_isValidName(const char *);
void Rf_KillAllDevices(void);
SEXP Rf_levelsgets(SEXP, SEXP);
void Rf_mainloop(void);
SEXP Rf_makeSubscript(SEXP, SEXP, R_xlen_t *, SEXP);
SEXP Rf_markKnown(const char *, SEXP);
SEXP Rf_mat2indsub(SEXP, SEXP, SEXP);
SEXP Rf_matchArg(SEXP, SEXP*);
SEXP Rf_matchArgExact(SEXP, SEXP*);
SEXP Rf_matchArgs(SEXP, SEXP, SEXP);
SEXP Rf_matchPar(const char *, SEXP*);
SEXP Rf_mkCLOSXP(SEXP, SEXP, SEXP);
SEXP Rf_mkFalse(void);
SEXP Rf_mkPROMISE(SEXP, SEXP);
SEXP Rf_mkQUOTE(SEXP);
SEXP Rf_mkTrue(void);
SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
void Rf_onintr(void);
RETSIGTYPE Rf_onsigusr1(int);
RETSIGTYPE Rf_onsigusr2(int);
R_xlen_t Rf_OneIndex(SEXP, SEXP, R_xlen_t, int, SEXP*, int, SEXP);
SEXP Rf_parse(FILE*, int);
void Rf_PrintDefaults(void);
void Rf_PrintGreeting(void);
void Rf_PrintValueEnv(SEXP, SEXP);
void Rf_PrintValueRec(SEXP, SEXP);
void Rf_PrintVersion(char *, size_t len);
void PrintVersion_part_1(char *, size_t len);
void Rf_PrintVersionString(char *, size_t len);
void Rf_PrintWarnings(void);

void process_site_Renviron(void);
void process_system_Renviron(void);
void process_user_Renviron(void);
SEXP Rf_promiseArgs(SEXP, SEXP);
void Rcons_vprintf(const char *, va_list);
SEXP R_data_class(SEXP , Rboolean);
SEXP R_data_class2(SEXP);
char *R_LibraryFileName(const char *, char *, size_t);
SEXP R_LoadFromFile(FILE*, int);
SEXP R_NewHashedEnv(SEXP, SEXP);
extern int R_Newhashpjw(const char *);
FILE* R_OpenLibraryFile(const char *);
SEXP R_Primitive(const char *);
void R_RestoreGlobalEnv(void);
void R_RestoreGlobalEnvFromFile(const char *, Rboolean);
void R_SaveGlobalEnv(void);
void R_SaveGlobalEnvToFile(const char *);
void R_SaveToFile(SEXP, FILE*, int);
void R_SaveToFileV(SEXP, FILE*, int, int);
Rboolean R_seemsOldStyleS4Object(SEXP object);
int R_SetOptionWarn(int);
int R_SetOptionWidth(int);
void R_Suicide(const char *);
void R_getProcTime(double *data);
int R_isMissing(SEXP symbol, SEXP rho);
const char *sexptype2char(SEXPTYPE type);
void Rf_sortVector(SEXP, Rboolean);
void Rf_SrcrefPrompt(const char *, SEXP);
#ifdef __cplusplus
void Rf_ssort(CXXR::StringVector*,int);
#endif
SEXP Rf_strmat2intmat(SEXP, SEXP, SEXP);
SEXP Rf_substituteList(SEXP, SEXP);
unsigned int TimeToSeed(void);
Rboolean Rf_tsConform(SEXP,SEXP);
SEXP Rf_tspgets(SEXP, SEXP);
SEXP Rf_type2symbol(SEXPTYPE);
void Rf_unbindVar(SEXP, SEXP);
#ifdef ALLOW_OLD_SAVE
void unmarkPhase(void);
#endif
SEXP R_LookupMethod(SEXP, SEXP, SEXP, SEXP);
int Rf_usemethod(const char *, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP*);
SEXP vectorIndex(SEXP, SEXP, int, int, int, SEXP, Rboolean);

/* ../main/bind.c */
SEXP Rf_ItemName(SEXP, R_xlen_t);

/* ../main/errors.c : */
void NORET ErrorMessage(SEXP, int, ...);
void WarningMessage(SEXP, R_WARNING, ...);
SEXP R_GetTraceback(int);

R_size_t R_GetMaxVSize(void);
void R_SetMaxVSize(R_size_t);
R_size_t R_GetMaxNSize(void);
void R_SetMaxNSize(R_size_t);
R_size_t R_Decode2Long(char *p, int *ierr);
void R_SetPPSize(R_size_t);

/* ../main/devices.c, used in memory.c, gnuwin32/extra.c */
#define R_MaxDevices 64

/* ../../main/printutils.c : */
typedef enum {
    Rprt_adj_left = 0,
    Rprt_adj_right = 1,
    Rprt_adj_centre = 2,
    Rprt_adj_none = 3
} Rprt_adj;

int	Rstrlen(SEXP, int);
const char *Rf_EncodeRaw(Rbyte, const char *);
const char *Rf_EncodeString(SEXP, int, int, Rprt_adj);
const char *Rf_EncodeReal2(double, int, int, int);
const char *Rf_EncodeChar(SEXP);


/* main/sort.c */
void orderVector1(int *indx, int n, SEXP key, Rboolean nalast,
		  Rboolean decreasing, SEXP rho);

/* main/subset.c */
SEXP R_subset3_dflt(SEXP, SEXP, SEXP);

/* main/subassign.c */
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);

#include <wchar.h>

/* main/util.c */
void NORET UNIMPLEMENTED_TYPE(const char *s, SEXP x);
void NORET UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
Rboolean Rf_strIsASCII(const char *str);
int utf8clen(char c);
int Rf_AdobeSymbol2ucs2(int n);
double R_strtod5(const char *str, char **endptr, char dec,
		 Rboolean NA, int exact);

typedef unsigned short ucs2_t;
size_t mbcsToUcs2(const char *in, ucs2_t *out, int nout, int enc);
/* size_t mbcsMblen(char *in);
size_t ucs2ToMbcs(ucs2_t *in, char *out);
size_t ucs2Mblen(ucs2_t *in); */
size_t Rf_utf8toucs(wchar_t *wc, const char *s);
size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n);
size_t Rf_ucstomb(char *s, const unsigned int wc);
size_t Rf_ucstoutf8(char *s, const unsigned int wc);
size_t Rf_mbtoucs(unsigned int *wc, const char *s, size_t n);
size_t Rf_wcstoutf8(char *s, const wchar_t *wc, size_t n);

SEXP Rf_installTrChar(SEXP);

const wchar_t *Rf_wtransChar(SEXP x); /* from sysutils.c */

#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))
size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);
Rboolean mbcsValid(const char *str);
Rboolean utf8Valid(const char *str);
char *Rf_strchr(const char *s, int c);
char *Rf_strrchr(const char *s, int c);

SEXP fixup_NaRm(SEXP args); /* summary.c */
void invalidate_cached_recodings(void);  /* from sysutils.c */
void resetICUcollator(void); /* from util.c */
void dt_invalidate_locale(); /* from Rstrptime.h */
extern int R_OutputCon; /* from connections.c */
extern int R_InitReadItemDepth, R_ReadItemDepth; /* from serialize.c */
void get_current_mem(size_t *,size_t *,size_t *); /* from memory.c */
unsigned long get_duplicate_counter(void);  /* from duplicate.c */
void reset_duplicate_counter(void);  /* from duplicate.c */
void BindDomain(char *); /* from main.c */
extern Rboolean LoadInitFile;  /* from startup.c */

// Unix and Windows versions
double R_getClockIncrement(void);
void R_getProcTime(double *data);
void InitDynload(void);
void R_CleanTempDir(void);

#ifdef Win32
void R_fixslash(char *s);
void R_fixbackslash(char *s);
wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand);

#if defined(SUPPORT_UTF8_WIN32)
#define mbrtowc(a,b,c,d) Rmbrtowc(a,b)
#define wcrtomb(a,b,c) Rwcrtomb(a,b)
#define mbstowcs(a,b,c) Rmbstowcs(a,b,c)
#define wcstombs(a,b,c) Rwcstombs(a,b,c)
size_t Rmbrtowc(wchar_t *wc, const char *s);
size_t Rwcrtomb(char *s, const wchar_t wc);
size_t Rmbstowcs(wchar_t *wc, const char *s, size_t n);
size_t Rwcstombs(char *s, const wchar_t *wc, size_t n);
#endif
#endif

FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand);
int Rf_Seql(SEXP a, SEXP b);
int Scollate(SEXP a, SEXP b);

double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA);
double R_strtod(const char *str, char **endptr);
double R_atof(const char *str);

/* unix/sys-std.c, main/options.c */
void set_rl_word_breaks(const char *str);

/* From localecharset.c */
extern const char *locale2charset(const char *);

/* Localization */

#ifndef NO_NLS
# ifdef ENABLE_NLS
#  include <libintl.h>
#  ifdef Win32
#   define _(String) libintl_gettext (String)
#   undef gettext /* needed for graphapp */
#  else
#   define _(String) gettext (String)
#  endif
#  define gettext_noop(String) String
#  define N_(String) gettext_noop (String)
#  else /* not NLS */
#  define _(String) (String)
#  define N_(String) String
#  define ngettext(String, StringP, N) (N > 1 ? StringP: String)
# endif
#endif

/* Macros for suspending interrupts: also in GraphicsDevice.h */
#define BEGIN_SUSPEND_INTERRUPTS do { \
    Rboolean __oldsusp__ = R_interrupts_suspended; \
    R_interrupts_suspended = TRUE;
#define END_SUSPEND_INTERRUPTS R_interrupts_suspended = __oldsusp__; \
    if (R_interrupts_pending && ! R_interrupts_suspended) \
        onintr(); \
} while(0)


#ifdef __GNUC__
# undef alloca
# define alloca(x) __builtin_alloca((x))
#else
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# endif
# if !HAVE_DECL_ALLOCA
extern void *alloca(size_t);
# endif
#endif

/* Required by C99, but might be slow */
#ifdef HAVE_LONG_DOUBLE
# define LDOUBLE long double
#else
# define LDOUBLE double
#endif

/* int_fast64_t is required by C99/C11
   Alternative would be to use intmax_t.
 */
#ifdef HAVE_INT64_T
# define LONG_INT int64_t
# define LONG_INT_MAX INT64_MAX
#elif defined(HAVE_INT_FAST64_T)
# define LONG_INT int_fast64_t
# define LONG_INT_MAX INT_FAST64_MAX
#endif

// for reproducibility for now: use exp10 or pown later if accurate enough.
#define Rexp10(x) pow(10.0, x)

/*
 * 2007/06/06 arr:
 * Function prototypes that don't appear to be defined anywhere else:
 */

Rboolean R_access_X11(void); /* from src/unix/X11.c */
SEXP R_execMethod(SEXP op, SEXP rho);
double R_getClockIncrement();
SEXP do_gpregexpr(SEXP pat, SEXP vec, int igcase_opt, int useBytes);
SEXP do_pgsub(SEXP pat, SEXP rep, SEXP vec,
	      int global, int igcase_opt, int useBytes);
const wchar_t *wtransChar(SEXP x);

#ifdef __cplusplus
}  /* extern "C" */
#endif
#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
