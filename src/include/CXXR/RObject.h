/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2007
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifndef ROBJECT_H
#define ROBJECT_H

#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned char Rbyte;

/* type for length of vectors etc */
typedef int R_len_t; /* will be long later, LONG64 or ssize_t on Win64 */
#define R_LEN_T_MAX INT_MAX

/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.

 * Note that the gap of 11 and 12 below is because of
 * the withdrawal of native "factor" and "ordered" types.
 *
 *			--> TypeTable[] in ../main/util.c for  typeof()
 */

/*  These exact numeric values are seldom used, but they are, e.g., in
 *  ../main/subassign.c
*/
#ifndef enum_SEXPTYPE
/* NOT YET using enum:
 *  1)	The SEXPREC struct below has 'SEXPTYPE type : 5'
 *	(making FUNSXP and CLOSXP equivalent in there),
 *	giving (-Wall only ?) warnings all over the place
 * 2)	Many switch(type) { case ... } statements need a final `default:'
 *	added in order to avoid warnings like [e.g. l.170 of ../main/util.c]
 *	  "enumeration value `FUNSXP' not handled in switch"
 */
typedef unsigned int SEXPTYPE;

#define NILSXP	     0	  /* nil = NULL */
#define SYMSXP	     1	  /* symbols */
#define LISTSXP	     2	  /* lists of dotted pairs */
#define CLOSXP	     3	  /* closures */
#define ENVSXP	     4	  /* environments */
#define PROMSXP	     5	  /* promises: [un]evaluated closure arguments */
#define LANGSXP	     6	  /* language constructs (special lists) */
#define SPECIALSXP   7	  /* special forms */
#define BUILTINSXP   8	  /* builtin non-special forms */
#define CHARSXP	     9	  /* "scalar" string type (internal only)*/
#define LGLSXP	    10	  /* logical vectors */
#define INTSXP	    13	  /* integer vectors */
#define REALSXP	    14	  /* real variables */
#define CPLXSXP	    15	  /* complex variables */
#define STRSXP	    16	  /* string vectors */
#define DOTSXP	    17	  /* dot-dot-dot object */
#define ANYSXP	    18	  /* make "any" args work.
			     Used in specifying types for symbol
			     registration to mean anything is okay  */
#define VECSXP	    19	  /* generic vectors */
#define EXPRSXP	    20	  /* expressions vectors */
#define BCODESXP    21    /* byte code */
#define EXTPTRSXP   22    /* external pointer */
#define WEAKREFSXP  23    /* weak reference */
#define RAWSXP      24    /* raw bytes */
#define S4SXP       25    /* S4, non-vector */

#define FUNSXP      99    /* Closure or Builtin or Special */


#else /* NOT YET */
/*------ enum_SEXPTYPE ----- */
typedef enum {
    NILSXP	= 0,	/* nil = NULL */
    SYMSXP	= 1,	/* symbols */
    LISTSXP	= 2,	/* lists of dotted pairs */
    CLOSXP	= 3,	/* closures */
    ENVSXP	= 4,	/* environments */
    PROMSXP	= 5,	/* promises: [un]evaluated closure arguments */
    LANGSXP	= 6,	/* language constructs (special lists) */
    SPECIALSXP	= 7,	/* special forms */
    BUILTINSXP	= 8,	/* builtin non-special forms */
    CHARSXP	= 9,	/* "scalar" string type (internal only)*/
    LGLSXP	= 10,	/* logical vectors */
    INTSXP	= 13,	/* integer vectors */
    REALSXP	= 14,	/* real variables */
    CPLXSXP	= 15,	/* complex variables */
    STRSXP	= 16,	/* string vectors */
    DOTSXP	= 17,	/* dot-dot-dot object */
    ANYSXP	= 18,	/* make "any" args work */
    VECSXP	= 19,	/* generic vectors */
    EXPRSXP	= 20,	/* expressions vectors */
    BCODESXP    = 21,   /* byte code */
    EXTPTRSXP   = 22,   /* external pointer */
    WEAKREFSXP  = 23,   /* weak reference */
    RAWSXP      = 24,   /* raw bytes */
    S4SXP         = 25,   /* S4 non-vector */

    FUNSXP	= 99	/* Closure or Builtin */
} SEXPTYPE;
#endif

#ifdef __cplusplus
/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 */

/* Flags */
struct sxpinfo_struct {
    SEXPTYPE type      :  5;/* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
			     * -> warning: `type' is narrower than values
			     *              of its type
			     * when SEXPTYPE was an enum */
    unsigned int obj   :  1;
    unsigned int named :  2;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;  /* functions and memory tracing */
    unsigned int spare :  1;  /* currently unused */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
}; /*		    Tot: 32 */

struct vecsxp_struct {
    R_len_t	length;
    R_len_t	truelength;
};

struct primsxp_struct {
    int offset;
};

struct symsxp_struct {
    struct SEXPREC *pname;
    struct SEXPREC *value;
    struct SEXPREC *internal;
};

struct listsxp_struct {
    struct SEXPREC *carval;
    struct SEXPREC *cdrval;
    struct SEXPREC *tagval;
};

struct envsxp_struct {
    struct SEXPREC *frame;
    struct SEXPREC *enclos;
    struct SEXPREC *hashtab;
};

struct closxp_struct {
    struct SEXPREC *formals;
    struct SEXPREC *body;
    struct SEXPREC *env;
};

struct promsxp_struct {
    struct SEXPREC *value;
    struct SEXPREC *expr;
    struct SEXPREC *env;
};

/* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */
#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    struct SEXPREC *attrib; \
    struct SEXPREC *gengc_next_node, *gengc_prev_node

/* The standard node structure consists of a header followed by the
   node data. */
typedef struct SEXPREC {
    SEXPREC_HEADER;
    union {
	struct primsxp_struct primsxp;
	struct symsxp_struct symsxp;
	struct listsxp_struct listsxp;
	struct envsxp_struct envsxp;
	struct closxp_struct closxp;
	struct promsxp_struct promsxp;
    } u;
} SEXPREC, *SEXP;

/* The generational collector uses a reduced version of SEXPREC as a
   header in vector nodes.  The layout MUST be kept consistent with
   the SEXPREC definition.  The standard SEXPREC takes up 7 words on
   most hardware; this reduced version should take up only 6 words.
   In addition to slightly reducing memory use, this can lead to more
   favorable data alignment on 32-bit architectures like the Intel
   Pentium III where odd word alignment of doubles is allowed but much
   less efficient than even word alignment. */
typedef struct VECTOR_SEXPREC {
    SEXPREC_HEADER;
    struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;

typedef union { VECTOR_SEXPREC s; double align; } SEXPREC_ALIGN;

/* General Cons Cell Attributes */
#define ATTRIB(x)	((x)->attrib)
#define OBJECT(x)	((x)->sxpinfo.obj)
#define MARK(x)		((x)->sxpinfo.mark)
#define TYPEOF(x)	((x)->sxpinfo.type)
#define NAMED(x)	((x)->sxpinfo.named)
#define TRACE(x)	((x)->sxpinfo.trace)
#define LEVELS(x)	((x)->sxpinfo.gp)
#define SET_OBJECT(x,v)	(((x)->sxpinfo.obj)=(v))
#define SET_TYPEOF(x,v)	(((x)->sxpinfo.type)=(v))
#define SET_NAMED(x,v)	(((x)->sxpinfo.named)=(v))
#define SET_TRACE(x,v)	(((x)->sxpinfo.trace)=(v))
#define SETLEVELS(x,v)	(((x)->sxpinfo.gp)=(v))

/* S4 object bit, set by R_do_new_object for all new() calls */
#define S4_OBJECT_MASK (1<<4)
#define IS_S4_OBJECT(x) ((x)->sxpinfo.gp & S4_OBJECT_MASK)
#define SET_S4_OBJECT(x) (((x)->sxpinfo.gp) |= S4_OBJECT_MASK)
#define UNSET_S4_OBJECT(x) (((x)->sxpinfo.gp) &= ~S4_OBJECT_MASK)

/* Vector Access Macros */
#define LENGTH(x)	(((VECSEXP) (x))->vecsxp.length)
#define TRUELENGTH(x)	(((VECSEXP) (x))->vecsxp.truelength)
#define SETLENGTH(x,v)		((((VECSEXP) (x))->vecsxp.length)=(v))
#define SET_TRUELENGTH(x,v)	((((VECSEXP) (x))->vecsxp.truelength)=(v))

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define DATAPTR(x)	(((SEXPREC_ALIGN *) (x)) + 1)
#define CHAR(x)		((char *) DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define RAW(x)		((Rbyte *) DATAPTR(x))
#define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
#define REAL(x)		((double *) DATAPTR(x))
#define STRING_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define VECTOR_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))

/* List Access Macros */
/* These also work for ... objects */
#define LISTVAL(x)	((x)->u.listsxp)
#define TAG(e)		((e)->u.listsxp.tagval)
#define CAR(e)		((e)->u.listsxp.carval)
#define CDR(e)		((e)->u.listsxp.cdrval)
#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CADDR(e)	CAR(CDR(CDR(e)))
#define CADDDR(e)	CAR(CDR(CDR(CDR(e))))
#define CAD4R(e)	CAR(CDR(CDR(CDR(CDR(e)))))
#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define MISSING(x)	((x)->sxpinfo.gp & MISSING_MASK)/* for closure calls */
#define SET_MISSING(x,v) do { \
  SEXP __x__ = (x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.formals)
#define BODY(x)		((x)->u.closxp.body)
#define CLOENV(x)	((x)->u.closxp.env)
#define DEBUG(x)	((x)->sxpinfo.debug)
#define SET_DEBUG(x,v)	(((x)->sxpinfo.debug)=(v))

/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)
#define DDVAL_MASK	1
#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#define SET_DDVAL_BIT(x) (((x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) (((x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) ((v) ? SET_DDVAL_BIT(x) : UNSET_DDVAL_BIT(x)) /* for ..1, ..2 etc */

/* Environment Access Macros */
#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define HASHTAB(x)	((x)->u.envsxp.hashtab)
#define ENVFLAGS(x)	((x)->sxpinfo.gp)	/* for environments */
#define SET_ENVFLAGS(x,v)	(((x)->sxpinfo.gp)=(v))

#else /* if not __cplusplus */

typedef struct SEXPREC *SEXP;

#define CHAR(x)		R_CHAR(x)

#endif /* __cplusplus */

char *(R_CHAR)(SEXP x);

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x);
int  (OBJECT)(SEXP x);
int  (MARK)(SEXP x);
int  (TYPEOF)(SEXP x);
int  (NAMED)(SEXP x);
void (SET_OBJECT)(SEXP x, int v);
void (SET_TYPEOF)(SEXP x, int v);
void (SET_NAMED)(SEXP x, int v);
void SET_ATTRIB(SEXP x, SEXP v);
void DUPLICATE_ATTRIB(SEXP to, SEXP from);

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x);
void (SET_S4_OBJECT)(SEXP x);
void (UNSET_S4_OBJECT)(SEXP x);

/* Vector Access Functions */
int  (LENGTH)(SEXP x);
int  (TRUELENGTH)(SEXP x);
void (SETLENGTH)(SEXP x, int v);
void (SET_TRUELENGTH)(SEXP x, int v);
int  (LEVELS)(SEXP x);
int  (SETLEVELS)(SEXP x, int v);

int  *(LOGICAL)(SEXP x);
int  *(INTEGER)(SEXP x);
Rbyte *(RAW)(SEXP x);
double *(REAL)(SEXP x);
Rcomplex *(COMPLEX)(SEXP x);
SEXP (STRING_ELT)(SEXP x, int i);
SEXP (VECTOR_ELT)(SEXP x, int i);
void SET_STRING_ELT(SEXP x, int i, SEXP v);
SEXP SET_VECTOR_ELT(SEXP x, int i, SEXP v);
SEXP *(STRING_PTR)(SEXP x);
SEXP *(VECTOR_PTR)(SEXP x);

/* List Access Functions */
/* These also work for ... objects */
#define CONS(a, b)	cons((a), (b))		/* data lists */
#define LCONS(a, b)	lcons((a), (b))		/* language lists */
SEXP (TAG)(SEXP e);
SEXP (CAR)(SEXP e);
SEXP (CDR)(SEXP e);
SEXP (CAAR)(SEXP e);
SEXP (CDAR)(SEXP e);
SEXP (CADR)(SEXP e);
SEXP (CDDR)(SEXP e);
SEXP (CADDR)(SEXP e);
SEXP (CADDDR)(SEXP e);
SEXP (CAD4R)(SEXP e);
int  (MISSING)(SEXP x);
void (SET_MISSING)(SEXP x, int v);
void SET_TAG(SEXP x, SEXP y);
SEXP SETCAR(SEXP x, SEXP y);
SEXP SETCDR(SEXP x, SEXP y);
SEXP SETCADR(SEXP x, SEXP y);
SEXP SETCADDR(SEXP x, SEXP y);
SEXP SETCADDDR(SEXP x, SEXP y);
SEXP SETCAD4R(SEXP e, SEXP y);

/* Closure Access Functions */
SEXP (FORMALS)(SEXP x);
SEXP (BODY)(SEXP x);
SEXP (CLOENV)(SEXP x);
int  (DEBUG)(SEXP x);
int  (TRACE)(SEXP x);
void (SET_DEBUG)(SEXP x, int v);
void (SET_TRACE)(SEXP x, int v);
void SET_FORMALS(SEXP x, SEXP v);
void SET_BODY(SEXP x, SEXP v);
void SET_CLOENV(SEXP x, SEXP v);

/* Symbol Access Functions */
SEXP (PRINTNAME)(SEXP x);
SEXP (SYMVALUE)(SEXP x);
SEXP (INTERNAL)(SEXP x);
int  (DDVAL)(SEXP x);
void (SET_DDVAL)(SEXP x, int v);
void SET_PRINTNAME(SEXP x, SEXP v);
void SET_SYMVALUE(SEXP x, SEXP v);
void SET_INTERNAL(SEXP x, SEXP v);

/* Environment Access Functions */
SEXP (FRAME)(SEXP x);
SEXP (ENCLOS)(SEXP x);
SEXP (HASHTAB)(SEXP x);
int  (ENVFLAGS)(SEXP x);
void (SET_ENVFLAGS)(SEXP x, int v);
void SET_FRAME(SEXP x, SEXP v);
void SET_ENCLOS(SEXP x, SEXP v);
void SET_HASHTAB(SEXP x, SEXP v);

/* Promise Access Functions */
/* First five have macro versions in Defn.h */
SEXP (PRCODE)(SEXP x);
SEXP (PRENV)(SEXP x);
SEXP (PRVALUE)(SEXP x);
int  (PRSEEN)(SEXP x);
void (SET_PRSEEN)(SEXP x, int v);
void SET_PRENV(SEXP x, SEXP v);
void SET_PRVALUE(SEXP x, SEXP v);
void SET_PRCODE(SEXP x, SEXP v);
void SET_PRSEEN(SEXP x, int v);

/* Hashing Functions */
/* There are macro versions in Defn.h */
int  (HASHASH)(SEXP x);
int  (HASHVALUE)(SEXP x);
void (SET_HASHASH)(SEXP x, int v);
void (SET_HASHVALUE)(SEXP x, int v);


/* External pointer access macros */
#define EXTPTR_PTR(x)	CAR(x)
#define EXTPTR_PROT(x)	CDR(x)
#define EXTPTR_TAG(x)	TAG(x)

#ifdef BYTECODE
/* Bytecode access macros */
#define BCODE_CODE(x)	CAR(x)
#define BCODE_CONSTS(x) CDR(x)
#define BCODE_EXPR(x)	TAG(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)
#else
#define isByteCode(x)	FALSE
#endif

/* CHARSXP charset bits */
#ifdef __cplusplus
# define LATIN1_MASK (1<<2)
# define IS_LATIN1(x) ((x)->sxpinfo.gp & LATIN1_MASK)
# define UNSET_LATIN1(x) (((x)->sxpinfo.gp) &= ~LATIN1_MASK)
# define UTF8_MASK (1<<3)
# define IS_UTF8(x) ((x)->sxpinfo.gp & UTF8_MASK)
# define UNSET_UTF8(x) (((x)->sxpinfo.gp) &= ~UTF8_MASK)
#else
/* Needed only for write-barrier testing */
int IS_LATIN1(SEXP x);
//void SET_LATIN1(SEXP x);
void UNSET_LATIN1(SEXP x);
int IS_UTF8(SEXP x);
//void SET_UTF8(SEXP x);
void UNSET_UTF8(SEXP x);
#endif

/* CHARSXP charset bits */

#ifndef __cplusplus
void SET_LATIN1(SEXP x);
#else
inline void SET_LATIN1(SEXP x)  {x->sxpinfo.gp |= LATIN1_MASK;}
#endif

#ifndef __cplusplus
void SET_UTF8(SEXP x);
#else
inline void SET_UTF8(SEXP x) {x->sxpinfo.gp |= UTF8_MASK;}
#endif

#ifdef __cplusplus
/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hasking groups.
 */

/* Primitive Access Macros */
#define SET_PRIMOFFSET(x,v)	(((x)->u.primsxp.offset)=(v))

/* Promise Access Macros */
#define PRCODE(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRVALUE(x)	((x)->u.promsxp.value)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))

/* Hashing Macros */
#define HASHASH(x)      ((x)->sxpinfo.gp)
#define HASHVALUE(x)    TRUELENGTH(x)
#define SET_HASHASH(x,v) (((x)->sxpinfo.gp)=(v))
#define SET_HASHVALUE(x,v) SET_TRUELENGTH(x, v)

/* Vector Heap Structure */
typedef struct {
	union {
		SEXP		backpointer;
		double		align;
	} u;
} VECREC, *VECP;

/* Vector Heap Macros */
#define BACKPOINTER(v)	((v).u.backpointer)
#define BYTE2VEC(n)	(((n)>0)?(((n)-1)/sizeof(VECREC)+1):0)
#define INT2VEC(n)	(((n)>0)?(((n)*sizeof(int)-1)/sizeof(VECREC)+1):0)
#define FLOAT2VEC(n)	(((n)>0)?(((n)*sizeof(double)-1)/sizeof(VECREC)+1):0)
#define COMPLEX2VEC(n)	(((n)>0)?(((n)*sizeof(Rcomplex)-1)/sizeof(VECREC)+1):0)
#define PTR2VEC(n)	(((n)>0)?(((n)*sizeof(SEXP)-1)/sizeof(VECREC)+1):0)
/* Bindings */
/* use the same bits (15 and 14) in symbols and bindings */
#define ACTIVE_BINDING_MASK (1<<15)
#define BINDING_LOCK_MASK (1<<14)
#define SPECIAL_BINDING_MASK (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define SET_ACTIVE_BINDING_BIT(b) ((b)->sxpinfo.gp |= ACTIVE_BINDING_MASK)
#define LOCK_BINDING(b) ((b)->sxpinfo.gp |= BINDING_LOCK_MASK)
#define UNLOCK_BINDING(b) ((b)->sxpinfo.gp &= (~BINDING_LOCK_MASK))

#else /* if not __cplusplus */

typedef struct VECREC *VECP;
void (SET_PRIMOFFSET)(SEXP x, int v);

Rboolean (IS_ACTIVE_BINDING)(SEXP b);
Rboolean (BINDING_IS_LOCKED)(SEXP b);
void (SET_ACTIVE_BINDING_BIT)(SEXP b);
void (LOCK_BINDING)(SEXP b);
void (UNLOCK_BINDING)(SEXP b);

#endif // __cplusplus

// Primitive access functions:

#ifndef __cplusplus
int PRIMOFFSET(SEXP x);
#else
inline int (PRIMOFFSET)(SEXP x) { return x->u.primsxp.offset; }
#endif

#ifdef __cplusplus
}
#endif

#endif /* ROBJECT_H */
