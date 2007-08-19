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

/** @file RObject.h
 * Future class RObject.
 */

#ifndef ROBJECT_H
#define ROBJECT_H

#include "R_ext/Boolean.h"
#include "Rf_namespace.h"

#ifdef __cplusplus
#include <cstddef>

extern "C" {
#endif

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
                          /* arr 2007/07/21: no SEXPREC now has this
			     type, but for backward compatibility
			     TYPEOF will return NILSXP if passed a
			     zero pointer.
			  */
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

struct primsxp_struct {
    int offset;
};

class RObject;

struct symsxp_struct {
    RObject *pname;
    RObject *value;
    RObject *internal;
};

struct listsxp_struct {
    RObject *carval;
    RObject *cdrval;
    RObject *tagval;
};

struct envsxp_struct {
    RObject *frame;
    RObject *enclos;
    RObject *hashtab;
};

struct closxp_struct {
    RObject *formals;
    RObject *body;
    RObject *env;
};

struct promsxp_struct {
    RObject *value;
    RObject *expr;
    RObject *env;
};

struct vecsxp_struct {
    R_len_t	length;
    R_len_t	truelength;
};

/* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */
#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    RObject *attrib; \
    RObject *gengc_next_node, *gengc_prev_node

/* The standard node structure consists of a header followed by the
   node data. */
typedef struct RObject {
    SEXPREC_HEADER;
    union {
	struct primsxp_struct primsxp;
	struct symsxp_struct symsxp;
	struct listsxp_struct listsxp;
	struct envsxp_struct envsxp;
	struct closxp_struct closxp;
	struct promsxp_struct promsxp;
	struct vecsxp_struct vecsxp;
    } u;
    void* m_data;
    size_t m_databytes;
} SEXPREC;

typedef RObject* SEXP;

/* S4 object bit, set by R_do_new_object for all new() calls */
#define S4_OBJECT_MASK (1<<4)

#define DDVAL_MASK	1


/*
 * In CXXR USE_RINTERNALS is defined only in source files inherited
 * from C R that need privileged access to C++ objects, e.g. because
 * the file implements what is or will be a friend function.
 */
#ifdef USE_RINTERNALS

/*
 * Well, that was the idea anyway.
 */

#endif // USE_RINTERNALS

#else /* if not __cplusplus */

typedef struct SEXPREC *SEXP;

#endif /* __cplusplus */

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even 
   if the macro version is in use.
*/

/* General Cons Cell Attributes */

/**
 * Return the attributes of an \c RObject.
 * @param x Pointer to the \c RObject whose attributes are required.
 * @return Pointer to the attributes object of \a x , or 0 if \a x is
 * a null pointer.
 */
#ifndef __cplusplus
SEXP ATTRIB(SEXP x);
#else
inline SEXP ATTRIB(SEXP x) {return x ? x->attrib : 0;}
#endif

/**
 * @deprecated
 */
#ifndef __cplusplus
int LEVELS(SEXP x);
#else
inline int LEVELS(SEXP x) {return x->sxpinfo.gp;}
#endif

/**
 * Object in use?
 * @param x Pointer to an \c RObject.
 * @return true iff \a x is considered to be in use by garbage collector.
 * @deprecated Depends on GC.
 */
#ifndef __cplusplus
int MARK(SEXP x);
#else
inline int MARK(SEXP x) {return x->sxpinfo.mark;}
#endif

/**
 * Object copying status.
 * @param x Pointer to \c RObject.
 * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
 * null pointer.
 */
#ifndef __cplusplus
int NAMED(SEXP x);
#else
inline int NAMED(SEXP x) {return x ? x->sxpinfo.named : 0;}
#endif

/**
 * Does \c RObject have a class attribute?.
 * @param x Pointer to an \c RObject.
 * @return true iff \a x has a class attribute.  Returns false if \a x
 * is 0.
 */
#ifndef __cplusplus
Rboolean OBJECT(const SEXP x);
#else
inline Rboolean OBJECT(const SEXP x)
{
    return Rboolean(x && x->sxpinfo.obj);
}
#endif

/**
 * Object tracing status.
 * @param x Pointer to \c RObject.
 * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
 * null pointer.
 */
#ifndef __cplusplus
int TRACE(SEXP x);
#else
inline int TRACE(SEXP x) {return x ? x->sxpinfo.trace : 0;}
#endif

/**
 * Object type.
 * @param x Pointer to \c RObject.
 * @return \c SEXPTYPE of \a x, or NILSXP if x is a null pointer.
 */
#ifndef __cplusplus
SEXPTYPE TYPEOF(const SEXP x);
#else
inline SEXPTYPE TYPEOF(const SEXP x)  {return x ? x->sxpinfo.type : NILSXP;}
#endif

/**
 * @deprecated
 */
#ifndef __cplusplus
int SETLEVELS(SEXP x, int v);
#else
inline int SETLEVELS(SEXP x, int v) {return x->sxpinfo.gp = v;}
#endif

/**
 * Replace x's attributes by \a v.
 * @param x Pointer to \c RObject.
 * @param v Pointer to attributes \c RObject.
 * @todo Could \a v be \c const ?
 */
void SET_ATTRIB(SEXP x, SEXP v);

/**
 * Set object copying status.  Does nothing if \a x is a null pointer.
 * @param x Pointer to \c RObject.
 * @param v Refer to 'R Internals' document.
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_NAMED(SEXP x, int v);
#else
inline void SET_NAMED(SEXP x, int v)
{
    if (!x) return;
    x->sxpinfo.named = v;
}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_OBJECT(SEXP x, int v);
#else
inline void SET_OBJECT(SEXP x, int v) {x->sxpinfo.obj = v;}
#endif

#ifndef __cplusplus
void SET_TRACE(SEXP x, int v);
#else
inline void SET_TRACE(SEXP x, int v) {x->sxpinfo.trace = v;}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_TYPEOF(SEXP x, int v);
#else
inline void SET_TYPEOF(SEXP x, int v) {x->sxpinfo.type = v;}
#endif

/**
 * Replace \a to's attributes by those of \a from.
 * @param to Pointer to \c RObject.
 * @param from Pointer to another \c RObject.
 */
void DUPLICATE_ATTRIB(SEXP to, const SEXP from);

/* S4 object testing */

/**
 * An S4 object?
 * @param x Pointer to \c RObject.
 * @return true iff \a x is an S4 object.  Returns false if \a x
 * is 0.
 */
#ifndef __cplusplus
Rboolean IS_S4_OBJECT(const SEXP x);
#else
inline Rboolean IS_S4_OBJECT(const SEXP x)
{
    return Rboolean(x && (x->sxpinfo.gp & S4_OBJECT_MASK));
}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_S4_OBJECT(SEXP x);
#else
inline void SET_S4_OBJECT(SEXP x)  {x->sxpinfo.gp |= S4_OBJECT_MASK;}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void UNSET_S4_OBJECT(SEXP x);
#else
inline void UNSET_S4_OBJECT(SEXP x)  {x->sxpinfo.gp &= ~S4_OBJECT_MASK;}
#endif

/**
 * @brief Create an S4 object.
 *
 * @return Pointer to the created vector.
 */
SEXP allocS4Object();

/* Bindings */
/* use the same bits (15 and 14) in symbols and bindings */
#define ACTIVE_BINDING_MASK (1<<15)
#define BINDING_LOCK_MASK (1<<14)
#define SPECIAL_BINDING_MASK (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK)

#ifndef __cplusplus
Rboolean IS_ACTIVE_BINDING(SEXP b);
#else
inline Rboolean IS_ACTIVE_BINDING(SEXP b)
{
    return Rboolean(b->sxpinfo.gp & ACTIVE_BINDING_MASK);
}
#endif

#ifndef __cplusplus
Rboolean BINDING_IS_LOCKED(SEXP b);
#else
inline Rboolean BINDING_IS_LOCKED(SEXP b)
{
    return Rboolean(b->sxpinfo.gp & BINDING_LOCK_MASK);
}
#endif

#ifndef __cplusplus
void SET_ACTIVE_BINDING_BIT(SEXP b);
#else
inline void SET_ACTIVE_BINDING_BIT(SEXP b)
{
    b->sxpinfo.gp |= ACTIVE_BINDING_MASK;
}
#endif

#ifndef __cplusplus
void LOCK_BINDING(SEXP b);
#else
inline void LOCK_BINDING(SEXP b) {b->sxpinfo.gp |= BINDING_LOCK_MASK;}
#endif

#ifndef __cplusplus
void UNLOCK_BINDING(SEXP b);
#else
inline void UNLOCK_BINDING(SEXP b) {b->sxpinfo.gp &= (~BINDING_LOCK_MASK);}
#endif

#ifdef __cplusplus
}
#endif

#endif /* ROBJECT_H */
