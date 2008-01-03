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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/** @file RObject.h
 *
 * Class RObject.
 */

#ifndef ROBJECT_H
#define ROBJECT_H

#include "R_ext/Boolean.h"

#ifdef __cplusplus

#include "CXXR/FlagWord.hpp"
#include "CXXR/GCNode.hpp"

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
/*------ enum_SEXPTYPE ----- */
typedef enum {
    NILSXP	= 0,	/* nil = NULL
			 *
			 * arr 2007/07/21: no RObject now has this
			 * type, but for backward compatibility TYPEOF
			 * will return NILSXP if passed a zero
			 * pointer.
			 */
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
    S4SXP       = 25,   /* S4 non-vector */

    FUNSXP	= 99	/* Closure or Builtin */
} SEXPTYPE;

#ifdef __cplusplus
}  // extern "C"

namespace CXXR {
    class RObject;

    struct primsxp_struct {
	int offset;
    };

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

    /** @brief Replacement for CR's SEXPREC.
     *
     * This class is the rough equivalent within CXXR of the SEXPREC
     * union within CR.  However, all functionality relating to
     * garbage collection has been factored out into the base class
     * GCNode, and as CXXR development proceeds other functionality
     * will be factored out into derived classes (corresponding
     * roughly, but not exactly, to different SEXPTYPEs within CR).
     *
     * Eventually this class may end up simply as the home of R
     * attributes.
     */
    struct RObject : public GCNode {
	/**
	 * @param stype Required type of the RObject.
	 */
	RObject(SEXPTYPE stype = ANYSXP)
	    : m_type(stype), m_gpbits(m_flags.m_flags)
	{}

	/**
	 * @return Pointer to the attributes of this object.
	 */
	const RObject* attributes() const {return m_attrib;}

	// Virtual methods of GCNode:
	void visitChildren(const_visitor* v) const;
	void visitChildren(visitor* v);

	/**
	 * @return pointer to first element (car) of this list.
	 */
	const RObject* car() const {return u.listsxp.carval;}

	/**
	 * @return pointer to tail (cdr) of this list.
	 */
	const RObject* cdr() const {return u.listsxp.cdrval;}

	/**
	 * @return pointer to enclosing environment.
	 */
	const RObject* enclosingEnvironment() const {return u.envsxp.enclos;}

	/**
	 * @return pointer to frame of this environment.
	 */
	const RObject* frame() const {return u.envsxp.frame;}

	/**
	 * @return pointer to hash table of this environment.
	 */
	const RObject* hashTable() const {return u.envsxp.hashtab;}

	/**
	 * @return length of this vector.
	 */
	R_len_t length() const {return u.vecsxp.length;}

	/**
	 * @return SEXPTYPE of this object.
	 */
	SEXPTYPE sexptype() const {return m_type;}

	/**
	 * @return pointer to tag of this list.
	 */
	const RObject* tag() const {return u.listsxp.tagval;}

        // To be protected in future:

	/** Destructor
	 *
	 * @note The destructor is protected to ensure that RObjects
	 * are allocated on the heap.  (See Meyers 'More Effective
	 * C++' Item 27.) Derived classes should likewise declare
	 * their constructors private or protected.
	 */
	virtual ~RObject();

	// To be private in future:

	SEXPTYPE m_type              : 7;
	bool m_has_class             : 1;
	unsigned int m_named         : 2;
	bool m_debug                 : 1;
	bool m_trace                 : 1;
	FlagWord m_flags;
	unsigned short& m_gpbits;
	RObject *m_attrib;
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
    };

    /* S4 object bit, set by R_do_new_object for all new() calls */
#define S4_OBJECT_MASK (1<<4)

#define DDVAL_MASK	1


    /*
     * In CXXR USE_RINTERNALS is defined only in source files inherited
     * from C R that need privileged access to C++ objects, e.g. because
     * the file implements what is or will be a friend function...
     */
#ifdef USE_RINTERNALS

    /*
     * ... Well, that was the idea anyway.
     */

#endif // USE_RINTERNALS

}  // namespace CXXR

typedef CXXR::RObject SEXPREC, *SEXP;

extern "C" {
#else /* if not __cplusplus */

typedef struct SEXPREC *SEXP;

#endif /* __cplusplus */

/**
 * Object type.
 * @param x Pointer to \c RObject.
 * @return \c SEXPTYPE of \a x, or NILSXP if x is a null pointer.
 */
#ifndef __cplusplus
SEXPTYPE TYPEOF(const SEXP x);
#else
inline SEXPTYPE TYPEOF(const SEXP x)  {return x ? x->sexptype() : NILSXP;}
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
    return Rboolean(x && x->m_has_class);
}
#endif

/* Various tests */

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is either a null
 * pointer (i.e. <tt>== R_NilValue</tt> in CXXR), or is an RObject
 * with SEXPTYPE NILSXP (should not happen in CXXR).
 */
#ifndef __cplusplus
Rboolean Rf_isNull(SEXP s);
#else
inline Rboolean Rf_isNull(SEXP s)
{
    return Rboolean(!s || TYPEOF(s) == NILSXP);
}
#endif

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is a symbol.
 */
#ifndef __cplusplus
Rboolean Rf_isSymbol(SEXP s);
#else
inline Rboolean Rf_isSymbol(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == SYMSXP);
}
#endif

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is a logical vector.
 */
#ifndef __cplusplus
Rboolean (Rf_isLogical)(SEXP s);
#else
inline Rboolean (Rf_isLogical)(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == LGLSXP);
}
#endif

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is a real vector.
 */
#ifndef __cplusplus
Rboolean Rf_isReal(SEXP s);
#else
inline Rboolean Rf_isReal(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == REALSXP);
}
#endif

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is a complex vector.
 */
#ifndef __cplusplus
Rboolean Rf_isComplex(SEXP s);
#else
inline Rboolean Rf_isComplex(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == CPLXSXP);
}
#endif

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is an expression.
 */
#ifndef __cplusplus
Rboolean Rf_isExpression(SEXP s);
#else
inline Rboolean Rf_isExpression(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == EXPRSXP);
}
#endif

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s is a vector of strings.
 */
#ifndef __cplusplus
Rboolean Rf_isString(SEXP s);
#else
inline Rboolean Rf_isString(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == STRSXP);
}
#endif

/**
 * @param s Pointer to an RObject.
 * @return TRUE iff the RObject pointed to by s has a class attribute.
 */
#ifndef __cplusplus
Rboolean Rf_isObject(SEXP s);
#else
inline Rboolean Rf_isObject(SEXP s)
{
    return OBJECT(s);
}
#endif

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
inline SEXP ATTRIB(SEXP x) {return x ? x->m_attrib : 0;}
#endif

/**
 * @deprecated
 */
#ifndef __cplusplus
int LEVELS(SEXP x);
#else
inline int LEVELS(SEXP x) {return x->m_gpbits;}
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
inline int NAMED(SEXP x) {return x ? x->m_named : 0;}
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
inline int TRACE(SEXP x) {return x ? x->m_trace : 0;}
#endif

/**
 * @deprecated
 */
#ifndef __cplusplus
int SETLEVELS(SEXP x, int v);
#else
inline int SETLEVELS(SEXP x, int v) {return x->m_gpbits = v;}
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
    x->m_named = v;
}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_OBJECT(SEXP x, int v);
#else
    inline void SET_OBJECT(SEXP x, int v) {x->m_has_class = v;}
#endif

#ifndef __cplusplus
void SET_TRACE(SEXP x, int v);
#else
inline void SET_TRACE(SEXP x, int v) {x->m_trace = v;}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_TYPEOF(SEXP x, SEXPTYPE v);
#else
inline void SET_TYPEOF(SEXP x, SEXPTYPE v) {x->m_type = v;}
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
    return Rboolean(x && (x->m_gpbits & S4_OBJECT_MASK));
}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void SET_S4_OBJECT(SEXP x);
#else
inline void SET_S4_OBJECT(SEXP x)  {x->m_gpbits |= S4_OBJECT_MASK;}
#endif

/**
 * @deprecated Ought to be private.
 */
#ifndef __cplusplus
void UNSET_S4_OBJECT(SEXP x);
#else
inline void UNSET_S4_OBJECT(SEXP x)  {x->m_gpbits &= ~S4_OBJECT_MASK;}
#endif

/**
 * @brief Create an S4 object.
 *
 * @return Pointer to the created vector.
 */
SEXP Rf_allocS4Object();

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
    return Rboolean(b->m_gpbits & ACTIVE_BINDING_MASK);
}
#endif

#ifndef __cplusplus
Rboolean BINDING_IS_LOCKED(SEXP b);
#else
inline Rboolean BINDING_IS_LOCKED(SEXP b)
{
    return Rboolean(b->m_gpbits & BINDING_LOCK_MASK);
}
#endif

#ifndef __cplusplus
void SET_ACTIVE_BINDING_BIT(SEXP b);
#else
inline void SET_ACTIVE_BINDING_BIT(SEXP b)
{
    b->m_gpbits |= ACTIVE_BINDING_MASK;
}
#endif

#ifndef __cplusplus
void LOCK_BINDING(SEXP b);
#else
inline void LOCK_BINDING(SEXP b) {b->m_gpbits |= BINDING_LOCK_MASK;}
#endif

#ifndef __cplusplus
void UNLOCK_BINDING(SEXP b);
#else
inline void UNLOCK_BINDING(SEXP b) {b->m_gpbits &= (~BINDING_LOCK_MASK);}
#endif

#ifdef __cplusplus
}
#endif

#endif /* ROBJECT_H */
