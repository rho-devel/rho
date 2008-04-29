/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file Environment.h
 * @brief Class CXXR::Environment and associated C interface.
 *
 * @todo Arguably R_GlobalEnv etc. should have type RObject* const.
 */

#ifndef RENVIRONMENT_H
#define RENVIRONMENT_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/ListVector.h"
#include "CXXR/PairList.h"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    /** @brief Mapping from names to R objects.
     *
     * An Environment defines a mapping from (pointers to)
     * CXXR::String objects to (pointers to) arbitrary objects of
     * classes derived from RObject.  Each Environment (except for the
     * standard empty environment) has an 'enclosing environment';
     * under the 'enclosing' relationship, Environment objects form a
     * tree with the empty environment as its root.
     */
    class Environment : public RObject {
    public:
	/**
	 * @param enclosing Pointer to the enclosing environment.
	 *
	 * @param namevals List of name-value pairs used to initialize
	 *          the environment.  Every element of this list must have
	 *          a tag (not checked), and these tags must be
	 *          distinct (not checked).  As presently implemented,
	 *          the constructed Environment takes ownership of
	 *          this list, so the calling code should not
	 *          subsequently modify it.
	 *
	 * @todo Probably the default for \a enclosing should be the
	 * empty environment.  \a namevals ought to be thoroughly
	 * checked.
	 */
	explicit Environment(Environment* enclosing = 0,
			     PairList* namevals = 0)
	    : RObject(ENVSXP), m_enclosing(enclosing), m_frame(namevals)
	{}

	/** @brief Base environment.
	 *
	 * @return pointer to the base environment.
	 */
	static Environment* base()
	{
	    return s_base_env;
	}

	/** @brief Access the enclosing environment.
	 *
	 * @return pointer to the enclosing environment.
	 */
	Environment* enclosingEnvironment() const
	{
	    return m_enclosing;
	}

	/** @brief Empty environment.
	 *
	 * @return const pointer to the standard empty environment.
	 */
	static const Environment* emptyEnvironment()
	{
	    return s_empty_env;
	}

	/** @brief Access the frame.
	 *
	 * @return pointer to the frame of this environment.
	 */
	PairList* frame()
	{
	    return m_frame;
	}

	/** @brief Access the frame (const variant).
	 *
	 * @return pointer to the frame of this environment.
	 */
	const PairList* frame() const
	{
	    return m_frame;
	}

	/** @brief Global environment.
	 *
	 * @return pointer to the global environment.
	 */
	static Environment* global()
	{
	    return s_global_env;
	}

	/** @brief Access the hash table.
	 *
	 * @return pointer to the hash table of this environment.
	 */
	ListVector* hashTable()
	{
	    return m_hashtable;
	}

	/** @brief Access the hash table (const variant).
	 *
	 * @return pointer to the hash table of this environment.
	 */
	const ListVector* hashTable() const
	{
	    return m_hashtable;
	}

	/** @brief Replace the enclosing environment.
	 *
	 * @param new_enclos Pointer to the environment now to be
	 *          considered to enclose this Environment.
	 *
	 * @todo This ought to check that the chain of ancestors
	 * is free of loops and terminates with the empty environment.
	 */
	void setEnclosingEnvironment(Environment* new_enclos)
	{
	    m_enclosing = new_enclos;
	    devolveAge(m_enclosing);
	}

	/** @brief Replace the frame.
	 *
	 * @param new_frame Pointer to the new frame of this
	 *          environment.  Every element of this list must have
	 *          a tag (not checked), and these tags must be
	 *          distinct (not checked).
	 */
	void setFrame(PairList* new_frame)
	{
	    m_frame = new_frame;
	    devolveAge(m_frame);
	}

	/** @brief Replace the hash table.
	 *
	 * @param new_hash_table Pointer to the new hash table.
	 *          (Because this member function will soon be
	 *          replaced, we won't go into the detailed
	 *          requirements for a hash table.)
	 */
	void setHashTable(ListVector* new_hash_table)
	{
	    m_hashtable = new_hash_table;
	    devolveAge(m_hashtable);
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "environment";
	}

	// Virtual function of RObject:
	const char* typeName() const;

	// Virtual function of GCNode:
	void visitChildren(const_visitor* v) const;
    private:
	static GCRoot<Environment> s_empty_env;
	static GCRoot<Environment> s_base_env;
	static GCRoot<Environment> s_global_env;

	Environment* m_enclosing;
	PairList* m_frame;
	ListVector* m_hashtable;

	// Declared private to ensure that Environment objects are
	// created only using 'new':
	~Environment() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Environment(const Environment&);
	Environment& operator=(const Environment&);
    };
}  // namespace CXXR

extern "C" {
#endif

    extern SEXP R_EmptyEnv;
    extern SEXP R_BaseEnv;
    extern SEXP R_GlobalEnv;

    /** @brief Is this a CXXR::Environment?
     *
     * @param s Pointer to an RObject.
     *
     * @return TRUE iff the RObject pointed to by s is an environment.
     */
#ifndef __cplusplus
    Rboolean Rf_isEnvironment(SEXP s);
#else
    inline Rboolean Rf_isEnvironment(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == ENVSXP);
    }
#endif

    /** @brief Access enclosing environment.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to the enclosing environment of \a x .
     */
#ifndef __cplusplus
    SEXP ENCLOS(SEXP x);
#else
    inline SEXP ENCLOS(SEXP x)
    {
	const CXXR::Environment& env
	    = *CXXR::SEXP_downcast<CXXR::Environment*>(x);
	return env.enclosingEnvironment();
    }
#endif

    /** @brief Access an environment's flags.
     *
     * @param x Pointer to a CXXR::Environment (not currently checked).
     *
     * @return the environment flags of \a x .
     */
#ifndef __cplusplus
    int ENVFLAGS(SEXP x);
#else
    inline int ENVFLAGS(SEXP x) {return x->m_gpbits;}
#endif

    /** @brief Access an environment's frame.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to the frame of \a x (may be null).
     */
#ifndef __cplusplus
    SEXP FRAME(SEXP x);
#else
    inline SEXP FRAME(SEXP x)
    {
	CXXR::Environment* env
	    = CXXR::SEXP_downcast<CXXR::Environment*>(x);
	return env->frame();
    }
#endif

    /** @brief Access an environment's hash table.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to the hash table of \a x (may be null).
     */
#ifndef __cplusplus
    SEXP HASHTAB(SEXP x);
#else
    inline SEXP HASHTAB(SEXP x)
    {
	CXXR::Environment* env
	    = CXXR::SEXP_downcast<CXXR::Environment*>(x);
	return env->hashTable();
    }
#endif

    /** @brief Set an environment's enclosing environment.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @param v Pointer to a CXXR::Environment (checked) intended to be
     *          the new enclosing environment of \a x.
     */
#ifndef __cplusplus
    void SET_ENCLOS(SEXP x, SEXP v);
#else
    inline void SET_ENCLOS(SEXP x, SEXP v)
    {
	CXXR::Environment* env
	    = CXXR::SEXP_downcast<CXXR::Environment*>(x);
	CXXR::Environment* enc
	    = CXXR::SEXP_downcast<CXXR::Environment*>(v);
	env->setEnclosingEnvironment(enc);
    }
#endif

    /** @brief Set environment flags.
     *
     * @param x Pointer to a CXXR::Environment (not currently checked).
     *
     * @param v The new flags.
     *
     * @deprecated
     */
#ifndef __cplusplus
    void SET_ENVFLAGS(SEXP x, int v);
#else
    inline void SET_ENVFLAGS(SEXP x, int v) {x->m_gpbits = v;}
#endif

    /** @brief Set environment's frame.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @param v Pointer to the new frame.  This must be a CXXR::PairList
     *          (checked), and every element of this list must have a tag
     *          (not checked), and these tags must be distinct (not
     *          checked).
     *
     * @todo Probably should be private.
     */
#ifndef __cplusplus
    void SET_FRAME(SEXP x, SEXP v);
#else
    inline void SET_FRAME(SEXP x, SEXP v)
    {
	CXXR::Environment* env
	    = CXXR::SEXP_downcast<CXXR::Environment*>(x);
	CXXR::PairList* pl
	    = CXXR::SEXP_downcast<CXXR::PairList*>(v);
	env->setFrame(pl);
    }
#endif

    /** @brief Set environment's hash table.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @param v Pointer to the new hash table, which must be a
     * CXXR::ListVector (checked), and satisfy other conditions.
     *
     * @todo To be removed quite soon.
     */
#ifndef __cplusplus
    void SET_HASHTAB(SEXP x, SEXP v);
#else
    inline void SET_HASHTAB(SEXP x, SEXP v)
    {
	CXXR::Environment* env
	    = CXXR::SEXP_downcast<CXXR::Environment*>(x);
	CXXR::ListVector* lv
	    = CXXR::SEXP_downcast<CXXR::ListVector*>(v);
	env->setHashTable(lv);
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RENVIRONMENT_H */
