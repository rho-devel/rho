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

/** @file Closure.h
 * @brief Class CXXR::Closure and associated C interface.
 */

#ifndef RCLOSURE_H
#define RCLOSURE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/Environment.h"
#include "CXXR/PairList.h"

namespace CXXR {
    /** @brief Class representing a functional programming closure.
     *
     * A closure associates a function definition (the body) with a
     * list of formal arguments and an environment.  In evaluating the
     * function, non-local variables within the function definition
     * are interpreted by reference to the specified environment (and
     * its enclosing environments).
     */
    class Closure : public RObject {
    public:
	/**
	 * @param formal_args List of formal arguments.
	 *
	 * @param body Pointer to the body of the Closure.  This must
	 *          be either a null pointer or a pointer to an object
	 *          of one of the following types: PairList,
	 *          Expression, Symbol (including SpecialSymbol),
	 *          ExpressionVector, ListVector or ByteCode
	 *          (checked).
	 *
	 * @param env pointer to the environment in which the Closure
	 *          is to be evaluated.
	 */
	Closure(const PairList* formal_args, const RObject* body,
		Environment* env = Environment::global());

	/** @brief Access the body of the Closure.
	 *
	 * @return Pointer to the body of the Closure.
	 */
	const RObject* body() const
	{
	    return m_body;
	}

	/** @brief Access the environment of the Closure.
	 *
	 * @return Pointer to the environment of the Closure.
	 */
	Environment* environment() const
	{
	    return m_environment;
	}

	/** @brief Access the formal argument list of the Closure.
	 *
	 * @return Pointer to the formal argument list of the Closure.
	 */
	const PairList* formalArgs() const
	{
	    return m_formals;
	}

	/** @brief Replace the environment of the closure.
	 *
	 * @param new_env Pointer to the environment now to be
	 *          considered as the environment of this Closure.  A
	 *          null pointer is not permissible (not checked).
	 */
	void setEnvironment(Environment* new_env)
	{
	    m_environment = new_env;
	    devolveAge(m_environment);
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "closure";
	}

	// Virtual function of RObject:
	const char* typeName() const;

	// Virtual function of GCNode:
	void visitChildren(const_visitor* v) const;
    private:
	const PairList* m_formals;
	const RObject* m_body;
	Environment* m_environment;

	// Declared private to ensure that Environment objects are
	// created only using 'new':
	~Closure() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Closure(const Closure&);
	Closure& operator=(const Closure&);
    };
}  // namespace CXXR

extern "C" {
#endif

    /** @brief Create a CXXR::Closure object.
     *
     * @param formal_args Pointer to a CXXR::PairList (checked) of
     *          formal arguments.
     *
     * @param body Pointer to the body of the CXXR::Closure.  This must be
     *          either a null pointer or a pointer to an object of one
     *          of the following types: LISTSXP, LANGSXP, SYMSXP,
     *          EXPRSXP, VECSXP or BCODESXP (checked).
     *
     * @param env pointer to the CXXR::Environment (checked) in which the
     *          closure is to be evaluated.
     *
     * @return pointer to the created closure object.
     */
    SEXP Rf_mkCLOSXP(SEXP formals, SEXP body, SEXP rho);

    /** @brief Access the body of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the body of \a x.
     */
#ifndef __cplusplus
    SEXP BODY(SEXP x);
#else
    inline SEXP BODY(SEXP x)
    {
	using namespace CXXR;
	const Closure& clo = *SEXP_downcast<Closure*>(x);
	return const_cast<RObject*>(clo.body());
    }
#endif

    /** @brief Access the environment of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the environment of x.
     */
#ifndef __cplusplus
    SEXP CLOENV(SEXP x);
#else
    inline SEXP CLOENV(SEXP x)
    {
	using namespace CXXR;
	Closure& clo = *SEXP_downcast<Closure*>(x);
	return clo.environment();
    }
#endif

    /**
     * @param x Pointer to a CXXR::Closure object.
     * @return \c true if debugging is set, i.e. evaluations of the
     *         function should run under the browser.
     */
#ifndef __cplusplus
    Rboolean DEBUG(SEXP x);
#else
    inline Rboolean DEBUG(SEXP x) {return Rboolean(x->m_debug);}
#endif

    /** @brief Access formal arguments of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the formal argument list of \a x.
     */
#ifndef __cplusplus
    SEXP FORMALS(SEXP x);
#else
    inline SEXP FORMALS(SEXP x)
    {
	using namespace CXXR;
	const Closure& clo = *SEXP_downcast<Closure*>(x);
	return const_cast<PairList*>(clo.formalArgs());
    }
#endif

#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */

#ifndef __cplusplus
    int MISSING(SEXP x);
#else
    inline int MISSING(SEXP x) {return x->m_flags.m_flags & MISSING_MASK;}
#endif

    /** @brief Replace the environment of a closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @param v Pointer to the environment now to be
     *          considered as the environment of this CXXR::Closure.  A
     *          null pointer is not permissible (not checked).
     */
#ifndef __cplusplus
    void SET_CLOENV(SEXP x, SEXP v);
#else
    inline void SET_CLOENV(SEXP x, SEXP v)
    {
	using namespace CXXR;
	Closure& clos = *SEXP_downcast<Closure*>(x);
	Environment* env = SEXP_downcast<Environment*>(v);
	clos.setEnvironment(env);
    }
#endif

#ifndef __cplusplus
    void SET_MISSING(SEXP x, int v);
#else
    inline void SET_MISSING(SEXP x, int v)
    {
	int other_flags = x->m_flags.m_flags & ~MISSING_MASK;
	x->m_flags.m_flags = other_flags | v;
    }
#endif

    /**
     * Set the debugging state of a CXXR::Closure object.
     * @param x Pointer a closure object.
     * @param v The new debugging state.
     */
#ifndef __cplusplus
    void SET_DEBUG(SEXP x, Rboolean v);
#else
    inline void SET_DEBUG(SEXP x, Rboolean v) {x->m_debug = v;}
#endif

#ifdef __cplusplus
}
#endif

#endif /* RCLOSURE_H */
