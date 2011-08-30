/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
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

#include "CXXR/FunctionBase.h"

#ifdef __cplusplus

#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/export.hpp>
#include <boost/serialization/split_member.hpp>
#include "CXXR/ArgMatcher.hpp"
#include "CXXR/BSerializer.hpp"
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
    class Closure : public FunctionBase {
    public:
	/**
	 * @param formal_args List of formal arguments.
	 *
	 * @param body Pointer to the body of the Closure.  This must
	 *          be either a null pointer or a pointer to an object
	 *          of one of the following types: PairList,
	 *          Expression, Symbol, ExpressionVector, ListVector
	 *          or ByteCode (checked).
	 *
	 * @param env pointer to the environment in which the Closure
	 *          is to be evaluated.
	 */
	Closure(const PairList* formal_args, RObject* body,
		Environment* env = Environment::global());

	/** @brief Copy constructor.
	 *
	 * @param pattern Closure to be copied.
	 */
	Closure(const Closure& pattern)
	    : FunctionBase(pattern), m_debug(false),
	      m_matcher(pattern.m_matcher), m_body(pattern.m_body),
	      m_environment(pattern.m_environment)
	{}

	/** @brief Access the body of the Closure.
	 *
	 * @return Pointer to the body of the Closure.
	 */
	const RObject* body() const
	{
	    return m_body;
	}

	/** @brief Is debugging enabled?
	 *
	 * @return true iff debugging is currently enabled for this
	 * Closure.
	 */
	bool debugging() const
	{
	    return m_debug;
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
	    return m_matcher->formalArgs();
	}

	/** @brief Set debugging status.
	 *
	 * @param on The required new debugging status (true =
	 *           enabled).
	 */
	void setDebugging(bool on)
	{
	    m_debug = on;
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
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "closure";
	}

	// Virtual function of FunctionBase:
	RObject* apply(const Expression* call,
		       const PairList* args, Environment* env);

	// Virtual functions of RObject:
        Closure* clone() const;
	const char* typeName() const;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Virtual function of GCNode:
	void detachReferents();
    private:
        friend class boost::serialization::access;
	bool m_debug;
	GCEdge<const ArgMatcher> m_matcher;
	GCEdge<> m_body;
	GCEdge<Environment> m_environment;

	// Declared private to ensure that Environment objects are
	// created only using 'new':
	~Closure() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Closure& operator=(const Closure&);

	// Called by apply() to handle debugging:
	void debug(Environment* newenv, const Expression* call,
		   const PairList* args, Environment* argsenv);

	// Serialization
	template<class Archive>
	void load(Archive & ar, const unsigned int version) {
	    ar & boost::serialization::base_object<RObject>(*this);
	    GCEdge<const PairList> fargs; // For deserialization
	    BSerializer::attrib("formal_args");
	    ar >> fargs;
	    // Protect from GC
	    GCStackRoot<const PairList> formal_args(fargs);
	    m_matcher=expose(new ArgMatcher(formal_args));
	    BSerializer::attrib("m_body");
	    ar >> m_body;
	    BSerializer::attrib("m_environment");
	    ar >> m_environment;
	}

	template<class Archive>
	void save(Archive & ar, const unsigned int version) const {
	    ar & boost::serialization::base_object<RObject>(*this);
	    GCEdge<const PairList> formal_args(m_matcher->formalArgs());
	    BSerializer::attrib("formal_args");
	    ar << formal_args;
	    BSerializer::attrib("m_body");
	    ar << m_body;
	    BSerializer::attrib("m_environment");
	    ar << m_environment;
	}

	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("Closure");
	    boost::serialization::split_member(ar, *this, version);
	}

	// Used by boost::serialization
	// Initialises fields not serialized
	Closure()
	    : FunctionBase(CLOSXP), m_debug(false) {}
	

    };
}  // namespace CXXR

BOOST_CLASS_EXPORT(CXXR::Closure)

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
    SEXP Rf_mkCLOSXP(SEXP formal_args, SEXP body, SEXP env);

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
	const Closure& clos = *SEXP_downcast<Closure*>(x);
	return const_cast<PairList*>(clos.formalArgs());
    }
#endif

    /** @brief Query debugging status.
     *
     * @param x Pointer to a CXXR::Closure object.
     *
     * @return \c true if debugging is set, i.e. evaluations of the
     *         function should run under the browser.
     *
     * @note In CXXR, RDEBUG() is applicable only to closures; use
     * ENV_DEBUG() to query the debugging (single-stepping) state
     * for environments.
     */
#ifndef __cplusplus
    Rboolean RDEBUG(SEXP x);
#else
    inline Rboolean RDEBUG(SEXP x)
    {
	using namespace CXXR;
	const Closure& clos = *SEXP_downcast<const Closure*>(x);
	return Rboolean(clos.debugging());
    }
#endif

#ifndef __cplusplus
    int RSTEP(SEXP x);
#else
    inline int RSTEP(SEXP x)
    {
	return 0;
    }
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

    /**
     * Set the debugging state of a CXXR::Closure object.
     *
     * @param x Pointer a CXXR::Closure object (checked).
     *
     * @param v The new debugging state.
     *
     * @note In CXXR, SET_RDEBUG() is applicable only to closures; use
     * SET_ENV_DEBUG() to set the debugging (single-stepping) state
     * for environments.
     */
#ifndef __cplusplus
    void SET_RDEBUG(SEXP x, Rboolean v);
#else
    inline void SET_RDEBUG(SEXP x, Rboolean v)
    {
	using namespace CXXR;
	Closure& clos = *SEXP_downcast<Closure*>(x);
	clos.setDebugging(v);
    }
#endif

#ifndef __cplusplus
    void SET_RSTEP(SEXP x, int v);
#else
    inline void SET_RSTEP(SEXP x, int v)
    {
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RCLOSURE_H */
