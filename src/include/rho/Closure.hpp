/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 * @brief Class rho::Closure and associated C interface.
 */

#ifndef RCLOSURE_H
#define RCLOSURE_H

#include "rho/FunctionBase.hpp"
#include "rho/ArgMatcher.hpp"
#include "rho/Environment.hpp"
#include "rho/PairList.hpp"

namespace rho {
    class ClosureContext;

    namespace JIT {
        class CompiledExpression;
    }

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
              m_num_invokes(0),
	      m_body(pattern.m_body),
	      m_environment(pattern.m_environment)
	{
            attachReference(m_matcher, pattern.m_matcher);
        }

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
	    return m_debug && s_debugging_enabled;
	}

	/** @brief Enable/disable function debugging.
	 *
	 * @param on True iff function debugging is to be enabled.
	 */
	static void enableDebugging(bool on)
	{
	    s_debugging_enabled = on;
	}

	/** @brief If function debugging currently enabled?
	 *
	 * @return true iff function debugging is currently enabled.
	 */
	static bool debuggingEnabled()
	{
	    return s_debugging_enabled;
	}

	/** @brief Access the environment of the Closure.
	 *
	 * @return Pointer to the environment of the Closure.
	 */
	Environment* environment() const
	{
	    return m_environment;
	}

	/** @brief Evaluate the Closure's body in a given environment.
	 *
	 * This function evaluates the Closure's body with \a env as
	 * the working environment, handling debugging if currently
	 * enabled for this Closure.
	 *
	 * @param env Non-null pointer to the working environment in
	 *          which evaluation of the body is to be carried out.
	 *
	 * @return the result of evaluation.
	 *
	 * @note This is a low-level function that is called by
	 * invoke() but may also be called directly.  The function
	 * does not carry out argument matching, nor does it create a
	 * ClosureContext: both these operations must be handled by
	 * the calling code.
	 */
	RObject* execute(Environment* env) const;

	/** @brief Access the ArgMatcher of this Closure.
	 *
	 * @return const pointer to this Closure's ArgMatcher object.
	 */
	const ArgMatcher* matcher() const
	{
	    return m_matcher;
	}

	/** @brief Create an environment suitable for evaluating this closure.
	 */
        Environment* createExecutionEnv() const;

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
	void setEnvironment(Environment* new_env);

 	/** @brief Replace the formals of the closure.
	 *
	 * @param formals Pointer to a pairlist containing the new
	 *          formal arguments to use for this closure.
	 */
	void setFormals(PairList* formals);

	/** @brief Replace the body of the closure.
	 *
	 * @param body Pointer to the new body to use for this closure.
	 */
	void setBody(RObject* body);

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "closure";
	}

	/** @brief Strip formal argument bindings from a Frame.
	 *
	 * This function removes from \a input_frame any bindings of
	 * the formal arguments of this Closure.  It is used in
	 * creating the working environment of an S3 method from the
	 * working environment of its generic.
	 *
	 * @param input_frame Non-null pointer to the Frame from which
	 *          bindings are to be stripped.
	 */
	void stripFormals(Frame* input_frame) const
	{
	    m_matcher->stripFormals(input_frame);
	}

	// Virtual functions of RObject:
        Closure* clone() const override;
	const char* typeName() const override;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const override;
        void applyToCoalescedReferences(std::function<void(const GCNode*)> fun) const override;

        void compile() const;
    protected:
	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	/** @brief Patrol entry and exit if debugging.
	 *
	 * DebugScope objects must be declared on the processor stack
	 * (i.e. as C++ automatic variables).  A DebugScope object
	 * relates to a particular Closure object.  If debugging is
	 * enabled for that Closure, then the DebugScope constructor
	 * will announce that the Closure function has been entered,
	 * enable single stepping for the working environment, and
	 * initiate the browser, and the DebugScope destructor will
	 * announce that the function is exiting.  If debugging is not
	 * enabled for the Closure, then the constructor and
	 * destructor do nothing.
	 */
	class DebugScope {
	public:
	    /** @brief Constructor.
	     *
	     * @param closure Non-null pointer to the Closure being
	     *          executed.
	     *
	     * @note If debugging is enabled for \a closure, the class
	     * uses the innermost ClosureContext to obtain any further
	     * information it requires.
	     */
	    DebugScope(const Closure* closure)
		: m_closure(closure)
	    {
		if (m_closure->debugging())
		    startDebugging();
	    }

	    ~DebugScope()
	    {
		if (m_closure->debugging())
		    endDebugging();
	    }
	private:
	    const Closure* m_closure;

	    void startDebugging() const;
	    void endDebugging() const;
	};

	bool m_debug;
        mutable int m_num_invokes;
        mutable GCEdge<JIT::CompiledExpression> m_compiled_body;

	const ArgMatcher* m_matcher = nullptr;
	GCEdge<> m_body;
	GCEdge<Environment> m_environment;
        static bool s_debugging_enabled;

	// If a JIT compiled version of this closure exists, invalidate it.
	void invalidateCompiledCode();

	// Declared private to ensure that Closure objects are
	// created only using 'new':
	~Closure();

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Closure& operator=(const Closure&);
    };
}  // namespace rho

extern "C" {
    /** @brief Create a rho::Closure object.
     *
     * @param formal_args Pointer to a rho::PairList (checked) of
     *          formal arguments.
     *
     * @param body Pointer to the body of the rho::Closure.  This must be
     *          either a null pointer or a pointer to an object of one
     *          of the following types: LISTSXP, LANGSXP, SYMSXP,
     *          EXPRSXP, VECSXP or BCODESXP (checked).
     *
     * @param env pointer to the rho::Environment (checked) in which the
     *          closure is to be evaluated.
     *
     * @return pointer to the created closure object.
     */
    SEXP Rf_mkCLOSXP(SEXP formal_args, SEXP body, SEXP env);

    /** @brief Access the body of a rho::Closure.
     *
     * @param x Pointer to a rho::Closure object (checked).
     *
     * @return Pointer to the body of \a x.
     */
    inline SEXP BODY(SEXP x)
    {
	using namespace rho;
	const Closure& clo = *SEXP_downcast<Closure*>(x);
	return const_cast<RObject*>(clo.body());
    }

    /** @brief Access the environment of a rho::Closure.
     *
     * @param x Pointer to a rho::Closure object (checked).
     *
     * @return Pointer to the environment of x.
     */
    inline SEXP CLOENV(SEXP x)
    {
	using namespace rho;
	Closure& clo = *SEXP_downcast<Closure*>(x);
	return clo.environment();
    }

    /** @brief Access formal arguments of a rho::Closure.
     *
     * @param x Pointer to a rho::Closure object (checked).
     *
     * @return Pointer to the formal argument list of \a x.
     */
    inline SEXP FORMALS(SEXP x)
    {
	using namespace rho;
	const Closure* clos = SEXP_downcast<Closure*>(x);
	return const_cast<PairList*>(clos->matcher()->formalArgs());
    }

    /** @brief Query debugging status.
     *
     * @param x Pointer to a rho::Closure object.
     *
     * @return \c true if debugging is set, i.e. evaluations of the
     *         function should run under the browser.
     *
     * @note In rho, RDEBUG() is applicable only to closures; use
     * ENV_DEBUG() to query the debugging (single-stepping) state
     * for environments.
     */
    inline Rboolean RDEBUG(SEXP x)
    {
	using namespace rho;
	const Closure& clos = *SEXP_downcast<const Closure*>(x);
	return Rboolean(clos.debugging());
    }

    inline int RSTEP(SEXP x)
    {
	return 0;
    }

    /** @brief Replace the environment of a closure.
     *
     * @param x Pointer to a rho::Closure object (checked).
     *
     * @param v Pointer to the environment now to be
     *          considered as the environment of this rho::Closure.  A
     *          null pointer is not permissible (not checked).
     */
    inline void SET_CLOENV(SEXP x, SEXP v)
    {
	using namespace rho;
	Closure& clos = *SEXP_downcast<Closure*>(x);
	Environment* env = SEXP_downcast<Environment*>(v);
	clos.setEnvironment(env);
    }

    /**
     * Set the debugging state of a rho::Closure object.
     *
     * @param x Pointer a rho::Closure object (checked).
     *
     * @param v The new debugging state.
     *
     * @note In rho, SET_RDEBUG() is applicable only to closures; use
     * SET_ENV_DEBUG() to set the debugging (single-stepping) state
     * for environments.
     */
    inline void SET_RDEBUG(SEXP x, Rboolean v)
    {
	using namespace rho;
	Closure& clos = *SEXP_downcast<Closure*>(x);
	clos.setDebugging(v);
    }

    inline void SET_RSTEP(SEXP x, int v)
    {
    }
}

#endif /* RCLOSURE_H */
