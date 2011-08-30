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

/** @file Environment.h
 * @brief Class CXXR::Environment and associated C interface.
 */

#ifndef RENVIRONMENT_H
#define RENVIRONMENT_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/export.hpp>
#include "CXXR/BSerializer.hpp"
#include "CXXR/StdFrame.hpp"
#include "CXXR/Symbol.h"

namespace CXXR {
    class FunctionBase;

    /** @brief Mapping from Symbols to R objects.
     *
     * An Environment has an associated Frame, which defines a mapping
     * from (pointers to) CXXR::Symbol objects to (pointers to)
     * arbitrary objects of classes derived from RObject.  Each
     * Environment (except for the standard empty environment) has an
     * 'enclosing environment', and the Environment class provides
     * facilities for searching for a binding for a Symbol first in
     * the Environment's own Frame, and then successively in enclosing
     * frames.
     *
     * @note This class does not in itself enforce the requirement
     * that working up the enclosing relationship will always lead to
     * the empty environment.
     *
     * @todo For provenance-tracking, there ought to be some way of
     * monitoring the event that the program \e fails to find a
     * binding for a Symbol.
     */
    class Environment : public RObject {
    public:
	/**
	 * @param enclosing Pointer to the enclosing environment.
	 */
	explicit Environment(Environment* enclosing)
	    : RObject(ENVSXP), m_enclosing(enclosing),
	      m_frame(expose(new StdFrame)), m_single_stepping(false)
	{}

	/**
	 * @param enclosing Pointer to the enclosing environment.
	 *
	 * @param initial_capacity A hint to the implementation that
	 *          the Frame of the constructed Environment should be
	 *          configured to have capacity for at least \a
	 *          initial_capacity Bindings.  This does not impose an
	 *          upper limit on the capacity of the Frame,
	 *          but some reconfiguration (and consequent time
	 *          penalty) may occur if it is exceeded.
	 */
	Environment(Environment* enclosing, size_t initial_capacity)
	    : RObject(ENVSXP), m_enclosing(enclosing),
	      m_frame(expose(new StdFrame(initial_capacity))),
	      m_single_stepping(false)
	{}

	/** @brief Constructor with specified Frame.
	 *
	 * This constructor is used, for example, in constructing the
	 * base namespace, which shares the Frame of the base
	 * environment.
	 *
	 * @param enclosing Pointer to the enclosing environment.
	 *
	 * @param frame Pointer to the Frame to be used by the
	 *          constructed Environment.
	 */
	Environment(Environment* enclosing, Frame* frame)
	    : RObject(ENVSXP), m_enclosing(enclosing), m_frame(frame),
	      m_single_stepping(false)
	{}

	/** @brief Environment constructor for serialization
	 */
	Environment() { }

	/** @brief Base environment.
	 *
	 * @return Pointer to the base environment.
	 */
	static Environment* base()
	{
	    return s_base;
	}

	/** @brief Base namespace.
	 *
	 * @return Pointer to the base namespace.
	 */
	static Environment* baseNamespace()
	{
	    return s_base_namespace;
	}

	/** @brief Access the enclosing Environment.
	 *
	 * @return pointer to the enclosing Environment.
	 */
	Environment* enclosingEnvironment() const
	{
	    return m_enclosing;
	}

	/** @brief Access the Environment's Frame.
	 *
	 * @return pointer to the Environment's Frame.
	 */
	Frame* frame()
	{
	    return m_frame;
	}

	/** @brief Access the Environment's Frame (const variant).
	 *
	 * @return const pointer to the Environment's Frame.
	 */
	const Frame* frame() const
	{
	    return m_frame;
	}

	/** @brief Global environment.
	 *
	 * @return Pointer to the global environment.
	 */
	static Environment* global()
	{
	    return s_global;
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
	}

	/** @brief Set single-stepping status
	 *
	 * @param on The required single-stepping status (true =
	 *           enabled).
	 */
	void setSingleStepping(bool on)
	{
	    m_single_stepping = on;
	}

	/** @brief Get single-stepping status.
	 *
	 * @return true if debugger should single-step within this
	 * Environment.
	 */
	bool singleStepping() const
	{
	    return m_single_stepping;
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "environment";
	}

	// Virtual functions of RObject:
	unsigned int packGPBits() const;
	const char* typeName() const;
	void unpackGPBits(unsigned int gpbits);

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Declared protected to ensure that Environment objects are
	// created only using 'new':
	~Environment() {}

	// Virtual function of GCNode:
	void detachReferents();
    private:
	friend class boost::serialization::access;

	template<class Archive>
	void serialize (Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("Environment");
	    ar & boost::serialization::base_object<RObject>(*this);
    	    ar & m_enclosing;
	    ar & m_frame;
	    ar & m_single_stepping;
	    ar & m_locked;
	}

	// Predefined environments.  R_EmptyEnvironment has no special
	// significance in CXXR, and may be abolished, so is not
	// included here:
	static Environment* s_base;
	static Environment* s_base_namespace;
	static Environment* s_global;

	GCEdge<Environment> m_enclosing;
	GCEdge<Frame> m_frame;
	bool m_single_stepping;
	bool m_locked;

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Environment(const Environment&);
	Environment& operator=(const Environment&);

	static void cleanup() {}

	static void initialize();

	friend class SchwarzCounter<Environment>;
    };

    /** @brief Search for a Binding for a Symbol.
     *
     * @param symbol Pointer to the Symbol for which a Binding is
     *          sought.
     *
     * @param env Environment in whose Frame a Binding is first to be
     *          sought; if no Binding is found there, the search will
     *          proceed through successive enclosing Environments.  It
     *          is permissible for \a env to be a null pointer, in
     *          which case (of course) no Binding will be found.
     *
     * @return The first element of the pair is a pointer to the
     * sought Binding, or a null pointer if no Binding was found.  The
     * second element of the pair is a pointer to the Environment in
     * whose frame the Binding was found, or a null pointer if no
     * Binding was found.
     */
    std::pair<Environment*, Frame::Binding*>
    findBinding(const Symbol* symbol, Environment* env);

    /** @brief Search for a Binding for a Symbol (const variant).
     *
     * @param symbol Pointer to the Symbol for which a Binding is
     *          sought.
     *
     * @param env Environment in whose Frame a Binding is first to be
     *          sought; if no Binding is found there, the search will
     *          proceed through successive enclosing Environments.  It
     *          is permissible for \a env to be a null pointer, in
     *          which case (of course) no Binding will be found.
     *
     * @return The first element of the pair is a pointer to the
     * sought Binding, or a null pointer if no Binding was found.  The
     * second element of the pair is a pointer to the Environment in
     * whose Frame the Binding was found, or a null pointer if no
     * Binding was found.
     */
    std::pair<const Environment*, const Frame::Binding*>
    findBinding(const Symbol* symbol, const Environment* env);

    /** @brief Search for a Binding of a Symbol to a FunctionBase.
     *
     * This function looks for a Binding of \a symbol, and tests
     * whether the Binding's value is a FunctionBase.
     *
     * If a Binding of \a symbol to a Promise is encountered, the
     * Promise is forced (within the Binding's environment) before
     * testing whether the value of the Promise is a FunctionBase.  In
     * this case, if the predicate is satisfied, the result of
     * evaluating the Promise is part of the returned value.
     *
     * If, in the course of searching for a suitable Binding, a
     * Binding of \a symbol to R_MissingArg is encountered, an error
     * is raised.
     *
     * Read/write monitors are invoked in the following circumstances:
     * (i) If a Promise is forced, any read monitor for the relevant
     * Binding is called before forcing it, and any write monitor for
     * the symbol's Binding is called immediately afterwards.  (ii) If
     * this function succeeds in finding a Binding to a FunctionBas,
     * then any read monitor for that Binding is called.
     *
     * @param symbol Pointer to the Symbol for which a Binding is
     *          sought.
     *
     * @param env Pointer to the Environment in which the search for a
     *          Binding is to start.  Must not be null.
     *
     * @param inherits If false, only the Frame of \a env will be
     *          searched; if true, the search will propagate as
     *          necessary to enclosing environments until either a
     *          Binding to a FunctionBase is found, or the chain of
     *          enclosing environments is exhausted.
     *
     * @return If a Binding to a FunctionBase was found, the
     * first element of of the pair is a pointer to the Environment in
     * which it was found, and the second element is the value of the
     * Binding, except that if the value was a Promise, the second
     * element is the result of evaluating the Promise.  If no Binding
     * satisfying the predicate was found, both elements of the pair
     * are null pointers.
     */
    std::pair<Environment*, FunctionBase*>
    findFunction(const Symbol* symbol, Environment* env, bool inherits = true);

    /** @brief Search for a Binding whose value satisfies a predicate.
     *
     * This function looks for a Binding of \a symbol, and tests
     * whether the Binding's value satisfies a predicate \a pred.
     *
     * If a Binding of \a symbol to a Promise is encountered, the
     * Promise is forced (within the Binding's environment) before
     * applying the predicate to the result of evaluating the Promise.
     * In this case, if the predicate is satisfied, the result of
     * evaluating the Promise is part of the returned value.
     *
     * Read/write monitors are invoked in the following circumstances:
     * (i) If a Promise is forced, any read monitor for the relevant
     * Binding is called before forcing it, and any write monitor for
     * the symbol's Binding is called immediately afterwards.  (ii) If
     * this function succeeds in finding a Binding satisfying the
     * predicate, then any read monitor for that Binding is called.
     *
     * @param UnaryPredicate A type of function or function object
     *          capable of accepting const RObject* and returning
     *          bool.
     *
     * @param symbol Pointer to the Symbol for which a Binding is
     *          sought.
     *
     * @param env Pointer to the Environment in which the search for a
     *          Binding is to start.  Must not be null.
     *
     * @param pred The UnaryPredicate object to be used to test
     *          candidate values.
     *
     * @param inherits If false, only the Frame of \a env will be
     *          searched; if true, the search will propagate as
     *          necessary to enclosing environments until either a
     *          Binding satisfying the predicate is found, or the
     *          chain of enclosing environments is exhausted.
     *
     * @return If a Binding satisfying the predicate was found, the
     * first element of of the pair is a pointer to the Environment in
     * which it was found, and the second element is the value of the
     * Binding, except that if the value was a Promise, the second
     * element is the result of evaluating the Promise.  If no Binding
     * satisfying the predicate was found, both elements of the pair
     * are null pointers.
     */
    template <typename UnaryPredicate>
    std::pair<Environment*, RObject*>
    findTestedValue(const Symbol* symbol, Environment* env,
		    UnaryPredicate pred, bool inherits)
    {
	using namespace std;
	RObject* val = 0;
	Frame::Binding* bdg;
	bool found = false;
	do {
	    bdg = env->frame()->binding(symbol);
	    if (bdg) {
		pair<RObject*, bool> pr = bdg->forcedValue();
		// If a Promise was forced, this may have invalidated
		// 'bdg' (Um, is this actually possible?), so we look
		// it up again.  However, beware that in this event,
		// the subsequent call to monitorRead() will be
		// applied to the wrong Binding, i.e. not the one from
		// which 'val' was derived.  It's hard to see how to
		// avoid this, because by the time that we've verified
		// that 'val' satisfies the predicate, the original
		// binding may have been destroyed.
		if (pr.second)
		    bdg = env->frame()->binding(symbol);
		val = pr.first;
		found = pred(val);
	    }
	} while (!found && inherits
		 && (env = env->enclosingEnvironment()));
	if (found) {
	    // Invoke read monitor (if any);
	    bdg->rawValue();
	    return make_pair(env, val);
	}
	return pair<Environment*, RObject*>(0, 0);
    }
}  // namespace CXXR

// Export Environment as a boost::serialization-isable class
BOOST_CLASS_EXPORT(CXXR::Environment)

namespace {
    CXXR::SchwarzCounter<CXXR::Environment> env_schwartz_ctr;
}

extern "C" {
#else /* if not __cplusplus */

    /* In C code, R_varloc_t is an opaque pointer: */
    typedef struct R_varloc_st *R_varloc_t;

#endif

    /* C-visible names for predefined environments */
    extern SEXP R_EmptyEnv;
    extern SEXP R_BaseEnv;
    extern SEXP R_GlobalEnv;
    extern SEXP R_BaseNamespace;

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
	using namespace CXXR;
	const Environment& env = *SEXP_downcast<Environment*>(x);
	return env.enclosingEnvironment();
    }
#endif

    /** @brief Should the debugger single-step?
     *
     * @param x Pointer to a CXXR::Environment object (checked).
     *
     * @return \c true if single-stepping is set, i.e. the debugger
     * should single-step within this environment.
     */
#ifndef __cplusplus
    Rboolean ENV_DEBUG(SEXP x);
#else
    inline Rboolean ENV_DEBUG(SEXP x)
    {
	using namespace CXXR;
	const Environment& env = *SEXP_downcast<const Environment*>(x);
	return Rboolean(env.singleStepping());
    }
#endif

    /** @brief Access an environment's Frame, represented as a PairList.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to a PairList representing the contents of the
     * Frame of \a x (may be null).  This PairList is generated on the
     * fly, so this is a relatively expensive operation.  Alterations
     * to the returned PairList will not alter the Environment's Frame.
     *
     * @note Beware that since (unlike CR) this isn't a simple
     * accessor function, its return value will need protection from
     * garbage collection.
     */
#ifndef __cplusplus
    SEXP FRAME(SEXP x);
#else
    inline SEXP FRAME(SEXP x)
    {
	using namespace CXXR;
	Environment* env = SEXP_downcast<Environment*>(x);
	return env->frame()->asPairList();
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
	using namespace CXXR;
	Environment* env = SEXP_downcast<Environment*>(x);
	Environment* enc = SEXP_downcast<Environment*>(v);
	env->setEnclosingEnvironment(enc);
    }
#endif

    /** @brief Enable/disable single-stepping of the debugger.
     *
     * @param x Pointer a CXXR::Environment object (checked).
     *
     * @param v The new single-stepping state (true = enabled).
     */
#ifndef __cplusplus
    void SET_ENV_DEBUG(SEXP x, Rboolean v);
#else
    inline void SET_ENV_DEBUG(SEXP x, Rboolean v)
    {
	using namespace CXXR;
	Environment& env = *SEXP_downcast<Environment*>(x);
	env.setSingleStepping(v);
    }
#endif

    /** @brief Set symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param val Pointer to the RObject now to be considered as
     *            the value of this symbol.  A null pointer or
     *            R_UnboundValue are permissible values of \a val.
     *
     * @todo No binding to R_UnboundValue ought to be created.
     */
#ifndef __cplusplus
    void SET_SYMVALUE(SEXP x, SEXP val);
#else
    inline void SET_SYMVALUE(SEXP x, SEXP val)
    {
	using namespace CXXR;
	const Symbol* sym = SEXP_downcast<Symbol*>(x);
	Environment::base()->frame()->obtainBinding(sym)->setValue(val);
    }
#endif

    /** @brief Symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::RObject representings \a x's value.
     *         Returns R_UnboundValue if no value is currently
     *         associated with the Symbol.
     */
#ifndef __cplusplus
    SEXP SYMVALUE(SEXP x);
#else
    inline SEXP SYMVALUE(SEXP x)
    {
	using namespace CXXR;
	const Symbol* sym = SEXP_downcast<Symbol*>(x);
	Frame::Binding* bdg = Environment::base()->frame()->binding(sym);
	return bdg ? bdg->value() : Symbol::unboundValue();
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RENVIRONMENT_H */
