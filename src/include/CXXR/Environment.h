/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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
#include <boost/serialization/nvp.hpp>

#include "CXXR/Frame.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/PairList.h"
#include "CXXR/Symbol.h"

/** @def DETACH_LOCAL_FRAMES
 *
 * If the preprocessor variable DETACH_LOCAL_FRAMES is defined, then
 * code is inserted which keeps track of whether the local Environment
 * of a Closure application may continue to be reachable after the
 * Closure application returns.  If not, then just before the Closure
 * application returns, the Environment is detached from its Frame.
 * This breaks possible loops in the GCNode/GCEdge graph, thus enabling
 * the Environment to be garbage-collected immediately.
 *
 * Unfortunately, enabling this extra code slows CXXR down very
 * slightly; however, it may prove beneficial in the future when
 * combined with provenance tracking.
 */
#ifdef DOXYGEN
#define DETACH_LOCAL_FRAMES
#endif

namespace CXXR {
    class FunctionBase;

    /** @brief Mapping from Symbols to R objects.
     *
     * An Environment has an associated Frame, which defines a mapping
     * from (pointers to) CXXR::Symbol objects to (pointers to)
     * arbitrary objects of classes derived from RObject.  An
     * Environment will normally have an 'enclosing environment', and
     * the Environment class provides facilities for searching for a
     * binding for a Symbol first in the Environment's own Frame, and
     * then successively in the Frames of enclosing Environments.
     *
     * @note This class does not in itself enforce the requirement
     * that the enclosing relationship must be acyclic.
     *
     * @todo For provenance-tracking, there ought to be some way of
     * monitoring the event that the program \e fails to find a
     * binding for a Symbol.
     */
    class Environment : public RObject {
    public:
	/** @brief Object authorising R 'break' and 'next' commands.
	 *
	 * LoopScope objects must be declared on the processor stack
	 * (i.e. as C++ automatic variables).  Each Environment object
	 * keeps track of the number of LoopScope objects associated
	 * with it.  The R commands 'break' and 'next' are legal only
	 * when the evaluation Environment has at least one LoopScope
	 * in existence; this can be determined by calling
	 * Environment::loopActive().
	 */
	class LoopScope {
	public:
	    /** @brief Constructor.
	     *
	     * @param env Pointer to the Environment with which this
	     *          LoopScope is to be associated.
	     */
	    LoopScope(Environment* env)
		: m_environment(env), m_prev_state(env->m_in_loop)
	    {
		env->m_in_loop = true;
	    }

	    ~LoopScope()
	    {
		m_environment->m_in_loop = m_prev_state;
	    }
	private:
	    GCStackRoot<Environment> m_environment;
	    bool m_prev_state;
	};

	/** @brief Object authorising R 'return' command.
	 *
	 * ReturnScope objects must be declared on the processor stack
	 * (i.e. as C++ automatic variables).  Each Environment object
	 * keeps track of the number of ReturnScope objects associated
	 * with it.  The R command 'return' is legal only when the
	 * evaluation Environment has at least one ReturnScope in
	 * existence; this can be determined by calling
	 * Environment::canReturn().  More generally, a transfer of
	 * control to a specified Environment using ReturnException
	 * will succeed only if canReturn() is true.
	 */
	class ReturnScope {
	public:
	    /** @brief Constructor.
	     *
	     * @param env Pointer to the Environment with which this
	     *          ReturnScope is to be associated.
	     */
	    ReturnScope(Environment* env)
		: m_environment(env), m_prev_state(env->m_can_return)
	    {
		env->m_can_return = true;
	    }

	    ~ReturnScope()
	    {
		m_environment->m_can_return = m_prev_state;
	    }
	private:
	    GCStackRoot<Environment> m_environment;
	    bool m_prev_state;
	};

	/** @brief Constructor.
	 *
	 * @param enclosing Pointer to the enclosing environment.
	 *
	 * @param frame Pointer to the Frame to be used by the
	 *          constructed Environment.
	 */
	Environment(Environment* enclosing, Frame* frame)
	    : RObject(ENVSXP), m_enclosing(enclosing), m_frame(frame),
	      m_single_stepping(false), m_locked(false), m_on_search_path(false),
	      m_leaked(false), m_in_loop(false), m_can_return(false)
	{}

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

	/** @brief Is R 'return' currently legal?
	 *
	 * @return true iff there is currently at least one ReturnScope
	 * object in existence associated with this Environment, so
	 * that a transfer of control using ReturnException will
	 * succeed.
	 */
	bool canReturn() const
	{
	    return m_can_return;
	}

	/** @brief Empty environment.
	 *
	 * CR accords a special status to the empty environment,
	 * R_EmptyEnv, which is an Environment whose Frame contains no
	 * Bindings, and which has no enclosing Environment.  In CR
	 * the search for a Symbol Binding terminates when it reaches
	 * the empty environment, without looking inside it.  In CXXR,
	 * although the empty environment still exists (for backwards
	 * compatibility)), it is not handled specially.  If the
	 * search for a Symbol reaches the empty environment, CXXR
	 * will look for the Symbol inside it - unsuccessfully of
	 * course - and the search then terminates because there is no
	 * enclosing Environment.
	 *
	 * @return Pointer to the empty environment.
	 *
	 * @note CXXR's own code does not include tests to prohibit
	 * the creation of bindings within the empty environment, but
	 * the effect of doing so is undefined.
	 */
	static Environment* empty()
	{
	    return s_empty;
	}

	/** @brief Access the enclosing Environment.
	 *
	 * @return pointer to the enclosing Environment.
	 */
	Environment* enclosingEnvironment() const
	{
	    return m_enclosing;
	}

	/** @brief Search for a Binding for a Symbol.
	 *
	 * The search starts in this Environment; if no Binding is
	 * found there, the search will proceed through successive
	 * enclosing Environments.

	 * @param symbol Pointer to the Symbol for which a Binding is
	 *          sought.
	 *
	 * @return A pointer to the sought Binding, or a null pointer if no
         *         Binding was found.
	 */
	Frame::Binding* findBinding(const Symbol* symbol) HOT_FUNCTION;

	/** @brief Search for a Binding for a Symbol (const variant).
	 *
	 * The search starts in this Environment; if no Binding is
	 * found there, the search will proceed through successive
	 * enclosing Environments.

	 * @param symbol Pointer to the Symbol for which a Binding is
	 *          sought.
	 *
	 * @return A pointer to the sought Binding, or a null pointer if no
         *         Binding was found.
	 */
	const Frame::Binding* findBinding(const Symbol* symbol) const
	{
	    return const_cast<Environment*>(this)->findBinding(symbol);
	}

	/** @brief Locate a namespace environment from its
	 *   specification.
	 *
	 * @param spec Non-null pointer to the specification of a
	 * namespace environment (as returned by namespaceSpec() ).
	 *
	 * @return A pointer to the namespace environment
	 * corresponding to \a spec .  This namespace is loaded if
	 * necessary, and deserialization fails if loading is
	 * unsuccessful.
	 *
	 * @todo Having deserialization fail entirely in the event that
	 * the namespace cannot be loaded seems insufficiently robust,
	 * but follows CR practice.
	 */
	static Environment* findNamespace(const StringVector* spec);

	/** @brief Locate a package environment from its name.
	 *
	 * @param name Name of a package, prefixed by <tt>package:</tt>.
	 *
	 * @return A pointer to the package environment corresponding
	 * to \a name .  This package is loaded if necessary.  If
	 * loading fails, the function returns a pointer to the global
	 * Environment (<tt>Environment::global()</tt>): this follows
	 * CR practice.
	 */
	static Environment* findPackage(const std::string& name);

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

	/** @brief Is R 'break' or 'next' currently legal?
	 *
	 * @return true iff there is currently at least one LoopScope
	 * object in existence associated with this Environment.
	 */
	bool loopActive() const
	{
	    return m_in_loop;
	}

	/** @brief Disconnect the Environment from its Frame, if safe.
	 *
	 * Just before the application of a Closure returns, this
	 * function is called on the local Environment of the Closure.
	 * If it appears that the Environment will no longer be
	 * reachable after the Closure returns (i.e., the Environment
	 * is not 'leaked'), it detaches the Environment from its
	 * Frame.  This breaks possible loops in the GCNode/GCEdge
	 * graph, thus enabling the Environment to be
	 * garbage-collected immediately.
	 *
	 * @note This function is an inlined no-op unless the
	 * preprocessor variable DETACH_LOCAL_FRAMES is defined in
	 * Environment.h.
	 */
	void maybeDetachFrame()
	{
#ifdef DETACH_LOCAL_FRAMES
	    if (!m_leaked)
		detachFrame();
#endif
	}

	/** @brief Look for Environment objects that may have
	 *  'leaked'.
	 *
	 * This function determines if any Environment objects are
	 * reachable from \a node, and if so marks them as 'leaked'.
	 * It is called in respect of any objects that are 'exported'
	 * from a Closure call, for example the return value of the
	 * call, and the objects of any non-local assignments within
	 * the call.
	 *
	 * @param node Pointer (possibly null) to the object to be
	 * scrutinised.
	 *
	 * @note This function is an inlined no-op unless the
	 * preprocessor variable DETACH_LOCAL_FRAMES is defined in
	 * Environment.h.
	 */
	static void monitorLeaks(const GCNode* node)
	{
#ifdef DETACH_LOCAL_FRAMES
	    if (node) {
		LeakMonitor monitor;
		monitor(node);
	    }
#endif
	}

	/** @brief Get namespace spec (if applicable).
	 *
	 * @return If this Environment is a namespace environment,
	 * this function returns the namespace specification.
	 * Otherwise returns a null pointer.
	 */
	const StringVector* namespaceSpec() const;

	/** @brief Get package name (if any).
	 *
	 * @return If this Environment is the Environment of a package, this
	 * function returns the name of the package (of the form
	 * "package:foo") as the first element of a StringVector.
	 * Otherwise returns a null pointer.
	 */
	const StringVector* packageName() const;

	/** @brief Replace the enclosing environment.
	 *
	 * @param new_enclos Pointer to the environment now to be
	 *          considered to enclose this Environment.
	 *
	 * @deprecated Retained for use in deserialization and in the
	 * R function \c parent.env<- (itself deprecated).  For other
	 * purposes, use instead slotBehind() and skipEnclosing(),
	 * which ensure that the 'enclosing' relationship remains
	 * acyclic.
	 */
	void setEnclosingEnvironment(Environment* new_enclos);

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

        /** @brief Attach a copy of this environment to the search path.
	 *
	 * @param pos The position in the search list to attach to.
	 *
	 * @param name The name to used for the attached environment.
	 *
	 * @return The environment that was attached.
	 */
	Environment* attachToSearchPath(int pos, StringVector* name);

	/** @brief Detach an element from the search path.
	 *
	 * @param pos The position in the search list to detach.
	 *
	 * @return The environment that was detached.
	 */
	static Environment* detachFromSearchPath(int pos);
	    
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

	// Virtual functions of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Virtual function of GCNode:
	void detachReferents();
    private:
	friend class boost::serialization::access;
	friend class SchwarzCounter<Environment>;
	friend class Frame;

	// PACKAGE_ENV because PACKAGE is defined (to "R") as a macro
	// within config.h .
	enum S11nType {EMPTY = 0, BASE, BASENAMESPACE,
		       GLOBAL, PACKAGE_ENV, NAMESPACE, OTHER};

	struct LeakMonitor : public GCNode::const_visitor {
	    LeakMonitor()
	    {}

	    // Virtual function of const_visitor:
	    void operator()(const GCNode* node);
	};

	// The class maintains a cache of Symbol Bindings found along
	// the search path:
        typedef 
            std::unordered_map<const Symbol*, Frame::Binding*,
                               std::hash<const Symbol*>,
                               std::equal_to<const Symbol*>,
                               CXXR::Allocator<std::pair<const Symbol* const,
                                                          Frame::Binding*> >
                               > Cache;

	static Cache* s_search_path_cache;

	// Predefined environments:
	static Environment* s_base;
	static Environment* s_base_namespace;
	static Environment* s_empty;
	static Environment* s_global;

	GCEdge<Environment> m_enclosing;
	GCEdge<Frame> m_frame;
	bool m_single_stepping;
	bool m_locked;
	bool m_on_search_path;
	// For local environments, m_leaked is set to true to signify
	// that the environment may continue to be reachable after the
	// return of the Closure call that created it.  It has no
	// particular meaning for non-local environments.
	mutable bool m_leaked;
	bool m_in_loop;
	bool m_can_return;

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Environment(const Environment&);
	Environment& operator=(const Environment&);

	// Declared private to ensure that Environment objects are
	// created only using 'new':
	~Environment()
	{
	    setOnSearchPath(false);
	}

	static void cleanup();

	void detachFrame();

	// Remove any mapping of 'sym' from the search path cache.  If called
        // with a null pointer, clear the cache entirely.
	static void flushFromSearchPathCache(const Symbol* sym);

	static void initialize();

	bool isSearchPathCachePortal() const
	{
	    return (this == s_global);
	}

	template<class Archive>
	void load(Archive& ar, const unsigned int version);

	// Set whether or not this Environment is a participant in the search
	// list cache:
        void setOnSearchPath(bool status);

	// Warn about package possibly not being available when
	// loading, and extract package name.
	static const char* package_s11n_aux(const StringVector* pkg_name);

	template<class Archive>
	void save(Archive& ar, const unsigned int version) const;

	template<class Archive>
	void serialize (Archive & ar, const unsigned int version) {
	    boost::serialization::split_member(ar, *this, version);
	}
    };

    /** @brief Search for a Binding of a Symbol to a FunctionBase.
     *
     * This function looks for a Binding of \a symbol, and tests
     * whether the Binding's value is a FunctionBase.
     *
     * If a Binding of \a symbol to a Promise is encountered, the
     * Promise is forced before testing whether the value of the
     * Promise is a FunctionBase.  In this case, if the predicate is
     * satisfied, the result of evaluating the Promise is part of the
     * returned value.
     *
     * If, in the course of searching for a suitable Binding, a
     * Binding of \a symbol to Symbol::missingArgument()
     * (R_MissingArg) is encountered, an error is raised.
     *
     * Read/write monitors are invoked in the following circumstances:
     * (i) If a Promise is forced, any read monitor for the relevant
     * Binding is called before forcing it, and any write monitor for
     * the symbol's Binding is called immediately afterwards.  (ii) If
     * this function succeeds in finding a Binding to a FunctionBase,
     * then any read monitor for that Binding is called.
     *
     * @param symbol Non-null pointer to the Symbol for which a
     *          Binding is sought.
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
     * @return Returns the sought function if a Binding to a FunctionBase was
     *         found.  Otherwise returns null.
     */
    FunctionBase*
    findFunction(const Symbol* symbol, Environment* env, bool inherits = true);

    /** @brief Search for a Binding whose value satisfies a predicate.
     *
     * This function looks for a Binding of \a symbol, and tests
     * whether the Binding's value satisfies a predicate \a pred.
     *
     * If a Binding of \a symbol to a Promise is encountered, the
     * Promise is forced before applying the predicate to the result
     * of evaluating the Promise.  In this case, if the predicate is
     * satisfied, the result of evaluating the Promise is part of the
     * returned value.
     *
     * Read/write monitors are invoked in the following circumstances:
     * (i) If a Promise is forced, any read monitor for the relevant
     * Binding is called before forcing it, and any write monitor for
     * the symbol's Binding is called immediately afterwards.  (ii) If
     * this function succeeds in finding a Binding satisfying the
     * predicate, then any read monitor for that Binding is called.
     *
     * @tparam UnaryPredicate A type of function or function object
     *           capable of accepting const RObject* and returning
     *           bool.
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
     * @return Returns the sought function if a Binding to a FunctionBase was
     *         found.  Otherwise returns null.
     * @return If a Binding satisfying the predicate was found, returns the
     *   value of the Binding, except that if the value was a Promise, returns
     *   the result of evaluating the Promise instead.  If no Binding
     *   satisfying the predicate was found, returns null.
     */
    template <typename UnaryPredicate>
    RObject* findTestedValue(const Symbol* symbol, Environment* env,
			     UnaryPredicate pred, bool inherits)
    {
	using namespace std;

	do {
	    Frame::Binding *bdg;
	    if (inherits && env == Environment::global()) {
		// findBinding() handles the details of the cache correctly.
		bdg = env->findBinding(symbol);
	    } else {
		bdg = env->frame()->binding(symbol);
	    }
	    if (bdg) {
		pair<RObject*, bool> fpr = bdg->forcedValue2();
		RObject* val = fpr.first;
		if (pred(val)) {
		    // Invoke read monitor (if any) only if
		    // forcedValue() did not force a Promise.  (If a
		    // Promise was forced, the read monitor will have
		    // been invoked anyway, and 'bdg' may now be
		    // junk.)
		    if (!fpr.second)
			bdg->rawValue();
		    return val;
		}
	    }
            env = env->enclosingEnvironment();
	} while (inherits && env);
	return NULL;
    }
}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::Environment)

namespace boost {
    namespace serialization {
	/** @brief Template specialisation.
	 *
	 * This specialisation is required because CXXR::Environment
	 * does not have a default constructor.  See the
	 * boost::serialization documentation for further details.
	 *
	 * @tparam Archive archive class from which deserialisation is
	 *           taking place.
	 *
	 * @param ar Archive from which deserialisation is taking
	 *           place.
         *
	 * @param t Pointer to the location at which a CXXR::Environment
	 *          object is to be constructed.
	 *
	 * @param version Ignored.
	 */
	template<class Archive>
	void load_construct_data(Archive& ar, CXXR::Environment* t,
				 const unsigned int version)
	{
	    new (t) CXXR::Environment(0, 0);
	}
    }  // namespace serialization
}  // namespace boost

namespace {
    CXXR::SchwarzCounter<CXXR::Environment> env_schwartz_ctr;
}

// ***** Implementation of non-inlined templated members *****

template<class Archive>
void CXXR::Environment::load(Archive& ar, const unsigned int version)
{
    ar >> BOOST_SERIALIZATION_BASE_OBJECT_NVP(RObject);
    S11nType envtype;
    ar >> BOOST_SERIALIZATION_NVP(envtype);
    Environment* reloc = 0;
    switch(envtype) {
    case EMPTY:
	reloc = s_empty;
	break;
    case BASE:
	reloc = s_base;
	break;
    case BASENAMESPACE:
	reloc = s_base_namespace;
	break;
    case GLOBAL:
	reloc = s_global;
	break;
    case PACKAGE_ENV:
	{
	    std::string pkgname;
	    ar >> BOOST_SERIALIZATION_NVP(pkgname);
	    reloc = findPackage(pkgname);
	}
	break;
    case NAMESPACE:
	{
	    GCStackRoot<const StringVector> nsspec;
	    GCNPTR_SERIALIZE(ar, nsspec);
	    reloc = findNamespace(nsspec);
	}
	break;
    case OTHER:
	{
	    GCNPTR_SERIALIZE(ar, m_enclosing);
	    GCNPTR_SERIALIZE(ar, m_frame);
	    ar >> BOOST_SERIALIZATION_NVP(m_single_stepping);
	    ar >> BOOST_SERIALIZATION_NVP(m_locked);
	}
	break;
    }
    if (reloc)
	S11nScope::defineRelocation(this, reloc);
}

template<class Archive>
void CXXR::Environment::save(Archive& ar, const unsigned int version) const
{
    ar << BOOST_SERIALIZATION_BASE_OBJECT_NVP(RObject);
    // EMPTY:
    if (this == s_empty) {
	S11nType envtype = EMPTY;
	ar << BOOST_SERIALIZATION_NVP(envtype);
	return;
    }
    // BASE:
    if  (this == s_base) {
	S11nType envtype = BASE;
	ar << BOOST_SERIALIZATION_NVP(envtype);
	return;
    }
    // BASENAMESPACE:
    if (this == s_base_namespace) {
	S11nType envtype = BASENAMESPACE;
	ar << BOOST_SERIALIZATION_NVP(envtype);
	return;
    }
    // GLOBAL:
    if (this == s_global) {
	S11nType envtype = GLOBAL;
	ar << BOOST_SERIALIZATION_NVP(envtype);
	return;
    }
    // PACKAGE_ENV:
    const StringVector* pkgsv = packageName();
    if (pkgsv) {
	S11nType envtype = PACKAGE_ENV;
	ar << BOOST_SERIALIZATION_NVP(envtype);
	std::string pkgname(package_s11n_aux(pkgsv));
	ar << BOOST_SERIALIZATION_NVP(pkgname);
	return;
    }
    // NAMESPACE:
    const StringVector* nsspec = namespaceSpec();
    if (nsspec) {
	S11nType envtype = NAMESPACE;
	ar << BOOST_SERIALIZATION_NVP(envtype);
	GCNPTR_SERIALIZE(ar, nsspec);
	return;
    }
    // OTHER:
    {
	S11nType envtype = OTHER;
	ar << BOOST_SERIALIZATION_NVP(envtype);
	GCNPTR_SERIALIZE(ar, m_enclosing);
	GCNPTR_SERIALIZE(ar, m_frame);
	ar << BOOST_SERIALIZATION_NVP(m_single_stepping);
	ar << BOOST_SERIALIZATION_NVP(m_locked);
    }
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
	return bdg ? bdg->unforcedValue() : Symbol::unboundValue();
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RENVIRONMENT_H */
