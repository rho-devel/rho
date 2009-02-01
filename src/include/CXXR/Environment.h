/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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
     * CXXR::Symbol objects to (pointers to) arbitrary objects of
     * classes derived from RObject.  Each Environment (except for the
     * standard empty environment) has an 'enclosing environment';
     * under the 'enclosing' relationship, Environment objects form a
     * directed acyclic graph under the 'enclosing' relationship.
     *
     * @note In CR, working up the enclosing relationship will always
     * lead to the empty environment.  This is not enforced in this
     * class, but is faked up in the C interface, which will translate
     * a null pointer to an Environment to a pointer to the empty
     * environment and <i>vice versa</i> as required.  (Not yet
     * implemented.)
     *
     * @note Unlike CR, this class does not use R_UnboundValue to
     * denote a non-existent mapping, but the CR behaviour is faked up
     * in the C interface: for example, an attempt to map a Symbol to
     * R_UnboundValue is converted to a removal of the Symbol's
     * mapping altogether.  (Not yet implemented.)
     */
    class Environment : public RObject {
    public:
	/**
	 * @param enclosing Pointer to the enclosing environment.
	 */
	explicit Environment(Environment* enclosing = 0)
	    : RObject(ENVSXP), m_enclosing(enclosing),
	      m_single_stepping(false), m_locked(false)
	{}

	/** @brief Base environment.
	 *
	 * @return pointer to the base environment.
	 */
	static Environment* base()
	{
	    return s_base_env;
	}

	/** @briefing Access binding of an already-defined Symbol.
	 *
	 * This function provides a pointer to a PairList element
	 * representing the binding of a symbol.  In this variant the
	 * pointer is non-const, and consequently the calling code can
	 * use it to modify the binding.  An error is raised if the
	 * environment is locked.
	 *
	 * @param symbol The Symbol for which a mapping is sought.
	 *
	 * @param recursive If false, a mapping is sought only in this
	 *          environment.  If true, the search works up through
	 *          enclosing environments: if an environment contains
	 *          a mapping, but to an object that does not satisfy
	 *          the test, then the search continues upwards.
	 *
	 * @return If no mapping was found, both elements of the
	 * returned pair are null pointers.  Otherwise, the first
	 * element of the pair is a pointer to the environment where
	 * the mapping was found, and the second element of the pair
	 * points to a PairList element defining the mapping: the tag
	 * of the element points to the Symbol, the car points to the
	 * mapped object (and may be a mapping to a null pointer).
	 * The PairList element also indicates whether the binding is
	 * locked.
	 */
	std::pair<Environment*, PairList*>
	binding(const Symbol* symbol, bool recursive = true);

	/** @briefing Access const binding of an already-defined Symbol.
	 *
	 * This function provides a pointer to a PairList element
	 * representing the binding of a symbol.  In this variant the
	 * pointer is const, and consequently the calling code can use
	 * it only to examine the binding.  No error is raised if the
	 * environment is locked.
	 *
	 * @param symbol The Symbol for which a mapping is sought.
	 *
	 * @param recursive If false, a mapping is sought only in this
	 *          environment.  If true, the search works up through
	 *          enclosing environments: if an environment contains
	 *          a mapping, but to an object that does not satisfy
	 *          the test, then the search continues upwards.
	 *
	 * @return If no mapping was found, both elements of the
	 * returned pair are null pointers.  Otherwise, the first
	 * element of the pair is a pointer to the environment where
	 * the mapping was found, and the second element of the pair
	 * points to a PairList element defining the mapping: the tag
	 * of the element points to the Symbol, the car points to the
	 * mapped object (and may be a mapping to a null pointer).
	 * The PairList element also indicates whether the binding is
	 * locked.
	 */
	std::pair<const Environment*, const PairList*>
	binding(const Symbol* symbol, bool recursive = true) const;

	/** @brief Remove all symbols from environment.
	 */
	virtual void clear() = 0;

	/** @brief Does the environment map a Symbol?
	 *
	 * This function tests whether an environment contains a
	 * definition of a Symbol.
	 *
	 * @param symbol The Symbol for which a mapping is sought.
	 *
	 * @param recursive If false, a mapping is sought only in this
	 *          environment.  If true, the search works up through
	 *          enclosing environments: if an environment contains
	 *          a mapping, but to an object that does not satisfy
	 *          the test, then the search continues upwards.
	 *
	 * @return A null pointer if no mapping was found, or a
	 * pointer to the environment in which a mapping was found.
	 */
	const Environment* contains(const Symbol* symbol,
				    bool recursive = true) const
	{
	    return binding(symbol, recursive).first;
	}

	/** @brief Does the environment map a Symbol?
	 *
	 * This function tests whether an environment contains a
	 * definition of a Symbol which maps the Symbol to an object
	 * which satisfies a caller-specified test.
	 *
	 * @param Test A type of function or function object type
	 * capable of accepting a <tt>const Symbol*</tt> and returning
	 * a type convertible to <tt>bool</tt>. 
	 *
	 * @param symbol The Symbol for which a mapping is sought.
	 *
	 * @param test A \a Test object/function used to determine
	 *          whether a satisfactory mapping has been found.
	 *
	 * @param recursive If false, a mapping is sought only in this
	 *          environment.  If true, the search works up through
	 *          enclosing environments: if an environment contains
	 *          a mapping, but to an object that does not satisfy
	 *          the test, then the search continues upwards.
	 *
	 * @return A null pointer if no satisfactory mapping was
	 * found, or a pointer to the environment in which a mapping
	 * passing the test was found.
	 */
	// template <typename Test>
	// Environment* contains(const Symbol* symbol, Test test, 
	//		      bool recursive = true);

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

	/** @brief Remove the mapping (if any) of a Symbol.
	 *
	 * This function causes any mapping for a specified Symbol to
	 * be removed from the environment.  Note that this function
	 * always applies to this specific environment: there is no
	 * search up through enclosing environments.
	 *
	 * An error is raised if the environment or the relevant
	 * binding is locked.
	 *
	 * @param symbol The Symbol for which the mapping is to be
	 *          removed.
	 *
	 * @return True iff the environment previously contained a
	 * mapping for \a symbol.
	 */
	bool erase(const Symbol* symbol);

	/** @brief Get contents as a PairList.
	 *
	 * Access the contents of this environment expressed
	 * as a PairList, with the tag of each PairList element
	 * representing a Symbol and the car value representing
	 * the object to which that Symbol is mapped.
	 *
	 * @return pointer to a PairList as described above.  The
	 * caller should not modify this PairList.
	 */
	virtual const PairList* frameList() const = 0;

	/** @brief Global environment.
	 *
	 * @return pointer to the global environment.
	 */
	static Environment* global()
	{
	    return s_global_env;
	}

	/** @brief Is the environment locked?
	 *
	 * @return true iff the environment is locked.
	 */
	bool isLocked() const
	{
	    return m_locked;
	}

	/** @brief Find the mapping of a Symbol.
	 *
	 * This function tests whether an environment contains a
	 * definition of a Symbol which maps the Symbol to an object
	 * which satisfies a caller-specified test, and if so returns
	 * a pointer to that object and the environment in which the
	 * definition was found.
	 *
	 * @param Test A type of function or function object type
	 * capable of accepting a <tt>const Symbol*</tt> and returning
	 * a type convertible to <tt>bool</tt>. 
	 *
	 * @param symbol The Symbol for which a mapping is sought.
	 *
	 * @param test A \a Test object/function used to determine
	 *          whether a satisfactory mapping has been found.
	 *
	 * @param recursive If false, a mapping is sought only in this
	 *          environment.  If true, the search works up through
	 *          enclosing environments: if an environment contains
	 *          a mapping, but to an object that does not satisfy
	 *          the test, then the search continues upwards.
	 *
	 * @return If no satisfactory mapping was found, both elements
	 * of the returned pair are null pointers.  Otherwise, the
	 * first element of the pair is a pointer to the environment
	 * where the mapping was found, and the second element of the
	 * pair points to a PairList element defining the mapping: the
	 * tag of the element points to the Symbol, the car points to
	 * the mapped object (and may be a mapping to a null pointer).
	 * The PairList element also indicates whether the binding is
	 * locked.
	 *
	 * @note The type of the return value will almost certainly
	 * change in subsequent refactorisation steps.
	 */
	//template <typename Test>
	//std::pair<const Environment*, PairList*>
	//lookup(const Symbol* symbol, Test test = True(), 
	//       bool recursive = true) const;

	/** @brief Get or create a binding for a Symbol.
	 *
	 * If the environment already contains a binding for a
	 * specified Symbol, the function returns it.  Otherwise a
	 * binding to the null pointer is created, and a pointer to
	 * that binding returned.
	 *
	 * Note that this function always applies to this specific
	 * environment: there is no search up through enclosing
	 * environments.
	 *
	 * An error is raised if the environment or the relevant
	 * binding is locked.
	 *
	 * @param symbol The Symbol for which a mapping is to be
	 *          created or modified.
	 *
	 * @return Pointer to the required binding, via which the
	 * calling code can modify the binding.
	 */
	PairList* obtainBinding(const Symbol* symbol)
	{
	    return frameObtainBinding(symbol);
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
	    propagateAge(m_enclosing);
	}

	/** @brief Set locking status.
	 *
	 * @param on The required locking status (true = locked).
	 *
	 * @note Possibly replace by a plain lock(): unlocking doesn't
	 * seem to happen.
	 */
	void setLocking(bool on)
	{
	    m_locked = on;
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

	/** @brief Number of symbols defined.
	 *
	 * @return the number of symbols defined in this environment.
	 */
	virtual size_t size() const = 0;

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
	void visitChildren(const_visitor* v) const;
    protected:
	// Declared protected to ensure that Environment objects are
	// created only using 'new':
	~Environment() {}
    private:
	static GCRoot<Environment> s_empty_env;
	static GCRoot<Environment> s_base_env;
	static GCRoot<Environment> s_global_env;

	Environment* m_enclosing;
	bool m_single_stepping;
	bool m_locked;

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Environment(const Environment&);
	Environment& operator=(const Environment&);

	// Erase symbol from this environment's own frame.
	// No locking checks.  Returns true iff symbol was previously
	// bound.
	virtual bool frameErase(const Symbol* symbol) = 0;

	// Access the binding of a symbol in this environment's own
	// frame.  No locking checks.
	virtual PairList* frameBinding(const Symbol* symbol) = 0;
	virtual const PairList* frameBinding(const Symbol* symbol) const = 0;

	// Access the binding of a symbol in this environment's own
	// frame; create a binding if symbol wasn't previously bound.
	// No locking checks.
	virtual PairList* frameObtainBinding(const Symbol* symbol) = 0;
    };

    /** @brief Incorporate bindings defined by a PairList into an
     *  Environment.
     *
     * Raises an error if the Environment is locked, or an attempt is made
     * to modify a binding that is locked.
     *
     * @param env Pointer to the Environment into whose frame new or
     *          modified bindings are to be incorporated.
     *
     * @param bindings List of symbol-value pairs defining bindings to
     *          be incorporated into the environment.  Every element
     *          of this list must have a Symbol as its tag (checked).
     *          If the list contains duplicate tags, later
     *          symbol-value pairs override earlier ones. Each
     *          resulting binding is locked and/or set active
     *          according to the m_active_binding and m_binding_locked
     *          fields of the corresponding PairList element.
     */
    void envReadPairList(Environment* env, PairList* bindings);

}  // namespace CXXR

typedef CXXR::PairList* R_varloc_t;

extern "C" {
#else /* if not __cplusplus */

    /* In C code, R_varloc_t is an opaque pointer: */
    typedef struct R_varloc_st *R_varloc_t;

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

    /** @brief Access an environment's frame list.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to the frame list of \a x (may be null).  The
     * calling code should not modify this list.
     */
#ifndef __cplusplus
    SEXP FRAME(SEXP x);
#else
    inline SEXP FRAME(SEXP x)
    {
	using namespace CXXR;
	Environment* env = SEXP_downcast<Environment*>(x);
	return const_cast<PairList*>(env->frameList());
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

#ifdef __cplusplus
}
#endif

#endif /* RENVIRONMENT_H */
