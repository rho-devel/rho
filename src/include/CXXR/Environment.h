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
 */

#ifndef RENVIRONMENT_H
#define RENVIRONMENT_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/ListVector.h"
#include "CXXR/PairList.h"
#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    class FunctionBase;

    /** @brief Mapping from Symbols to R objects.
     *
     * An Environment defines a mapping from (pointers to)
     * CXXR::Symbol objects to (pointers to) arbitrary objects of
     * classes derived from RObject.  Each Environment (except for the
     * standard empty environment) has an 'enclosing environment'.
     *
     * @note In CR, working up the enclosing relationship will always
     * lead to the empty environment.  This is not enforced in this
     * class.
     */
    class Environment : public RObject {
    public:
	/** @brief Representation of a binding of a Symbol to an
	 *  RObject.
	 *
	 * A binding may be identified as 'active', in which case it
	 * encapsulates a function (i.e. an object of type
	 * FunctionBase).  The value of the binding, as returned by
	 * value(), is then determined by evaluating this function
	 * with no arguments.  Setting the value of the binding, by
	 * calling assign(), simply invokes that same function with
	 * the supplied value as its single argument: the encapsulated
	 * function is not altered.
	 *
	 * @note Arguably plain bindings and active bindings ought to
	 * be modelled by different classes.
	 */
	class Binding {
	public:
	    /** @brief Default constructor
	     *
	     * initialize() must be called before the Binding object
	     * can be used.
	     */
	    Binding()
		: m_environment(0), m_symbol(0), m_value(0),
		  m_missing(0), m_active(false), m_locked(false)
	    {}

	    /** @brief Represent this Binding as a PairList element.
	     *
	     * This function creates a new PairList element which
	     * represents the information in the Binding in the form
	     * returned by CR's FRAME() function.
	     *
	     * If the Binding is active, then the 'car' of the created
	     * PairList element will contain a pointer to the
	     * encapsulated function.
	     *
	     * @param tail Value to be set as the tail (CDR) of the
	     *          created PairList element.
	     *
	     * @return The created PairList element.
	     */
	    PairList* asPairList(PairList* tail = 0) const;

	    /** @brief Bind value to Symbol.
	     *
	     * In the case of non-active bindings, this function has
	     * exactly the same effect as setValue(): it changes the
	     * value to which this Binding's symbol is bound to \a
	     * new_value.
	     *
	     * For active bindings, it invokes the encapsulated
	     * function with \a new_value as its sole argument.
	     *
	     * Raises an error if the Binding is locked.
	     *
	     * @param new_value Pointer (possibly null) to the new
	     *          value.  See function description.
	     */
	    void assign(RObject* new_value);

	    /** @brief Get pointer to environment.
	     *
	     * @return Pointer to the Environment to which this
	     * Binding belongs.
	     */
	    const Environment* environment() const
	    {
		return m_environment;
	    }

	    /** @brief Get binding information from a PairList
	     * element.
	     *
	     * This function sets the value of this Binding, and its
	     * active, locked and missing status, from the
	     * corresponding fields of a PairList element.
	     *
	     * If the PairList element defines the Binding as active,
	     * then its 'car' is considered to contain a pointer to
	     * the active binding function.
	     *
	     * Raises an error if the Binding is locked.
	     *
	     * @param pl Non-null pointer to the PairList element from
	     *          which binding information is to be obtained.
	     *          If \a pl has a tag, it must be a pointer to
	     *          the Symbol bound by this Binding.
	     */
	    void fromPairList(PairList* pl);

	    /** @brief Initialize the Binding.
	     *
	     * This function initializes the Environment and Symbol
	     * pointers of this Binding.  This function may be called
	     * at most once for any Binding object, and must be called
	     * before any other use is made of the Binding.
	     *
	     * @param env Pointer to the Environment to which this
	     *          Binding belongs.  Must be non-null.
	     *
	     * @param sym Pointer to the Symbol bound by this
	     *          Binding.  Must be non-null.
	     */
	    void initialize(const Environment* env, const Symbol* sym);

	    /** @brief Is this an active Binding?
	     *
	     * @return true iff this is an active Binding.
	     */
	    bool isActive() const
	    {
		return m_active;
	    }

	    /** @brief Is this Binding locked?
	     *
	     * @return true iff this Binding is locked.
	     */
	    bool isLocked() const
	    {
		return m_locked;
	    }

	    /** @brief Binding's missing status.
	     *
	     * @return the 'missing' status of this Binding.  0 means
	     * 'not missing'.
	     *
	     * @todo Document the other possible return values, and
	     * clarify the relationship of this field to
	     * <tt>R_MissingArg</tt>.
	     */
	    short int missing() const
	    {
		return m_missing;
	    }

	    /** @brief Get raw value bound to the Symbol.
	     *
	     * 'raw' here means that in the case of an active Binding,
	     * the function returns a pointer to the encapsulated
	     * function rather than the result of evaluating the
	     * encapsulated function.
	     *
	     * @return The value bound to a Symbol by this Binding.
	     */
	    RObject* rawValue() const
	    {
		return m_value;
	    }

	    /** @brief Sets this to be an active Binding encapsulating
	     * a specified function.
	     *
	     * When invoked for an existing active Binding, this
	     * function simply replaces the encapsulated function.
	     *
	     * Raises an error if the Binding is locked.
	     *
	     * Also raises an error if the Binding is not currently
	     * marked active but has a non-null value.  (This is
	     * slightly less strict than CR, which only allows active
	     * status to be set on a newly created binding.)
	     *
	     * @param function The function used to implement the
	     *          active binding.
	     */
	    void setFunction(FunctionBase* function);

	    /** @brief Lock/unlock this Binding.
	     *
	     * @param on true iff the Binding is to be locked.
	     */
	    void setLocking(bool on)
	    {
		m_locked = on;
	    }

	    /** @brief Set the 'missing status of this Binding'.
	     *
	     * Raises an error if the Binding is locked.
	     *
	     * @param missingval The required 'missing' status.  Refer
	     *          to the documentation of missing() for the
	     *          possible values.
	     */
	    void setMissing(short int missingval);

	    /** @brief Define the object to which this Binding's
	     *         Symbol is bound.
	     *
	     * Raises an error if the Binding is locked or active.
	     *
	     * @param new_value Pointer (possibly null) to the RObject
	     *          to which this Binding's Symbol is now to be
	     *          bound.
	     */
	    void setValue(RObject* new_value);

	    /** @brief Bound symbol.
	     *
	     * @return Pointer to the Symbol bound by this Binding.
	     */
	    const Symbol* symbol() const
	    {
		return m_symbol;
	    }

	    /** @brief Get value bound to the Symbol.
	     *
	     * For an active binding, this evaluates the encapsulated
	     * function and returns the result rather than returning a
	     * pointer to the encapsulated function itself.
	     *
	     * @return The value bound to a Symbol by this Binding.
	     */
	    RObject* value() const;

	    /** @brief Auxiliary function to
	     *         Environment::visitChildren().
	     *
	     * This function conducts a visitor to those objects
	     * derived from GCNode which become 'children' of this
	     * Binding's Environment as a result of its containing
	     * this Binding.
	     *
	     * @param v Pointer to the visitor object.
	     */
	    void visitChildren(const_visitor* v) const;
	private:
	    const Environment* m_environment;
	    const Symbol* m_symbol;
	    RObject* m_value;
	    short int m_missing;
	    bool m_active;
	    bool m_locked;
	};

	typedef void (*monitor)(const Binding&);

	/**
	 * @param enclosing Pointer to the enclosing environment.
	 */
	explicit Environment(Environment* enclosing = 0)
	    : RObject(ENVSXP), m_enclosing(enclosing),
	      m_single_stepping(false), m_locked(false), 
	      m_read_monitor(0), m_write_monitor(0)
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
	 * This function provides a pointer to the Binding of a
	 * Symbol.  In this variant the pointer is non-const, and
	 * consequently the calling code can use it to modify the
	 * binding (provided the Binding is not locked).
	 *
	 * @param symbol The Symbol for which a mapping is sought.
	 *
	 * @param recursive If false, a mapping is sought only in this
	 *          environment.  If true, the search works up through
	 *          enclosing environments.
	 *
	 * @return A pointer to the required binding, or a null
	 * pointer if it was not found..
	 */
	Binding* binding(const Symbol* symbol, bool recursive = true);

	/** @briefing Access const binding of an already-defined Symbol.
	 *
	 * This function provides a pointer to a PairList element
	 * representing the binding of a symbol.  In this variant the
	 * pointer is const, and consequently the calling code can use
	 * it only to examine the binding.
	 *
	 * @param symbol The Symbol for which a mapping is sought.
	 *
	 * @param recursive If false, a mapping is sought only in this
	 *          environment.  If true, the search works up through
	 *          enclosing environments.
	 *
	 * @return A pointer to the required binding, or a null
	 * pointer if it was not found..
	 */
	const Binding* binding(const Symbol* symbol,
			       bool recursive = true) const;

	/** @brief Remove all symbols from the Environment.
	 *
	 * Raises an error if the Environment is locked.
	 */
	virtual void clear() = 0;

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

	/** @brief Remove the Binding (if any) of a Symbol.
	 *
	 * This function causes any Binding for a specified Symbol to
	 * be removed from the environment.  Note that this function
	 * always applies to this specific environment: there is no
	 * search up through enclosing environments.
	 *
	 * An error is raised if the Environment is locked (whether or
	 * not it contains a binding of \a symbol ).
	 *
	 * @param symbol The Symbol for which the Binding is to be
	 *          removed.
	 *
	 * @return True iff the environment previously contained a
	 * mapping for \a symbol.
	 */
	virtual bool erase(const Symbol* symbol) = 0;

	/** @brief Get contents as a PairList.
	 *
	 * Access the contents of this Environment expressed
	 * as a PairList, with the tag of each PairList element
	 * representing a Symbol and the car value representing
	 * the object to which that Symbol is mapped, and with the
	 * Binding's active, locked and missing status indicated as in
	 * CR's FRAME() function.
	 *
	 * @return pointer to a PairList as described above.
	 *
	 * @note The PairList is generated on demand, so this
	 * operation is relatively expensive in time.  Modifications
	 * to the returned PairList will have no effect on the
	 * Environment itself.
	 */
	virtual PairList* frameList() const = 0;

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

	/** @brief Lock this Environment.
	 *
	 * Locking an Environment prevents the addition or removal of
	 * Bindings.  Optionally, the existing bindings can be locked,
	 * preventing them from being modified.
	 *
	 * This operation is permitted even if the Environment is
	 * already locked, but will have no effect unless it newly
	 * locks the Bindings in the Environment.
	 * 
	 * @param lock_bindings true iff all the existing Bindings in
	 * the Environment are to be locked.
	 */
	void lock(bool lock_bindings)
	{
	    m_locked = true;
	    if (lock_bindings)
		lockBindings();
	}

	/** @brief Lock all Bindings in this Environment.
	 *
	 * This operation affects only Bindings currently existing.
	 * It does not prevent Bindings being added subsequently, and
	 * such Bindings will not be locked.
	 *
	 * It is permitted to apply this function to a locked
	 * Environment.
	 */
	virtual void lockBindings() = 0;

	/** @brief Get or create a Binding for a Symbol.
	 *
	 * If the environment already contains a Binding for a
	 * specified Symbol, the function returns it.  Otherwise a
	 * Binding to the null pointer is created, and a pointer to
	 * that Binding returned.
	 *
	 * Note that this function always applies to this specific
	 * environment: there is no search up through enclosing
	 * environments.
	 *
	 * An error is raised if a new Binding needs to be created and
	 * the Environment is locked.
	 *
	 * @param symbol The Symbol for which a Binding is to be
	 *          obtained.
	 *
	 * @return Pointer to the required Binding.
	 */
	virtual Binding* obtainBinding(const Symbol* symbol) = 0;

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

	/** @brief Monitor reading of Symbol values.
	 *
	 * This function allows the user to define a function to be
	 * called whenever a Symbol's value is read from a Binding
	 * within (the local frame of) this Environment.
	 *
	 * In the case of an active Binding, the monitor is called
	 * whenever the encapsulated function is accessed: note that
	 * this includes calls to Binding::assign().
	 *
	 * @param new_monitor Pointer, possibly null, to the new
	 *          monitor function.  A null pointer signifies that
	 *          no read monitoring is to take place, which is the
	 *          default state.
	 *
	 * @return Pointer, possibly null, to the monitor being
	 * displaced by \a new_monitor.
	 *
	 * @note The presence or absence of a monitor is not
	 * considered to be part of the state of an Environment
	 * object, and hence this function is const.
	 */
	monitor setReadMonitor(monitor new_monitor) const
	{
	    monitor old = m_read_monitor;
	    m_read_monitor = new_monitor;
	    return old;
	}

	/** @brief Monitor writing of Symbol values.
	 *
	 * This function allows the user to define a function to be
	 * called whenever a Symbol's value is modified in a Binding
	 * within (the local frame of) this Environment.
	 *
	 * In the case of an active Binding, the monitor is called
	 * only when the encapsulated function is initially set or
	 * changed: in particular the monitor is \e not invoked by
	 * calls to Binding::assign().
	 *
	 * The monitor is not called when a Binding is newly created
	 * within an Environment (with the Symbol bound by default to
	 * a null pointer).
	 *
	 * @param new_monitor Pointer, possibly null, to the new
	 *          monitor function.  A null pointer signifies that
	 *          no write monitoring is to take place, which is the
	 *          default state.
	 *
	 * @return Pointer, possibly null, to the monitor being
	 * displaced by \a new_monitor.
	 *
	 * @note The presence or absence of a monitor is not
	 * considered to be part of the state of an Environment
	 * object, and hence this function is const.
	 */
	monitor setWriteMonitor(monitor new_monitor) const
	{
	    monitor old = m_write_monitor;
	    m_write_monitor = new_monitor;
	    return old;
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

	/** @brief Number of Symbols bound.
	 *
	 * @return the number of Symbols for which Bindings exist in
	 * this environment.
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
	mutable monitor m_read_monitor;
	mutable monitor m_write_monitor;

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Environment(const Environment&);
	Environment& operator=(const Environment&);

	// Access the binding of a symbol in this environment's own
	// frame.  No locking checks.
	virtual Binding* frameBinding(const Symbol* symbol) = 0;
	virtual const Binding* frameBinding(const Symbol* symbol) const = 0;

	// Monitoring functions:
	friend class Binding;

	void monitorRead(const Binding& bdg) const
	{
	    if (m_read_monitor) m_read_monitor(bdg);
	}

	void monitorWrite(const Binding& bdg) const
	{
	    if (m_write_monitor) m_write_monitor(bdg);
	}
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

typedef CXXR::Environment::Binding* R_varloc_t;

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
