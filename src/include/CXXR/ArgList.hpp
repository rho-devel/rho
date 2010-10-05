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

/** @file ArgList.hpp
 * @brief Class CXXR::ArgList.
 */

#ifndef ARGLIST_HPP
#define ARGLIST_HPP 1

#include "CXXR/GCStackRoot.hpp"
#include "CXXR/PairList.h"
#include "CXXR/Symbol.h"

namespace CXXR {
    class DottedArgs;

    /** @brief Class encapsulating the argument list of a FunctionBase.
     *
     * ArgList objects must be declared on the processor stack
     * (i.e. as C++ automatic variables).
     * 
     * This class encapsulates a list of the argument supplied in a
     * call to a FunctionBase object, and provides facilities for
     * manipulating the list in the various ways required
     */
    class ArgList {
    public:
	/** @brief Constructor.
	 *
	 * @param args Pointer, possibly null, to a PairList of
	 *          arguments supplied in a call of a FunctionBase.
	 *          The calling code must not modify \a args after
	 *          calling this constructor.
	 *
	 * @param evaluated true iff the arguments in \a args have
	 *          already been evaluated.
	 */
	ArgList(const PairList* args, bool evaluated)
	    : m_list(args), m_evaluated(evaluated), m_wrapped(false)
	{}

	/** @brief Evaluate the arguments in the ArgList.
	 *
	 * This function is a no-op if the ArgList has already been
	 * evaluated.
	 *
	 * Except as regards the handling of ... and missing values
	 * described next, this function replaces the argument list
	 * with a list of the same length as the current list, and
	 * whose tags are the same as those of the current list, but
	 * whose elements ('car' values) are the result of evaluating
	 * the corresponding elements of the current list within \a
	 * env.
	 *
	 * If an element of the list has value R_DotsSymbol, the
	 * binding of this Symbol within \a env is examined.  If it is
	 * bound to NULL or to Symbol::missingArgument()
	 * (R_MissingArg), this element of the current list is
	 * discarded.  If it is bound to a DottedArgs list, then this
	 * element of the current list is replaced by the one or more
	 * elements resulting from evaluating the elements of the
	 * DottedArgs list, and carrying across the corresponding
	 * tags.  If the DottedArgs list contains any elements with
	 * value Symbol::missingArgument(), these are carried through
	 * unchanged into the resulting list, irrespective of the
	 * setting of \a allow_missing.  Any other binding of
	 * R_DotsSymbol within \a env is an error.
	 *
	 * If any element of the current list has the value
	 * Symbol::missingArgument(), then this element of the list is
	 * carried through unchanged into the resulting list, unless
	 * \a allow_missing is false, in which case an error is
	 * raised.
	 *
	 * If any element of the current list has as value a Symbol
	 * missing within \a env, then this is converted in the
	 * resulting list into an element with the same tag but whose
	 * value is Symbol::missingArgument().  This applies unless \a
	 * allow_missing is false, in which case an error is raised.
	 *
	 * @param env The Environment in which evaluations are to take
	 *          place.
	 *
	 * @param allow_missing This affects the handling of any
	 *          elements of the current list whose value is either
	 *          Symbol::missingArgument() or a Symbol which is
	 *          missing within \a env.  If \a allow_missing is
	 *          true, such elements are carried through unchanged
	 *          into the resulting list; otherwise, such elements
	 *          cause an error to be raised.
	 *
	 * @note This function is intended within CXXR to supersede
	 * CR's evalList() and evalListKeepMissing().
	 */
	void evaluate(Environment* env, bool allow_missing = false);

	/** @brief Access the argument list as a PairList.
	 *
	 * @return pointer, possibly null, to the list of arguments in
	 * their current state.
	 */
	const PairList* list() const
	{
	    return m_list;
	}

	/** @brief Merge in new argument values..
	 *
	 * This function is used in implementing NextMethod.
	 * Basically it prepends the arguments in \a newargs to the
	 * current ArgList.  However, if any existing element of the
	 * ArgList has the same non-null tag as an element of \a
	 * newargs, that element of the current ArgList is discarded
	 * in the result.
	 *
	 * @param extraargs Pointer, possibly null, to a list whose
	 *          elements represent unevaluated argument values.
	 *          It is an error to call this function if the
	 *          ArgList has already been evaluated or wrapped in
	 *          Promises.
	 */
	void merge(const ConsCell* extraargs);

	/** @brief Convert tag of supplied argument to a Symbol.
	 *
	 * If \a tag is a null pointer or already points to a Symbol,
	 * then \a tag itself is returned.
	 *
	 * If \a tag points to a StringVector with at least one
	 * element, and the first element is a String of length at
	 * least one, then the function returns a pointer to a Symbol
	 * with that first element, translated in the current locale,
	 * as its name.
	 *
	 * In all other cases the function returns a Symbol whose name
	 * is obtained by an abbreviated SIMPLEDEPARSE of \a tag.
	 *
	 * @param tag Pointer (possibly null) to the tag to be
	 *          processed.
	 *
	 * @return pointer to the representation of \a tag as a
	 * Symbol.
	 *
	 * @todo This function probably ought to be fussier about what
	 * it accepts as input.
	 */
	inline static const Symbol* tag2Symbol(const RObject* tag)
	{
	    return ((!tag || tag->sexptype() == SYMSXP)
		    ? static_cast<const Symbol*>(tag)
		    : coerceTag(tag));
	}

	/** @brief Wrap elements of the argument list in Promise
	 * objects.
	 *
	 * Basically, this function wraps any argument in the ArgList
	 * whose value is not Symbol::missingArgument() is wrapped in
	 * a Promise to be evaluated in \a env.
	 *
	 * If any argument has the value CXXR::DotsSymbol, the action
	 * depends on what this Symbol is bound to within \a env (and
	 * its enclosing environments).  If it is unbound, or bound to
	 * a null pointer or to Symbol::missingArgument(), then this
	 * element of the ArgList is discarded.  If it is bound to a
	 * DottedArgs list, then the elements of that list are added
	 * to the ArgList as arguments in their own right, with each
	 * argument value being wrapped in a Promise to be evaluated
	 * in \a env.  Any other binding of DotsSymbol is an error.
	 *
	 * Any tags in the ArgList or in a DottedArgs list are carried
	 * across to the corresponding element of the modified list, but
	 * coerced using tag2Symbol() into a form suitable for
	 * argument matching.
	 *
	 * @param env Pointer to the Environment to which Promises in
	 *          the output list are to be keyed.
	 *
	 * @note It would be desirable to avoid producing a new
	 * PairList, and to absorb this functionality directly into
	 * the ArgMatcher::match() function.  But at present the
	 * Promise-wrapped list is recorded in the context set up by
	 * Closure::apply(), and used for other purposes.
	 */
	void wrapInPromises(Environment* env);
    private:
	GCStackRoot<const PairList> m_list;
	bool m_evaluated, m_wrapped;

	// Coerce a tag that is not already a Symbol into Symbol form:
	static const Symbol* coerceTag(const RObject* tag);
    };
}

#endif  // ARGLIST_HPP
