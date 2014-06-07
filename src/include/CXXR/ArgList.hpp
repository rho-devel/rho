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
	/** @brief Way (if any) in which ArgList has been processed.
	 */
	enum Status {
	    RAW,       /**< Unprocessed. */
	    PROMISED,  /**< Argument values are wrapped in Promise
			* objects, and non-null tags have been coerced
			* to Symbols.  This is the form expected by
			* ArgMatcher::match().
			*/
	    EVALUATED  /**< Argument values have been evaluated, and
			* ... arguments expanded.
			*/
	};

	/** @brief Constructor.
	 *
	 * @param args Pointer, possibly null, to a PairList of
	 *          unevaluated arguments supplied in a call of a
	 *          FunctionBase.  The calling code must not modify \a
	 *          args after calling this constructor.
	 *
	 * @param status The Status of the argument list provided by
	 *          \a args.  No check is made that the \a args list
	 *          is actually consistent with the value of \a status.
	 */
	ArgList(const PairList* args, Status status)
	    : m_orig_list(args),
	      m_list(PairList::cons(0, const_cast<PairList*>(args))),
	      m_status(status)
	{}

	/** @brief Evaluate the arguments in the ArgList.
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
	 * After the call, the ArgList will have Status EVALUATED; it
	 * is an error to call this function if the ArgList already
	 * has Status EVALUATED.
	 *
	 * @param env The Environment in which evaluations are to take
	 *          place.  If firstArg() has previously been called
	 *          for this ArgList, then \a env must be identical to
	 *          the \a env argument of that firstArg() call.
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

	/** @brief Get the first argument.
	 *
	 * This function accesses the value of the first argument in
	 * the ArgList, evaluating it in \a env if the ArgList has not
	 * already been evaluated.
	 *
	 * However, if the first argument is DotsSymbol (...), the
	 * binding of this Symbol within \a env is examined.  If it is
	 * bound to a DottedArgs list, the return value is the value
	 * of the first element is this DottedArgs list, evaluated if
	 * necessary.
	 *
	 * In seeking the first argument of the ArgList, the function
	 * ignores any initial elements of the ArgList whose value is
	 * DotsSymbol (...) if the binding of this Symbol within \a
	 * env is NULL or Symbol::missingArgument().  Any other binding of
	 * DotsSymbol is an error; it is also an error is a DotsSymbol
	 * argument is encountered but DotsSymbol is unbound within \a
	 * env.
	 *
	 * If evaluation of the first argument takes place, the class
	 * records this fact, and takes steps to ensure that it is not
	 * subsequently reevaluated.
	 *
	 * @param env Pointer to the Environment in which the first
	 *          argument is to be evaluated.  If the ArgList has
	 *          already been evaluated, this pointer can be null.
	 *
	 * @return If, following the procedure described above, no
	 * first argument was found, then the first element of the
	 * pair is false and the second element null.  Otherwise, the
	 * first element is true and the second element is a pointer
	 * to the value of the first argument.
	 */
	std::pair<bool, RObject*> firstArg(Environment* env);

	/** @brief Access the argument list as a PairList.
	 *
	 * @return pointer, possibly null, to the list of arguments in
	 * their current state.
	 */
	const PairList* list() const
	{
	    return m_list->tail();
	}

	/** @brief Merge in new argument values..
	 *
	 * This function is used in implementing NextMethod.  If any
	 * element of the ArgList has the same non-null tag as an
	 * element of \a extraargs, then the value (car) of that
	 * element of the ArgList is replaced by the value of the
	 * corresponding element of \a extraargs.
	 *
	 * Any elements of \a extraargs that do not override existing
	 * arguments as described in the previous paragraph are
	 * appended in order to the ArgList.
	 *
	 * It is an error to call this function unless the ArgList has
	 * PROMISED status.
	 *
	 * @param extraargs Pointer, possibly null, to a list whose
	 *          elements represent Promise-wrapped argument
	 *          values.
	 *
	 * @note This behavior conforms to the R manpage for
	 * NextMethod.  However, the White Book says that unnamed
	 * arguments in \a extraargs should be \e prepended to the
	 * ArgList.
	 */
	void merge(const ConsCell* extraargs);

	/** @brief How has this ArgList been processed?
	 *
	 * @return The current Status of the Arglist.
	 */
	Status status() const
	{
	    return m_status;
	}

	/** @brief Remove argument names.
	 *
	 * This function removes any tags from the ArgList.  This will
	 * force any subsequent argument matching to be based on
	 * argument position alone.
	 */
	void stripTags();

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
	 * whose value is not Symbol::missingArgument() in a Promise
	 * to be evaluated in \a env.  However, if the ArgList
	 * currently has Status EVALUATED, the \a env parameter is
	 * ignored, and the function simply wraps the argument values
	 * in pre-forced Promise objects.
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
	 * After the call, the ArgList will have Status PROMISED; it
	 * is an error to call this function if the ArgList already
	 * has Status PROMISED.
	 *
	 * @param env Pointer to the Environment to which Promises in
	 *          the output list are to be keyed.  As noted above,
	 *          this parameter is ignored if the ArgList has
	 *          Status EVALUATED.  Otherwise, if firstArg() has
	 *          previously been called for this ArgList, then \a
	 *          env must be identical to the \a env argument of
	 *          that firstArg() call.
	 *
	 * @note It would be desirable to avoid producing a new
	 * PairList, and to absorb this functionality directly into
	 * the ArgMatcher::match() function.  But at present the
	 * Promise-wrapped list is recorded in the context set up by
	 * Closure::apply(), and used for other purposes.
	 */
	void wrapInPromises(Environment* env);
    private:
	const PairList* const m_orig_list;  // Pointer to the argument
	  // list supplied to the constructor. 
	GCStackRoot<PairList> m_list;  // The current argument list,
	  // preceded by a dummy element to simplify coding.  The
	  // class code should never modify the PairList supplied as
	  // an argument to the constructor, even though the
	  // constructor casts const away when it initialises this
	  // data member.
	GCStackRoot<> m_first_arg;  // If the first argument needed to
	  // be evaluated in a call to firstArg(), this is a pointer
	  // to the resulting value, and m_first_arg_env points to the
	  // Environment in which evaluation took place.  Both
	  // pointers are reset to null once the first argument has
	  // been processed in a subsequent call to evaluate() or
	  // wrapInPromises(). 
	GCStackRoot<Environment> m_first_arg_env;
	Status m_status;

	// Not implemented.  Declared private to suppress
	// compiler-generated versions:
	ArgList(const ArgList&);
	ArgList& operator=(const ArgList&);

	// Coerce a tag that is not already a Symbol into Symbol form:
	static const Symbol* coerceTag(const RObject* tag);
    };
}

#endif  // ARGLIST_HPP
