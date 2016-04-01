/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file ArgList.hpp
 * @brief Class rho::ArgList.
 */

#ifndef ARGLIST_HPP
#define ARGLIST_HPP 1

#include <boost/range.hpp>
#include "rho/GCStackRoot.hpp"
#include "rho/PairList.hpp"
#include "rho/Symbol.hpp"
#include "R_ext/Error.h"

namespace rho {
    class DottedArgs;
    class Expression;

    enum class MissingArgHandling {
	Drop,
	Keep,
	Error,
    };

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

	class const_iterator : public std::iterator<std::forward_iterator_tag,
						    ConsCell>
	{
	public:
	    const_iterator operator++(int) {
		const_iterator return_value = *this;
		++(*this);
		return return_value;
	    }

	    const_iterator operator++() {
		if (m_dots) {
		    m_dots = m_dots->tail();
		    if (m_dots)
			return *this;
		}
		m_arg = m_arg->tail();
		if (m_arg && m_arg->car() == DotsSymbol) {
		    handleDots();
		}
		return *this;
	    }

	    bool operator==(const_iterator other) const {
		return m_arg == other.m_arg && m_dots == other.m_dots;
	    }

	    bool operator!=(const_iterator other) const {
		return !(*this == other);
	    }

	    const ConsCell& operator*() const {
		return *(operator->)();
	    }
	    const ConsCell* operator->() const {
		if (m_dots)
		    return m_dots;
		return m_arg;
	    }

	    static const_iterator end(Environment* env) {
		return const_iterator(nullptr, env);
	    }
	private:
	    friend class ArgList;

	    const_iterator(ConsCell* arg, Environment* env);

	    ConsCell* m_arg;
	    ConsCell* m_dots;
	    Environment* m_env;

	    void handleDots();
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
	      m_list(const_cast<PairList*>(args)),
	      m_status(status)
	{}

	/** @brief Constructor.
	 *
	 * @param args List, possibly empty, of the arguments to pass.
	 *          The argument list has no names, so argument matching
	 *          will be done solely by position.
	 *
	 * @param status The Status of the argument list provided by
	 *          \a args.  No check is made that the \a args list
	 *          is actually consistent with the value of \a status.
	 */
	ArgList(std::initializer_list<RObject*> args, Status status)
	    : ArgList(PairList::make(args.size(), args.begin()), status) { }

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
	 * @note This function is intended within rho to supersede
	 * CR's evalList() and evalListKeepMissing().
	 */
        void evaluate(Environment* env,
		      MissingArgHandling allow_missing
		        = MissingArgHandling::Error);

	/** @brief Evaluate the arguments in the ArgList.
	 *
	 * This is a simpler, faster version of evaluate(), which differs
	 * in the following ways:
	 *  - R_DotsSymbol is not supported at all.  As a result, the list
	 *    of evaluated args is the same length as the unevaluated args.
	 *  - The evaluated arguments are placed into the \a evaluated_args
	 *    buffer provided by the caller.
	 *  - The arglist is not updated at all.  As a result, calling a
	 *    subsequent evaluation of the arglist will result in multiple
	 *    evaluations being performed.
	 *  - It is not an error to call this if the arguments have already
	 *    been evaluated.  In that case, it simply copies the values
	 *    into the array.
	 *
	 * @param env The Environment in which evaluations are to take
	 *          place.  If firstArg() has previously been called
	 *          for this ArgList, then \a env must be identical to
	 *          the \a env argument of that firstArg() call.
	 *
	 * @param num_args The length of the \a evaluated_args buffer.
	 *         Must also match the length of the arglist.
	 *
	 * @param evaluated_args An array to write the evaluated args into.
	 *
	 * @param allow_missing This affects the handling of any
	 *          elements of the current list whose value is either
	 *          Symbol::missingArgument() or a Symbol which is
	 *          missing within \a env.  If \a allow_missing is
	 *          true, such elements are carried through unchanged
	 *          into the resulting list; otherwise, such elements
	 *          cause an error to be raised.
	 */
	void evaluateToArray(Environment* env,
			     int num_args, RObject** evaluated_args,
			     MissingArgHandling allow_missing
			         = MissingArgHandling::Error);

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

        /** @brief Return the length of the arglist.
         */
	size_t size() const {
	    return listLength(list());
	}

        /** @brief Return the argument at the specified position.
         */
        RObject* get(int position) const;

        /** @brief Return the tag at the specified position.
         */
        const RObject* getTag(int position) const;

	/** @brief Iterator through the argument list, expanding '...'.
	 *
	 * @return iterator_range that iterates through the list of arguments.
	 *        If this arglist contains '...', then the iteration will
	 *        replace it with the elements that the '...' expands into.
	 */
	boost::iterator_range<const_iterator>
	getExpandedArgs(Environment* env) const {
	    if (m_first_arg_env && env != m_first_arg_env)
		Rf_error("Internal error: first arg of ArgList"
			 " previously evaluated in different environment");
	    return boost::make_iterator_range(const_iterator(m_list, env),
					      const_iterator::end(env));
	}

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

	/** @brief Does this arglist contain an unexpanded '...'?
         */
        bool has3Dots() const;

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
	static const Symbol* tag2Symbol(const RObject* tag);

	/** @brief Wrap elements of the argument list in Promise
	 * objects.
	 *
	 * Basically, this function wraps any argument in the ArgList
	 * whose value is not Symbol::missingArgument() in a Promise
	 * to be evaluated in \a env.  However, if the ArgList
	 * currently has Status EVALUATED, the \a env parameter is
	 * ignored, and the function simply wraps the argument values
	 * in pre-forced Promise objects with arguments taken from
	 * \a call.
	 *
	 * If any argument has the value rho::DotsSymbol, the action
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
	 * @param call The call that the arguments came from.  Ignored unless
	 *          the ArgList has status EVALUATED.
	 *
	 * @note It would be desirable to avoid producing a new
	 * PairList, and to absorb this functionality directly into
	 * the ArgMatcher::match() function.  But at present the
	 * Promise-wrapped list is recorded in the context set up by
	 * Closure::apply(), and used for other purposes.
	 */
	void wrapInPromises(Environment* env,
			    const Expression* call = nullptr);

    private:
	const PairList* const m_orig_list;  // Pointer to the argument
	  // list supplied to the constructor. 
	GCStackRoot<PairList> m_list;  // The current argument list. The
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

	RObject* evaluateSingleArgument(const RObject* arg,
					Environment* env,
					MissingArgHandling allow_missing,
					int arg_number);

	void setList(PairList* list) {
	    m_list = list;
	}

	PairList* mutable_list() {
	    // Mustn't modify the list supplied to the constructor:
	    if (m_list != nullptr && m_list == m_orig_list) {
		m_list = m_list->clone();
	    }
	    return m_list;
	}

	/* @brief Appends item to the end of m_list.  Handles empty lists correctly.
	 *
	 * @param value The value to add.
	 * @param tag The tag to associate with the value.
	 * @param last_element A pointer to the last element of m_list.
	 *
	 * @return A pointer to the current last element of m_list.
	 */
	PairList* append(RObject* value, const RObject* tag,
			 PairList* last_element);

	void wrapInForcedPromises(Environment* env,
				  const ArgList* evaluated_values);

	// Not implemented.  Declared private to suppress
	// compiler-generated versions:
	ArgList(const ArgList&);
	ArgList& operator=(const ArgList&);

	// Coerce a tag that is not already a Symbol into Symbol form:
	static const Symbol* coerceTag(const RObject* tag);
    };
}

#endif  // ARGLIST_HPP
