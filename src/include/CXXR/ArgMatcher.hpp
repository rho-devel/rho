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

/** @file ArgMatcher.hpp
 * @brief Class CXXR::ArgMatcher.
 */

#ifndef ARGMATCHER_HPP
#define ARGMATCHER_HPP 1

#include <map>
#include <vector>
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCNode.hpp"
#include "CXXR/String.h"
#include "CXXR/Symbol.h"

namespace CXXR {
    class CachedString;
    class Environment;
    class Frame;
    class PairList;
    class RObject;
    class Symbol;

    /** @brief Class to match formal and supplied arguments.
     *
     * This class encapsulates a list of formal arguments, each
     * possibly with a default binding, and provides (via match())
     * facilities to match the formal arguments to a list of supplied
     * arguments and place the resulting bindings within a specified
     * Frame.
     */
    class ArgMatcher : public GCNode {
    public:
	/** @brief Constructor.
	 *
	 * @param formals PairList of formal arguments, possibly
	 *          empty.  The elements of this list must have
	 *          distinct Symbol objects as their tags.  At most
	 *          one element may have '...' as its tag.
	 */
	explicit ArgMatcher(const PairList* formals);

	/** @brief Enable/disable warning if tag partially matched.
	 *
	 * @param on true iff the class is to be configured to raise a warning
	 *          if a supplied argument is matched to a formal
	 *          argument by virtue of partial matching on the
	 *          tag.  The default is not to raise such warnings.
	 */
	static void enableWarnOnPartialMatch(bool on)
	{
	    s_warn_on_partial_match = on;
	}

	/** @brief Formal arguments.
	 *
	 * @return Pointer to the formal argument list of this
	 * ArgMatcher object.
	 */
	const PairList* formalArgs() const
	{
	    return m_formals;
	}

	/** @brief Do the formals include '...'?
	 *
	 * @return true iff the formals list includes '...'.
	 */
	bool has3Dots() const
	{
	    return m_has_dots;
	}

	/** @brief Match formal and supplied arguments.
	 *
	 * Argument matching is carried out as described in Sec. 4.3.2
	 * of the 'R Language Definition' document.
	 *
	 * Following the call, the bindings in the Frame of \a
	 * target_env will be set according to the following
	 * decreasing order of precedence:
	 *
	 * <ol>
	 * <li>Bindings, other than to Symbol::missingArgument()
	 * (<tt>R_MissingArg</tt>), provided by the list of \a
	 * supplied arguments.  The Binding will have origin EXPLICIT.</li>
	 *
	 * <li>Bindings (other than to Symbol::missingArgument())
	 * supplied as default values to the formal parameters in the
	 * constructor to this ArgMatcher object.  The Binding will have
	 * origin DEFAULTED.  The default value will be wrapped in a
	 * Promise object keyed to \a target_env .</li>
	 *
	 * <li>Bindings already present in the Frame of \a target_env
	 * prior to the call of this function.</li>
	 *
	 * <li>Formal arguments not bound by the preceding clauses
	 * will be bound to Symbol::missingArgument(), and the Binding
	 * will have origin MISSING.</li>
	 * </ol>
	 *
	 * @param target_env Pointer to the Environment in whose Frame
	 *          bindings will be inserted as a result of the
	 *          argument matching process.  Any default arguments
	 *          used will be wrapped in Promise objects keyed to
	 *          this Environment.
	 *
	 * @param supplied PairList, possibly empty, of supplied
	 *          arguments.  It is not required for the elements of
	 *          the list to have tags, but if an element does have
	 *          a tag, the tag must be a Symbol.  (The function
	 *          does not check this, so strange bugs may ensue if
	 *          this precondition is not fulfilled.)  Typically
	 *          this list will be the output of prepareArgs().
	 */
	void match(Environment* target_env, const PairList* supplied) const;

	/** @brief Number of formal arguments.
	 *
	 * @return the number of formal arguments. '<tt>...</tt>' is
	 * counted as a single argument.
	 */
	size_t numFormals() const
	{
	    return m_formal_data.size() + m_has_dots;
	}

	/** @brief Prepare argument list for matching.
	 *
	 * This function takes the argument list \a raw_args, converts
	 * into a form suitable for argument matching by
	 * ArgMatcher::match(), and returns the resulting list.
	 * Basically, any argument whose value is not
	 * Symbol::missingArgument() is wrapped in a Promise to be
	 * evaluated in \a env.
	 *
	 * If any argument in \a raw_args has the value
	 * CXXR::DotsSymbol, the action depends on what this Symbol is
	 * bound to within \a env (and its enclosing environments).
	 * If it is unbound, or bound to a null pointer or to
	 * Symbol::missingArgument(), then this element of \a raw_args
	 * is ignored: nothing corresponding to it is added to the
	 * output list.  If it is bound to a DottedArgs list, then the
	 * elements of that list are added to the output list as
	 * arguments in their own right, with each argument value
	 * being wrapped in a Promise to be evaluated in \a env.  Any
	 * other binding of DotsSymbol is an error.
	 *
	 * Any tags in \a raw_args or in a DottedArgs list are carried
	 * across to the corresponding element of the output list, but
	 * coerced using tagSymbol() into a form suitable for
	 * argument matching.
	 *
	 * @param raw_args Pointer (possibly null) to the argument
	 *          list to be prepared for matching.
	 *
	 * @param env Pointer to the Environment to which Promises in
	 *          the output list are to be keyed.
	 *
	 * @return the list of prepared arguments.
	 *
	 * @note It would be desirable to avoid producing a new
	 * PairList, and to absorb this functionality directly into
	 * the match() function.  But at present the output list in
	 * recorded in the context set up by Closure::apply(), and
	 * used for other purposes.
	 */
	static PairList* prepareArgs(const PairList* raw_args, Environment* env);

	/** @brief Copy formal bindings from one Environment to another.
	 *
	 * This function is used in dispatching S4 methods.  In the
	 * function, \a fromenv points to an Environment which must
	 * contain a Binding for every formal argument of this
	 * ArgMatcher.  The function creates a copy of each of these
	 * Bindings in another Environment, pointed to by \a toenv.
	 * Each copied Binding preserves both the value and origin of
	 * the original Binding.
	 *
	 * If, within \a fromenv, a formal argument hs origin
	 * DEFAULTED and is bound to an (as yet unforced) Promise to
	 * be evaluated within \a fromenv (as will arise when the
	 * Binding represents a default argument supplied by the
	 * corresponding generic), then when this Binding is copied to \a toenv,
	 * its value is replaced by the corresponding default
	 * Binding supplied by this ArgMatcher.  (This will be Promise
	 * to be evaluated within \a toenv.)
	 *
	 * @param fromenv non-null pointer to an Environment which
	 *          must contain a Binding for every formal argument
	 *          of this ArgMatcher.
	 *
	 * @param toenv non-null pointer to another (unlocked)
	 *          Environment.
	 *
	 * @note If, prior to the call, \a toenv already contains one
	 * of more Bindings of the formal arguments of this
	 * ArgMatcher, then these Bindings are replaced by those
	 * propagated from \a fromenv.
	 */
	void propagateFormalBindings(const Environment* fromenv,
				     Environment* toenv) const;

	/** @brief Strip formal argument bindings from a Frame.
	 *
	 * This function removes from \a input_frame any bindings of
	 * the formal arguments of this Argmatcher.  It is used in
	 * creating the working environment of an S3 method from the
	 * working environment of its generic.
	 *
	 * @param input_frame Non-null pointer to the Frame from which
	 *          bindings are to be stripped.
	 */
	void stripFormals(Frame* input_frame) const;

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
	inline static const Symbol* tagSymbol(const RObject* tag)
	{
	    return ((!tag || tag->sexptype() == SYMSXP)
		    ? static_cast<const Symbol*>(tag)
		    : coerceTag(tag));
	}

	/** @brief Give warning if tag partially matched?
	 *
	 * @return true iff the class is configured to raise a warning
	 * if a supplied argument is matched to a formal argument by
	 * virtue of partial matching on the tag.
	 */
	static bool warnOnPartialMatch()
	{
	    return s_warn_on_partial_match;
	}

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Virtual function of GCNode:
	void detachReferents();
    private:
	static bool s_warn_on_partial_match;

	struct FormalData {
	    const Symbol* symbol;
	    bool follows_dots;  // true if ... occurs earlier in the
				// formals list.
	    RObject* value;
	};

	enum MatchStatus {UNMATCHED = 0, EXACT_TAG, PARTIAL_TAG, POSITIONAL};

	GCEdge<const PairList> m_formals;

	// Data on formals (other than "...") in order of occurrence:
	typedef std::vector<FormalData, Allocator<FormalData> > FormalVector;
	FormalVector m_formal_data;

	struct Comparator {
	    bool operator()(const CachedString* l, const CachedString* r) const
	    {
		return l->stdstring() < r->stdstring();
	    }
	};

	// Mapping from tag names to index within m_formal_data:
	typedef std::map<const CachedString*, unsigned int, Comparator,
			 Allocator<std::pair<const CachedString*,
					     unsigned int> > > FormalMap;
	FormalMap m_formal_index;

	bool m_has_dots;  // True if formals include "..."

	struct SuppliedData {
	    const Symbol* tag;
	    RObject* value;
	    FormalMap::const_iterator fm_iter;
	    unsigned int index;
	};

	// Data relating to supplied arguments that have not yet been
	// matched.  Empty except during the operation of match().
	typedef std::list<SuppliedData, Allocator<SuppliedData> > SuppliedList;

	// Coerce a tag that is not already a Symbol into Symbol form:
	static const Symbol* coerceTag(const RObject* tag);

	// Turn remaining arguments, if any, into a DottedArgs object
	// bound to '...'.  Leave supplied_list empty.
	static void handleDots(Frame* frame, SuppliedList* supplied_list);

	// Return true if 'shorter' is a prefix of 'longer', or is
	// identical to 'longer':
	static bool isPrefix(const CachedString* shorter,
			     const CachedString* longer);

	// Create a Binding in the Frame of target_env for the Symbol
	// in fdata, setting its Origin and applying default value
	// appropriately.  Default values are wrapped in Promises
	// keyed to target_env.
	void makeBinding(Environment* target_env, const FormalData& fdata,
			 RObject* supplied_value) const;

	// Raise an error because there are unused supplied arguments,
	// as indicated in supplied_list.
	static void unusedArgsError(const SuppliedList& supplied_list);
    };
}

#endif  // ARGMATCHER_HPP
