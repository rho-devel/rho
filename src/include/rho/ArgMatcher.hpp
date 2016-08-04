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

/** @file ArgMatcher.hpp
 * @brief Class rho::ArgMatcher.
 */

#ifndef ARGMATCHER_HPP
#define ARGMATCHER_HPP 1

#include <list>
#include <map>
#include <vector>
#include "boost/range.hpp"
#include "rho/ArgList.hpp"
#include "rho/Frame.hpp"
#include "rho/Promise.hpp"
#include "rho/String.hpp"

namespace rho {
    class ArgMatchInfo;
    class Environment;
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
     *
     * The class also provides other services relating to the formal
     * arguments and their default values.
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

	/** @brief Constructor.
	 *
	 * @param arg_names The names of the formal parameters, possibly empty.
	 *          The elements of this list must be distinct and at most one
	 *          can be "...".
	 *
	 * The constructed argument matcher does not supply default values.
	 */
	explicit ArgMatcher(std::initializer_list<const char*> arg_names)
	    : ArgMatcher(makePairList(arg_names)) { }

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
            return m_dots_position >= 0;
	}

	/** @brief Create ArgMatcher with specified formal argument names.
	 *
	 * This function creates an ArgMatcher object to match a
	 * sequence of formal arguments whose names are given by
	 * Symbols supplied as arguments to the function.  The
	 * function makes no provision for default values to be
	 * supplied for the formals.
	 *
	 * If any parameter to the function is a null, this signifies
	 * that there are not that many formal arguments, and all
	 * subsequent parameters must also be null.
	 *
	 * @param fml1 Pointer to the Symbol representing the first
	 *          formal argument.  (It can be null, but this would
	 *          probably make the created ArgMatcher pointless.)
	 *
	 * @param fml2 Pointer to the Symbol representing the second
	 *          formal argument, or a null pointer.
	 *
	 * @param fml3 Pointer to the Symbol representing the third
	 *          formal argument, or a null pointer.
	 *
	 * @param fml4 Pointer to the Symbol representing the fourth
	 *          formal argument, or a null pointer.
	 *
	 * @param fml5 Pointer to the Symbol representing the fifth
	 *          formal argument, or a null pointer.
	 *
	 * @param fml6 Pointer to the Symbol representing the sixth
	 *          formal argument, or a null pointer.
	 *
	 * @return Pointer to a newly create ArgMatcher object to
	 * match the specified formals.
	 */
	static ArgMatcher* make(Symbol* fml1, Symbol* fml2 = nullptr,
				Symbol* fml3 = nullptr, Symbol* fml4 = nullptr,
				Symbol* fml5 = nullptr, Symbol* fml6 = nullptr);

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
	 * @param target_env Non-null pointer to the Environment in
	 *          whose Frame bindings will be inserted as a result
	 *          of the argument matching process.  Any default
	 *          arguments used will be wrapped in Promise objects
	 *          keyed to this Environment.
	 *
	 * @param supplied Non-null pointer to the ArgList containing
	 *          the supplied arguments, which must have had
	 *          ArgList::wrapInPromises() applied.
	 */
	void match(Environment* target_env, const ArgList* supplied) const;

        void match(Environment* target_env, const ArgList* supplied,
		   const ArgMatchInfo* matching) const;

	/** @brief Number of formal arguments.
	 *
	 * @return the number of formal arguments. '<tt>...</tt>' is
	 * counted as a single argument.
	 */
	size_t numFormals() const
	{
	    return m_formal_data.size();
	}

	/** @brief Store information required to match arguments quickly.
	 *
	 * This function runs the normal argument matching algorithm, but
	 * instead of creating bindings in a frame, it generates an object
	 * summarizing how the matching is done.  This can be passed to future
	 * calls to match() that use arglist with the same tags as \a args,
	 * resulting in dramatically faster matching.
	 *
	 * This function returns nullptr if the matching can't be cached (e.g.
	 * because \a args contains <tt>...</tt> and throws an error if the
	 * arguments don't match this matcher's formals.
	 *
	 * @param args An arglist with the pattern of tags to match against.
	 *           The values of the arguments are ignored.
	 */
	const ArgMatchInfo* createMatchInfo(const ArgList* args) const;

	/** @brief Copy formal bindings from one Environment to another.
	 *
	 * This function is used in dispatching S4 methods to create
	 * the working environment for the method.  In the
	 * function, \a fromenv points to an Environment which must
	 * contain a Binding for every formal argument of this
	 * ArgMatcher.  \a toenv points to another Environment (which
	 * will become the working environment of the method).
	 *
	 * The function works through the Bindings in \a fromenv in
	 * turn.  If a Binding has Origin EXPLICIT the function will
	 * simply create a copy of the Binding in \a toenv.
	 *
	 * If a Binding has Origin DEFAULTED, it is treated as if it had
	 * Origin MISSING, as described next.
	 *
	 * If a Binding has Origin MISSING, then it is handled in the
	 * same way as a missing argument in ordinary argument
	 * matching.  That is to say, if this ArgMatcher provides a
	 * default for the formal argument in question, that default
	 * is used in created a Binding in \a toenv.  (The value of
	 * this Binding will be a Promise to be evaluated in \a
	 * toenv.)  Otherwise the Binding created in \a toenv will
	 * have Origin MISSING and value Symbol::missingArgument().
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
	 * ArgMatcher, it is at present undefined whether or not these
	 * Bindings are replaced by those propagated from \a fromenv.
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
	void visitReferents(const_visitor* v) const override;
        void applyToCoalescedReferences(std::function<void(const GCNode*)> fun) const override;

	class MatchCallback;
    protected:
	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	static bool s_warn_on_partial_match;

	struct FormalData {
	    const Symbol* symbol;
	    bool follows_dots;  // true if ... occurs earlier in the
				// formals list.
	    RObject* value;
	    unsigned int index;
	};

	enum MatchStatus {UNMATCHED = 0, EXACT_TAG, PARTIAL_TAG, POSITIONAL};

	const PairList* m_formals;

	// Data on formals (including "...") in order of occurrence:
	typedef std::vector<FormalData, Allocator<FormalData> > FormalVector;
	FormalVector m_formal_data;

	struct Comparator {
	    bool operator()(const String* l, const String* r) const
	    {
		assert(l);
		assert(r);
		return l->stdstring() < r->stdstring();
	    }
	};

	// Mapping from tag names to index within m_formal_data:
	// Doesn't include '...'.
	typedef std::map<const String*, unsigned int, Comparator,
			 Allocator<std::pair<const String*,
					     unsigned int> > > FormalMap;
	FormalMap m_formal_index;

	int m_dots_position;  // -1 if formals doesn't include "..."

	struct SuppliedData {
	    const Symbol* tag;
	    RObject* value;
	    FormalMap::const_iterator fm_iter;
	    unsigned int index;
	};

	// Data relating to supplied arguments that have not yet been
	// matched.
	typedef std::list<SuppliedData, Allocator<SuppliedData> > SuppliedList;

        void match(const ArgList* supplied, MatchCallback* callback) const;
        void matchWithCache(const ArgList* supplied, MatchCallback* callback,
			    const ArgMatchInfo* matching) const;

	// Return true if 'shorter' is a prefix of 'longer', or is
	// identical to 'longer':
	static bool isPrefix(const String* shorter, const String* longer);

	// Create a Binding in the Frame of target_env for the Symbol
	// in fdata, setting its Origin and applying default value
	// appropriately.  Default values are wrapped in Promises
	// keyed to target_env.
	static void makeBinding(Environment* target_env, const FormalData& fdata,
				Frame::Binding::Origin origin,
				RObject* supplied_value);

	// Raise an error because there are unused supplied arguments,
	// as indicated in supplied_list.
	static void unusedArgsError(const SuppliedList& supplied_list);

	static PairList* makePairList(
	    std::initializer_list<const char*> arg_names);
    };

    class ArgMatchInfo {
    public:
	ArgMatchInfo(int num_formals, const PairList* args);

	bool arglistTagsMatch(const PairList* args) const;

	int getSindex(int findex) const {
	    return m_values[findex];
	}

	boost::iterator_range<std::vector<int>::const_iterator> getDotArgs() const {
	    return boost::make_iterator_range(m_values.begin() + m_num_formals,
					      m_values.end());
	}

	bool operator==(const ArgMatchInfo& other) const {
	    return (m_num_formals == other.m_num_formals
		    && m_values == other.m_values
		    && m_tags == other.m_tags);
	}

	struct Hash;

	// TODO: This should be private.
	int m_num_formals;
	std::vector<int> m_values;
	std::vector<const Symbol*> m_tags;

    	// TODO: ArgMatchers and ArgLists are good candidates
    	//   for interning, which would likely save significant memory.
    };
}


#endif  // ARGMATCHER_HPP
