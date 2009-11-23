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
     *
     * @todo Move R_warn_partial_match_args functionality into this class.
     */
    class ArgMatcher : public GCNode {
    public:
	/** @brief Constructor.
	 *
	 * @param formals PairList of formal arguments, possibly
	 *          empty.  The elements of this list must have
	 *          distinct Symbol objects as their tags.  At most
	 *          one element may have '...' as its tag.
	 *
	 * @todo Try to change the type of \a formals to <tt>const
	 * PairList*</tt> in due course.
	 */
	explicit ArgMatcher(PairList* formals);

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
	 *          a tag, the tag must be either a Symbol, a
	 *          CachedString, or a StringVector with at least one
	 *          element.  In the last case (StringVector), only
	 *          the first element is considered, and it is
	 *          translated into the current locale if necessary.
	 *
	 * @param supplieds_env If non-null, then each supplied
	 *          argument is wrapped inside a Promise to be
	 *          evaluated within \a supplieds_env.
	 *
	 * @todo Try to change the type of \a supplied to <tt>const
	 * PairList*</tt> in due course.
	 */
	void match(Environment* target_env, PairList* supplied,
		   Environment* supplieds_env);

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Virtual function of GCNode:
	void detachReferents();
    private:
	struct FormalData {
	    Symbol* symbol;
	    bool follows_dots;  // true if ... occurs earlier in the
				// formals list.
	    RObject* value;
	};

	enum MatchStatus {UNMATCHED = 0, EXACT_TAG, PARTIAL_TAG, POSITIONAL};

	GCEdge<PairList> m_formals;

	// Data on formals (other than "...") in order of occurrence:
	typedef std::vector<FormalData, Allocator<FormalData> > FormalVector;
	FormalVector m_formal_data;

	// ***** FIXME Allocator *****
	// Mapping from tag names to index within m_formal_data:
	typedef std::map<const CachedString*, unsigned int,
			 String::Comparator> FormalMap;
	FormalMap m_formal_index;

	bool m_has_dots;  // True if formals include "..."

	struct SuppliedData {
	    GCEdge<CachedString> name;
	    GCEdge<> value;
	    FormalMap::const_iterator fm_iter;
	    unsigned int index;
	};

	// Data relating to supplied arguments that have not yet been
	// matched.  Empty except during the operation of match().
	typedef std::list<SuppliedData, Allocator<SuppliedData> > SuppliedList;
	SuppliedList m_supplied_list;

	// Turn remaining arguments, if any, into a DottedArgs object
	// bound to '...'.  Leave m_supplied_list empty.
	void handleDots(Frame* frame);

	// Return true if 'shorter' is a prefix of 'longer', or is
	// identical to 'longer':
	static bool isPrefix(const CachedString* shorter,
			     const CachedString* longer);

	// Create a Binding in the Frame of target_env for the Symbol
	// in fdata, setting its Origin and applying default value
	// appropriately.  Default values are wrapped in Promises
	// keyed to target_env.
	void makeBinding(Environment* target_env, const FormalData& fdata,
			 RObject* supplied_value);

	// Convert tag to CachedString, raising error if not convertible.
	static CachedString* tag2cs(RObject* tag);

	// Raise an error because there are unused supplied arguments,
	// as indicated in m_supplied_list.  Leave m_supplied_list empty.
	void unusedArgsError();
    };
}

#endif  // ARGMATCHER_HPP
