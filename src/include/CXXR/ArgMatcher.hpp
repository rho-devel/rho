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
	 * @param defaults_env If the \a formals list contains default
	 *          values for any arguments, the default values will
	 *          be evaluated in the Environment pointed to by \a
	 *          defaults_env.  \a defaults_env may be a null
	 *          pointer only if there are no default arguments.
	 *
	 * @todo Try to change the type of \a formals to <tt>const
	 * PairList*</tt> in due course.
	 */
	explicit ArgMatcher(PairList* formals,
			    Environment* defaults_env);

	/** @brief Match formal and supplied arguments.
	 *
	 * Argument matching is carried out as described in Sec. 4.3.2
	 * of the 'R Language Definition' document.
	 *
	 * @param frame Pointer to the Frame in which bindings will be
	 *          inserted as a result of the argument matching
	 *          process.
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
	 * @todo Try to change the type of \a supplied to <tt>const
	 * PairList*</tt> in due course.
	 */
	void match(Frame* frame, PairList* supplied);

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
	GCEdge<Environment> m_defaults_env;

	// Data on formals in order of occurrence:
	typedef std::vector<FormalData, Allocator<FormalData> > FormalVector;
	FormalVector m_formal_data;

	// Mapping from tag names to index within m_formal_data:
	typedef std::map<const CachedString*, unsigned int,
			 String::Comparator> FormalMap;
	FormalMap m_formal_index;

	// Create a Binding in frame for the Symbol in fdata, setting
	// its Origin and apply default value appropriately:
	void makeBinding(Frame* frame, const FormalData& fdata,
			 RObject* supplied_value);

	// Convert tag to CachedString, raising error if not convertible.
	static const CachedString* tag2cs(RObject* tag);
    };
}

#endif  // ARGMATCHER_HPP
