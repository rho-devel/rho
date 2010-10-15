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

#ifndef S3LAUNCHER_HPP
#define S3LAUNCHER_HPP 1

#include "CXXR/GCNode.hpp"

#include "CXXR/StringVector.h"

namespace CXXR {
    class Environment;
    class FunctionBase;
    class Symbol;

    /** @brief Class used to prepare a call to an S3 method.
     */
    class S3Launcher : public GCNode {
    public:
	static S3Launcher*
	create(RObject* object, std::string generic, std::string group,
	       Environment* call_env, Environment* table_env,
	       bool allow_default);

	/** @brief Search for an S3 method.
	 *
	 * This function searches for a definition of an S3 method
	 * bound to \a symbol.
	 *
	 * The search is first carried out in \a call_env and its
	 * enclosing environments, as in findFunction().
	 *
	 * If this fails to find a binding of \a symbol to a
	 * FunctionBase, the search then determines whether the Frame
	 * of \a table_env defines an S3 methods table, that is to
	 * say, whether it contains a Binding of the symbol
	 * <tt>.__S3MethodsTable__.</tt> to an Environment.  If so, a
	 * Binding of \a symbol is sought in the Frame of that
	 * Environment (i.e. the Environment bound to
	 * <tt>.__S3MethodsTable__.</tt>).  If the value of the
	 * Binding is a Promise, the Promise is forced and the result
	 * of evaluating the Promise is used as part of the returned
	 * value.
	 *
	 * If, in the course of searching for a suitable Binding, a
	 * Binding of \a symbol to Symbol::missingArgument()
	 * (R_MissingArg) is encountered, an error is raised.
	 *
	 * Read/write monitors are invoked in the following
	 * circumstances: (i) If a Promise is forced, any read monitor
	 * for the relevant Binding is called before forcing it, and
	 * any write monitor for the symbol's Binding is called
	 * immediately afterwards.  (ii) If this function succeeds in
	 * finding a Binding to a FunctionBase, then any read monitor
	 * for that Binding is called.
	 *
	 * @param symbol Non-null pointer to the Symbol for which a
	 *          Binding is sought.  In practice the name of the Symbol
	 *          will be of an articulated form such as
	 *          <tt>print.lm</tt> or <tt>summary.default</tt>;
	 *          however, the function neither checks nor relies on
	 *          this.
	 *
	 * @param call_env Non-null pointer to the Environment in which
	 *          the search for a Binding is first carried out.  In
	 *          practice this will be the call Environment of the S3
	 *          generic.
	 *
	 * @param table_env Non-null pointer to the Environment in whose
	 *          Frame an S3 methods table (i.e. an Environment bound to
	 *          <tt>.__S3MethodsTable__.</tt>) is sought, if no method
	 *          was found in \a call_env.
	 *
	 * @return If a Binding to a FunctionBase was found, the first
	 * element of the pair is the value of the Binding (except
	 * that if the value was a Promise, the first element is the
	 * result of evaluating the Promise) and the second element of
	 * the pair is \c true if the Binding was found by searching
	 * in \a call_env, and \c false if it was found in the S3
	 * methods table.  If no Binding to a FunctionBase was found,
	 * the first element of the pair will be null and the second
	 * element <tt>false</tt>.
	 *
	 * @note An S3 methods table is currently sought only within
	 * the Frame of \a table_env itself; the search does not
	 * proceed to enclosing Environments.  This follows CR, but it
	 * perhaps unduly restrictive.
	 */
	static std::pair<FunctionBase*, bool>
	findMethod(const Symbol* symbol, Environment* call_env,
		   Environment* table_env);

	bool usingClass() const
	{
	    return m_index < m_classes->size();
	}

	// Data members, to be private in due course:
	GCEdge<StringVector> m_classes;  // Pointer to a vector of
	  // class names, or a null pointer.  If null, subsequent
	  // fields are not meaningful.
	GCEdge<FunctionBase> m_function;  // Pointer to the method found, or
	  // null if no method was found.  If null, subsequent fields
	  // are not meaningful.
	GCEdge<Symbol> m_symbol;  // Pointer to the Symbol naming the
	  // method found.  
	size_t m_index;  // Location within the classes vector to which
	  // 'function' corresponds, or one past the end if using a
	  // default method.
	bool m_using_group;  // True iff 'function' is a group method.
	
	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Virtual function of GCNode:
	void detachReferents();
    private:
	S3Launcher()
	    : m_using_group(false)
	{}
    };
}

#endif  // S3LAUNCHER_HPP
