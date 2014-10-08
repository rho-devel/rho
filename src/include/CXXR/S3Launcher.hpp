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

/** @file ArgMatcher.hpp
 * @brief Class CXXR::ArgMatcher.
 */

#ifndef S3LAUNCHER_HPP
#define S3LAUNCHER_HPP 1

#include "CXXR/GCNode.hpp"

#include "CXXR/StringVector.h"

namespace CXXR {
    class Environment;
    class Frame;
    class FunctionBase;
    class Symbol;

    /** @brief Class to select and call to S3 methods.
     *
     * This class provides facilities for selecting an S3 method to
     * call according to the object to be dispatched on, and for
     * setting up the call to the S3 method.
     */
    class S3Launcher : public GCNode {
    public:
	/** @brief Add the special S3 method bindings to a Frame.
	 *
	 * This function adds to \a frame the special bindings of
	 * <tt>.Class</tt>, <tt>.Generic</tt>,
	 * <tt>.GenericCallEnv</tt>, <tt>.GenericDefEnv</tt>,
	 * <tt>.Method</tt> and where appropriate <tt>.Group</tt>
	 * needed for an S3 method call.
	 *
	 * Beware that the resulting binding of <tt>.Method</tt> will
	 * not be appropriate for methods of the <tt>Ops</tt> group.
	 *
	 * @param frame Non-null pointer to the Frame to which
	 *          Bindings are to be added.
	 */
	void addMethodBindings(Frame* frame) const;

	/** @brief Vector of classes.
	 *
	 * @return Pointer to the vector of classes associated with
	 * the dispatch object.
	 */
	const StringVector* classes() const
	{
	    return m_classes;
	}

	/** @brief Name of method's class
	 *
	 * @return pointer to the name of the class to which this
	 * method corresponds, or a null pointer if no class-specific
	 * method was found.
	 */
	String* className() const
	{
	    if (!usingClass())
		return 0;
	    return (*m_classes)[m_index];
	}

	/** @brief Attempt to create an S3Launcher object.
	 *
	 * @param object Pointer, possibly null, to the object to be
	 *          dispatched on.  The search for a method will work
	 *          in turn through this object's vector of classes
	 *          until a method is found.  (This vector will be
	 *          empty if \a object is null.)
	 *
	 * @param generic Name of the generic function for which a
	 *          method is sought.
	 *
	 * @param group Name of the function group for which a method
	 *          is sought.  Within each <tt>class</tt> in the
	 *          classes vector, the function first looks for a
	 *          function called <em>generic</em><tt>.class</tt>
	 *          and then, if there is none, for a function called
	 *          <em>group</em><tt>.class</tt>, before moving on if
	 *          necessary to the next element of the classes
	 *          vector.  \a group may be an empty string, in which
	 *          case group methods are not considered.
	 *
	 * @param call_env Non-null pointer to the Environment in which
	 *          the search for a Binding is first carried out.  In
	 *          practice this will be the call Environment of the S3
	 *          generic.
	 *
	 * @param table_env Pointer, possibly null, to the Environment
	 *          in whose Frame an S3 methods table (i.e. an
	 *          Environment bound to <tt>.__S3MethodsTable__.</tt>)
	 *          is sought, if no method was found in \a call_env.
	 *          If \a table_env is null, no S3 methods table is sought.
	 *
	 * @param allow_default true iff the function should look for
	 *          a function called
	 *          <em>generic</em><tt>.default</tt> if no
	 *          class-specific method was found.
	 *
	 * @return A null pointer if no suitable S3 method was found,
	 * or a pointer to an S3Launcher object for the method found.
	 */
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
	 * FunctionBase, and \a table_env is not null, the search then
	 * determines whether the Frame of \a table_env defines an S3
	 * methods table, that is to say, whether it contains a
	 * Binding of the symbol <tt>.__S3MethodsTable__.</tt> to an
	 * Environment.  If so, a Binding of \a symbol is sought in
	 * the Frame of that Environment (i.e. the Environment bound
	 * to <tt>.__S3MethodsTable__.</tt>).  If the value of the
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
	 * @param table_env Pointer, possibly null, to the Environment
	 *          in whose Frame an S3 methods table (i.e. an
	 *          Environment bound to <tt>.__S3MethodsTable__.</tt>)
	 *          is sought, if no method was found in \a call_env.
	 *          If \a table_env is null, no S3 methods table is sought.
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

	/** @brief Function implementing the method.
	 *
	 * @return Pointer to the function implementing the method
	 * found.
	 */
	FunctionBase* function() const
	{
	    return m_function;
	}

	/** @brief Class for which method was found.
	 *
	 * @return the location (counting from zero) within the
	 * classes() vector to which this S3 method corresponds.
	 */
	std::size_t locInClasses() const
	{
	    return m_index;
	}

	/** @brief Method name as Symbol.
	 *
	 * @return pointer to the Symbol containing the name of the
	 * method found.
	 */
	Symbol* symbol() const
	{
	    return m_symbol;
	}

	/** @brief Was a class-specific method found?
	 *
	 * @return true iff the method found corresponds to a specific
	 * class (whose name will be given by className() ) rather
	 * than being a default method.
	 */
	bool usingClass() const
	{
	    return m_index < m_classes->size();
	}

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const override;
    protected:
	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	std::string m_generic;
	std::string m_group;
	GCEdge<Environment> m_call_env;
	GCEdge<Environment> m_table_env;
	GCEdge<StringVector> m_classes;  // Pointer to a vector of
	  // class names, or a null pointer.  If null, subsequent
	  // fields are not meaningful.
	GCEdge<FunctionBase> m_function;  // Pointer to the method found, or
	  // null if no method was found.  If null, subsequent fields
	  // are not meaningful.
	Symbol* m_symbol;  // Pointer to the Symbol naming the method found.  
	std::size_t m_index;  // Location within the classes vector to which
	  // 'function' corresponds, or one past the end if using a
	  // default method.
	bool m_using_group;  // True iff 'function' is a group method.

	S3Launcher(const std::string& generic, const std::string& group,
		   Environment* call_env, Environment* table_env)
	    : m_generic(generic), m_group(group), m_call_env(call_env),
	      m_table_env(table_env), m_using_group(false)
	{}
    };
}

#endif  // S3LAUNCHER_HPP
