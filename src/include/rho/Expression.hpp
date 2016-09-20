/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file Expression.h
 * @brief Class rho::Expression and associated C interface.
 */

#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "rho/FunctionBase.hpp"
#include "rho/PairList.hpp"

namespace rho {
    class ArgList;
    class ArgMatchInfo;
    class BuiltInFunction;
    class Closure;
    class Frame;
    class FunctionBase;

    /** @brief Singly linked list representing an R expression.
     *
     * R expression, represented as a LISP-like singly-linked list,
     * each element containing pointers to a 'car' object and to a
     * 'tag' object, as well as a pointer to the next element of the
     * list.  (Any of these pointers may be null.)  A Expression
     * object is considered to 'own' its car, its tag, and all its
     * successors.
     *
     * Most expressions should be represented using the CachingExpression class,
     * instead of this, as it has better performance.  This class is primarily
     * useful for expressions that are only evaluated once, where the function
     * is known to be a primitive and for SET_TYPEOF.
     */
    class Expression : public ConsCell {
    public:
	/**
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 *
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 *
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit Expression(RObject* cr = nullptr, PairList* tl = nullptr,
			    const RObject* tg = nullptr)
	    : ConsCell(LANGSXP, cr, tl, tg)
	{}

	Expression(RObject* function,
		   std::initializer_list<RObject*> unnamed_args);

	/** @brief Copy constructor.
	 *
	 * @param pattern Expression to be copied.
	 */
	Expression(const Expression& pattern) = default;

        const RObject* getFunction() const {
            return car();
        }

        const PairList* getArgs() const {
            return tail();
        }

	void check1arg(const char* formal) const;

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "language";
	}

        // Used by jit/RuntimeImpl.cpp and Expression.cpp
        RObject* evaluateFunctionCall(const FunctionBase* func,
                                      Environment* env,
                                      ArgList* raw_arglist) const;

        // Used by Expression.cpp, engine.cpp (with evaluated arguments),
        // objects.cpp (with promised arguments) and DotInternal.cpp
	RObject* applyBuiltIn(const BuiltInFunction* func,
                              Environment* env,
                              ArgList* arglist) const;

	/** @brief Invoke the function.
	 *
	 * This differs from apply() in that it is assumed that any
	 * required wrapping of the function arguments in Promise
	 * objects will have been carried out before invoke() is
	 * called, whereas apply() carries out this wrapping itself.
	 *
	 * @param env Non-null pointer to the Environment in which the
	 *          function is to be evaluated.
	 *
	 * @param arglist Non-null pointer to the
	 *          ArgList containing the arguments with which the
	 *          function is to be invoked.
	 *
	 * @param call Pointer to the Expression calling the function.
	 *
	 * @param method_bindings This pointer will be non-null if and
	 *          only if this invocation represents a method call,
	 *          in which case it points to a Frame containing
	 *          Bindings that should be added to the working
	 *          environment, for example bindings of the Symbols
	 *          \c .Generic and \c .Class.
	 *
	 * @return The result of applying the function.
         *
         * @note This function is used mainly by code that implements method
         *         dispatch.
	 */
        RObject* invokeClosure(const Closure* func,
                               Environment* calling_env,
                               ArgList* arglist,
                               const Frame* method_bindings = nullptr) const;

	// Virtual functions of RObject:
	Expression* clone() const override;
        RObject* evaluate(Environment* env) override;
	const char* typeName() const override;

    protected:
	// Declared protected to ensure that Expression objects are
	// allocated only using 'new':
	~Expression() {}

        FunctionBase* getFunction(Environment* env) const;

        RObject* evaluateClosureCall(const Closure* func,
				     Environment* env,
                                     ArgList* arglist,
                                     const Frame* method_bindings) const;

        RObject* invokeClosureImpl(const Closure* func,
                                   Environment* calling_env,
                                   ArgList* arglist,
                                   const Frame* method_bindings) const;

	RObject* evaluateBuiltInCall(const BuiltInFunction* func,
                                     Environment* env,
                                     ArgList* arglist) const;

	RObject* evaluateDirectBuiltInCall(const BuiltInFunction* func,
                                           Environment* env,
                                           ArgList* arglist) const;

	RObject* evaluateIndirectBuiltInCall(const BuiltInFunction* func,
                                            Environment* env,
                                            ArgList* arglist) const;

	template<typename... Args>
	RObject* evaluateBuiltInWithEvaluatedArgs(const BuiltInFunction*,
						  Environment*,
						  const PairList*,
						  Args...) const;
	template<typename... Args>
	RObject* evaluateFixedArityBuiltIn(const BuiltInFunction*,
					   Environment*,
					   const PairList*,
					   bool evaluated,
					   Args...) const;
	RObject* evaluateFixedArityBuiltIn(const BuiltInFunction*,
					   Environment*, ArgList*) const;

        static void importMethodBindings(const Frame* method_bindings,
                                         Frame* newframe);
        static Environment* getMethodCallingEnv();

	virtual void matchArgsIntoEnvironment(const Closure* func,
					      Environment* calling_env,
					      ArgList* arglist,
					      Environment* execution_env) const;

	Expression& operator=(const Expression&) = delete;
    };

    /** @brief Singly linked list representing an R expression.
     *
     * R expression, represented as a LISP-like singly-linked list,
     * each element containing pointers to a 'car' object and to a
     * 'tag' object, as well as a pointer to the next element of the
     * list.  (Any of these pointers may be null.)  A Expression
     * object is considered to 'own' its car, its tag, and all its
     * successors.
     *
     * Unlike the regular Expression class, this class caches the results of
     * parameter matching to closure calls for improved performance.
     */
    class CachingExpression : public Expression {
    public:
	/**
	 * @param cr Pointer to the 'car' of the element to be
	 *           constructed.
	 *
	 * @param tl Pointer to the 'tail' (LISP cdr) of the element
	 *           to be constructed.
	 *
	 * @param tg Pointer to the 'tag' of the element to be constructed.
	 */
	explicit CachingExpression(RObject* cr = nullptr,
				   PairList* tl = nullptr,
				   const RObject* tg = nullptr)
	    : Expression(cr, tl, tg)
	{}

	CachingExpression(RObject* function,
			  std::initializer_list<RObject*> unnamed_args);

	/** @brief Copy constructor.
	 *
	 * @param pattern CachingExpression to be copied.
	 */
	CachingExpression(const CachingExpression& pattern)
	    : Expression(pattern) {
	    // Don't copy the cache, as the new expression may be about to get
	    // modified.
	    // TODO: is there a way we can automatically detect modifications
	    //   of *this and invalidate the cache?
	}

	CachingExpression(const Expression& pattern) : Expression(pattern)
	{}

	// Virtual functions of RObject:
	CachingExpression* clone() const override;

	// For GCNode
	void visitReferents(const_visitor* v) const override;
    protected:
	void detachReferents() override;
    private:
	// Object used for recording details from previous evaluations of
	// this expression, for the purpose of optimizing future evaluations.
	// In the future, this will likely include type recording as well.
        mutable struct {
	    GCEdge<const FunctionBase> m_function;
            const ArgMatchInfo* m_arg_match_info;
	} m_cache;

	void matchArgsIntoEnvironment(const Closure* func,
				      Environment* calling_env,
				      ArgList* arglist,
				      Environment* execution_env)
	    const override;

	// Declared private to ensure that CachingExpression objects are
	// allocated only using 'new':
	~CachingExpression() {}

	CachingExpression& operator=(const CachingExpression&) = delete;
    };

} // namespace rho

/** @brief Pointer to expression currently being evaluated.
 */
extern rho::GCRoot<> R_CurrentExpr;

/** @brief Expression currently being evaluated.
 *
 * @return Pointer to the Expression currently being evaluated.
 */
SEXP Rf_currentExpression();

/** @brief Designate the Expression currently being evaluated.
 *
 * @param e Pointer to the Expression now to be evaluated.  (Not
 *          currently checked in any way.)
 */
void Rf_setCurrentExpression(SEXP e);

#endif /* EXPRESSION_H */
